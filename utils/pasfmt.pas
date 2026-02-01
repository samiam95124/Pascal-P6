{*******************************************************************************
*                                                                              *
*                         PASCALINE SOURCE FORMATTER                           *
*                                                                              *
* LICENSING:                                                                   *
*                                                                              *
* Copyright (c) 2024, Scott A. Franco                                          *
* All rights reserved.                                                         *
*                                                                              *
* Redistribution and use in source and binary forms, with or without           *
* modification, are permitted provided that the following conditions are met:  *
*                                                                              *
* 1. Redistributions of source code must retain the above copyright notice,    *
*    this list of conditions and the following disclaimer.                     *
* 2. Redistributions in binary form must reproduce the above copyright         *
*    notice, this list of conditions and the following disclaimer in the       *
*    documentation and/or other materials provided with the distribution.      *
*                                                                              *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   *
* POSSIBILITY OF SUCH DAMAGE.                                                  *
*                                                                              *
*                     Pascaline Source Code Formatter                          *
*                     *******************************                          *
*                                                                              *
* Formats Pascaline source code according to the coding standard.              *
* Derived from parse.pas with error recovery intact for robust handling        *
* of incorrect input.                                                          *
*                                                                              *
* Usage: pasfmt [options] <inputfile>                                          *
*                                                                              *
* Converts inputfile.pas to inputfile.pas.fmt                                  *
*                                                                              *
* Options:                                                                     *
*   -h, --help           Display help message                                  *
*   -l, --list           List source lines while processing                    *
*   -s, --iso7185        Format as ISO 7185 Pascal (not Pascaline)             *
*   -e, --experr         Show expanded error messages (default on)             *
*   -c, --columns <n>    Set wrap column (default 80)                          *
*   -b, --bracketbreak   Add blank lines around begin/end, const, etc.         *
*   -f, --flushleft      Flush left for multi-line definitions                 *
*   -z, --columnize      Align = signs in const/type/var blocks                *
*   -n, --sourceline     Add original source line numbers as comments          *
*   -r, --noerror        Suppress error output (keep error count)             *
*                                                                              *
* Instruction File:                                                            *
*                                                                              *
* If inputfile.fmt exists, it is read as an instruction file. The format is:  *
*                                                                              *
*   command [param]                                                            *
*   ! comment                                                                  *
*                                                                              *
* Commands (same as command line options):                                     *
*   list or l           - List source lines while processing                   *
*   iso7185 or s        - Format as ISO 7185 Pascal (not Pascaline)            *
*   experr or e         - Show expanded error descriptions                     *
*   columns or c <n>    - Set wrap column (default 80)                         *
*   bracketbreak or b   - Add blank lines around begin/end, const, etc.        *
*   flushleft or f      - Flush left for multi-line definitions                *
*   columnize or z      - Align = signs in const/type/var blocks               *
*   sourceline or n     - Add original source line numbers as comments         *
*   noerror or r        - Suppress error output (keep error count)            *
*                                                                              *
* Comments start with '!' and continue to end of line. They can appear at the  *
* start of a line or after commands.                                           *
*                                                                              *
*******************************************************************************}

program pasfmt(output, command);

joins parse, services;

uses strings;

label 99; { terminate immediately }

const
    { maximum lengths }
    maxids   = 250;   { maximum characters in id string }
    maxlinp  = 2000;  { maximum line length }
    cmdmax   = 250;   { maximum command line length }
    maxres   = 66;    { number of reserved words }
    reslen   = 9;     { maximum length of reserved words }
    fillen   = maxids;
    lindent  = 4;     { spaces per indent level }
    strglgth = 2000;  { string buffer length }
    maxftl   = 519;   { maximum fatal error number }

type
    { symbol types - complete set from parse.pas }
    symbol = (ident, intconst, realconst, stringconst, notsy, mulop, addop,
              relop, lparent, rparent, lbrack, rbrack, comma, semicolon,
              period, arrow, colon, becomes, range, labelsy, constsy, typesy,
              varsy, funcsy, progsy, procsy, setsy, packedsy, arraysy,
              recordsy, filesy, beginsy, ifsy, casesy, repeatsy, whilesy,
              forsy, withsy, gotosy, endsy, elsesy, untilsy, ofsy, dosy,
              tosy, downtosy, thensy, nilsy, forwardsy, modulesy, usessy,
              privatesy, externalsy, viewsy, fixedsy, processsy, monitorsy,
              sharesy, classsy, issy, overloadsy, overridesy, referencesy,
              joinssy, staticsy, inheritedsy, selfsy, virtualsy, trysy,
              exceptsy, extendssy, onsy, resultsy, operatorsy, outsy,
              propertysy, channelsy, streamsy, othersy, hexsy, octsy, binsy,
              numsy, eofsy);

    operatort = (mul, rdiv, andop, idiv, imod, plus, minus, orop, ltop, leop,
                 geop, gtop, neop, eqop, inop, noop, xorop, notop, bcmop);

    setofsys = set of symbol;

    { character types }
    chtp = (letter, number, special, illegal, chstrquo, chcolon, chperiod,
            chlt, chgt, chlparen, chspace, chlcmt, chrem, chhex, choct, chbin);

    { strings and identifiers }
    idstr = packed array [1..maxids] of char;
    restr = packed array [1..reslen] of char;
    filnam = packed array [1..fillen] of char;
    linbufp = packed array [1..maxlinp] of char;
    csstr = packed array [1..strglgth] of char;

    { error tracking }
    errptr = ^errlin;
    errlin = record { line error tracking }
        next: errptr; { next entry }
        errlinno: integer; { line number }
    end;

var
    { files }
    prd:      text;   { input source file }
    prr:      text;   { output formatted file }

    { scanner state }
    ch:       char;              { current character }
    eol:      boolean;           { end of line flag }
    eofinp:   boolean;           { end of file flag }
    sy:       symbol;            { current symbol }
    op:       operatort;         { current operator }
    id:       idstr;             { current identifier }
    kk:       integer;           { length of identifier }
    ival:     integer;           { integer value }
    rval:     real;              { real value }
    lgth:     integer;           { string length }
    sval:     csstr;             { string value }

    { input buffer }
    inplin:   linbufp;           { input line buffer }
    inppos:   integer;           { position in input line }
    inplen:   integer;           { length of input line }
    lineno:   integer;           { line number }
    chcnt:    integer;           { character count for error position }

    { output state }
    outlin:   linbufp;           { output line buffer }
    outpos:   integer;           { position in output line }
    outlineno: integer;          { source line that started current output line }
    indent:   integer;           { current indent level }
    atbol:    boolean;           { at beginning of line }
    lastsy:   symbol;            { last symbol output }

    { comment buffer }
    cmtbuf:   linbufp;           { comment buffer }
    cmtlen:   integer;           { comment length }
    hascmt:   boolean;           { has pending comment }
    cmtlncmt: boolean;           { is a line comment }

    { error tracking }
    toterr:   integer;           { total errors }
    errinx:   0..10;             { number of errors on current line }
    errlist:  array [1..10] of packed record
                  pos: integer;
                  nmr: 1..maxftl
              end;
    errtbl:   array [1..maxftl] of integer; { error occurrence tracking }
    errltb:   array [1..maxftl] of errptr;  { error line tracking }
    experr:   boolean;           { expanded error descriptions }
    listmode: boolean;           { source listing enabled }
    iso7185:  boolean;           { restrict to ISO 7185 language }

    { formatting options }
    wrapcolumn:   integer;       { wrap column for output lines }
    bracketbreak: boolean;       { add blank lines around structural keywords }
    flushleft:    boolean;       { flush left for multi-line definitions }
    columnize:    boolean;       { align = signs in const/type/var blocks }
    sourceline:   boolean;       { add original source line numbers as comments }
    noerror:      boolean;       { suppress error output (keep error count) }

    { command line parsing }
    cmdhan:   parse.parhan;      { command line parser handle }
    inshan:   parse.parhan;      { instruction file parser handle }

    { reserved words }
    rw:       array [1..maxres] of restr;   { reserved words }
    rsy:      array [1..maxres] of symbol;  { reserved word symbols }
    rop:      array [1..maxres] of operatort; { reserved word operators }

    { character tables }
    chartp:   array [char] of chtp;
    ordint:   array [char] of integer;

    { special symbol tables }
    ssy:      array [char] of symbol;
    sop:      array [char] of operatort;

    { file names }
    srcfil:   filnam;            { source filename }
    dstfil:   filnam;            { destination filename }
    insfil:   filnam;            { instruction filename }
    p, n, e:  filnam;            { path, name, extension components }

    { symbol sets for error recovery }
    constbegsys, simptypebegsys, typebegsys, blockbegsys,
    selectsys, facbegsys, statbegsys, typedels: setofsys;

{******************************************************************************

                              Utility Routines

******************************************************************************}

{ compare reserved word to identifier }
function strequri(a: restr; var b: idstr): boolean;
var m: boolean; i: integer;
begin
    m := true;
    for i := 1 to reslen do
        if lcase(a[i]) <> lcase(b[i]) then m := false;
    for i := reslen + 1 to maxids do
        if b[i] <> ' ' then m := false;
    strequri := m
end;

{ forward declarations for error handling }
procedure wrtsrclin; forward;
procedure endofline; forward;

{******************************************************************************

                              Input Routines

******************************************************************************}

procedure readline;
var i: integer;
begin
    inplen := 0; inppos := 1;
    for i := 1 to maxlinp do inplin[i] := ' ';
    if not eof(prd) then begin
        while not eoln(prd) do begin
            if inplen < maxlinp then begin
                inplen := inplen + 1;
                read(prd, inplin[inplen])
            end else begin
                read(prd, ch) { skip overflow }
            end
        end;
        readln(prd);
        lineno := lineno + 1
    end else
        eofinp := true;
    eol := false;
    if listmode then wrtsrclin
end;

procedure nextch;
begin
    if inppos > inplen then begin
        if eof(prd) then begin
            eofinp := true;
            ch := ' '
        end else begin
            readline;
            eol := true;
            ch := ' '
        end
    end else begin
        ch := inplin[inppos];
        inppos := inppos + 1;
        chcnt := chcnt + 1;
        eol := false
    end
end;

{******************************************************************************

                              Output Routines

******************************************************************************}

procedure flush_line;
var i: integer;
begin
    { clamp outpos to valid range }
    if outpos > maxlinp then outpos := maxlinp;
    { emit source line number as comment if sourceline option enabled }
    if sourceline then write(prr, '{', outlineno:6, '} ');
    if outpos > 0 then begin
        { trim trailing spaces }
        while (outpos > 0) and (outlin[outpos] = ' ') do
            outpos := outpos - 1;
        for i := 1 to outpos do write(prr, outlin[i]);
        writeln(prr)
    end else
        writeln(prr);
    outpos := 0;
    for i := 1 to maxlinp do outlin[i] := ' ';
    atbol := true
end;

procedure emit_char(c: char);
begin
    { check for column wrap before adding character }
    if (wrapcolumn > 0) and (outpos >= wrapcolumn) then
        flush_line;
    if atbol then begin
        { record source line that starts this output line }
        outlineno := lineno;
        if indent > 0 then begin
            { emit indent - check maxlinp to prevent buffer overflow }
            while (outpos < indent * lindent) and (outpos < maxlinp) do begin
                outpos := outpos + 1;
                outlin[outpos] := ' '
            end
        end
    end;
    atbol := false;
    outpos := outpos + 1;
    if outpos <= maxlinp then outlin[outpos] := c
end;

procedure emit_space;
begin
    { check for column wrap - break line at space boundaries }
    if (wrapcolumn > 0) and (outpos >= wrapcolumn) then
        flush_line;
    { note: must check outpos > 0 before accessing outlin[outpos]
      because Pascaline doesn't short-circuit and evaluations }
    if not atbol and (outpos > 0) then
        if (outlin[outpos] <> ' ') and
           (outlin[outpos] <> '(') and (outlin[outpos] <> '[') then begin
            outpos := outpos + 1;
            if outpos <= maxlinp then outlin[outpos] := ' '
        end
end;

procedure emit_newline;
begin
    flush_line
end;

procedure emit_blank_line;
begin
    if not atbol then flush_line;
    if sourceline then write(prr, '{', lineno:6, '} ');
    writeln(prr)
end;

procedure emit_kw(s: restr; len: integer);
var i: integer;
begin
    emit_space;
    { check if keyword would exceed wrap column, wrap first if so }
    if (wrapcolumn > 0) and (outpos + len > wrapcolumn) then
        flush_line;
    for i := 1 to len do emit_char(s[i])
end;

procedure emit_int(v: integer);
var s: array [1..20] of char;
    len, i: integer;
    c: char;
begin
    emit_space;
    if v = 0 then emit_char('0')
    else begin
        if v < 0 then begin emit_char('-'); v := -v end;
        len := 0;
        while v > 0 do begin
            len := len + 1;
            s[len] := chr(ord('0') + (v mod 10));
            v := v div 10
        end;
        { reverse }
        for i := 1 to len div 2 do begin
            c := s[i];
            s[i] := s[len - i + 1];
            s[len - i + 1] := c
        end;
        for i := 1 to len do emit_char(s[i])
    end
end;

procedure emit_symbol;
var i: integer;
begin
    case sy of
        ident: begin
            emit_space;
            { check if identifier would exceed wrap column, wrap first if so }
            if (wrapcolumn > 0) and (outpos + kk > wrapcolumn) then
                flush_line;
            for i := 1 to kk do emit_char(id[i])
        end;
        intconst: emit_int(ival);
        realconst: begin
            { real output - must be careful about buffer interleaving }
            { flush current buffer, write real directly, continue on same line }
            if atbol then outlineno := lineno;
            if sourceline and atbol then write(prr, '{', outlineno:6, '} ');
            if outpos > 0 then begin
                { trim trailing spaces }
                while (outpos > 0) and (outlin[outpos] = ' ') do
                    outpos := outpos - 1;
                for i := 1 to outpos do write(prr, outlin[i]);
                write(prr, ' ')
            end;
            write(prr, rval:1);
            { reset buffer state for continuation }
            outpos := 0;
            for i := 1 to maxlinp do outlin[i] := ' ';
            atbol := false
        end;
        stringconst: begin
            emit_space;
            { check if string would exceed wrap column, wrap first if so }
            if (wrapcolumn > 0) and (outpos + lgth + 2 > wrapcolumn) then
                flush_line;
            emit_char('''');
            for i := 1 to lgth do begin
                emit_char(sval[i]);
                if sval[i] = '''' then emit_char('''')
            end;
            emit_char('''')
        end;
        hexsy: begin emit_space; emit_char('$') end;
        octsy: begin emit_space; emit_char('&') end;
        binsy: begin emit_space; emit_char('%') end;
        notsy: emit_kw('not      ', 3);
        mulop: case op of
            mul:   begin emit_space; emit_char('*') end;
            rdiv:  begin emit_space; emit_char('/') end;
            andop: emit_kw('and      ', 3);
            idiv:  emit_kw('div      ', 3);
            imod:  emit_kw('mod      ', 3);
            else { do nothing }
        end;
        addop: case op of
            plus:  begin emit_space; emit_char('+') end;
            minus: begin emit_space; emit_char('-') end;
            orop:  emit_kw('or       ', 2);
            xorop: emit_kw('xor      ', 3);
            else { do nothing }
        end;
        relop: case op of
            ltop: begin emit_space; emit_char('<') end;
            leop: begin emit_space; emit_char('<'); emit_char('=') end;
            geop: begin emit_space; emit_char('>'); emit_char('=') end;
            gtop: begin emit_space; emit_char('>') end;
            neop: begin emit_space; emit_char('<'); emit_char('>') end;
            eqop: begin emit_space; emit_char('=') end;
            inop: emit_kw('in       ', 2);
            else { do nothing }
        end;
        lparent: emit_char('(');
        rparent: emit_char(')');
        lbrack: emit_char('[');
        rbrack: emit_char(']');
        comma: emit_char(',');
        semicolon: emit_char(';');
        period: emit_char('.');
        arrow: emit_char('^');
        colon: emit_char(':');
        becomes: begin emit_space; emit_char(':'); emit_char('=') end;
        range: begin emit_char('.'); emit_char('.') end;
        labelsy: emit_kw('label    ', 5);
        constsy: emit_kw('const    ', 5);
        typesy: emit_kw('type     ', 4);
        varsy: emit_kw('var      ', 3);
        funcsy: emit_kw('function ', 8);
        progsy: emit_kw('program  ', 7);
        procsy: emit_kw('procedure', 9);
        setsy: emit_kw('set      ', 3);
        packedsy: emit_kw('packed   ', 6);
        arraysy: emit_kw('array    ', 5);
        recordsy: emit_kw('record   ', 6);
        filesy: emit_kw('file     ', 4);
        beginsy: emit_kw('begin    ', 5);
        ifsy: emit_kw('if       ', 2);
        casesy: emit_kw('case     ', 4);
        repeatsy: emit_kw('repeat   ', 6);
        whilesy: emit_kw('while    ', 5);
        forsy: emit_kw('for      ', 3);
        withsy: emit_kw('with     ', 4);
        gotosy: emit_kw('goto     ', 4);
        endsy: emit_kw('end      ', 3);
        elsesy: emit_kw('else     ', 4);
        untilsy: emit_kw('until    ', 5);
        ofsy: emit_kw('of       ', 2);
        dosy: emit_kw('do       ', 2);
        tosy: emit_kw('to       ', 2);
        downtosy: emit_kw('downto   ', 6);
        thensy: emit_kw('then     ', 4);
        nilsy: emit_kw('nil      ', 3);
        forwardsy: emit_kw('forward  ', 7);
        modulesy: emit_kw('module   ', 6);
        usessy: emit_kw('uses     ', 4);
        privatesy: emit_kw('private  ', 7);
        externalsy: emit_kw('external ', 8);
        viewsy: emit_kw('view     ', 4);
        fixedsy: emit_kw('fixed    ', 5);
        processsy: emit_kw('process  ', 7);
        monitorsy: emit_kw('monitor  ', 7);
        sharesy: emit_kw('share    ', 5);
        classsy: emit_kw('class    ', 5);
        issy: emit_kw('is       ', 2);
        overloadsy: emit_kw('overload ', 8);
        overridesy: emit_kw('override ', 8);
        referencesy: emit_kw('reference', 9);
        joinssy: emit_kw('joins    ', 5);
        staticsy: emit_kw('static   ', 6);
        inheritedsy: emit_kw('inherited', 9);
        selfsy: emit_kw('self     ', 4);
        virtualsy: emit_kw('virtual  ', 7);
        trysy: emit_kw('try      ', 3);
        exceptsy: emit_kw('except   ', 6);
        extendssy: emit_kw('extends  ', 7);
        onsy: emit_kw('on       ', 2);
        resultsy: emit_kw('result   ', 6);
        operatorsy: emit_kw('operator ', 8);
        outsy: emit_kw('out      ', 3);
        propertysy: emit_kw('property ', 8);
        channelsy: emit_kw('channel  ', 7);
        streamsy: emit_kw('stream   ', 6);
        othersy: emit_kw('others   ', 6);
        else { eofsy - do nothing }
    end;
    lastsy := sy
end;

procedure emit_comment;
var i: integer;
begin
    if hascmt then begin
        emit_space;
        if cmtlncmt then begin
            emit_char('{');
            emit_char(' ')
        end else
            emit_char('{');
        for i := 1 to cmtlen do emit_char(cmtbuf[i]);
        if cmtlncmt then begin
            emit_char(' ');
            emit_char('}')
        end else
            emit_char('}');
        hascmt := false;
        cmtlen := 0
    end
end;

{******************************************************************************

                              Scanner (insymbol)

******************************************************************************}

procedure insymbol;
label 1;
var i, k, v, r: integer;
    ferr: boolean;
    strend: boolean;
    string_buf: csstr;
    ev: integer;
    rv: real;
    sgn: integer;
begin
1:
    { Skip spaces and controls }
    while (ch <= ' ') and not eofinp do begin
        if eol then begin
            endofline; { output any errors from previous line }
            if hascmt then emit_comment
        end;
        nextch
    end;

    if eofinp then begin
        sy := eofsy; op := noop
    end else if chartp[ch] = illegal then begin
        sy := othersy; op := noop;
        toterr := toterr + 1;
        nextch
    end else
    case chartp[ch] of
        letter: begin
            k := 0; ferr := true;
            for i := 1 to maxids do id[i] := ' ';
            repeat
                if k < maxids then begin
                    k := k + 1; id[k] := ch
                end else if ferr then ferr := false;
                nextch
            until not (chartp[ch] in [letter, number]);
            if k >= kk then kk := k
            else begin
                repeat id[kk] := ' '; kk := kk - 1 until kk = k
            end;
            sy := ident; op := noop;
            if k <= reslen then
                for i := 1 to maxres do
                    if strequri(rw[i], id) then begin
                        sy := rsy[i]; op := rop[i]
                    end;
            { in ISO 7185 mode, Pascaline-specific reserved words are identifiers }
            if iso7185 then
                if (sy >= forwardsy) or (op > noop) then begin
                    sy := ident; op := noop
                end
        end;
        chhex, choct, chbin, number: begin
            op := noop; r := 10;
            if chartp[ch] = chhex then begin r := 16; nextch end
            else if chartp[ch] = choct then begin r := 8; nextch end
            else if chartp[ch] = chbin then begin r := 2; nextch end;
            if (r = 10) or (chartp[ch] = number) or (chartp[ch] = letter) then begin
                v := 0;
                repeat
                    if ch <> '_' then begin
                        if (ordint[ch] >= 0) and (ordint[ch] < r) then
                            v := v * r + ordint[ch]
                    end;
                    nextch
                until (chartp[ch] <> number) and (ch <> '_') and
                      ((chartp[ch] <> letter) or (r < 16));
                ival := v;
                sy := intconst;
                { check for real }
                if ((ch = '.') and (inplin[inppos] <> '.') and
                    (inplin[inppos] <> ')')) or (lcase(ch) = 'e') then begin
                    rv := v; ev := 0;
                    if ch = '.' then begin
                        nextch;
                        repeat
                            rv := rv * 10 + ordint[ch]; nextch; ev := ev - 1
                        until chartp[ch] <> number
                    end;
                    if lcase(ch) = 'e' then begin
                        nextch; sgn := 1;
                        if (ch = '+') or (ch = '-') then begin
                            if ch = '-' then sgn := -1;
                            nextch
                        end;
                        i := 0;
                        while chartp[ch] = number do begin
                            i := i * 10 + ordint[ch]; nextch
                        end;
                        ev := ev + i * sgn
                    end;
                    { apply exponent }
                    if ev > 0 then
                        while ev > 0 do begin rv := rv * 10; ev := ev - 1 end
                    else
                        while ev < 0 do begin rv := rv / 10; ev := ev + 1 end;
                    rval := rv;
                    sy := realconst
                end
            end else begin
                { radix prefix alone }
                if r = 16 then sy := hexsy
                else if r = 8 then sy := octsy
                else sy := binsy
            end
        end;
        chstrquo: begin
            nextch; lgth := 0; sy := stringconst; op := noop; strend := false;
            for i := 1 to strglgth do string_buf[i] := ' ';
            repeat
                if ch = '''' then begin
                    nextch;
                    if ch = '''' then begin
                        lgth := lgth + 1;
                        if lgth <= strglgth then string_buf[lgth] := ch;
                        nextch
                    end else
                        strend := true
                end else if ch = chr(92) then begin
                    (* escape sequence - backslash *)
                    nextch;
                    lgth := lgth + 1;
                    if lgth <= strglgth then string_buf[lgth] := ch;
                    nextch
                end else begin
                    lgth := lgth + 1;
                    if lgth <= strglgth then string_buf[lgth] := ch;
                    nextch
                end
            until eol or strend;
            if lgth = 1 then
                ival := ord(string_buf[1]);
            sval := string_buf
        end;
        chcolon: begin
            op := noop; nextch;
            if ch = '=' then begin
                sy := becomes; nextch
            end else
                sy := colon
        end;
        chperiod: begin
            op := noop; nextch;
            if ch = '.' then begin sy := range; nextch end
            else if ch = ')' then begin sy := rbrack; nextch end
            else sy := period
        end;
        chlt: begin
            nextch; sy := relop;
            if ch = '=' then begin op := leop; nextch end
            else if ch = '>' then begin op := neop; nextch end
            else op := ltop
        end;
        chgt: begin
            nextch; sy := relop;
            if ch = '=' then begin op := geop; nextch end
            else op := gtop
        end;
        chlparen: begin
            nextch;
            if ch = '*' then begin
                { comment - collect it }
                nextch;
                cmtlen := 0; hascmt := true; cmtlncmt := false;
                repeat
                    while (ch <> '*') and (ch <> '}') and not eofinp do begin
                        if cmtlen < maxlinp then begin
                            cmtlen := cmtlen + 1;
                            cmtbuf[cmtlen] := ch
                        end;
                        nextch
                    end;
                    if ch = '}' then begin nextch; goto 1 end;
                    if ch = '*' then begin
                        nextch;
                        if ch = ')' then begin nextch; goto 1 end
                        else begin
                            if cmtlen < maxlinp then begin
                                cmtlen := cmtlen + 1;
                                cmtbuf[cmtlen] := '*'
                            end
                        end
                    end
                until eofinp;
                goto 1
            end else if ch = '.' then begin
                sy := lbrack; nextch
            end else begin
                sy := lparent; op := noop
            end
        end;
        chlcmt: begin
            (* brace comment *)
            nextch;
            cmtlen := 0; hascmt := true; cmtlncmt := false;
            while (ch <> '}') and not eofinp do begin
                if cmtlen < maxlinp then begin
                    cmtlen := cmtlen + 1;
                    cmtbuf[cmtlen] := ch
                end;
                nextch
            end;
            if ch = '}' then nextch;
            goto 1
        end;
        chrem: begin
            { ! line comment }
            cmtlen := 0; hascmt := true; cmtlncmt := true;
            nextch; { skip ! }
            while not eol and not eofinp do begin
                if cmtlen < maxlinp then begin
                    cmtlen := cmtlen + 1;
                    cmtbuf[cmtlen] := ch
                end;
                nextch
            end;
            goto 1
        end;
        special: begin
            sy := ssy[ch]; op := sop[ch];
            nextch
        end;
        chspace: begin
            sy := othersy; op := noop
        end
    end
end;

{******************************************************************************

                              Error Recovery

******************************************************************************}

procedure errmsg(ferrnr: integer);
begin case ferrnr of
    1:   write('Error in simple type');
    2:   write('Identifier expected');
    3:   write('''program'' expected');
    4:   write(''')'' expected');
    5:   write(''':'' expected');
    6:   write('Illegal symbol');
    7:   write('Error in parameter list');
    8:   write('''of'' expected');
    9:   write('''('' expected');
    10:  write('Error in type');
    11:  write('''['' expected');
    12:  write(''']'' expected');
    13:  write('''end'' expected');
    14:  write(''';'' expected');
    15:  write('Integer expected');
    16:  write('''='' expected');
    17:  write('''begin'' expected');
    18:  write('Error in declaration part');
    19:  write('Error in field-list');
    20:  write(''','' expected');
    21:  write('''.'' expected');
    22:  write('Integer or identifier expected');
    23:  write('''except'' expected');
    24:  write('''on'' or ''except'' expected');
    25:  write('Illegal source character');
    26:  write('String constant too long');
    50:  write('Error in constant');
    51:  write(''':='' expected');
    52:  write('''then'' expected');
    53:  write('''until'' expected');
    54:  write('''do'' expected');
    55:  write('''to''/''downto'' expected');
    58:  write('Error in factor');
    59:  write('Error in variable');
    104: write('Identifier not declared');
    116: write('Error in type of standard procedure parameter');
    129: write('Type conflict of operands');
    147: write('Label type incompatible with selecting expression');
    156: write('Multidefined case label');
    202: write('String constant must not exceed source line');
    255: write('Too many errors on this source line');
    else write('Error');
    end
end;

procedure wrtsrclin;
begin
    write(lineno:6, '  ':2);
    writeln(inplin:inplen)
end;

procedure endofline;
var lastpos, freepos, currpos, currnmr, f, j, k: integer; df: boolean;
begin
    if errinx > 0 then begin
        if not noerror then begin
            { output source line if not already listed }
            if not listmode then wrtsrclin;
            write(lineno:6, ' ****  ':9);
            lastpos := -1; freepos := 1;
            for k := 1 to errinx do begin
                currpos := errlist[k].pos;
                currnmr := errlist[k].nmr;
                if currpos = lastpos then write(',')
                else begin
                    while freepos < currpos do begin
                        write(' '); freepos := freepos + 1
                    end;
                    write('^');
                    lastpos := currpos
                end;
                if currnmr < 10 then f := 1
                else if currnmr < 100 then f := 2
                else f := 3;
                write(currnmr:f);
                freepos := freepos + f + 1
            end;
            writeln;
            if experr then begin
                for k := 1 to errinx do begin
                    df := false;
                    for j := 1 to k - 1 do
                        if errlist[j].nmr = errlist[k].nmr then df := true;
                    if not df then begin
                        write(lineno:6, ' ****  ':9);
                        write(errlist[k].nmr:3, ' ');
                        errmsg(errlist[k].nmr); writeln
                    end
                end
            end
        end;
        errinx := 0
    end;
    chcnt := 0
end;

procedure error(ferrnr: integer);
var ep: errptr;
begin
    if (ferrnr < 1) or (ferrnr > maxftl) then
        ferrnr := 6; { use "Illegal symbol" as fallback }
    errtbl[ferrnr] := errtbl[ferrnr] + 1; { track this error }
    { track error lines }
    new(ep); ep^.errlinno := lineno; ep^.next := errltb[ferrnr];
    errltb[ferrnr] := ep;
    if errinx >= 9 then begin
        errlist[10].nmr := 255; errinx := 10
    end else begin
        errinx := errinx + 1;
        errlist[errinx].nmr := ferrnr
    end;
    errlist[errinx].pos := chcnt;
    toterr := toterr + 1
end;

procedure skip(fsys: setofsys);
{ skip input until relevant symbol found - crucial for error recovery }
begin
    if not eofinp then begin
        while not (sy in fsys) and not eofinp do begin
            emit_symbol;
            insymbol
        end
    end
end;

{******************************************************************************

                              Parser/Formatter

******************************************************************************}

procedure expression(fsys: setofsys); forward;
procedure statement(fsys: setofsys); forward;
procedure block(fsys: setofsys); forward;

procedure selector(fsys: setofsys);
{ handle variable selectors: .field, ^, [index], (params) }
begin
    while sy in [period, arrow, lbrack, lparent] do begin
        case sy of
            period: begin
                emit_symbol; insymbol;
                if sy = ident then begin emit_symbol; insymbol end
            end;
            arrow: begin
                emit_symbol; insymbol
            end;
            lbrack: begin
                emit_symbol; insymbol;
                expression(fsys + [rbrack, comma]);
                while sy = comma do begin
                    emit_symbol; insymbol;
                    expression(fsys + [rbrack, comma])
                end;
                if sy = rbrack then begin emit_symbol; insymbol end
                else error(12)
            end;
            lparent: begin
                emit_symbol; insymbol;
                if sy <> rparent then begin
                    expression(fsys + [rparent, comma]);
                    while sy = comma do begin
                        emit_symbol; insymbol;
                        expression(fsys + [rparent, comma])
                    end
                end;
                if sy = rparent then begin emit_symbol; insymbol end
                else error(4)
            end
        end
    end
end;

procedure factor(fsys: setofsys);
begin
    if not (sy in facbegsys) then begin
        error(58); skip(fsys + facbegsys)
    end;
    if sy in facbegsys then
    case sy of
        ident: begin
            emit_symbol; insymbol;
            selector(fsys)
        end;
        intconst, realconst, stringconst, nilsy: begin
            emit_symbol; insymbol
        end;
        hexsy, octsy, binsy: begin
            emit_symbol; insymbol;
            if sy = intconst then begin emit_symbol; insymbol end
        end;
        lparent: begin
            emit_symbol; insymbol;
            expression(fsys + [rparent]);
            if sy = rparent then begin emit_symbol; insymbol end
            else error(4)
        end;
        lbrack: begin
            { set constructor }
            emit_symbol; insymbol;
            if sy <> rbrack then begin
                expression(fsys + [rbrack, comma, range]);
                while sy in [comma, range] do begin
                    emit_symbol; insymbol;
                    expression(fsys + [rbrack, comma, range])
                end
            end;
            if sy = rbrack then begin emit_symbol; insymbol end
            else error(12)
        end;
        notsy: begin
            emit_symbol; insymbol;
            factor(fsys)
        end;
        inheritedsy: begin
            emit_symbol; insymbol;
            if sy = ident then begin emit_symbol; insymbol end;
            selector(fsys)
        end;
        selfsy, resultsy: begin
            emit_symbol; insymbol;
            selector(fsys)
        end;
        else { other symbol - skip }
    end
end;

procedure term(fsys: setofsys);
begin
    factor(fsys + [mulop]);
    while sy = mulop do begin
        emit_symbol; insymbol;
        factor(fsys + [mulop])
    end
end;

procedure simpleexpression(fsys: setofsys);
begin
    if sy in [addop] then begin
        emit_symbol; insymbol
    end;
    term(fsys + [addop]);
    while sy = addop do begin
        emit_symbol; insymbol;
        term(fsys + [addop])
    end
end;

procedure expression(fsys: setofsys);
begin
    simpleexpression(fsys + [relop]);
    while sy = relop do begin
        emit_symbol; insymbol;
        simpleexpression(fsys + [relop])
    end
end;

procedure constexpression(fsys: setofsys);
{ constant expression - same as expression for formatting }
begin
    expression(fsys)
end;

procedure typ(fsys: setofsys); forward;

procedure simpletype(fsys: setofsys);
begin
    if sy = lparent then begin
        { enumerated type }
        emit_symbol; insymbol;
        while sy = ident do begin
            emit_symbol; insymbol;
            if sy = comma then begin emit_symbol; insymbol end
        end;
        if sy = rparent then begin emit_symbol; insymbol end
        else error(4)
    end else if sy = ident then begin
        emit_symbol; insymbol;
        if sy = range then begin
            { subrange }
            emit_symbol; insymbol;
            constexpression(fsys)
        end
    end else if sy in [addop, intconst, realconst, stringconst] then begin
        constexpression(fsys);
        if sy = range then begin
            emit_symbol; insymbol;
            constexpression(fsys)
        end
    end
end;

procedure fieldlist(fsys: setofsys);
{ record field list }
var done: boolean;
begin
    while sy = ident do begin
        { field declarations }
        while sy = ident do begin
            emit_symbol; insymbol;
            if sy = comma then begin emit_symbol; insymbol end
            else if sy = colon then begin emit_symbol; insymbol; typ(fsys + [semicolon]) end
        end;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    if sy = casesy then begin
        { variant part }
        emit_symbol; insymbol;
        if sy = ident then begin emit_symbol; insymbol end;
        if sy = colon then begin emit_symbol; insymbol end;
        if sy = ident then begin emit_symbol; insymbol end;
        if sy = ofsy then begin emit_symbol; emit_newline; insymbol end
        else error(8);
        indent := indent + 1;
        done := false;
        while not done and not (sy in [endsy, rparent, eofsy]) do begin
            { case labels }
            while sy in [ident, intconst, stringconst, addop, othersy] do begin
                constexpression(fsys + [comma, colon, range]);
                if sy = range then begin emit_symbol; insymbol; constexpression(fsys + [comma, colon]) end;
                if sy = comma then begin emit_symbol; insymbol end
            end;
            if sy = colon then begin emit_symbol; insymbol end;
            if sy = lparent then begin
                emit_symbol; emit_newline; insymbol;
                indent := indent + 1;
                fieldlist(fsys + [rparent]);
                indent := indent - 1;
                if sy = rparent then begin emit_symbol; insymbol end
            end;
            if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
            else done := true
        end;
        indent := indent - 1
    end
end;

procedure typ(fsys: setofsys);
{ type definition }
begin
    if not (sy in typebegsys) then begin
        error(10); skip(fsys + typebegsys)
    end;
    if sy in typebegsys then
    case sy of
        ident: begin
            emit_symbol; insymbol
        end;
        packedsy: begin
            emit_symbol; insymbol;
            typ(fsys)
        end;
        arraysy: begin
            emit_symbol; insymbol;
            if sy = lbrack then begin
                emit_symbol; insymbol;
                simpletype(fsys + [rbrack, comma, ofsy]);
                while sy = comma do begin
                    emit_symbol; insymbol;
                    simpletype(fsys + [rbrack, comma, ofsy])
                end;
                if sy = rbrack then begin emit_symbol; insymbol end
                else error(12)
            end;
            if sy = ofsy then begin emit_symbol; insymbol end
            else error(8);
            typ(fsys)
        end;
        recordsy: begin
            if bracketbreak then emit_blank_line;
            emit_symbol; emit_newline; insymbol;
            indent := indent + 1;
            fieldlist(fsys + [endsy]);
            indent := indent - 1;
            if bracketbreak then emit_newline;
            if sy = endsy then begin emit_symbol; insymbol end
            else error(13);
            if bracketbreak then emit_blank_line
        end;
        setsy: begin
            emit_symbol; insymbol;
            if sy = ofsy then begin emit_symbol; insymbol end
            else error(8);
            simpletype(fsys)
        end;
        filesy: begin
            emit_symbol; insymbol;
            if sy = ofsy then begin emit_symbol; insymbol; typ(fsys) end
        end;
        arrow: begin
            { pointer type }
            emit_symbol; insymbol;
            if sy = ident then begin emit_symbol; insymbol end
        end;
        lparent: begin
            { enumerated type }
            simpletype(fsys)
        end;
        else begin
            { subrange or simple type }
            simpletype(fsys)
        end
    end
end;

procedure parameterlist(fsys: setofsys);
{ procedure/function parameter list }
var done: boolean;
begin
    if sy = lparent then begin
        emit_symbol; insymbol;
        done := false;
        while not done and not (sy in [rparent, eofsy]) do begin
            if sy in [varsy, viewsy, outsy] then begin
                emit_symbol; insymbol
            end;
            if sy = ident then begin
                emit_symbol; insymbol;
                while sy = comma do begin
                    emit_symbol; insymbol;
                    if sy = ident then begin emit_symbol; insymbol end
                end
            end;
            if sy = colon then begin
                emit_symbol; insymbol;
                typ(fsys + [semicolon, rparent])
            end;
            if sy = semicolon then begin emit_symbol; insymbol end
            else done := true
        end;
        if sy = rparent then begin emit_symbol; insymbol end
        else error(4)
    end
end;

procedure procdeclaration(fsys: setofsys);
{ procedure or function declaration }
begin
    emit_symbol; insymbol; { procsy or funcsy }
    if sy = ident then begin emit_symbol; insymbol end;
    parameterlist(fsys + [colon, semicolon]);
    if sy = colon then begin
        { function result type }
        emit_symbol; insymbol;
        if sy = ident then begin emit_symbol; insymbol end
    end;
    if sy = semicolon then begin emit_symbol; emit_newline; insymbol end;
    { directives - in ISO 7185 mode, forward/external are identifiers }
    while (sy in [forwardsy, externalsy, overloadsy, virtualsy, overridesy, staticsy]) or
          ((sy = ident) and (strequri('forward  ', id) or strequri('external ', id))) do begin
        emit_symbol; insymbol;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    { body if not forward/external }
    if sy in [labelsy, constsy, typesy, varsy, procsy, funcsy, beginsy] then
        block(fsys)
end;

procedure compoundstatement(fsys: setofsys);
{ begin...end block }
var done: boolean;
begin
    emit_symbol; emit_newline; insymbol; { beginsy }
    if bracketbreak then emit_blank_line; { blank line after begin }
    indent := indent + 1;
    done := false;
    while not done do begin
        statement(fsys + [semicolon, endsy]);
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
        else done := true
    end;
    indent := indent - 1;
    if bracketbreak then emit_blank_line; { blank line before end }
    if sy = endsy then begin emit_symbol; insymbol end
    else error(13)
end;

procedure ifstatement(fsys: setofsys);
begin
    emit_symbol; insymbol; { ifsy }
    expression(fsys + [thensy]);
    if sy = thensy then begin
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        statement(fsys + [elsesy]);
        indent := indent - 1
    end else error(52);
    if sy = elsesy then begin
        emit_newline;
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        statement(fsys);
        indent := indent - 1
    end
end;

procedure casestatement(fsys: setofsys);
begin
    emit_symbol; insymbol; { casesy }
    expression(fsys + [ofsy, comma, colon]);
    if sy = ofsy then begin emit_symbol; emit_newline; insymbol end
    else error(8);
    indent := indent + 1;
    { case list elements }
    while not (sy in [endsy, elsesy, eofsy]) do begin
        { case labels - include othersy for 'others' keyword }
        if sy in constbegsys + [othersy] then begin
            repeat
                if sy = othersy then begin
                    emit_symbol; insymbol
                end else
                    constexpression(fsys + [comma, colon, range, semicolon]);
                if sy = range then begin
                    emit_symbol; insymbol;
                    constexpression(fsys + [comma, colon, semicolon])
                end;
                if sy = comma then begin emit_symbol; insymbol end
                else if sy <> colon then begin
                    { error recovery - skip to colon or end }
                    error(147);
                    skip(fsys + [colon, semicolon, endsy, elsesy])
                end
            until sy in [colon, semicolon, endsy, elsesy, eofsy];
            if sy = colon then begin emit_symbol; emit_newline; insymbol end;
            indent := indent + 1;
            statement(fsys + [semicolon, endsy, elsesy]);
            indent := indent - 1
        end else begin
            { error - unexpected symbol in case statement }
            error(147);
            skip(fsys + [semicolon, endsy, elsesy])
        end;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    if sy = elsesy then begin
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        statement(fsys + [semicolon, endsy]);
        indent := indent - 1;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    indent := indent - 1;
    if sy = endsy then begin emit_symbol; insymbol end
    else error(13)
end;

procedure repeatstatement(fsys: setofsys);
var done: boolean;
begin
    emit_symbol; emit_newline; insymbol; { repeatsy }
    indent := indent + 1;
    done := false;
    while not done do begin
        statement(fsys + [semicolon, untilsy]);
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
        else done := true
    end;
    indent := indent - 1;
    if sy = untilsy then begin
        emit_newline;
        emit_symbol; insymbol
    end else error(53);
    expression(fsys)
end;

procedure whilestatement(fsys: setofsys);
begin
    emit_symbol; insymbol; { whilesy }
    expression(fsys + [dosy]);
    if sy = dosy then begin
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        statement(fsys);
        indent := indent - 1
    end else error(54)
end;

procedure forstatement(fsys: setofsys);
begin
    emit_symbol; insymbol; { forsy }
    if sy = ident then begin emit_symbol; insymbol end;
    if sy = becomes then begin emit_symbol; insymbol end
    else error(51);
    expression(fsys + [tosy, downtosy, dosy]);
    if sy in [tosy, downtosy] then begin
        emit_symbol; insymbol
    end else error(55);
    expression(fsys + [dosy]);
    if sy = dosy then begin
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        statement(fsys);
        indent := indent - 1
    end else error(54)
end;

procedure withstatement(fsys: setofsys);
begin
    emit_symbol; insymbol; { withsy }
    if sy = ident then begin
        emit_symbol; insymbol;
        selector(fsys + [comma, dosy])
    end;
    while sy = comma do begin
        emit_symbol; insymbol;
        if sy = ident then begin
            emit_symbol; insymbol;
            selector(fsys + [comma, dosy])
        end
    end;
    if sy = dosy then begin
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        statement(fsys);
        indent := indent - 1
    end else error(54)
end;

procedure trystatement(fsys: setofsys);
var done: boolean;
begin
    emit_symbol; emit_newline; insymbol; { trysy }
    indent := indent + 1;
    done := false;
    while not done do begin
        statement(fsys + [semicolon, exceptsy]);
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
        else done := true
    end;
    indent := indent - 1;
    if sy = exceptsy then begin
        emit_newline;
        emit_symbol; emit_newline; insymbol;
        indent := indent + 1;
        { exception handlers }
        done := false;
        while not done do begin
            if sy = onsy then begin
                emit_symbol; insymbol;
                if sy = ident then begin emit_symbol; insymbol end;
                if sy = colon then begin emit_symbol; insymbol end;
                if sy = ident then begin emit_symbol; insymbol end;
                if sy = dosy then begin emit_symbol; emit_newline; insymbol end;
                indent := indent + 1;
                statement(fsys + [semicolon, endsy, onsy]);
                indent := indent - 1;
                if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
            end else if sy in statbegsys then begin
                statement(fsys + [semicolon, endsy]);
                if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
                else done := true
            end else
                done := true
        end;
        indent := indent - 1
    end;
    if sy = endsy then begin emit_symbol; insymbol end
    else error(13)
end;

procedure statement(fsys: setofsys);
begin
    emit_comment;
    if not (sy in statbegsys + [ident, intconst]) then begin
        error(6); skip(fsys)
    end;
    if sy in statbegsys + [ident, intconst] then begin
        case sy of
            ident: begin
                emit_symbol; insymbol;
                selector(fsys + [becomes]);
                if sy = becomes then begin
                    emit_symbol; insymbol;
                    expression(fsys)
                end else if sy = colon then begin
                    { label }
                    emit_symbol; insymbol;
                    statement(fsys)
                end
                { else procedure call - already handled by selector }
            end;
            intconst: begin
                { numeric label }
                emit_symbol; insymbol;
                if sy = colon then begin emit_symbol; insymbol end;
                statement(fsys)
            end;
            beginsy: compoundstatement(fsys);
            ifsy: ifstatement(fsys);
            casesy: casestatement(fsys);
            repeatsy: repeatstatement(fsys);
            whilesy: whilestatement(fsys);
            forsy: forstatement(fsys);
            withsy: withstatement(fsys);
            gotosy: begin
                emit_symbol; insymbol;
                if sy in [ident, intconst] then begin emit_symbol; insymbol end
            end;
            trysy: trystatement(fsys);
            else { empty statement }
        end
    end;
    if not (sy in [semicolon, endsy, untilsy, elsesy, exceptsy, eofsy] + fsys) then begin
        error(6); skip(fsys)
    end
end;

procedure block(fsys: setofsys);
{ declaration part and compound statement }
begin
    emit_blank_line;
    { label declarations }
    if sy = labelsy then begin
        emit_symbol; insymbol;
        while sy in [ident, intconst] do begin
            emit_symbol; insymbol;
            if sy = comma then begin emit_symbol; insymbol end
        end;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    { constant declarations }
    if sy = constsy then begin
        emit_blank_line;
        emit_symbol; emit_newline; insymbol;
        if bracketbreak then emit_blank_line; { blank line after const }
        if not flushleft then indent := indent + 1; { skip indent if flushleft }
        while sy = ident do begin
            emit_symbol; insymbol;
            if sy = relop then begin emit_symbol; insymbol end
            else error(16);
            constexpression(fsys + [semicolon]);
            if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
        end;
        if not flushleft then indent := indent - 1
    end;
    { type declarations }
    if sy = typesy then begin
        emit_blank_line;
        emit_symbol; emit_newline; insymbol;
        if bracketbreak then emit_blank_line; { blank line after type }
        if not flushleft then indent := indent + 1; { skip indent if flushleft }
        while sy = ident do begin
            emit_symbol; insymbol;
            if sy = relop then begin emit_symbol; insymbol end
            else error(16);
            typ(fsys + [semicolon]);
            if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
        end;
        if not flushleft then indent := indent - 1
    end;
    { variable declarations }
    if sy = varsy then begin
        emit_blank_line;
        emit_symbol; emit_newline; insymbol;
        if bracketbreak then emit_blank_line; { blank line after var }
        if not flushleft then indent := indent + 1; { skip indent if flushleft }
        while sy = ident do begin
            emit_symbol; insymbol;
            while sy = comma do begin
                emit_symbol; insymbol;
                if sy = ident then begin emit_symbol; insymbol end
            end;
            if sy = colon then begin emit_symbol; insymbol end
            else error(5);
            typ(fsys + [semicolon]);
            if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
        end;
        if not flushleft then indent := indent - 1
    end;
    { procedure and function declarations }
    while sy in [procsy, funcsy, operatorsy] do begin
        emit_blank_line;
        procdeclaration(fsys + [procsy, funcsy, operatorsy, beginsy]);
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    { compound statement }
    if sy = beginsy then begin
        emit_blank_line;
        compoundstatement(fsys)
    end
end;

procedure programblock(fsys: setofsys);
{ program or module header and body }
begin
    if sy in [progsy, modulesy] then begin
        emit_symbol; insymbol;
        if sy = ident then begin emit_symbol; insymbol end;
        if sy = lparent then begin
            emit_symbol; insymbol;
            while sy = ident do begin
                emit_symbol; insymbol;
                if sy = comma then begin emit_symbol; insymbol end
            end;
            if sy = rparent then begin emit_symbol; insymbol end
            else error(4)
        end;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    { joins clause }
    if sy = joinssy then begin
        emit_blank_line;
        emit_symbol; insymbol;
        while sy = ident do begin
            emit_symbol; insymbol;
            if sy = comma then begin emit_symbol; insymbol end
        end;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    { uses clause }
    if sy = usessy then begin
        emit_blank_line;
        emit_symbol; insymbol;
        while sy = ident do begin
            emit_symbol; insymbol;
            if sy = comma then begin emit_symbol; insymbol end
        end;
        if sy = semicolon then begin emit_symbol; emit_newline; insymbol end
    end;
    { private section }
    if sy = privatesy then begin
        emit_blank_line;
        emit_symbol; emit_newline; insymbol
    end;
    { declarations and body }
    block(fsys + [period]);
    { final period }
    if sy = period then begin emit_symbol; emit_newline end
end;

{******************************************************************************

                              Initialization

******************************************************************************}

procedure initreserved;
begin
    { reserved words - matches parse.pas }
    rw[1]  := 'and      '; rsy[1]  := mulop;     rop[1]  := andop;
    rw[2]  := 'array    '; rsy[2]  := arraysy;   rop[2]  := noop;
    rw[3]  := 'begin    '; rsy[3]  := beginsy;   rop[3]  := noop;
    rw[4]  := 'case     '; rsy[4]  := casesy;    rop[4]  := noop;
    rw[5]  := 'const    '; rsy[5]  := constsy;   rop[5]  := noop;
    rw[6]  := 'div      '; rsy[6]  := mulop;     rop[6]  := idiv;
    rw[7]  := 'do       '; rsy[7]  := dosy;      rop[7]  := noop;
    rw[8]  := 'downto   '; rsy[8]  := downtosy;  rop[8]  := noop;
    rw[9]  := 'else     '; rsy[9]  := elsesy;    rop[9]  := noop;
    rw[10] := 'end      '; rsy[10] := endsy;     rop[10] := noop;
    rw[11] := 'file     '; rsy[11] := filesy;    rop[11] := noop;
    rw[12] := 'for      '; rsy[12] := forsy;     rop[12] := noop;
    rw[13] := 'function '; rsy[13] := funcsy;    rop[13] := noop;
    rw[14] := 'goto     '; rsy[14] := gotosy;    rop[14] := noop;
    rw[15] := 'if       '; rsy[15] := ifsy;      rop[15] := noop;
    rw[16] := 'in       '; rsy[16] := relop;     rop[16] := inop;
    rw[17] := 'label    '; rsy[17] := labelsy;   rop[17] := noop;
    rw[18] := 'mod      '; rsy[18] := mulop;     rop[18] := imod;
    rw[19] := 'nil      '; rsy[19] := nilsy;     rop[19] := noop;
    rw[20] := 'not      '; rsy[20] := notsy;     rop[20] := noop;
    rw[21] := 'of       '; rsy[21] := ofsy;      rop[21] := noop;
    rw[22] := 'or       '; rsy[22] := addop;     rop[22] := orop;
    rw[23] := 'packed   '; rsy[23] := packedsy;  rop[23] := noop;
    rw[24] := 'procedure'; rsy[24] := procsy;    rop[24] := noop;
    rw[25] := 'program  '; rsy[25] := progsy;    rop[25] := noop;
    rw[26] := 'record   '; rsy[26] := recordsy;  rop[26] := noop;
    rw[27] := 'repeat   '; rsy[27] := repeatsy;  rop[27] := noop;
    rw[28] := 'set      '; rsy[28] := setsy;     rop[28] := noop;
    rw[29] := 'then     '; rsy[29] := thensy;    rop[29] := noop;
    rw[30] := 'to       '; rsy[30] := tosy;      rop[30] := noop;
    rw[31] := 'type     '; rsy[31] := typesy;    rop[31] := noop;
    rw[32] := 'until    '; rsy[32] := untilsy;   rop[32] := noop;
    rw[33] := 'var      '; rsy[33] := varsy;     rop[33] := noop;
    rw[34] := 'while    '; rsy[34] := whilesy;   rop[34] := noop;
    rw[35] := 'with     '; rsy[35] := withsy;    rop[35] := noop;
    { extended reserved words }
    rw[36] := 'forward  '; rsy[36] := forwardsy; rop[36] := noop;
    rw[37] := 'module   '; rsy[37] := modulesy;  rop[37] := noop;
    rw[38] := 'uses     '; rsy[38] := usessy;    rop[38] := noop;
    rw[39] := 'private  '; rsy[39] := privatesy; rop[39] := noop;
    rw[40] := 'external '; rsy[40] := externalsy; rop[40] := noop;
    rw[41] := 'view     '; rsy[41] := viewsy;    rop[41] := noop;
    rw[42] := 'fixed    '; rsy[42] := fixedsy;   rop[42] := noop;
    rw[43] := 'process  '; rsy[43] := processsy; rop[43] := noop;
    rw[44] := 'monitor  '; rsy[44] := monitorsy; rop[44] := noop;
    rw[45] := 'share    '; rsy[45] := sharesy;   rop[45] := noop;
    rw[46] := 'class    '; rsy[46] := classsy;   rop[46] := noop;
    rw[47] := 'is       '; rsy[47] := issy;      rop[47] := noop;
    rw[48] := 'overload '; rsy[48] := overloadsy; rop[48] := noop;
    rw[49] := 'override '; rsy[49] := overridesy; rop[49] := noop;
    rw[50] := 'reference'; rsy[50] := referencesy; rop[50] := noop;
    rw[51] := 'joins    '; rsy[51] := joinssy;   rop[51] := noop;
    rw[52] := 'static   '; rsy[52] := staticsy;  rop[52] := noop;
    rw[53] := 'inherited'; rsy[53] := inheritedsy; rop[53] := noop;
    rw[54] := 'self     '; rsy[54] := selfsy;    rop[54] := noop;
    rw[55] := 'virtual  '; rsy[55] := virtualsy; rop[55] := noop;
    rw[56] := 'try      '; rsy[56] := trysy;     rop[56] := noop;
    rw[57] := 'except   '; rsy[57] := exceptsy;  rop[57] := noop;
    rw[58] := 'extends  '; rsy[58] := extendssy; rop[58] := noop;
    rw[59] := 'on       '; rsy[59] := onsy;      rop[59] := noop;
    rw[60] := 'result   '; rsy[60] := resultsy;  rop[60] := noop;
    rw[61] := 'operator '; rsy[61] := operatorsy; rop[61] := noop;
    rw[62] := 'out      '; rsy[62] := outsy;     rop[62] := noop;
    rw[63] := 'property '; rsy[63] := propertysy; rop[63] := noop;
    rw[64] := 'channel  '; rsy[64] := channelsy; rop[64] := noop;
    rw[65] := 'stream   '; rsy[65] := streamsy;  rop[65] := noop;
    rw[66] := 'others   '; rsy[66] := othersy;   rop[66] := noop
end;

procedure initchartables;
var c: char;
begin
    for c := chr(0) to chr(255) do begin
        chartp[c] := illegal;
        ordint[c] := 0;
        ssy[c] := othersy;
        sop[c] := noop
    end;
    { letters }
    for c := 'a' to 'z' do chartp[c] := letter;
    for c := 'A' to 'Z' do chartp[c] := letter;
    chartp['_'] := letter;
    { digits }
    for c := '0' to '9' do begin
        chartp[c] := number;
        ordint[c] := ord(c) - ord('0')
    end;
    { hex digits }
    for c := 'a' to 'f' do ordint[c] := ord(c) - ord('a') + 10;
    for c := 'A' to 'F' do ordint[c] := ord(c) - ord('A') + 10;
    { special characters }
    chartp[''''] := chstrquo;
    chartp[':'] := chcolon;
    chartp['.'] := chperiod;
    chartp['<'] := chlt;
    chartp['>'] := chgt;
    chartp['('] := chlparen;
    chartp['{'] := chlcmt;
    chartp['!'] := chrem;
    chartp['$'] := chhex;
    chartp['&'] := choct;
    chartp['%'] := chbin;
    chartp[' '] := chspace;
    { single-char symbols }
    chartp['+'] := special; ssy['+'] := addop; sop['+'] := plus;
    chartp['-'] := special; ssy['-'] := addop; sop['-'] := minus;
    chartp['*'] := special; ssy['*'] := mulop; sop['*'] := mul;
    chartp['/'] := special; ssy['/'] := mulop; sop['/'] := rdiv;
    chartp['='] := special; ssy['='] := relop; sop['='] := eqop;
    chartp[','] := special; ssy[','] := comma;
    chartp[';'] := special; ssy[';'] := semicolon;
    chartp[')'] := special; ssy[')'] := rparent;
    chartp['['] := special; ssy['['] := lbrack;
    chartp[']'] := special; ssy[']'] := rbrack;
    chartp['^'] := special; ssy['^'] := arrow;
    chartp['@'] := special; ssy['@'] := arrow
end;

procedure initsymbolsets;
begin
    constbegsys := [addop, intconst, realconst, stringconst, ident,
                    lparent, lbrack, notsy];
    simptypebegsys := [lparent] + constbegsys;
    typebegsys := [arrow, packedsy, arraysy, recordsy, setsy, filesy] + simptypebegsys;
    blockbegsys := [labelsy, constsy, typesy, varsy, procsy, funcsy, beginsy];
    selectsys := [arrow, period, lbrack];
    facbegsys := [intconst, realconst, stringconst, ident, lparent, lbrack,
                  notsy, nilsy, hexsy, octsy, binsy, inheritedsy, selfsy, resultsy];
    statbegsys := [beginsy, gotosy, ifsy, whilesy, repeatsy, forsy, withsy,
                   casesy, trysy];
    typedels := [arraysy, recordsy, setsy, filesy]
end;

{******************************************************************************

Parse command line options

Parses options from the command line. Options are:
  -l, --list     List source lines while processing
  -s, --iso7185  Format as ISO 7185 Pascal (not Pascaline)
  -e, --experr   Show expanded error descriptions (default on)

******************************************************************************}

{ print help message }
procedure prthelp;
begin
    writeln('pasfmt - Pascaline Source Code Formatter');
    writeln;
    writeln('Usage: pasfmt [options] <inputfile>');
    writeln;
    writeln('Options:');
    writeln('  -h, --help           Display this help message');
    writeln('  -l, --list           List source lines (default: off)');
    writeln('  -s, --iso7185        Format as ISO 7185 Pascal (default: off/Pascaline)');
    writeln('  -e, --experr         Show expanded error messages (default: on)');
    writeln('  -c, --columns <n>    Set wrap column (default: 80)');
    writeln('  -b, --bracketbreak   Add blank lines around begin/end, etc. (default: on)');
    writeln('  -f, --flushleft      Flush left for const/type/var blocks (default: off)');
    writeln('  -z, --columnize      Align = signs in const/type/var blocks (default: off)');
    writeln('  -n, --sourceline     Add source line numbers as comments (default: off)');
    writeln('  -r, --noerror        Suppress error output (keep error count) (default: off)');
    writeln;
    writeln('Instruction file:');
    writeln('  If <inputfile>.fmt exists, it is read for additional options.');
    writeln('  Format: one command per line, ! for comments');
    goto 99
end;

procedure paropt;
var w:      filnam;   { word holder }
    err:    boolean;  { error flag }
    optfnd: boolean;  { option found }
    tv:     integer;  { temp value for integer options }

{ set true/false flag from option }
procedure setflg(view a, n: string; var f: boolean);
var ts: packed array [1..40] of char;
    i: integer;
begin
    if compp(w, n) or compp(w, a) then begin
        f := true;
        optfnd := true
    end else begin
        { try negative form: n + name }
        for i := 1 to 40 do ts[i] := ' ';
        ts[1] := 'n';
        for i := 1 to max(n) do if i < 40 then ts[i+1] := n[i];
        if compp(w, ts) then begin
            f := false;
            optfnd := true
        end else begin
            { try negative short form }
            for i := 1 to 40 do ts[i] := ' ';
            ts[1] := 'n';
            for i := 1 to max(a) do if i < 40 then ts[i+1] := a[i];
            if compp(w, ts) then begin
                f := false;
                optfnd := true
            end
        end
    end
end;

{ set integer value from option }
procedure setint(view a, n: string; var v: integer);
begin
    if compp(w, n) or compp(w, a) then begin
        parse.skpspc(cmdhan);
        parse.parnum(cmdhan, tv, 10, err);
        if err then begin
            writeln('*** Error: Invalid number for option: ', w:*);
            goto 99
        end;
        v := tv;
        optfnd := true
    end
end;

begin
    parse.skpspc(cmdhan); { skip spaces }
    while parse.chkchr(cmdhan) = services.optchr do begin { parse option }
        optfnd := false;
        parse.getchr(cmdhan); { skip option marker }
        { allow double option character (--option) }
        if parse.chkchr(cmdhan) = services.optchr then parse.getchr(cmdhan);
        parse.parlab(cmdhan, w, err); { parse option label }
        if err then begin
            writeln('*** Error: Invalid option');
            goto 99
        end;
        { check for help first }
        if compp(w, 'help') or compp(w, 'h') or compp(w, '?') then prthelp;
        { boolean options }
        setflg('l', 'list', listmode);
        setflg('s', 'iso7185', iso7185);
        setflg('e', 'experr', experr);
        setflg('b', 'bracketbreak', bracketbreak);
        setflg('f', 'flushleft', flushleft);
        setflg('z', 'columnize', columnize);
        setflg('n', 'sourceline', sourceline);
        setflg('r', 'noerror', noerror);
        { integer options }
        setint('c', 'columns', wrapcolumn);
        if not optfnd then begin
            writeln('*** Error: Unknown option: ', w:*);
            goto 99
        end;
        parse.skpspc(cmdhan)
    end
end;

{******************************************************************************

Parse and load instruction file

Parses and loads a list of instructions from the instruction file. The format
of the instruction file is:

! comment

command [parameter]

Where command is one of:

list or l           - List source lines while processing
iso7185 or s        - Format as ISO 7185 Pascal (not Pascaline)
experr or e         - Show expanded error descriptions
columns or c <n>    - Set wrap column (default 80)
bracketbreak or b   - Add blank lines around begin/end, const, etc.
flushleft or f      - Flush left for multi-line definitions
columnize or z      - Align = signs in const/type/var blocks
sourceline or n     - Add original source line numbers as comments

Comments start with '!' and continue to end of line. They can appear at the
start of a line or after commands.

******************************************************************************}

procedure parinst(view ifn: string);
var cmd: filnam;  { command word }
    err: boolean; { parsing error }
    tv:  integer; { temp value for integer parameters }
begin
    parse.openpar(inshan); { open parser }
    parse.openfil(inshan, ifn, cmdmax); { open file to parse }
    while not parse.endfil(inshan) do begin { process instructions }
        parse.skpspc(inshan); { skip leading spaces }
        if parse.chkchr(inshan) = '!' then { skip comment line }
            parse.getlin(inshan)
        else if not parse.endlin(inshan) then begin { command line }
            parse.parlab(inshan, cmd, err); { get command word }
            if err then begin
                writeln('*** Error: Invalid command in instruction file');
                parse.prterr(inshan, output, 'Invalid command', true);
                parse.getlin(inshan)
            end else begin
                { match commands }
                if compp(cmd, 'list') or compp(cmd, 'l') then
                    listmode := true
                else if compp(cmd, 'iso7185') or compp(cmd, 's') then
                    iso7185 := true
                else if compp(cmd, 'experr') or compp(cmd, 'e') then
                    experr := true
                else if compp(cmd, 'columns') or compp(cmd, 'c') then begin
                    parse.skpspc(inshan);
                    parse.parnum(inshan, tv, 10, err);
                    if err then begin
                        writeln('*** Warning: Invalid columns value in instruction file');
                        parse.prterr(inshan, output, 'Invalid number', true)
                    end else
                        wrapcolumn := tv
                end
                else if compp(cmd, 'bracketbreak') or compp(cmd, 'b') then
                    bracketbreak := true
                else if compp(cmd, 'flushleft') or compp(cmd, 'f') then
                    flushleft := true
                else if compp(cmd, 'columnize') or compp(cmd, 'z') then
                    columnize := true
                else if compp(cmd, 'sourceline') or compp(cmd, 'n') then
                    sourceline := true
                else if compp(cmd, 'noerror') or compp(cmd, 'r') then
                    noerror := true
                else begin
                    writeln('*** Warning: Unknown command in instruction file: ', cmd:*);
                    parse.prterr(inshan, output, 'Unknown command', true)
                end;
                { skip to comment or end of line }
                parse.skpspc(inshan);
                while not parse.endlin(inshan) and (parse.chkchr(inshan) <> '!') do
                    parse.getchr(inshan);
                parse.getlin(inshan)
            end
        end else
            parse.getlin(inshan) { skip empty line }
    end;
    parse.closefil(inshan); { close instruction file }
    parse.closepar(inshan) { close parser }
end;

procedure initialize;
var i: integer;
    err: boolean;
begin
    initreserved;
    initchartables;
    initsymbolsets;

    { initialize state }
    for i := 1 to maxlinp do begin
        inplin[i] := ' ';
        outlin[i] := ' ';
        cmtbuf[i] := ' '
    end;
    inppos := 1;
    inplen := 0;
    outpos := 0;
    outlineno := 0;
    lineno := 0;
    chcnt := 0;
    indent := 0;
    atbol := true;
    eol := false;
    eofinp := false;
    ch := ' ';
    kk := 1;
    lgth := 0;
    toterr := 0;
    errinx := 0;
    experr := true; { show expanded error messages }
    listmode := false;  { don't list source by default }
    wrapcolumn := 80;   { default wrap column }
    bracketbreak := true;
    flushleft := false;
    columnize := false;
    sourceline := false;
    noerror := false;
    for i := 1 to maxftl do begin errtbl[i] := 0; errltb[i] := nil end;
    hascmt := false;
    cmtlen := 0;
    cmtlncmt := false;
    lastsy := eofsy;
    for i := 1 to maxids do begin
        id[i] := ' ';
        srcfil[i] := ' ';
        dstfil[i] := ' ';
        insfil[i] := ' '
    end;
    for i := 1 to strglgth do sval[i] := ' ';

    { parse command line }
    iso7185 := false; { default to Pascaline mode }
    parse.openpar(cmdhan); { open parser }
    parse.opencommand(cmdhan, cmdmax); { open command line }
    paropt; { parse command options }
    if parse.endlin(cmdhan) then begin
        writeln('*** Error: No input file specified');
        prthelp
    end;
    parse.skpspc(cmdhan); { skip spaces }
    parse.parfil(cmdhan, srcfil, false, err); { parse filename }
    if err then begin
        writeln('*** Error: Invalid filename');
        goto 99
    end;
    paropt; { parse any trailing options }
    { break filename into path, name, extension }
    services.brknam(srcfil, p, n, e);
    { check for instruction file: path + name + .fmt }
    services.maknam(insfil, p, n, 'fmt');
    if exists(insfil) then parinst(insfil);
    { create input filename: path + name + .pas }
    services.maknam(srcfil, p, n, 'pas');
    assign(prd, srcfil);
    reset(prd);
    { create output filename: path + name + .pas.fmt }
    services.maknam(dstfil, p, n, 'pas');
    cat(dstfil, '.fmt');
    assign(prr, dstfil);
    rewrite(prr)
end;

{******************************************************************************

                              Main Program

******************************************************************************}

var
    f: boolean;
    i: 1..maxftl;
    ep, epl: errptr;
    col, tv, nw: integer; { for error listing wrapping }
begin
    initialize;
    readline;
    nextch;
    insymbol;
    programblock([eofsy]);
    endofline; { flush any final errors }
    flush_line;
    if hascmt then begin emit_comment; emit_newline end;
    writeln('Formatted output written to: ', dstfil);

    { output error summary }
    writeln('Errors in program: ', toterr:1);
    if not noerror then begin
        f := true;
        for i := 1 to maxftl do if errtbl[i] > 0 then begin
            if f then begin
                writeln;
                writeln('Error numbers in listing:');
                writeln('-------------------------');
                f := false
            end;
            write(i:3, ' ', errtbl[i]:3, ' ');
            { calculate starting column based on actual output width }
            tv := errtbl[i]; nw := 0;
            repeat nw := nw + 1; tv := tv div 10 until tv = 0;
            if nw < 3 then nw := 3; { :3 means minimum 3 chars }
            col := 3 + 1 + nw + 1; { i:3 + space + count + space }
            { reverse the line list for chronological order }
            epl := nil;
            while errltb[i] <> nil do begin
                ep := errltb[i]; errltb[i] := ep^.next;
                ep^.next := epl; epl := ep
            end;
            ep := epl;
            while ep <> nil do begin
                { calculate width of next number }
                tv := ep^.errlinno; nw := 0;
                repeat nw := nw + 1; tv := tv div 10 until tv = 0;
                if ep^.next <> nil then nw := nw + 1; { comma }
                if col + nw > 80 then begin { wrap at 80 columns }
                    writeln;
                    write('        ':8); { indent continuation }
                    col := 8
                end;
                write(ep^.errlinno:1);
                col := col + nw;
                ep := ep^.next;
                if ep <> nil then write(',')
            end;
            writeln;
            write('        ':8); errmsg(i); writeln
        end;
        if not f then writeln
    end;
99:
end.
