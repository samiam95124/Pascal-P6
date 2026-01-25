{******************************************************************************

DIF - File Difference Utility

Compares two text files and outputs the minimum differences in standard diff
format. Handles both Unix and Windows line endings transparently.

Usage:

dif [-w|-nw] file1 file2

Options:

-w   Ignore whitespace (default)
-nw  Exact comparison

******************************************************************************}

program dif(command, output);

uses strings;

const

    { maximum number of lines per file } maxlines = 50000;
    { maximum length of a single line } maxlinelen = 1000;

type

    { line array index } lineinx = 1..maxlines;
    { line number including zero } linenum = 0..maxlines;
    { line buffer type } linebuf = packed array [1..maxlinelen] of char;
    { edit operation type } editop = (opkeep, opdel, opins);
    { pointer to edit record } editptr = ^editrec;
    { edit record for diff operations }
    editrec = record

        { operation type } op: editop;
        { line number in file 1 } line1: linenum;
        { line number in file 2 } line2: linenum;
        { next edit record } next: editptr

    end;

var

    { lines from file 1 } lines1: array [lineinx] of pstring;
    { lines from file 2 } lines2: array [lineinx] of pstring;
    { number of lines in file 1 } nlines1: linenum;
    { number of lines in file 2 } nlines2: linenum;
    { ignore whitespace flag } ignorewhite: boolean;
    { command line position } cmdpos: integer;
    { command line buffer } cmdline: linebuf;
    { command line length } cmdlen: integer;
    { head of edit list } editlist: editptr;
    { tail of edit list } editlast: editptr;

{******************************************************************************

Skip space

Advances command line position past any space characters.

******************************************************************************}

procedure skipspace;

begin

    while (cmdpos <= cmdlen) and (cmdline[cmdpos] = ' ') do
        cmdpos := cmdpos + 1

end;

{******************************************************************************

Get command character

Returns the current character at command line position, or space if past end.

******************************************************************************}

function cmdchar:
    { current character } char;

begin

    if cmdpos <= cmdlen then cmdchar := cmdline[cmdpos]
    else cmdchar := ' '

end;

{******************************************************************************

Advance command position

Moves to the next character in the command line if not at end.

******************************************************************************}

procedure cmdnext;

begin

    if cmdpos <= cmdlen then cmdpos := cmdpos + 1

end;

{******************************************************************************

Read command line

Reads the command line from the command file into the command buffer.

******************************************************************************}

procedure readcmdline;

var { character buffer } c: char;

begin

    cmdlen := 0;
    while not eoln(command) do begin

        read(command, c);
        if cmdlen < maxlinelen then begin

            cmdlen := cmdlen + 1;
            cmdline[cmdlen] := c

        end

    end;
    cmdpos := 1

end;

{******************************************************************************

Get filename

Extracts the next filename from the command line.

******************************************************************************}

procedure getfilename(
    { returned filename } var r: pstring);

var { character count } i: integer;
    { copy index } j: integer;
    { filename buffer } buf: linebuf;

begin

    skipspace;
    i := 0;
    while (cmdpos <= cmdlen) and (cmdline[cmdpos] <> ' ') do begin

        i := i + 1;
        if i <= maxlinelen then buf[i] := cmdline[cmdpos];
        cmdpos := cmdpos + 1

    end;
    if i = 0 then r := nil
    else begin

        new(r, i);
        for j := 1 to i do r^[j] := buf[j]

    end

end;

{******************************************************************************

Parse command line

Parses command line options and extracts the two filenames.

******************************************************************************}

procedure parsecmd(
    { first filename } var file1: pstring;
    { second filename } var file2: pstring);

begin

    ignorewhite := true;
    file1 := nil;
    file2 := nil;
    skipspace;
    while cmdchar = '-' do begin

        cmdnext;
        if cmdchar = 'n' then begin

            cmdnext;
            if cmdchar = 'w' then begin

                ignorewhite := false;
                cmdnext

            end

        end
        else if cmdchar = 'w' then begin

            ignorewhite := true;
            cmdnext

        end;
        skipspace

    end;
    getfilename(file1);
    getfilename(file2)

end;

{******************************************************************************

Check whitespace

Returns true if the character is a space or tab.

******************************************************************************}

function iswhite(
    { character to check } c: char):
    { true if whitespace } boolean;

var { tab character } tab: char;

begin

    tab := chr(9);

    iswhite := (c = ' ') or (c = tab)

end;

{******************************************************************************

Strip whitespace

Returns a copy of the string with all whitespace removed.

******************************************************************************}

function stripwhite(
    { input string } s: pstring):
    { string without whitespace } pstring;

var { input index } i: integer;
    { output index } j: integer;
    { string length } slen: integer;
    { working buffer } buf: linebuf;
    { result string } r: pstring;

begin

    if s = nil then begin

        new(r, 1);
        r^[1] := ' '

    end else begin

        slen := len(s);
        j := 0;
        for i := 1 to slen do
            if not iswhite(s^[i]) then begin

                j := j + 1;
                buf[j] := s^[i]

            end;
        if j = 0 then begin

            new(r, 1);
            r^[1] := ' '

        end else begin

            new(r, j);
            for i := 1 to j do r^[i] := buf[i]

        end

    end;

    result r

end;

{******************************************************************************

Lines match

Compares two lines, optionally ignoring whitespace.

******************************************************************************}

function linesmatch(
    { first line } a: pstring;
    { second line } b: pstring):
    { true if lines match } boolean;

var { stripped first line } sa: pstring;
    { stripped second line } sb: pstring;

begin

    if (a = nil) and (b = nil) then linesmatch := true
    else if (a = nil) or (b = nil) then linesmatch := false
    else if ignorewhite then begin

        sa := stripwhite(a);
        sb := stripwhite(b);
        linesmatch := compc(sa, sb)

    end else
        linesmatch := compc(a, b)

end;

{******************************************************************************

Read file 1

Reads the first file into the lines1 array.

******************************************************************************}

procedure readfile1(
    { input file } var f: text;
    { line count returned } var nlines: linenum);

var { line buffer } buf: linebuf;
    { character count } i: integer;
    { copy index } j: integer;
    { character buffer } c: char;

begin

    nlines := 0;
    while not eof(f) do begin

        i := 0;
        while not eoln(f) do begin

            read(f, c);
            if i < maxlinelen then begin

                i := i + 1;
                buf[i] := c

            end

        end;
        readln(f);
        nlines := nlines + 1;
        if nlines > maxlines then begin

            writeln('*** Error: File too large (>', maxlines:1, ' lines)');
            halt

        end;
        if i = 0 then begin

            new(lines1[nlines], 1);
            lines1[nlines]^[1] := ' '

        end else begin

            new(lines1[nlines], i);
            for j := 1 to i do lines1[nlines]^[j] := buf[j]

        end

    end

end;

{******************************************************************************

Read file 2

Reads the second file into the lines2 array.

******************************************************************************}

procedure readfile2(
    { input file } var f: text;
    { line count returned } var nlines: linenum);

var { line buffer } buf: linebuf;
    { character count } i: integer;
    { copy index } j: integer;
    { character buffer } c: char;

begin

    nlines := 0;
    while not eof(f) do begin

        i := 0;
        while not eoln(f) do begin

            read(f, c);
            if i < maxlinelen then begin

                i := i + 1;
                buf[i] := c

            end

        end;
        readln(f);
        nlines := nlines + 1;
        if nlines > maxlines then begin

            writeln('*** Error: File too large (>', maxlines:1, ' lines)');
            halt

        end;
        if i = 0 then begin

            new(lines2[nlines], 1);
            lines2[nlines]^[1] := ' '

        end else begin

            new(lines2[nlines], i);
            for j := 1 to i do lines2[nlines]^[j] := buf[j]

        end

    end

end;

{******************************************************************************

Add edit

Adds an edit operation to the edit list.

******************************************************************************}

procedure addedit(
    { operation type } op: editop;
    { line number in file 1 } l1: linenum;
    { line number in file 2 } l2: linenum);

var { new edit record } e: editptr;

begin

    new(e);
    e^.op := op;
    e^.line1 := l1;
    e^.line2 := l2;
    e^.next := nil;
    if editlist = nil then editlist := e
    else editlast^.next := e;
    editlast := e

end;

{******************************************************************************

Match length

Returns the number of consecutive matching lines starting at the given
positions in both files.

******************************************************************************}

function matchlen(
    { starting position in file 1 } i1: linenum;
    { starting position in file 2 } i2: linenum):
    { number of matching lines } linenum;

var { match count } n: linenum;

begin

    n := 0;
    while (i1 + n <= nlines1) and (i2 + n <= nlines2) and
          (lines1[i1 + n] <> nil) and (lines2[i2 + n] <> nil) and
          linesmatch(lines1[i1 + n], lines2[i2 + n]) do
        n := n + 1;

    matchlen := n

end;

{******************************************************************************

Find match

Finds the best matching position in file 2 for the current position in file 1.

******************************************************************************}

procedure findmatch(
    { current position in file 1 } i1: linenum;
    { current position in file 2 } i2: linenum;
    { best matching position } var best2: linenum;
    { length of best match } var bestlen: linenum);

var { loop index } j: linenum;
    { match length } mlen: linenum;

begin

    best2 := 0;
    bestlen := 0;
    for j := i2 to nlines2 do begin

        mlen := matchlen(i1, j);
        if mlen > bestlen then begin

            bestlen := mlen;
            best2 := j

        end

    end

end;

{******************************************************************************

Compute diff

Computes the difference between the two files and builds the edit list.

******************************************************************************}

procedure computediff;

var { position in file 1 } i1: linenum;
    { position in file 2 } i2: linenum;
    { best match position } best2: linenum;
    { best match length } bestlen: linenum;
    { scan position } j: linenum;

begin

    editlist := nil;
    editlast := nil;
    i1 := 1;
    i2 := 1;
    while (i1 <= nlines1) or (i2 <= nlines2) do begin

        if (i1 <= nlines1) and (i2 <= nlines2) and
           (lines1[i1] <> nil) and (lines2[i2] <> nil) and
           linesmatch(lines1[i1], lines2[i2]) then begin

            addedit(opkeep, i1, i2);
            i1 := i1 + 1;
            i2 := i2 + 1

        end else begin

            findmatch(i1, i2, best2, bestlen);
            if bestlen > 0 then begin

                while i2 < best2 do begin

                    addedit(opins, i1 - 1, i2);
                    i2 := i2 + 1

                end

            end else begin

                j := i1 + 1;
                while (j <= nlines1) and (i2 <= nlines2) and
                      (lines1[j] <> nil) and (lines2[i2] <> nil) and
                      not linesmatch(lines1[j], lines2[i2]) do
                    j := j + 1;
                if (j <= nlines1) and (i2 <= nlines2) then begin

                    while i1 < j do begin

                        addedit(opdel, i1, i2 - 1);
                        i1 := i1 + 1

                    end

                end else begin

                    if i1 <= nlines1 then begin

                        addedit(opdel, i1, i2 - 1);
                        i1 := i1 + 1

                    end;
                    if i2 <= nlines2 then begin

                        addedit(opins, i1 - 1, i2);
                        i2 := i2 + 1

                    end

                end

            end

        end

    end

end;

{******************************************************************************

Show diff

Outputs the differences in standard diff format.

******************************************************************************}

procedure showdiff;

var { edit list pointer } ep: editptr;
    { deletion count } delcount: linenum;
    { insertion count } inscount: linenum;
    { deletion start line } delstart: linenum;
    { insertion start line } insstart: linenum;
    { position in file 1 } l1pos: linenum;
    { position in file 2 } l2pos: linenum;
    { loop index } i: linenum;
    { in change block flag } inchange: boolean;
    { keep loop flag } keeploop: boolean;

begin

    ep := editlist;
    l1pos := 0;
    l2pos := 0;
    while ep <> nil do begin

        { skip keep entries, tracking position }
        keeploop := true;
        while (ep <> nil) and keeploop do begin

            if ep^.op <> opkeep then
                keeploop := false
            else begin

                l1pos := ep^.line1;
                l2pos := ep^.line2;
                ep := ep^.next

            end

        end;
        if ep <> nil then begin

            { count dels and ins in this change block }
            delcount := 0;
            inscount := 0;
            delstart := 0;
            insstart := 0;
            inchange := true;
            while (ep <> nil) and inchange do begin

                if ep^.op = opkeep then
                    inchange := false
                else begin

                    if ep^.op = opdel then begin

                        if delcount = 0 then delstart := ep^.line1;
                        delcount := delcount + 1

                    end else begin

                        if inscount = 0 then insstart := ep^.line2;
                        inscount := inscount + 1

                    end;
                    ep := ep^.next

                end

            end;
            { output diff header }
            if (delcount > 0) and (inscount > 0) then begin

                if delcount = 1 then write(delstart:1)
                else write(delstart:1, ',', delstart + delcount - 1:1);
                write('c');
                if inscount = 1 then writeln(insstart:1)
                else writeln(insstart:1, ',', insstart + inscount - 1:1)

            end else if delcount > 0 then begin

                if delcount = 1 then write(delstart:1)
                else write(delstart:1, ',', delstart + delcount - 1:1);
                writeln('d', l2pos:1)

            end else if inscount > 0 then begin

                write(l1pos:1, 'a');
                if inscount = 1 then writeln(insstart:1)
                else writeln(insstart:1, ',', insstart + inscount - 1:1)

            end;
            { output deleted lines }
            if delcount > 0 then
                for i := delstart to delstart + delcount - 1 do
                    if lines1[i] <> nil then writeln('< ', lines1[i]^);
            { separator }
            if (delcount > 0) and (inscount > 0) then
                writeln('---');
            { output inserted lines }
            if inscount > 0 then
                for i := insstart to insstart + inscount - 1 do
                    if lines2[i] <> nil then writeln('> ', lines2[i]^)

        end

    end

end;

var

    { first filename } file1: pstring;
    { second filename } file2: pstring;
    { first file handle } f1: text;
    { second file handle } f2: text;

begin

    editlist := nil;
    editlast := nil;
    readcmdline;
    parsecmd(file1, file2);
    if (file1 = nil) or (file2 = nil) then begin

        writeln('Usage: dif [-w|-nw] file1 file2');
        writeln('  -w   Ignore whitespace (default)');
        writeln('  -nw  Exact comparison');
        halt

    end;
    assign(f1, file1^);
    reset(f1);
    readfile1(f1, nlines1);
    close(f1);
    assign(f2, file2^);
    reset(f2);
    readfile2(f2, nlines2);
    close(f2);
    computediff;
    showdiff

end.
