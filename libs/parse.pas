{******************************************************************************
*                                                                             *
*                        Character Level Parsing Library                      *
*                                                                             *
*                             2002/8 S. A. Moore                              *
*                                                                             *
* Contains functions to parse at the character level. The user opens a        *
* parsing level that includes a file to read input from. Each parsing level   *
* has a line buffer associated with it, and parsing occurs indirectly from    *
* that buffer. The user can choose whether to parse a single line at a time,  *
* or multiple lines, or a mix of those two. It is possible to nest file       *
* levels, and there is a stacking system to hold the current parsing position *
* for backtracking. The backtrack system does not track further than the      *
* current line, but there is nothing to prevent adding that. Right now, the   *
* behavior is to dump all line positions silently when advancing to the next  *
* line.                                                                       *
*                                                                             *
* Parlib can be used in several common modes:                                 *
*                                                                             *
* - Place a string into the parser, and parse that.                           *
*                                                                             *
* - Open the "command" file as the source, read a single line from it, and    *
*   parse that.                                                               *
*                                                                             *
* - Parse full file.                                                          *
*                                                                             *
* - Parse multiple files, with nesting.                                       *
*                                                                             *
* Parlib is generally used for parsing command lines, but also finds use      *
* parsing simple command files, initalization files, etc.                     *
*                                                                             *
* Parlib might be a building block to a supergeneral parser, but note that    *
* parlib is somewhat Pascal syntax oriented at present (for example in the    *
* numeric radix specifiers). It would be nice to have a BNF defined parsing   *
* facillity ontop parlib.                                                     *
*                                                                             *
* Routines in this module:                                                    *
*                                                                             *
* procedure openpar(var ph: parhan);                                          *
*                                                                             *
*      Opens a parser instance, setting the given handle valid.               *
*                                                                             *
* procedure closepar(ph: parhan);                                             *
*                                                                             *
*      Closes a parser instance, and disconnects its handle.                  *
*                                                                             *
* procedure openstr(ph: parhan; view s: string);                              *
*                                                                             *
*      Opens a string as a parser file.                                       *
*                                                                             *
* procedure openfil(ph: parhan; view fn: string; blen: integer);              *
*                                                                             *
*      Opens a file for parsing.                                              *
*                                                                             *
* procedure opencommand(ph: parhan; view fn: string; blen: integer);          *
*                                                                             *
*      Opens a file for parsing.                                              *
*                                                                             *
* procedure closefil(ph: parhan);                                             *
*                                                                             *
*      Close a file or string level for parsing.                              *
*                                                                             *
* function endfil(ph: parhan): boolean;                                       *
*                                                                             *
*      Check end of file reached.                                             *
*                                                                             *
* function endlin(ph: parhan): boolean;                                       *
*                                                                             *
*      Check end of line reached.                                             *
*                                                                             *
* function chkchr(ph: parhan): char;                                          *
*                                                                             *
*      Return next character on current line.                                 *
*                                                                             *
* procedure getchr(ph: parhan);                                               *
*                                                                             *
*      Advance to next character on current line.                             *
*                                                                             *
* procedure skpspc(ph: parhan);                                               *
*                                                                             *
*      Skip spaces on current line.                                           *
*                                                                             *
* procedure getchrl(ph: parhan);                                              *
*                                                                             *
*      Advance to next character, including on next line.                     *
*                                                                             *
* procedure skpspcl(ph: parhan);                                              *
*                                                                             *
*      Skip spaces, including any number of blank lines.                      *
*                                                                             *
* procedure getlin(ph: parhan);                                               *
*                                                                             *
*      Dump current line and advance to next.                                 *
*                                                                             *
* procedure pushpos(ph: parhan);                                              *
*                                                                             *
*      Remember and stack current line position.                              *
*                                                                             *
* procedure poppos(ph: parhan);                                               *
*                                                                             *
*      Restore last line position in stack.                                   *
*                                                                             *
* procedure dmppos(ph: parhan);                                               *
*                                                                             *
*      Dispose of last line position in stack.                                *
*                                                                             *
* function chklab(ph: parhan): boolean;                                       *
*                                                                             *
*      Check next character sequence is valid label start.                    *
*                                                                             *
* procedure parlab(ph: parhan; var l: string; var err: boolean);              *
*                                                                             *
*      Parse label.                                                           *
*                                                                             *
* function chknum(ph: parhan; r: integer): boolean;                           *
*                                                                             *
*      Check next character sequence is valid number start.                   *
*                                                                             *
* procedure parnum(ph: parhan; var i: integer; r: integer; var err: boolean); *
*                                                                             *
*      Parse number, including radix.                                         *
*                                                                             *
* function chkfil(ph: parhan): boolean;                                       * 
*                                                                             *
*      Check next character sequence is valid filename start.                 *
*                                                                             *
* procedure parfil(ph: parhan; var n: string; path: boolean;                  *
*                  var err: boolean);                                         *
*                                                                             *
*      Parse filename.                                                        *
*                                                                             *
* procedure parwrd(ph: parhan; var n: string; var err: boolean);              *
*                                                                             *
*      Parse space delimited word.                                            *
*                                                                             *
* procedure setfch(ph: parhan; view vc: schar);                              *
*                                                                             *
*      Set valid file characters.                                             *
*                                                                             *
* function chkstr(ph: parhan): boolean;                                       *
*                                                                             *
*      Check next character sequence is valid string.                         *
*                                                                             *
* procedure parstr(ph: parhan; var s: string; var err: boolean);              *
*                                                                             *
*      Parse string.                                                          *
*                                                                             *
* procedure prterr(ph: parhan; var ef: text; view es: string; pl: boolean);   *
*                                                                             *
*      Print formatted error with diagnostics.                                *
*                                                                             *
* procedure trclin(ph: parhan; trc: boolean);                                 *
*                                                                             *
*      Print each line read from parse file.                                  *
*                                                                             *
* Notes:                                                                      *
*                                                                             *
* 1. Could add \force sequences to parstr.                                    *
*                                                                             *
* 2. Could add floating point parse routine.                                  *
*                                                                             *
******************************************************************************}

module parse;

const maxpar = 100; { number of logical parse handles }

type 

   parhan = 1..maxpar; { logical parse handle }

{ functions }

procedure openpar(var ph: parhan); external;
procedure closepar(ph: parhan); external;
procedure openstr(ph: parhan; view s: string); external;
procedure openfil(ph: parhan; view fn: string; blen: integer); external;
procedure opencommand(ph: parhan; blen: integer); external;
procedure closefil(ph: parhan); external;
function endfil(ph: parhan): boolean; external;
function endlin(ph: parhan): boolean; external;
function chkchr(ph: parhan): char; external;
procedure getchr(ph: parhan); external;
procedure skpspc(ph: parhan); external;
procedure getchrl(ph: parhan); external;
procedure skpspcl(ph: parhan); external;
procedure getlin(ph: parhan); external;
procedure pushpos(ph: parhan); external;
procedure poppos(ph: parhan); external;
procedure dmppos(ph: parhan); external;
function chklab(ph: parhan): boolean; external;
procedure parlab(ph: parhan; var l: string; var err: boolean); external;
function chknum(ph: parhan; r: integer): boolean; external;
procedure parnum(ph: parhan; var i: integer; r: integer; var err: boolean);
   external;
function chkfil(ph: parhan): boolean; external;
procedure parfil(ph: parhan; var n: string; path: boolean; var err: boolean);
   external;
procedure parwrd(ph: parhan; var w: string; var err: boolean); external;
procedure setfch(ph: parhan; view vc: schar); external;
function chkstr(ph: parhan): boolean; external;
procedure parstr(ph: parhan; var s: string; var err: boolean); external;
procedure prterr(ph: parhan; var ef: text; view es: string; pl: boolean);
   external;
procedure trclin(ph: parhan; trc: boolean); external;

begin
end.
