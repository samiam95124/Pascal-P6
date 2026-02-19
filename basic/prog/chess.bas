rem =====================================================================
rem  Chess Game for IP Basic
rem  Human vs Computer with ASCII art board and ANSI color display
rem  Uses gnome terminal escape codes for colored board rendering
rem  Computer uses alpha-beta minimax search (3-4 ply)
rem =====================================================================

rem =====================================================================
rem  Global constants and escape code strings
rem =====================================================================
esc$ = chr$(27)
csi$ = esc$ + "["
cls$ = csi$ + "2J" + csi$ + "H"
rst$ = csi$ + "0m"
hide$ = csi$ + "?25l"
show$ = csi$ + "?25h"
bold$ = csi$ + "1m"

rem board square background colors (24-bit RGB)
ltsq$ = csi$ + "48;2;240;217;181m"
dksq$ = csi$ + "48;2;181;136;99m"

rem piece foreground colors
wfg$ = csi$ + "38;2;255;255;255m" + bold$
bfg$ = csi$ + "38;2;40;40;40m" + bold$

rem piece type constants
pawn% = 1
knight% = 2
bishop% = 3
rook% = 4
queen% = 5
king% = 6

rem search depth for AI
maxdepth% = 3

rem infinity for alpha-beta
inf% = 30000

rem =====================================================================
rem  Piece art DATA (6 pieces x 4 rows, each 7 chars wide)
rem  Order: pawn, knight, bishop, rook, queen, king
rem =====================================================================
data "       ", "  (o)  ", "  /+\  ", " /___\ "
data "   _   ", "  /o)  ", "  |/   ", " /___\ "
data "   o   ", "  /+\  ", "  ) (  ", " /___\ "
data " |_|_| ", " |   | ", " |   | ", " /___\ "
data " _\|/_ ", "  ) (  ", "  | |  ", " /___\ "
data "  _+_  ", " ( | ) ", "  ) (  ", " /___\ "

rem =====================================================================
rem  Board and game state arrays
rem =====================================================================
dim board%(8, 8)
dim pa$(6, 4)

rem move list arrays for each search level (0-4)
dim m0fr%(200), m0fc%(200), m0tr%(200), m0tc%(200)
dim m1fr%(200), m1fc%(200), m1tr%(200), m1tc%(200)
dim m2fr%(200), m2fc%(200), m2tr%(200), m2tc%(200)
dim m3fr%(200), m3fc%(200), m3tr%(200), m3tc%(200)
dim m4fr%(200), m4fc%(200), m4tr%(200), m4tc%(200)
dim nm%(4)

rem captured piece storage per level
dim cap%(4)
rem source piece storage per level (original piece before promotion)
dim src%(4)
rem en passant flag per level
dim epf%(4)

rem game state
side% = 1
gameover% = 0
eprow% = 0
epcol% = 0
wkc% = 1
wqc% = 1
bkc% = 1
bqc% = 1
flip% = 0
human% = 1
movenum% = 0

rem castling state save per level
dim swkc%(4), swqc%(4), sbkc%(4), sbqc%(4)
dim sepr%(4), sepc%(4)

rem piece-square table for pawns (row bonus from white perspective)
dim pstpawn%(8)
pstpawn%(1) = 0
pstpawn%(2) = 50
pstpawn%(3) = 30
pstpawn%(4) = 15
pstpawn%(5) = 10
pstpawn%(6) = 5
pstpawn%(7) = 0
pstpawn%(8) = 0

rem center control bonus by column
dim ccol%(8)
ccol%(1) = 0
ccol%(2) = 1
ccol%(3) = 2
ccol%(4) = 3
ccol%(5) = 3
ccol%(6) = 2
ccol%(7) = 1
ccol%(8) = 0

dim crow%(8)
crow%(1) = 0
crow%(2) = 1
crow%(3) = 2
crow%(4) = 3
crow%(5) = 3
crow%(6) = 2
crow%(7) = 1
crow%(8) = 0

rem knight deltas
dim kndr%(8), kndc%(8)
kndr%(1) = -2: kndc%(1) = -1
kndr%(2) = -2: kndc%(2) = 1
kndr%(3) = -1: kndc%(3) = -2
kndr%(4) = -1: kndc%(4) = 2
kndr%(5) = 1: kndc%(5) = -2
kndr%(6) = 1: kndc%(6) = 2
kndr%(7) = 2: kndc%(7) = -1
kndr%(8) = 2: kndc%(8) = 1

rem king/queen direction deltas
dim kdr%(8), kdc%(8)
kdr%(1) = -1: kdc%(1) = -1
kdr%(2) = -1: kdc%(2) = 0
kdr%(3) = -1: kdc%(3) = 1
kdr%(4) = 0: kdc%(4) = -1
kdr%(5) = 0: kdc%(5) = 1
kdr%(6) = 1: kdc%(6) = -1
kdr%(7) = 1: kdc%(7) = 0
kdr%(8) = 1: kdc%(8) = 1

rem =====================================================================
rem  Load piece art from DATA
rem =====================================================================
restore
for p% = 1 to 6
    for r% = 1 to 4
        read pa$(p%, r%)
    next r%
next p%

rem =====================================================================
rem  Utility procedures
rem =====================================================================

procedure cls
    print cls$;
endproc

function findch%(s$, c$)
    local i, f
    i% = 1
    f% = 0
    while i% <= len(s$) and f% = 0
        if mid$(s$, i%, 1) = c$ then f% = 1 else i% = i% + 1
    wend
    if f% = 0 then i% = 0
endfunc i%

rem =====================================================================
rem  Board initialization
rem =====================================================================

procedure initboard
    local r, c
    rem clear the board
    for r% = 1 to 8
        for c% = 1 to 8
            board%(r%, c%) = 0
        next c%
    next r%
    rem black pieces on rows 1-2
    board%(1, 1) = -rook%
    board%(1, 2) = -knight%
    board%(1, 3) = -bishop%
    board%(1, 4) = -queen%
    board%(1, 5) = -king%
    board%(1, 6) = -bishop%
    board%(1, 7) = -knight%
    board%(1, 8) = -rook%
    for c% = 1 to 8
        board%(2, c%) = -pawn%
    next c%
    rem white pieces on rows 7-8
    for c% = 1 to 8
        board%(7, c%) = pawn%
    next c%
    board%(8, 1) = rook%
    board%(8, 2) = knight%
    board%(8, 3) = bishop%
    board%(8, 4) = queen%
    board%(8, 5) = king%
    board%(8, 6) = bishop%
    board%(8, 7) = knight%
    board%(8, 8) = rook%
    rem reset game state
    side% = 1
    gameover% = 0
    eprow% = 0: epcol% = 0
    wkc% = 1: wqc% = 1: bkc% = 1: bqc% = 1
    movenum% = 0
endproc

rem =====================================================================
rem  Board display with ASCII art pieces and ANSI colors
rem =====================================================================

procedure drawboard
    local r, c, ar, dr, p, ap, sq, lt, pc
    print cls$;
    print hide$;
    rem column headers
    print "    ";
    for c% = 1 to 8
        print "   " + mid$("12345678", c%, 1) + "   ";
    next c%
    print
    rem draw each board row
    for r% = 1 to 8
        if flip% = 0 then
            dr% = r%
        else
            dr% = 9 - r%
        endif
        rem draw 4 art rows per board row
        for ar% = 1 to 4
            rem row label on 2nd art row
            if ar% = 2 then
                print " " + mid$("12345678", dr%, 1) + "  ";
            else
                print "    ";
            endif
            rem draw 8 columns
            for c% = 1 to 8
                p% = board%(dr%, c%)
                ap% = abs(p%)
                rem determine square color
                if (dr% + c%) mod 2 = 0 then
                    sq% = 1
                    print ltsq$;
                else
                    sq% = 0
                    print dksq$;
                endif
                rem set piece color
                if p% > 0 then
                    print wfg$;
                else
                    if p% < 0 then
                        print bfg$;
                    endif
                endif
                rem print piece art or empty space
                if ap% >= 1 and ap% <= 6 then
                    lt$ = pa$(ap%, ar%)
                    rem substitute letter based on piece color
                    if ar% = 2 then
                        rem row 2 has the piece letter
                        if ap% = pawn% then pc$ = "P"
                        if ap% = knight% then pc$ = "N"
                        if ap% = bishop% then pc$ = "B"
                        if ap% = rook% then pc$ = "R"
                        if ap% = queen% then pc$ = "Q"
                        if ap% = king% then pc$ = "K"
                        if p% < 0 then pc$ = lcase$(pc$)
                        rem find letter position and substitute
                        if ap% = pawn% then lt$ = "  (" + pc$ + ")  "
                        if ap% = knight% then lt$ = "  /" + pc$ + ")  "
                        if ap% = bishop% then lt$ = "  /" + pc$ + "\  "
                        if ap% = rook% then lt$ = " | " + pc$ + " | "
                        if ap% = queen% then lt$ = "  )" + pc$ + "(  "
                        if ap% = king% then lt$ = " ( " + pc$ + " ) "
                    endif
                    print lt$;
                else
                    print "       ";
                endif
                print rst$;
            next c%
            print
        next ar%
    next r%
    rem column footers
    print "    ";
    for c% = 1 to 8
        print "   " + mid$("12345678", c%, 1) + "   ";
    next c%
    print
    print show$;
endproc

rem =====================================================================
rem  Add move to move list at given level
rem =====================================================================

procedure addmove(fr%, fc%, tr%, tc%, lv%)
    nm%(lv%) = nm%(lv%) + 1
    if lv% = 0 then m0fr%(nm%(0)) = fr%: m0fc%(nm%(0)) = fc%: m0tr%(nm%(0)) = tr%: m0tc%(nm%(0)) = tc%
    if lv% = 1 then m1fr%(nm%(1)) = fr%: m1fc%(nm%(1)) = fc%: m1tr%(nm%(1)) = tr%: m1tc%(nm%(1)) = tc%
    if lv% = 2 then m2fr%(nm%(2)) = fr%: m2fc%(nm%(2)) = fc%: m2tr%(nm%(2)) = tr%: m2tc%(nm%(2)) = tc%
    if lv% = 3 then m3fr%(nm%(3)) = fr%: m3fc%(nm%(3)) = fc%: m3tr%(nm%(3)) = tr%: m3tc%(nm%(3)) = tc%
    if lv% = 4 then m4fr%(nm%(4)) = fr%: m4fc%(nm%(4)) = fc%: m4tr%(nm%(4)) = tr%: m4tc%(nm%(4)) = tc%
endproc

rem get move from list at given level
function gmfr%(i%, lv%)
    local r
    r% = m4fr%(i%)
    if lv% = 0 then r% = m0fr%(i%)
    if lv% = 1 then r% = m1fr%(i%)
    if lv% = 2 then r% = m2fr%(i%)
    if lv% = 3 then r% = m3fr%(i%)
endfunc r%

function gmfc%(i%, lv%)
    local r
    r% = m4fc%(i%)
    if lv% = 0 then r% = m0fc%(i%)
    if lv% = 1 then r% = m1fc%(i%)
    if lv% = 2 then r% = m2fc%(i%)
    if lv% = 3 then r% = m3fc%(i%)
endfunc r%

function gmtr%(i%, lv%)
    local r
    r% = m4tr%(i%)
    if lv% = 0 then r% = m0tr%(i%)
    if lv% = 1 then r% = m1tr%(i%)
    if lv% = 2 then r% = m2tr%(i%)
    if lv% = 3 then r% = m3tr%(i%)
endfunc r%

function gmtc%(i%, lv%)
    local r
    r% = m4tc%(i%)
    if lv% = 0 then r% = m0tc%(i%)
    if lv% = 1 then r% = m1tc%(i%)
    if lv% = 2 then r% = m2tc%(i%)
    if lv% = 3 then r% = m3tc%(i%)
endfunc r%

rem =====================================================================
rem  Move generation
rem =====================================================================

procedure genmoves(s%, lv%)
    local r, c, p, ap, tr, tc, d, nr, nc, fwd, blk
    nm%(lv%) = 0
    if s% > 0 then fwd% = -1 else fwd% = 1
    for r% = 1 to 8
        for c% = 1 to 8
            p% = board%(r%, c%)
            if p% = 0 or sgn(p%) <> s% then goto gmnext
            ap% = abs(p%)

            rem --- Pawn moves ---
            if ap% <> pawn% then goto gmpdone
                rem single push
                tr% = r% + fwd%
                if tr% >= 1 and tr% <= 8 then
                    if board%(tr%, c%) = 0 then
                        addmove(r%, c%, tr%, c%, lv%)
                        rem double push from starting row
                        if s% > 0 and r% = 7 then
                            if board%(5, c%) = 0 then addmove(r%, c%, 5, c%, lv%)
                        endif
                        if s% < 0 and r% = 2 then
                            if board%(4, c%) = 0 then addmove(r%, c%, 4, c%, lv%)
                        endif
                    endif
                    rem captures
                    if c% > 1 then
                        if board%(tr%, c% - 1) <> 0 and sgn(board%(tr%, c% - 1)) <> s% then
                            addmove(r%, c%, tr%, c% - 1, lv%)
                        endif
                        rem en passant
                        if tr% = eprow% and c% - 1 = epcol% then
                            addmove(r%, c%, tr%, c% - 1, lv%)
                        endif
                    endif
                    if c% < 8 then
                        if board%(tr%, c% + 1) <> 0 and sgn(board%(tr%, c% + 1)) <> s% then
                            addmove(r%, c%, tr%, c% + 1, lv%)
                        endif
                        rem en passant
                        if tr% = eprow% and c% + 1 = epcol% then
                            addmove(r%, c%, tr%, c% + 1, lv%)
                        endif
                    endif
                endif
            gmpdone:

            rem --- Knight moves ---
            if ap% <> knight% then goto gmkndone
                for d% = 1 to 8
                    nr% = r% + kndr%(d%)
                    nc% = c% + kndc%(d%)
                    if nr% >= 1 and nr% <= 8 and nc% >= 1 and nc% <= 8 then
                        if board%(nr%, nc%) = 0 or sgn(board%(nr%, nc%)) <> s% then
                            addmove(r%, c%, nr%, nc%, lv%)
                        endif
                    endif
                next d%
            gmkndone:

            rem --- Bishop moves (diagonals) ---
            if ap% <> bishop% and ap% <> queen% then goto gmbdone
                for d% = 1 to 4
                    if d% = 1 then nr% = -1: nc% = -1
                    if d% = 2 then nr% = -1: nc% = 1
                    if d% = 3 then nr% = 1: nc% = -1
                    if d% = 4 then nr% = 1: nc% = 1
                    tr% = r% + nr%: tc% = c% + nc%
                    blk% = 0
                    while tr% >= 1 and tr% <= 8 and tc% >= 1 and tc% <= 8 and blk% = 0
                        if board%(tr%, tc%) = 0 then
                            addmove(r%, c%, tr%, tc%, lv%)
                        else
                            if sgn(board%(tr%, tc%)) <> s% then
                                addmove(r%, c%, tr%, tc%, lv%)
                            endif
                            blk% = 1
                        endif
                        if blk% = 0 then tr% = tr% + nr%: tc% = tc% + nc%
                    wend
                next d%
            gmbdone:

            rem --- Rook moves (straights) ---
            if ap% <> rook% and ap% <> queen% then goto gmrdone
                for d% = 1 to 4
                    if d% = 1 then nr% = -1: nc% = 0
                    if d% = 2 then nr% = 1: nc% = 0
                    if d% = 3 then nr% = 0: nc% = -1
                    if d% = 4 then nr% = 0: nc% = 1
                    tr% = r% + nr%: tc% = c% + nc%
                    blk% = 0
                    while tr% >= 1 and tr% <= 8 and tc% >= 1 and tc% <= 8 and blk% = 0
                        if board%(tr%, tc%) = 0 then
                            addmove(r%, c%, tr%, tc%, lv%)
                        else
                            if sgn(board%(tr%, tc%)) <> s% then
                                addmove(r%, c%, tr%, tc%, lv%)
                            endif
                            blk% = 1
                        endif
                        if blk% = 0 then tr% = tr% + nr%: tc% = tc% + nc%
                    wend
                next d%
            gmrdone:

            rem --- King moves ---
            if ap% <> king% then goto gmkgdone
                for d% = 1 to 8
                    nr% = r% + kdr%(d%)
                    nc% = c% + kdc%(d%)
                    if nr% >= 1 and nr% <= 8 and nc% >= 1 and nc% <= 8 then
                        if board%(nr%, nc%) = 0 or sgn(board%(nr%, nc%)) <> s% then
                            addmove(r%, c%, nr%, nc%, lv%)
                        endif
                    endif
                next d%
                rem castling
                if s% > 0 and r% = 8 and c% = 5 then
                    rem white kingside
                    if wkc% = 1 and board%(8, 6) = 0 and board%(8, 7) = 0 then
                        addmove(8, 5, 8, 7, lv%)
                    endif
                    rem white queenside
                    if wqc% = 1 and board%(8, 4) = 0 and board%(8, 3) = 0 and board%(8, 2) = 0 then
                        addmove(8, 5, 8, 3, lv%)
                    endif
                endif
                if s% < 0 and r% = 1 and c% = 5 then
                    rem black kingside
                    if bkc% = 1 and board%(1, 6) = 0 and board%(1, 7) = 0 then
                        addmove(1, 5, 1, 7, lv%)
                    endif
                    rem black queenside
                    if bqc% = 1 and board%(1, 4) = 0 and board%(1, 3) = 0 and board%(1, 2) = 0 then
                        addmove(1, 5, 1, 3, lv%)
                    endif
                endif
            gmkgdone:

            gmnext:
        next c%
    next r%
endproc

rem =====================================================================
rem  Check detection - is side s% in check?
rem =====================================================================

function incheck%(s%)
    local kr, kc, r, c, d, nr, nc, p, ap, fwd, blk, found
    rem find the king
    kr% = 0: kc% = 0
    for r% = 1 to 8
        for c% = 1 to 8
            if board%(r%, c%) = s% * king% then kr% = r%: kc% = c%
        next c%
    next r%
    found% = 0
    if kr% = 0 then found% = 0: goto ichkdone
    rem check knight attacks
    for d% = 1 to 8
        nr% = kr% + kndr%(d%)
        nc% = kc% + kndc%(d%)
        if nr% >= 1 and nr% <= 8 and nc% >= 1 and nc% <= 8 then
            if board%(nr%, nc%) = -s% * knight% then found% = 1
        endif
    next d%
    if found% = 1 then goto ichkdone
    rem check sliding attacks (bishop/rook/queen)
    for d% = 1 to 8
        nr% = kr% + kdr%(d%)
        nc% = kc% + kdc%(d%)
        r% = 1
        blk% = 0
        while nr% >= 1 and nr% <= 8 and nc% >= 1 and nc% <= 8 and blk% = 0
            p% = board%(nr%, nc%)
            if p% <> 0 then
                blk% = 1
                if sgn(p%) <> s% then
                    ap% = abs(p%)
                    rem diagonal directions
                    if d% = 1 or d% = 3 or d% = 6 or d% = 8 then
                        if ap% = bishop% or ap% = queen% then found% = 1
                        if ap% = king% and r% = 1 then found% = 1
                    endif
                    rem straight directions
                    if d% = 2 or d% = 4 or d% = 5 or d% = 7 then
                        if ap% = rook% or ap% = queen% then found% = 1
                        if ap% = king% and r% = 1 then found% = 1
                    endif
                endif
            else
                nr% = nr% + kdr%(d%)
                nc% = nc% + kdc%(d%)
                r% = r% + 1
            endif
        wend
    next d%
    if found% = 1 then goto ichkdone
    rem check pawn attacks
    if s% > 0 then fwd% = -1 else fwd% = 1
    nr% = kr% + fwd%
    if nr% >= 1 and nr% <= 8 then
        if kc% > 1 then
            if board%(nr%, kc% - 1) = -s% * pawn% then found% = 1
        endif
        if kc% < 8 then
            if board%(nr%, kc% + 1) = -s% * pawn% then found% = 1
        endif
    endif
    ichkdone:
endfunc found%

rem =====================================================================
rem  Make move - execute on board, save state for unmake
rem =====================================================================

procedure makemove(fr%, fc%, tr%, tc%, lv%)
    local p, ap
    p% = board%(fr%, fc%)
    ap% = abs(p%)
    rem save state
    src%(lv%) = p%
    cap%(lv%) = board%(tr%, tc%)
    epf%(lv%) = 0
    swkc%(lv%) = wkc%: swqc%(lv%) = wqc%
    sbkc%(lv%) = bkc%: sbqc%(lv%) = bqc%
    sepr%(lv%) = eprow%: sepc%(lv%) = epcol%
    rem en passant capture
    if ap% = pawn% and tc% = epcol% and tr% = eprow% then
        if p% > 0 then
            board%(tr% + 1, tc%) = 0
        else
            board%(tr% - 1, tc%) = 0
        endif
        epf%(lv%) = 1
    endif
    rem set en passant target for double pawn push
    eprow% = 0: epcol% = 0
    if ap% = pawn% then
        if abs(tr% - fr%) = 2 then
            eprow% = (fr% + tr%) / 2
            epcol% = fc%
        endif
    endif
    rem make the move
    board%(tr%, tc%) = p%
    board%(fr%, fc%) = 0
    rem pawn promotion (auto-queen)
    if ap% = pawn% then
        if p% > 0 and tr% = 1 then board%(tr%, tc%) = queen%
        if p% < 0 and tr% = 8 then board%(tr%, tc%) = -queen%
    endif
    rem castling - move the rook too
    if ap% = king% then
        rem white kingside castle
        if fr% = 8 and fc% = 5 and tr% = 8 and tc% = 7 then
            board%(8, 8) = 0: board%(8, 6) = rook%
        endif
        rem white queenside castle
        if fr% = 8 and fc% = 5 and tr% = 8 and tc% = 3 then
            board%(8, 1) = 0: board%(8, 4) = rook%
        endif
        rem black kingside castle
        if fr% = 1 and fc% = 5 and tr% = 1 and tc% = 7 then
            board%(1, 8) = 0: board%(1, 6) = -rook%
        endif
        rem black queenside castle
        if fr% = 1 and fc% = 5 and tr% = 1 and tc% = 3 then
            board%(1, 1) = 0: board%(1, 4) = -rook%
        endif
    endif
    rem update castling rights
    if ap% = king% then
        if p% > 0 then wkc% = 0: wqc% = 0
        if p% < 0 then bkc% = 0: bqc% = 0
    endif
    if ap% = rook% then
        if fr% = 8 and fc% = 8 then wkc% = 0
        if fr% = 8 and fc% = 1 then wqc% = 0
        if fr% = 1 and fc% = 8 then bkc% = 0
        if fr% = 1 and fc% = 1 then bqc% = 0
    endif
    rem if a rook is captured, update castling
    if tr% = 8 and tc% = 8 then wkc% = 0
    if tr% = 8 and tc% = 1 then wqc% = 0
    if tr% = 1 and tc% = 8 then bkc% = 0
    if tr% = 1 and tc% = 1 then bqc% = 0
endproc

rem =====================================================================
rem  Unmake move - reverse the last move at given level
rem =====================================================================

procedure unmake(fr%, fc%, tr%, tc%, lv%)
    local p
    p% = src%(lv%)
    rem restore piece to source (handles promotion: restores original pawn)
    board%(fr%, fc%) = p%
    rem restore captured piece at destination
    board%(tr%, tc%) = cap%(lv%)
    rem undo en passant capture
    if epf%(lv%) = 1 then
        if p% > 0 then
            board%(tr% + 1, tc%) = -pawn%
        else
            board%(tr% - 1, tc%) = pawn%
        endif
    endif
    rem undo castling rook moves
    if abs(p%) = king% then
        if fr% = 8 and fc% = 5 and tr% = 8 and tc% = 7 then
            board%(8, 6) = 0: board%(8, 8) = rook%
        endif
        if fr% = 8 and fc% = 5 and tr% = 8 and tc% = 3 then
            board%(8, 4) = 0: board%(8, 1) = rook%
        endif
        if fr% = 1 and fc% = 5 and tr% = 1 and tc% = 7 then
            board%(1, 6) = 0: board%(1, 8) = -rook%
        endif
        if fr% = 1 and fc% = 5 and tr% = 1 and tc% = 3 then
            board%(1, 4) = 0: board%(1, 1) = -rook%
        endif
    endif
    rem restore state
    wkc% = swkc%(lv%): wqc% = swqc%(lv%)
    bkc% = sbkc%(lv%): bqc% = sbqc%(lv%)
    eprow% = sepr%(lv%): epcol% = sepc%(lv%)
endproc

rem =====================================================================
rem  Check if a move is legal (doesn't leave own king in check)
rem =====================================================================

function islegal%(fr%, fc%, tr%, tc%, s%, lv%)
    local chk, res
    makemove(fr%, fc%, tr%, tc%, lv%)
    chk% = incheck%(s%)
    unmake(fr%, fc%, tr%, tc%, lv%)
    if chk% = 1 then res% = 0 else res% = 1
endfunc res%

rem =====================================================================
rem  Static evaluation from white's perspective
rem =====================================================================

function evaluate%
    local r, c, p, ap, sc, psq
    sc% = 0
    for r% = 1 to 8
        for c% = 1 to 8
            p% = board%(r%, c%)
            if p% <> 0 then
                ap% = abs(p%)
                rem material value
                if ap% = pawn% then psq% = 100
                if ap% = knight% then psq% = 320
                if ap% = bishop% then psq% = 330
                if ap% = rook% then psq% = 500
                if ap% = queen% then psq% = 900
                if ap% = king% then psq% = 20000
                rem positional bonuses
                if ap% = pawn% then
                    if p% > 0 then
                        psq% = psq% + pstpawn%(r%)
                    else
                        psq% = psq% + pstpawn%(9 - r%)
                    endif
                endif
                rem center control for knights and bishops
                if ap% = knight% or ap% = bishop% then
                    psq% = psq% + ccol%(c%) + crow%(r%)
                endif
                rem mobility bonus for knights
                if ap% = knight% then
                    psq% = psq% + 2
                endif
                rem king safety: prefer corners in middlegame
                rem (simple: penalty for king in center)
                if ap% = king% then
                    if c% >= 3 and c% <= 6 then psq% = psq% - 20
                endif
                if p% > 0 then
                    sc% = sc% + psq%
                else
                    sc% = sc% - psq%
                endif
            endif
        next c%
    next r%
endfunc sc%

rem =====================================================================
rem  Alpha-Beta search
rem  Returns score from the perspective of side s%
rem  s%: side to move (1=white, -1=black)
rem  depth%: remaining depth
rem  alpha%, beta%: pruning bounds
rem  lv%: search level (for move array indexing)
rem =====================================================================

function alphabeta%(s%, depth%, alpha%, beta%, lv%)
    local i, n, fr, fc, tr, tc, sc, best, haslegal, a, cut, res
    rem base case: evaluate
    if depth% <> 0 then goto abmain
    sc% = evaluate%
    if s% < 0 then sc% = -sc%
    res% = sc%
    goto abdone
    abmain:
    rem generate moves
    genmoves(s%, lv%)
    n% = nm%(lv%)
    best% = -inf% - 1
    haslegal% = 0
    a% = alpha%
    cut% = 0
    i% = 1
    while i% <= n% and cut% = 0
        fr% = gmfr%(i%, lv%)
        fc% = gmfc%(i%, lv%)
        tr% = gmtr%(i%, lv%)
        tc% = gmtc%(i%, lv%)
        rem check legality
        if islegal%(fr%, fc%, tr%, tc%, s%, lv%) = 1 then
            haslegal% = 1
            makemove(fr%, fc%, tr%, tc%, lv%)
            sc% = -alphabeta%(-s%, depth% - 1, -beta%, -a%, lv% + 1)
            unmake(fr%, fc%, tr%, tc%, lv%)
            if sc% > best% then best% = sc%
            if sc% > a% then a% = sc%
            if a% >= beta% then cut% = 1
        endif
        i% = i% + 1
    wend
    rem determine result
    if cut% = 1 then res% = a% else res% = best%
    if haslegal% = 0 then
        if incheck%(s%) = 1 then res% = -inf% + lv% else res% = 0
    endif
    abdone:
endfunc res%

rem =====================================================================
rem  Computer move selection
rem =====================================================================

procedure compmove
    local i, n, fr, fc, tr, tc, sc, best, bi, a, nmov
    print "Computer is thinking..."
    genmoves(side%, 0)
    n% = nm%(0)
    best% = -inf% - 1
    bi% = 0
    nmov% = 0
    i% = 1
    while i% <= n%
        fr% = gmfr%(i%, 0)
        fc% = gmfc%(i%, 0)
        tr% = gmtr%(i%, 0)
        tc% = gmtc%(i%, 0)
        if islegal%(fr%, fc%, tr%, tc%, side%, 0) = 1 then
            nmov% = nmov% + 1
            makemove(fr%, fc%, tr%, tc%, 0)
            sc% = -alphabeta%(-side%, maxdepth% - 1, -inf%, inf%, 1)
            unmake(fr%, fc%, tr%, tc%, 0)
            rem add small random factor to break ties
            sc% = sc% + int(rnd(1) * 3) - 1
            if sc% > best% then
                best% = sc%
                bi% = i%
            endif
        endif
        i% = i% + 1
    wend
    if bi% > 0 then
        fr% = gmfr%(bi%, 0)
        fc% = gmfc%(bi%, 0)
        tr% = gmtr%(bi%, 0)
        tc% = gmtc%(bi%, 0)
        makemove(fr%, fc%, tr%, tc%, 0)
        movenum% = movenum% + 1
        print "Computer moves: " + str$(fr%) + "," + str$(fc%) + " " + str$(tr%) + "," + str$(tc%)
    else
        gameover% = 1
        if incheck%(side%) = 1 then
            print "Checkmate! You win!"
        else
            print "Stalemate! Draw."
        endif
    endif
endproc

rem =====================================================================
rem  Human move input and validation
rem =====================================================================

procedure getmove
    local mv, sp, cm, from, dest, fr, fc, tr, tc, valid, i, n, mfr, mfc, mtr, mtc, ok
    valid% = 0
    while valid% = 0
        print
        input "Your move (row,col row,col): ", mv$
        ok% = 1
        rem parse the move string
        sp% = findch%(mv$, " ")
        if sp% = 0 then
            print "Invalid format. Use: row,col row,col (e.g., 7,5 5,5)"
            ok% = 0
        endif
        if ok% = 1 then
            from$ = left$(mv$, sp% - 1)
            dest$ = mid$(mv$, sp% + 1, len(mv$) - sp%)
            cm% = findch%(from$, ",")
            if cm% = 0 then
                print "Invalid format. Use: row,col row,col"
                ok% = 0
            endif
        endif
        if ok% = 1 then
            fr% = val(left$(from$, cm% - 1))
            fc% = val(mid$(from$, cm% + 1, len(from$) - cm%))
            cm% = findch%(dest$, ",")
            if cm% = 0 then
                print "Invalid format. Use: row,col row,col"
                ok% = 0
            endif
        endif
        if ok% = 1 then
            tr% = val(left$(dest$, cm% - 1))
            tc% = val(mid$(dest$, cm% + 1, len(dest$) - cm%))
        endif
        if ok% = 1 then
            if fr% < 1 or fr% > 8 or fc% < 1 or fc% > 8 or tr% < 1 or tr% > 8 or tc% < 1 or tc% > 8 then
                print "Position out of bounds (1-8)."
                ok% = 0
            endif
        endif
        if ok% = 1 then
            if board%(fr%, fc%) = 0 then
                print "No piece at that position."
                ok% = 0
            endif
        endif
        if ok% = 1 then
            if sgn(board%(fr%, fc%)) <> human% then
                print "That is not your piece."
                ok% = 0
            endif
        endif
        if ok% <> 1 then goto gmvaldone
            rem check if move is in the legal move list
            genmoves(side%, 0)
            n% = nm%(0)
            valid% = 0
            for i% = 1 to n%
                mfr% = gmfr%(i%, 0)
                mfc% = gmfc%(i%, 0)
                mtr% = gmtr%(i%, 0)
                mtc% = gmtc%(i%, 0)
                if mfr% = fr% and mfc% = fc% and mtr% = tr% and mtc% = tc% then
                    if islegal%(fr%, fc%, tr%, tc%, side%, 0) = 1 then
                        valid% = 1
                    endif
                endif
            next i%
            if valid% = 0 then
                print "Illegal move."
            endif
        gmvaldone:
    wend
    rem make the move
    makemove(fr%, fc%, tr%, tc%, 0)
    movenum% = movenum% + 1
endproc

rem =====================================================================
rem  Check for game end (no legal moves for side s%)
rem =====================================================================

function checkend%(s%)
    local i, n, haslegal, res
    genmoves(s%, 0)
    n% = nm%(0)
    haslegal% = 0
    i% = 1
    while i% <= n% and haslegal% = 0
        if islegal%(gmfr%(i%, 0), gmfc%(i%, 0), gmtr%(i%, 0), gmtc%(i%, 0), s%, 0) = 1 then
            haslegal% = 1
        endif
        i% = i% + 1
    wend
    if haslegal% = 1 then
        res% = 0
    else
        if incheck%(s%) = 1 then res% = 1 else res% = 2
    endif
endfunc res%

rem =====================================================================
rem  Main game
rem =====================================================================

cls
print "=============================================="
print "         CHESS - IP Basic Edition"
print "=============================================="
print
print "Board coordinates: 1,1 (top-left) to 8,8 (bottom-right)"
print "Enter moves as: row,col row,col"
print "Example: 7,5 5,5 (move pawn from row 7 col 5 to row 5 col 5)"
print
print "Computer uses alpha-beta search at depth " + str$(maxdepth%)
print

input "Play as White or Black? (W/B): ", ch$
ch$ = ucase$(ch$)
if ch$ = "B" then
    human% = -1
    flip% = 1
    print "You play Black. Computer (White) moves first."
else
    human% = 1
    flip% = 0
    print "You play White. You move first."
endif
print
input "Press Enter to start...", ch$

randomize
initboard

rem main game loop
mainloop:
    endmsg$ = ""
    drawboard
    print
    rem check for game end
    ge% = checkend%(side%)
    if ge% = 1 then if side% = human% then endmsg$ = "Checkmate! Computer wins!"
    if ge% = 1 then if side% <> human% then endmsg$ = "Checkmate! You win!"
    if ge% = 2 then endmsg$ = "Stalemate! The game is a draw."
    if ge% <> 0 then goto gameend
    rem announce check
    if incheck%(side%) = 1 then print "Check!"
    rem whose turn
    if side% <> human% then goto compturn
    getmove
    goto turndone
    compturn:
    compmove
    if gameover% = 1 then goto gameend
    input "Press Enter to continue...", ch$
    turndone:
    rem switch sides
    side% = -side%
goto mainloop

gameend:
print
if endmsg$ <> "" then print endmsg$
print "Game over."
input "Play again? (Y/N): ", ch$
if ucase$(ch$) <> "Y" then goto quitgame
initboard
goto mainloop
quitgame:
print rst$;
print show$;
end
