rem =====================================================================
rem  Backgammon for IP Basic
rem  Human vs Computer with ANSI color board display
rem  Computer uses positional evaluation with exhaustive move search
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

rem board colors
brd$ = csi$ + "48;2;139;90;43m"
dk$ = csi$ + "48;2;0;100;0m"
lt$ = csi$ + "48;2;180;0;0m"
bar$ = csi$ + "48;2;80;50;20m"
offc$ = csi$ + "48;2;60;40;15m"

rem checker colors
wfg$ = csi$ + "38;2;255;255;240m" + bold$
bfg$ = csi$ + "38;2;30;30;30m" + bold$

rem =====================================================================
rem  Board state
rem =====================================================================
dim bd%(24)
dim sbd%(24)
wbar% = 0
bbar% = 0
woff% = 0
boff% = 0
side% = 1
human% = 1
gameover% = 0
d1% = 0
d2% = 0
dim du%(4)
ndice% = 0
dim c1f%(30), c1t%(30)
dim s2bd%(24)

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
    local i
    for i% = 1 to 24
        bd%(i%) = 0
    next i%
    rem white checkers (positive)
    bd%(24) = 2
    bd%(13) = 5
    bd%(8) = 3
    bd%(6) = 5
    rem black checkers (negative)
    bd%(1) = -2
    bd%(12) = -5
    bd%(17) = -3
    bd%(19) = -5
    wbar% = 0: bbar% = 0
    woff% = 0: boff% = 0
    side% = 1
    gameover% = 0
endproc

rem =====================================================================
rem  Board display
rem  Layout: points 13-18 | BAR | 19-24 on top
rem          points 12-7  | BAR | 6-1   on bottom
rem  Each point is 4 chars wide, show up to 6 checkers stacking inward
rem =====================================================================

procedure drawboard
    local r, c, pt, cnt, ac, ch, bg, fg, i
    print cls$;
    print hide$;
    rem top point numbers
    print brd$;
    print " ";
    for c% = 13 to 18
        if c% < 10 then print "  " + str$(c%) + " "; else print " " + str$(c%) + " ";
    next c%
    print " BAR";
    for c% = 19 to 24
        if c% < 10 then print "  " + str$(c%) + " "; else print " " + str$(c%) + " ";
    next c%
    print " OFF";
    print rst$
    rem top border
    print brd$ + " ";
    for c% = 1 to 6
        if c% mod 2 = 1 then print dk$; else print lt$;
        print "----";
    next c%
    print bar$ + "----";
    for c% = 1 to 6
        if c% mod 2 = 1 then print lt$; else print dk$;
        print "----";
    next c%
    print offc$ + "----";
    print rst$
    rem top half rows (6 rows, checkers on points 13-24 stack downward)
    for r% = 1 to 6
        print brd$ + " ";
        rem left side: points 13-18
        for c% = 0 to 5
            pt% = 13 + c%
            cnt% = bd%(pt%)
            ac% = abs(cnt%)
            if c% mod 2 = 0 then print dk$; else print lt$;
            if r% <= ac% then
                if cnt% > 0 then print wfg$; else print bfg$;
                if r% = 6 and ac% > 6 then
                    if cnt% > 0 then print " W" + str$(ac%); else print " B" + str$(ac%);
                else
                    if cnt% > 0 then print " OO"; else print " XX";
                endif
                print " ";
            else
                print "    ";
            endif
        next c%
        rem bar
        print bar$;
        if r% = 2 then
            print bfg$ + " ";
            if bbar% > 0 then print "X" + str$(bbar%); else print "  ";
            print " ";
        else
            if r% = 5 then
                print wfg$ + " ";
                if wbar% > 0 then print "O" + str$(wbar%); else print "  ";
                print " ";
            else
                print "    ";
            endif
        endif
        rem right side: points 19-24
        for c% = 0 to 5
            pt% = 19 + c%
            cnt% = bd%(pt%)
            ac% = abs(cnt%)
            if c% mod 2 = 0 then print lt$; else print dk$;
            if r% <= ac% then
                if cnt% > 0 then print wfg$; else print bfg$;
                if r% = 6 and ac% > 6 then
                    if cnt% > 0 then print " W" + str$(ac%); else print " B" + str$(ac%);
                else
                    if cnt% > 0 then print " OO"; else print " XX";
                endif
                print " ";
            else
                print "    ";
            endif
        next c%
        rem off area
        print offc$;
        if r% = 2 then
            print bfg$ + " ";
            if boff% > 0 then print "X" + str$(boff%); else print "  ";
            print " ";
        else
            if r% = 5 then
                print wfg$ + " ";
                if woff% > 0 then print "O" + str$(woff%); else print "  ";
                print " ";
            else
                print "    ";
            endif
        endif
        print rst$
    next r%
    rem middle separator
    print brd$ + " ";
    for c% = 1 to 6
        if c% mod 2 = 1 then print dk$; else print lt$;
        print "====";
    next c%
    print bar$ + "====";
    for c% = 1 to 6
        if c% mod 2 = 1 then print lt$; else print dk$;
        print "====";
    next c%
    print offc$ + "====";
    print rst$
    rem bottom half rows (6 rows, checkers on points 12-1 stack upward)
    for r% = 6 to 1 step -1
        print brd$ + " ";
        rem left side: points 12-7
        for c% = 0 to 5
            pt% = 12 - c%
            cnt% = bd%(pt%)
            ac% = abs(cnt%)
            if c% mod 2 = 0 then print dk$; else print lt$;
            if r% <= ac% then
                if cnt% > 0 then print wfg$; else print bfg$;
                if r% = 6 and ac% > 6 then
                    if cnt% > 0 then print " W" + str$(ac%); else print " B" + str$(ac%);
                else
                    if cnt% > 0 then print " OO"; else print " XX";
                endif
                print " ";
            else
                print "    ";
            endif
        next c%
        rem bar
        print bar$;
        if r% = 5 then
            print bfg$ + " ";
            if bbar% > 0 then print "X" + str$(bbar%); else print "  ";
            print " ";
        else
            if r% = 2 then
                print wfg$ + " ";
                if wbar% > 0 then print "O" + str$(wbar%); else print "  ";
                print " ";
            else
                print "    ";
            endif
        endif
        rem right side: points 6-1
        for c% = 0 to 5
            pt% = 6 - c%
            cnt% = bd%(pt%)
            ac% = abs(cnt%)
            if c% mod 2 = 0 then print lt$; else print dk$;
            if r% <= ac% then
                if cnt% > 0 then print wfg$; else print bfg$;
                if r% = 6 and ac% > 6 then
                    if cnt% > 0 then print " W" + str$(ac%); else print " B" + str$(ac%);
                else
                    if cnt% > 0 then print " OO"; else print " XX";
                endif
                print " ";
            else
                print "    ";
            endif
        next c%
        rem off area
        print offc$;
        if r% = 5 then
            print bfg$ + " ";
            if boff% > 0 then print "X" + str$(boff%); else print "  ";
            print " ";
        else
            if r% = 2 then
                print wfg$ + " ";
                if woff% > 0 then print "O" + str$(woff%); else print "  ";
                print " ";
            else
                print "    ";
            endif
        endif
        print rst$
    next r%
    rem bottom border
    print brd$ + " ";
    for c% = 1 to 6
        if c% mod 2 = 1 then print dk$; else print lt$;
        print "----";
    next c%
    print bar$ + "----";
    for c% = 1 to 6
        if c% mod 2 = 1 then print lt$; else print dk$;
        print "----";
    next c%
    print offc$ + "----";
    print rst$
    rem bottom point numbers
    print brd$;
    print " ";
    for c% = 12 to 7 step -1
        if c% < 10 then print "  " + str$(c%) + " "; else print " " + str$(c%) + " ";
    next c%
    print " BAR";
    for c% = 6 to 1 step -1
        if c% < 10 then print "  " + str$(c%) + " "; else print " " + str$(c%) + " ";
    next c%
    print " OFF";
    print rst$
    print show$;
endproc

rem =====================================================================
rem  Dice rolling
rem =====================================================================

procedure rolldice
    d1% = int(rnd(1) * 6) + 1
    d2% = int(rnd(1) * 6) + 1
    if d1% = d2% then ndice% = 4 else ndice% = 2
    du%(1) = d1%
    du%(2) = d2%
    if ndice% = 4 then du%(3) = d1%: du%(4) = d1%
endproc

rem =====================================================================
rem  Check if all checkers are in home board (for bearing off)
rem =====================================================================

function allhome%(s%)
    local i, cnt, ok
    ok% = 1
    if s% = 1 and wbar% > 0 then ok% = 0: goto ahdone
    if s% = -1 and bbar% > 0 then ok% = 0: goto ahdone
    if s% <> 1 then goto ahblk
    rem white home = points 1-6
    for i% = 7 to 24
        if bd%(i%) > 0 then ok% = 0
    next i%
    goto ahdone
    ahblk:
    rem black home = points 19-24
    for i% = 1 to 18
        if bd%(i%) < 0 then ok% = 0
    next i%
    ahdone:
endfunc ok%

rem =====================================================================
rem  Check if a move is legal
rem  fp% = from point (0 = bar), die% = die value, s% = side
rem  Returns destination point (1-24) or 25 (bear off) or 0 (illegal)
rem =====================================================================

function chkmov%(fp%, die%, s%)
    local dp, ok, highest, i, ac, ah
    ok% = 0
    rem compute destination
    if s% = 1 then dp% = fp% - die% else dp% = fp% + die%
    rem entering from bar
    if fp% = 0 and s% = 1 then dp% = 25 - die%
    if fp% = 0 and s% = -1 then dp% = die%
    rem bearing off white
    if s% <> 1 or dp% >= 1 then goto cmblk
    ah% = allhome%(1)
    if ah% = 0 then dp% = 0: goto cmdone
    if dp% = 0 then dp% = 25: goto cmcheck
    goto cmboff
    cmboff:
    rem dp < 0 means die is larger than point number
    highest% = 0
    for i% = 6 to 1 step -1
        if bd%(i%) > 0 and highest% = 0 then highest% = i%
    next i%
    if fp% = highest% then dp% = 25 else dp% = 0
    goto cmdone
    cmblk:
    rem bearing off black
    if s% <> -1 or dp% <= 24 then goto cmcheck
    ah% = allhome%(-1)
    if ah% = 0 then dp% = 0: goto cmdone
    if dp% = 25 then goto cmcheck
    goto cmbboff
    cmbboff:
    highest% = 0
    for i% = 19 to 24
        if bd%(i%) < 0 and highest% = 0 then highest% = i%
    next i%
    if fp% = highest% then dp% = 25 else dp% = 0
    goto cmdone
    cmcheck:
    rem check destination is on board
    if dp% < 1 or dp% > 24 then goto cmchk2
    ac% = bd%(dp%)
    if ac% = 0 then ok% = 1
    if s% = 1 and ac% > 0 then ok% = 1
    if s% = -1 and ac% < 0 then ok% = 1
    if s% = 1 and ac% = -1 then ok% = 1
    if s% = -1 and ac% = 1 then ok% = 1
    if ok% = 0 then dp% = 0
    goto cmdone
    cmchk2:
    if dp% <> 25 then dp% = 0
    cmdone:
endfunc dp%

rem =====================================================================
rem  Check if any legal move exists for side s% with die value dv%
rem =====================================================================

function hasmove%(s%, dv%)
    local i, res, dp
    res% = 0
    rem must enter from bar first
    if s% = 1 and wbar% > 0 then dp% = chkmov%(0, dv%, s%) else dp% = 0
    if dp% > 0 then res% = 1
    if s% = 1 and wbar% > 0 then goto hmdone
    if s% = -1 and bbar% > 0 then dp% = chkmov%(0, dv%, s%) else dp% = 0
    if dp% > 0 then res% = 1
    if s% = -1 and bbar% > 0 then goto hmdone
    rem check all points
    for i% = 1 to 24
        dp% = 0
        if s% = 1 and bd%(i%) > 0 then dp% = chkmov%(i%, dv%, s%)
        if s% = -1 and bd%(i%) < 0 then dp% = chkmov%(i%, dv%, s%)
        if dp% > 0 then res% = 1
    next i%
    hmdone:
endfunc res%

rem =====================================================================
rem  Execute a move: from point fp% to destination dp%, side s%
rem  dp% = 25 means bear off
rem =====================================================================

procedure domove(fp%, dp%, s%)
    rem remove checker from source
    if fp% = 0 and s% = 1 then wbar% = wbar% - 1
    if fp% = 0 and s% = -1 then bbar% = bbar% - 1
    if fp% <> 0 and s% = 1 then bd%(fp%) = bd%(fp%) - 1
    if fp% <> 0 and s% = -1 then bd%(fp%) = bd%(fp%) + 1
    rem place checker at destination
    if dp% = 25 and s% = 1 then woff% = woff% + 1
    if dp% = 25 and s% = -1 then boff% = boff% + 1
    if dp% = 25 then goto dmdone
    rem check for hit (blot)
    if s% = 1 and bd%(dp%) = -1 then bd%(dp%) = 0: bbar% = bbar% + 1
    if s% = -1 and bd%(dp%) = 1 then bd%(dp%) = 0: wbar% = wbar% + 1
    if s% = 1 then bd%(dp%) = bd%(dp%) + 1
    if s% = -1 then bd%(dp%) = bd%(dp%) - 1
    dmdone:
endproc

rem =====================================================================
rem  Save and restore board state (for AI search)
rem =====================================================================

procedure saveboard
    local i
    for i% = 1 to 24
        sbd%(i%) = bd%(i%)
    next i%
    swb% = wbar%: sbb% = bbar%
    swo% = woff%: sbo% = boff%
endproc

procedure restboard
    local i
    for i% = 1 to 24
        bd%(i%) = sbd%(i%)
    next i%
    wbar% = swb%: bbar% = sbb%
    woff% = swo%: boff% = sbo%
endproc

rem =====================================================================
rem  Evaluation function (from white's perspective)
rem  Higher = better for white
rem =====================================================================

function evaluate%
    local i, sc, cnt, ac
    sc% = 0
    rem borne off bonus (big)
    sc% = sc% + woff% * 200 - boff% * 200
    rem bar penalty (big)
    sc% = sc% - wbar% * 150 + bbar% * 150
    for i% = 1 to 24
        cnt% = bd%(i%)
        ac% = abs(cnt%)
        if cnt% <= 0 then goto evblk
        rem white checker - pip count penalty
        sc% = sc% - i% * 3
        rem blot penalty
        if cnt% = 1 and i% >= 19 then sc% = sc% - 40
        if cnt% = 1 and i% < 19 then sc% = sc% - 20
        rem point made bonus
        if cnt% >= 2 then sc% = sc% + 10
        if cnt% >= 2 and i% <= 6 then sc% = sc% + 15
        rem prime bonus
        if cnt% >= 2 and i% > 1 and bd%(i% - 1) >= 2 then sc% = sc% + 8
        goto evnxt
        evblk:
        if cnt% >= 0 then goto evnxt
        rem black checker - pip count
        sc% = sc% + (25 - i%) * 3
        rem blot penalty for black
        if ac% = 1 and i% <= 6 then sc% = sc% + 40
        if ac% = 1 and i% > 6 then sc% = sc% + 20
        rem point made bonus for black
        if ac% >= 2 then sc% = sc% - 10
        if ac% >= 2 and i% >= 19 then sc% = sc% - 15
        rem prime bonus for black
        if ac% >= 2 and i% < 24 and bd%(i% + 1) <= -2 then sc% = sc% - 8
        evnxt:
    next i%
endfunc sc%

rem =====================================================================
rem  Find all legal moves for side s% with die value dv%
rem  Store in mf%/mt% arrays, return count in nm%
rem =====================================================================
dim mf%(30), mt%(30)

procedure findmoves(s%, dv%)
    local i, dp
    nm% = 0
    rem must enter from bar first
    if s% = 1 and wbar% > 0 then dp% = chkmov%(0, dv%, s%) else dp% = 0
    if s% = 1 and wbar% > 0 and dp% > 0 then nm% = nm% + 1: mf%(nm%) = 0: mt%(nm%) = dp%
    if s% = 1 and wbar% > 0 then goto fmdone
    if s% = -1 and bbar% > 0 then dp% = chkmov%(0, dv%, s%) else dp% = 0
    if s% = -1 and bbar% > 0 and dp% > 0 then nm% = nm% + 1: mf%(nm%) = 0: mt%(nm%) = dp%
    if s% = -1 and bbar% > 0 then goto fmdone
    rem check all points
    for i% = 1 to 24
        dp% = 0
        if s% = 1 and bd%(i%) > 0 then dp% = chkmov%(i%, dv%, s%)
        if s% = -1 and bd%(i%) < 0 then dp% = chkmov%(i%, dv%, s%)
        if dp% > 0 then nm% = nm% + 1: mf%(nm%) = i%: mt%(nm%) = dp%
    next i%
    fmdone:
endproc

rem =====================================================================
rem  Second-level save/restore for nested AI search
rem =====================================================================

procedure savebrd2
    local i
    for i% = 1 to 24
        s2bd%(i%) = bd%(i%)
    next i%
    s2wb% = wbar%: s2bb% = bbar%
    s2wo% = woff%: s2bo% = boff%
endproc

procedure rstbrd2
    local i
    for i% = 1 to 24
        bd%(i%) = s2bd%(i%)
    next i%
    wbar% = s2wb%: bbar% = s2bb%
    woff% = s2wo%: boff% = s2bo%
endproc

rem =====================================================================
rem  Doubles greedy: pick best move, execute, repeat 4 times
rem =====================================================================

procedure dblgrdy
    local i, j, bsc, bmf, bmt, sc, nm2
    for i% = 1 to 4
        findmoves(side%, d1%)
        nm2% = nm%
        if nm2% = 0 then print "  (no move)": goto dgdone
        bsc% = -99999
        bmf% = -1: bmt% = -1
        for j% = 1 to nm2%
            savebrd2
            domove(mf%(j%), mt%(j%), side%)
            sc% = evaluate%
            if side% < 0 then sc% = -sc%
            rstbrd2
            if sc% <= bsc% then goto dgnxt
            bsc% = sc%
            bmf% = mf%(j%): bmt% = mt%(j%)
            dgnxt:
        next j%
        if bmf% < 0 then print "  (no move)": goto dgdone
        domove(bmf%, bmt%, side%)
        if bmt% = 25 then print "  " + str$(bmf%) + " off"
        if bmt% <> 25 then print "  " + str$(bmf%) + " -> " + str$(bmt%)
    next i%
    dgdone:
endproc

rem =====================================================================
rem  Computer move
rem =====================================================================

procedure compmove
    local i, j, n1, n2, sc, best, bf1, bt1, bf2, bt2
    print "Computer is thinking..."
    rolldice
    print "Dice: " + str$(d1%) + " " + str$(d2%)
    best% = -99999
    bf1% = -1: bt1% = -1: bf2% = -1: bt2% = -1
    saveboard
    if ndice% > 2 then goto cpdbl
    rem --- non-doubles: try d1 first, then d2 ---
    findmoves(side%, d1%)
    n1% = nm%
    if n1% > 0 then goto cpd1ok
    rem d1 has no moves, try d2 only
    findmoves(side%, d2%)
    n1% = nm%
    for i% = 1 to n1%
        savebrd2
        domove(mf%(i%), mt%(i%), side%)
        sc% = evaluate%
        if side% < 0 then sc% = -sc%
        rstbrd2
        if sc% <= best% then goto cpd2nx2
        best% = sc%
        bf1% = mf%(i%): bt1% = mt%(i%)
        cpd2nx2:
    next i%
    goto cpd2ord
    cpd1ok:
    rem store first-move list
    for i% = 1 to n1%
        c1f%(i%) = mf%(i%): c1t%(i%) = mt%(i%)
    next i%
    for i% = 1 to n1%
        savebrd2
        domove(c1f%(i%), c1t%(i%), side%)
        findmoves(side%, d2%)
        n2% = nm%
        if n2% > 0 then goto cpd1d2
        rem only first die used
        sc% = evaluate%
        if side% < 0 then sc% = -sc%
        if sc% <= best% then goto cpd1s1
        best% = sc%
        bf1% = c1f%(i%): bt1% = c1t%(i%)
        bf2% = -1: bt2% = -1
        cpd1s1:
        rstbrd2
        goto cpd1nxt
        cpd1d2:
        for j% = 1 to n2%
            domove(mf%(j%), mt%(j%), side%)
            sc% = evaluate%
            if side% < 0 then sc% = -sc%
            if sc% <= best% then goto cpd1s2
            best% = sc%
            bf1% = c1f%(i%): bt1% = c1t%(i%)
            bf2% = mf%(j%): bt2% = mt%(j%)
            cpd1s2:
            rstbrd2
            domove(c1f%(i%), c1t%(i%), side%)
        next j%
        rstbrd2
        cpd1nxt:
    next i%
    cpd2ord:
    rem --- try d2 first, then d1 (if dice differ) ---
    if d1% = d2% then goto cpexec
    restboard
    findmoves(side%, d2%)
    n1% = nm%
    if n1% = 0 then goto cpexec
    for i% = 1 to n1%
        c1f%(i%) = mf%(i%): c1t%(i%) = mt%(i%)
    next i%
    for i% = 1 to n1%
        savebrd2
        domove(c1f%(i%), c1t%(i%), side%)
        findmoves(side%, d1%)
        n2% = nm%
        if n2% > 0 then goto cpd2d1
        sc% = evaluate%
        if side% < 0 then sc% = -sc%
        if sc% <= best% then goto cpd2s1
        best% = sc%
        bf1% = c1f%(i%): bt1% = c1t%(i%)
        bf2% = -1: bt2% = -1
        cpd2s1:
        rstbrd2
        goto cpd2nxt
        cpd2d1:
        for j% = 1 to n2%
            domove(mf%(j%), mt%(j%), side%)
            sc% = evaluate%
            if side% < 0 then sc% = -sc%
            if sc% <= best% then goto cpd2s2
            best% = sc%
            bf1% = c1f%(i%): bt1% = c1t%(i%)
            bf2% = mf%(j%): bt2% = mt%(j%)
            cpd2s2:
            rstbrd2
            domove(c1f%(i%), c1t%(i%), side%)
        next j%
        rstbrd2
        cpd2nxt:
    next i%
    cpexec:
    rem --- execute best non-doubles moves ---
    restboard
    if bf1% < 0 then goto cpnone
    domove(bf1%, bt1%, side%)
    if bt1% = 25 then print "  " + str$(bf1%) + " off"
    if bt1% <> 25 then print "  " + str$(bf1%) + " -> " + str$(bt1%)
    if bf2% < 0 then goto cpdone
    domove(bf2%, bt2%, side%)
    if bt2% = 25 then print "  " + str$(bf2%) + " off"
    if bt2% <> 25 then print "  " + str$(bf2%) + " -> " + str$(bt2%)
    goto cpdone
    cpnone:
    print "  No legal moves."
    goto cpdone
    cpdbl:
    rem --- doubles: greedy approach, pick best move 4 times ---
    dblgrdy
    cpdone:
endproc

rem =====================================================================
rem  Human move input
rem =====================================================================

procedure getmove
    local mv, fp, dp, dv, ok, cm, di, hm, remain
    rolldice
    drawboard
    print
    print "Dice: " + str$(d1%) + " " + str$(d2%)
    if ndice% = 4 then print " (doubles!)"
    hm% = 0
    remain% = ndice%
    for di% = 1 to ndice%
        du%(di%) = 0
    next di%
    gmtop:
    if remain% = 0 then goto gmhdone
    rem check if any move is possible with remaining dice
    ok% = 0
    for di% = 1 to ndice%
        if du%(di%) <> 0 then goto gmchknx
        dv% = d1%
        if di% = 2 then dv% = d2%
        if di% > 2 then dv% = d1%
        if hasmove%(side%, dv%) = 1 then ok% = 1
        gmchknx:
    next di%
    if ok% = 0 then print "No legal moves with remaining dice.": goto gmhdone
    rem show remaining dice
    print "Remaining dice:";
    for di% = 1 to ndice%
        if du%(di%) <> 0 then goto gmshwnx
        dv% = d1%
        if di% = 2 then dv% = d2%
        if di% > 2 then dv% = d1%
        print " " + str$(dv%);
        gmshwnx:
    next di%
    print
    gmretry:
    rem get move from player
    rem check if entering from bar
    fp% = -1
    if side% = 1 and wbar% > 0 then fp% = 0
    if side% = -1 and bbar% > 0 then fp% = 0
    if fp% = 0 then goto gminbar
    rem normal move input
    input "Move (from dest, e.g. 24 22 or 6 off): ", mv$
    cm% = findch%(mv$, " ")
    if cm% = 0 then print "Invalid format. Use: from dest": goto gmretry
    fp% = val(left$(mv$, cm% - 1))
    dest$ = mid$(mv$, cm% + 1, len(mv$) - cm%)
    if dest$ = "off" or dest$ = "OFF" then dp% = 25 else dp% = val(dest$)
    goto gmparse
    gminbar:
    print "You have checkers on the bar. Enter destination point."
    input "Dest point: ", mv$
    fp% = 0
    dp% = val(mv$)
    gmparse:
    rem figure out which die is needed
    if fp% = 0 and side% = 1 then dv% = 25 - dp%
    if fp% = 0 and side% = -1 then dv% = dp%
    if fp% > 0 and side% = 1 then dv% = fp% - dp%
    if fp% > 0 and side% = -1 then dv% = dp% - fp%
    if fp% > 0 and dp% = 25 and side% = 1 then dv% = fp%
    if fp% > 0 and dp% = 25 and side% = -1 then dv% = 25 - fp%
    rem check if this die value is available
    ok% = 0
    for di% = 1 to ndice%
        if du%(di%) <> 0 or ok% <> 0 then goto gmdienx
        if di% = 1 and d1% = dv% then ok% = 1: du%(di%) = 1
        if di% = 2 and d2% = dv% then ok% = 1: du%(di%) = 1
        if di% > 2 and d1% = dv% then ok% = 1: du%(di%) = 1
        gmdienx:
    next di%
    rem for bearing off, die can be larger than needed
    if ok% <> 0 or dp% <> 25 then goto gmchkok
    for di% = 1 to ndice%
        if du%(di%) <> 0 or ok% <> 0 then goto gmbonx
        if di% = 1 then dv% = d1%
        if di% = 2 then dv% = d2%
        if di% > 2 then dv% = d1%
        if chkmov%(fp%, dv%, side%) = 25 then ok% = 1: du%(di%) = 1
        gmbonx:
    next di%
    gmchkok:
    if ok% = 0 then print "No matching die available for that move.": goto gmretry
    rem validate the move
    if chkmov%(fp%, dv%, side%) = dp% then goto gmvalid
    print "Illegal move."
    rem undo die marking we just set
    for di% = ndice% to 1 step -1
        if du%(di%) = 1 and ok% = 1 then du%(di%) = 0: ok% = 0
    next di%
    goto gmretry
    gmvalid:
    rem execute the move
    domove(fp%, dp%, side%)
    hm% = hm% + 1
    remain% = remain% - 1
    if remain% <= 0 then goto gmtop
    drawboard
    print
    print "Dice: " + str$(d1%) + " " + str$(d2%)
    goto gmtop
    gmhdone:
endproc

rem =====================================================================
rem  Check for game end
rem =====================================================================

function checkwin%
    local res
    res% = 0
    if woff% = 15 then res% = 1
    if boff% = 15 then res% = -1
endfunc res%

rem =====================================================================
rem  Main game
rem =====================================================================

cls
print "=============================================="
print "       BACKGAMMON - IP Basic Edition"
print "=============================================="
print
print "White moves from point 24 toward point 1"
print "Black moves from point 1 toward point 24"
print "Enter moves as: from dest (e.g. 24 22)"
print "Use 'off' to bear off (e.g. 3 off)"
print "From bar: just enter destination point"
print
input "Play as White or Black? (W/B): ", ch$
ch$ = ucase$(ch$)
human% = 1
if ch$ = "B" then human% = -1: print "You play Black. Computer (White) moves first."
if ch$ <> "B" then print "You play White. You move first."
print
input "Press Enter to start...", ch$

randomize
initboard

rem main game loop
mainloop:
    drawboard
    print
    rem check for win
    gw% = checkwin%
    if gw% = 1 and human% = 1 then print "You win!": goto gameend
    if gw% = 1 and human% <> 1 then print "Computer wins!": goto gameend
    if gw% = -1 and human% = -1 then print "You win!": goto gameend
    if gw% = -1 and human% <> -1 then print "Computer wins!": goto gameend
    rem whose turn
    if side% = human% then getmove: goto mldone
    compmove
    if gameover% = 1 then goto gameend
    input "Press Enter to continue...", ch$
    mldone:
    rem switch sides
    side% = -side%
goto mainloop

gameend:
print
print "Game over."
input "Play again? (Y/N): ", ch$
if ucase$(ch$) <> "Y" then goto quitgame
initboard
goto mainloop
quitgame:
print rst$;
print show$;
end
