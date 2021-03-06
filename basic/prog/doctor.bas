100 REM             DOCTOR,  Version PC2.1
120 REM Written by Joseph Weizenbaum - as described in the book "EXPERIMENTS
130 REM   IN ARTIFICAL INTELLIGENCE FOR SMALL COMPUTERS" by John Krutch
140 REM   Library of Congress catalog card number: 80-53270
150 REM Program has been modified by Jeff Sussman in the following ways:
160 REM       1) To work using IBM PERSONAL COMPUTER BASIC
170 REM       2) To use INSTR function to greatly decrease execution time
180 REM       3) To convert all input to uppercase
190 REM       4) To allow input of non-punctuated or punctuated sentences
200 DEF FNUP$(A$)=CHR$(ASC(A$+" ")+ABS(A$>="a" AND A$<="z")*(ASC("A")-ASC("a")))
210 REM             Supervisor
220 GOSUB 290       REM  Initialization
230 GOSUB 360       REM  Input (and associated functions)
240 GOSUB 480       REM  Swap word for opposite form
250 GOSUB 640       REM  Keyword processing
260 GOSUB 1140      REM  Marker remover
270 GOSUB 1200      REM  Doctor's reply
280 GOTO 230
290 REM 		Initialization
300 NM = 28
310 NU = 82
320 RANDOMIZE 100*VAL(MID$(TIME$,1,2))+VAL(MID$(TIME$,4,2))
330 CLS: PRINT: PRINT
340 PRINT "PLEASE STATE YOUR PROBLEM."
350 RETURN
360 REM 		Input (and associated functions)
370 RESTORE
380 X = 0 : PA$= ""
390 CSRSAVE=CSRLIN:LOCATE 25,1: PRINT "         ";: LOCATE CSRSAVE,1
400 LINE INPUT ":";PA$: IF INSTR(".?!",RIGHT$(PA$,1)) = 0 THEN PA$ = PA$ + "."
410 CSRSAVE=CSRLIN:LOCATE 25,1:COLOR 31: PRINT "ANALYZING";: LOCATE CSRSAVE,1:      COLOR 7
420 FOR Z=1 TO LEN(PA$):MID$(PA$,Z,1)=FNUP$(MID$(PA$,Z,1)):NEXT Z:
430 IF PA$ = "" THEN PRINT "DO YOU HAVE A PROBLEM?": X = 1: GOTO 470
440 IF PA$ = PR$ THEN PRINT "PLEASE DON'T REPEAT YOURSELF!": X = 1: GOTO 470
450 PR$ = PA$
460 PA$ = " " + LEFT$(PA$, LEN(PA$) - 1) + " "
470 RETURN
480 REM 		Swap word for opposite form
490 IF X = 1 THEN 630
500 FOR I = 1 TO NM/2
510 READ TE$, TN$
520 PS=INSTR(PA$, TE$)
530 IF PS <> 0 THEN PA$ = LEFT$(PA$, PS -1) + TN$ + MID$(PA$, PS + LEN(TE$))
540 DATA " MOM "," MOTHER "," DAD "," FATHER "
550 DATA " DREAMS "," DREAM "
560 DATA " I "," YOU@ "," YOU "," I "," ME "," YOU "
570 DATA " MY "," YOUR* "
580 DATA " YOUR "," MY "," MYSELF "," YOURSELF* "
590 DATA " YOURSELF "," MYSELF "
600 DATA " I'M "," YOU'RE* "," YOU'RE "," I'M "," AM "," ARE@ "
610 DATA " WERE "," WAS "
620 NEXT I
630 RETURN
640 REM 		Keyword processing
650 REM 		A. Checking for keywords
660 IF X = 1 THEN 1130
670 FOR I = 1 TO NU
680 READ KE$, JU
690 PS=INSTR(PA$, KE$)
700 IF PS <> 0 THEN DR$ = MID$(PA$, PS + LEN(KE$)):                                    IF DR$ <> ""  THEN 730 ELSE 1130
710 NEXT I
720 GOTO 1020
730 DR$ = LEFT$(DR$, LEN(DR$) - 1): GOTO 1130
740 DATA "COMPUTER",1,"MACHINE",1
750 DATA " NAME ",2,"ALIKE",3," LIKE ",3," SAME ",3
760 DATA "YOU@ REMEMBER",4,"DO I REMEMBER",5,"YOU@ DREAMED",6
770 DATA " DREAM ",7," IF ",8,"EVERYBODY",9,"EVERYONE",9
780 DATA "NOBODY",9,"NO ONE",9,"WAS YOU@",10,"YOU@ WAS",11
790 DATA "WAS I",12,"YOUR* MOTHER",13,"YOUR* FATHER",13
800 DATA "YOUR* SISTER",13,"YOU* BROTHER",13,"YOUR* WIFE",13
810 DATA "YOUR* HUSBAND",13,"YOUR* CHILDREN",13,"YOUR*",14
820 DATA "ALWAYS",15,"ARE I",16,"ARE@ YOU",18," HOW ",25
830 DATA "BECAUSE",19,"CAN I",20,"CAN YOU@",21,"CERTAINLY",22
840 DATA "DEUTSCH",23,"ESPANOL",23,"FRANCAIS",23,"HELLO",24
850 DATA "I REMIND YOU OF",3,"I ARE",26,"I'M",26
860 DATA "ITALIANO",23,"MAYBE",28," MY ",29," NO ",30
870 DATA "PERHAPS",28,"SORRY",31,"WHAT ",25,"WHEN ",25
880 DATA "WHY DON'T I",32,"WHY CAN'T YOU@",33,"YES",22
890 DATA "YOU@ WANT",34,"YOU@ NEED",34," ARE ",17," I ",27
900 DATA "YOU@ ARE@ SAD",35,"YOU'RE* SAD",35
910 DATA "YOU@ ARE@ UNHAPPY",35,"YOU'RE* UNHAPPY",35
920 DATA "YOU@ ARE@ DEPRESSED",35,"YOU'RE* DEPRESSED",35
930 DATA "YOU@ ARE@ SICK",35,"YOU'RE* SICK",35
940 DATA "YOU@ ARE@ HAPPY",36,"YOU'RE* HAPPY",36
950 DATA "YOU@ ARE@ ELATED",36,"YOU'RE* ELATED",36
960 DATA "YOU@ ARE@ GLAD",36,"YOU'RE* GLAD",36
970 DATA "YOU@ ARE@ BETTER",36,"YOU'RE* BETTER",36
980 DATA "YOU@ FEEL YOU@",37,"YOU@ THINK YOU@",37
990 DATA "YOU@ BELIEVE YOU@",37,"YOU@ WISH YOU@",37
1000 DATA " YOU@ ARE@",38,"YOU'RE*",38,"YOU@ CAN'T",39
1010 DATA "YOU@ CANNOT",39,"YOU@ DON'T",40,"YOU@ FEEL",41
1020 REM            B. No keywords found
1030 IF YO$ = "" THEN 1040 ELSE RAN = INT(RND*5+1):                                  ON RAN GOTO 1040, 1040, 1040, 1090, 1090
1040 RAN = INT(RND*4+1): ON RAN GOTO 1050, 1060, 1070, 1080
1050 PRINT "I AM NOT SURE I UNDERSTAND YOU FULLY.": X = 1: GOTO 1130
1060 PRINT "PLEASE GO ON.": X = 1: GOTO 1130
1070 PRINT "WHAT DOES THAT SUGGEST TO YOU?": X = 1: GOTO 1130
1080 PRINT "DO YOU FEEL STRONGLY ABOUT DISCUSSING SUCH THINGS?":X = 1:GOTO 1130
1090 RAN = INT(RND*3+1): ON RAN GOTO 1100, 1110, 1120
1100 PRINT "LET'S DISCUSS FURTHER WHY YOUR" + YO$ + ".": X = 1: GOTO 1130
1110 PRINT "EARLIER YOU SAID YOUR" + YO$ + ".": X = 1: GOTO 1130
1120 PRINT "DOES THAT HAVE ANYTHING TO DO WITH THE FACT THAT YOUR" + YO$ + "?":      X = 1: GOTO 1130
1130 RETURN
1140 REM            Marker remover
1150 IF X = 1 THEN 1190
1160 FOR PS = 1 TO LEN(DR$)
1170 IF MID$(DR$, PS, 1) = "@" OR MID$(DR$, PS, 1) = "*"                             THEN DR$ = LEFT$(DR$, PS - 1) + MID$(DR$, PS + 1)
1180 NEXT PS
1190 RETURN
1200 REM            Doctor's reply
1210 REM            A. Line to jump to
1220 IF X = 1 THEN 1700
1230 ON JU GOTO 1250,1260,1270,1280,1290,1300,1310,1320,1330,1340,1350,1360,         1370,1380,1390,1400,1410,1420,1430,1440,1450,1460,1470,1480,1490,1500,1510,     1520,1530,1540,1550,1600,1610,1620,1630,1640,1650,1660,1670,1680,1690
1240 REM          B. Replies
1250 PRINT "DO COMPUTERS WORRY YOU?": GOTO 1700
1260 PRINT "I AM NOT INTERESTED IN NAMES.": GOTO 1700
1270 PRINT "IN WHAT WAY?": GOTO 1700
1280 PRINT "DO YOU OFTEN THINK OF" + DR$ + "?": GOTO 1700
1290 PRINT "DID YOU THINK I WOULD FORGET" + DR$ + "?": GOTO 1700
1300 PRINT "REALLY, " + DR$ + "?": GOTO 1700
1310 PRINT "WHAT DOES THAT DREAM SUGGEST TO YOU?": GOTO 1700
1320 PRINT "DO YOU THINK ITS LIKELY THAT IF " + DR$ + "?": GOTO 1700
1330 PRINT "REALLY, " + KE$ + "?": GOTO 1700
1340 PRINT "WHAT IF YOU WERE" + DR$ + "?": GOTO 1700
1350 PRINT "WERE YOU REALLY?": GOTO 1700
1360 PRINT "WOULD YOU BELIEVE I WAS" + DR$ + "?": GOTO 1700
1370 PRINT "TELL ME MORE ABOUT YOUR FAMILY.": GOTO 1700
1380 PRINT "YOUR" + DR$ + ".": GOSUB 1710: GOTO 1700
1390 PRINT "CAN YOU THINK OF A SPECIFIC EXAMPLE?": GOTO 1700
1400 PRINT "WHY ARE YOU INTERESTED IN WHETHER I AM" + DR$ + " OR NOT?":              GOTO 1700
1410 PRINT "DID YOU THINK THEY MIGHT NOT BE " + DR$ + "?": GOTO 1700
1420 PRINT "DO YOU BELIEVE YOU ARE" + DR$ + "?": GOTO 1700
1430 PRINT "IS THAT THE REAL REASON?": GOTO 1700
1440 PRINT "YOU BELIEVE I CAN" + DR$ + ", DON'T YOU?": GOTO 1700
1450 PRINT "WHETHER OR NOT YOU CAN" + DR$ + "DEPENDS ON MORE THAN ME.":              GOTO 1700
1460 PRINT "YOU SEEM QUITE POSITIVE.": GOTO 1700
1470 PRINT "I AM SORRY, I SPEAK ONLY ENGLISH.": GOTO 1700
1480 PRINT "HOW DO YOU DO.": GOTO 1700
1490 PRINT "WHY DO YOU ASK?": GOTO 1700
1500 PRINT "WHAT MAKES YOU THINK I AM" + DR$ + "?": GOTO 1700
1510 PRINT "WE WERE DISCUSSING YOU, NOT ME.": GOTO 1700
1520 PRINT "YOU DON'T SEEM QUITE CERTAIN.": GOTO 1700
1530 PRINT "WHY ARE YOU CONCERNED OVER MY " + DR$ + "?": GOTO 1700
1540 PRINT "ARE YOU SAYING 'NO' JUST TO BE NEGATIVE?": GOTO 1700
1550 RAN = INT(RND*4+1): ON RAN GOTO 1560, 1570, 1580, 1590
1560 PRINT "PLEASE DON'T APOLOGIZE.": GOTO 1700
1570 PRINT "APOLOGIES ARE NOT NECESSARY.": GOTO 1700
1580 PRINT "WHAT FEELINGS DO YOU HAVE WHEN YOU APOLOGIZE?": GOTO 1700
1590 PRINT "YOU NEEDN'T FEEL THAT YOU HAVE TO APOLOGIZE.": GOTO 1700
1600 PRINT "DO YOU BELIEVE I DON'T" + DR$ + "?": GOTO 1700
1610 PRINT "DO YOU THINK YOU SHOULD BE ABLE TO" + DR$ + "?": GOTO 1700
1620 PRINT "WHAT WOULD IT MEAN TO YOU IF YOU GOT" + DR$ + "?": GOTO 1700
1630 GOSUB 1740: GOSUB 1770: PRINT "I AM SORRY TO HEAR YOU ARE" + DR$ + ".":         GOTO 1700
1640 GOSUB 1740: GOSUB 1770: PRINT "HOW HAVE I HELPED YOU TO BE" + DR$ + "?":        GOTO 1700
1650 PRINT "DO YOU REALLY THINK SO?": GOTO 1700
1660 PRINT "IS IT BECAUSE YOU ARE" + DR$ + " THAT YOU CAME TO ME?": GOTO 1700
1670 PRINT "HOW DO YOU KNOW YOU CAN'T" + DR$ + "?": GOTO 1700
1680 PRINT "DON'T YOU REALLY" + DR$ +"?": GOTO 1700
1690 PRINT "TELL ME MORE ABOUT SUCH FEELINGS.": GOTO 1700
1700 RETURN
1710 REM          Special processing if keyword is MY
1720 IF LEN(DR$) > 11 THEN YO$ = DR$
1730 RETURN
1740 REM          Remove "@" marker from key phrase if present
1750 IF MID$(KE$, 4, 1) = "@" THEN DR$ = RIGHT$(KE$, LEN(KE$) - 9)
1760 RETURN
1770 REM          Remove "*" marker from key phrase if present
1780 IF MID$(KE$, 7, 1) = "*" THEN DR$ = RIGHT$(KE$, LEN(KE$) - 7)
1790 RETURN
