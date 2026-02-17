10 rem ============================================================
20 rem  fulltest.bas - Comprehensive test of BASIC interpreter
30 rem  Tests all language features systematically
40 rem ============================================================
50 rem
100 rem ============================================================
110 rem  Section 1: PRINT and basic output
120 rem ============================================================
130 print "=== Section 1: PRINT and basic output ==="
140 print "Hello, World!"
150 print
160 print "Line 1"; " Line 2"
170 print "Col1","Col2","Col3"
180 print "No newline at end";
190 print " ...continued"
200 print "Tab test:"; tab(20); "at col 20"
210 print "Two values:"; 42; 3.14
220 rem
230 rem Verify empty print gives blank line
240 print
250 print "Section 1 complete"
260 print
300 rem ============================================================
310 rem  Section 2: Variables and LET assignment
320 rem ============================================================
330 print "=== Section 2: Variables and LET assignment ==="
340 let a% = 42
350 b% = 100
360 c = 3.14159
370 d = 2.71828
380 a$ = "Hello"
390 b$ = "World"
400 print "Integer a% ="; a%
410 print "Integer b% ="; b%
420 print "Real c ="; c
430 print "Real d ="; d
440 print "String a$ = "; a$
450 print "String b$ = "; b$
460 rem Test variable reassignment
470 a% = 99
480 print "a% reassigned to"; a%
490 print "Section 2 complete"
500 print
600 rem ============================================================
610 rem  Section 3: Arithmetic operators
620 rem ============================================================
630 print "=== Section 3: Arithmetic operators ==="
640 print "10 + 3 ="; 10 + 3
650 print "10 - 3 ="; 10 - 3
660 print "10 * 3 ="; 10 * 3
670 print "10 / 3 ="; 10 / 3
680 print "10 div 3 ="; 10 div 3
690 print "10 mod 3 ="; 10 mod 3
700 print "2 ^ 10 ="; 2 ^ 10
710 rem Test operator precedence
720 print "2 + 3 * 4 ="; 2 + 3 * 4
730 print "(2 + 3) * 4 ="; (2 + 3) * 4
740 print "2 ^ 3 ^ 2 ="; 2 ^ 3 ^ 2
750 rem Unary minus
760 print "-5 + 10 ="; -5 + 10
770 print "-(3 + 4) ="; -(3 + 4)
780 rem Mixed integer and real
790 print "5 + 2.5 ="; 5 + 2.5
800 print "10 / 4 ="; 10 / 4
810 print "Section 3 complete"
820 print
900 rem ============================================================
910 rem  Section 4: Comparison operators
920 rem ============================================================
930 print "=== Section 4: Comparison operators ==="
940 print "5 = 5 is"; 5 = 5
950 print "5 = 6 is"; 5 = 6
960 print "5 <> 6 is"; 5 <> 6
970 print "5 <> 5 is"; 5 <> 5
980 print "3 < 5 is"; 3 < 5
990 print "5 < 3 is"; 5 < 3
1000 print "5 > 3 is"; 5 > 3
1010 print "3 > 5 is"; 3 > 5
1020 print "3 <= 3 is"; 3 <= 3
1030 print "3 <= 5 is"; 3 <= 5
1040 print "5 <= 3 is"; 5 <= 3
1050 print "5 >= 5 is"; 5 >= 5
1060 print "5 >= 3 is"; 5 >= 3
1070 print "3 >= 5 is"; 3 >= 5
1080 rem String comparisons
1090 print "abc = abc is"; "abc" = "abc"
1100 print "abc = def is"; "abc" = "def"
1110 print "abc <> def is"; "abc" <> "def"
1120 print "Section 4 complete"
1130 print
1200 rem ============================================================
1210 rem  Section 5: Logical operators
1220 rem ============================================================
1230 print "=== Section 5: Logical operators ==="
1240 print "1 and 1 ="; 1 and 1
1250 print "1 and 0 ="; 1 and 0
1260 print "0 and 0 ="; 0 and 0
1270 print "1 or 0 ="; 1 or 0
1280 print "0 or 0 ="; 0 or 0
1290 print "1 xor 0 ="; 1 xor 0
1300 print "1 xor 1 ="; 1 xor 1
1310 print "not 0 ="; not 0
1320 print "not 1 ="; not 1
1340 print "Section 5 complete"
1350 print
1400 rem ============================================================
1410 rem  Section 6: String operations
1420 rem ============================================================
1430 print "=== Section 6: String operations ==="
1440 a$ = "Hello"
1450 b$ = " World"
1460 rem String concatenation
1470 print "Hello + World = "; a$ + b$
1480 rem String functions
1490 print "len(Hello) ="; len(a$)
1500 print "left$(Hello, 3) = "; left$(a$, 3)
1510 print "right$(Hello, 3) = "; right$(a$, 3)
1520 print "mid$(Hello, 2, 3) = "; mid$(a$, 2, 3)
1530 print "asc(H) ="; asc("H")
1540 print "chr$(65) = "; chr$(65)
1550 print "str$(42) = "; str$(42)
1560 print "str$(3.14) = "; str$(3.14)
1570 print "val(123) ="; val("123")
1580 print "lcase$(HELLO) = "; lcase$("HELLO")
1590 print "ucase$(hello) = "; ucase$("hello")
1600 print "Section 6 complete"
1610 print
1700 rem ============================================================
1710 rem  Section 7: Math functions
1720 rem ============================================================
1730 print "=== Section 7: Math functions ==="
1740 print "abs(-5) ="; abs(-5)
1750 print "abs(5) ="; abs(5)
1760 print "abs(-3.7) ="; abs(-3.7)
1770 print "sgn(-5) ="; sgn(-5)
1780 print "sgn(0) ="; sgn(0)
1790 print "sgn(5) ="; sgn(5)
1800 print "int(3.7) ="; int(3.7)
1810 print "int(-3.7) ="; int(-3.7)
1820 print "sqr(25) ="; sqr(25)
1830 print "sqr(2) ="; sqr(2)
1840 rem Trig functions (using known values)
1850 print "sin(0) ="; sin(0)
1860 print "cos(0) ="; cos(0)
1870 print "tan(0) ="; tan(0)
1880 print "atn(0) ="; atn(0)
1890 print "log(1) ="; log(1)
1900 print "exp(0) ="; exp(0)
1910 print "Section 7 complete"
1920 print
2000 rem ============================================================
2010 rem  Section 8: IF/THEN/ELSE single line
2020 rem ============================================================
2030 print "=== Section 8: IF/THEN/ELSE single line ==="
2040 if 1 then print "IF 1 THEN: true (correct)"
2050 if 0 then print "IF 0 THEN: should not print"
2060 rem IF/THEN/ELSE single line
2070 if 1 > 0 then print "1>0: true branch" else print "1>0: false branch"
2080 if 1 < 0 then print "1<0: true branch" else print "1<0: false branch"
2090 rem IF with GOTO
2100 if 1 then goto 2120
2110 print "ERROR: should have been skipped by goto"
2120 print "GOTO from IF worked"
2130 print "Section 8 complete"
2140 print
2200 rem ============================================================
2210 rem  Section 9: IF/THEN/ELSE/ENDIF multi-line
2220 rem ============================================================
2230 print "=== Section 9: IF/THEN/ELSE/ENDIF multi-line ==="
2240 if 5 > 3 then
2250    print "Multi-line IF: true branch line 1"
2260    print "Multi-line IF: true branch line 2"
2270 else
2280    print "Multi-line IF: false branch (should not print)"
2290 endif
2300 if 3 > 5 then
2310    print "Should not print (false condition)"
2320 else
2330    print "Multi-line ELSE: correct branch"
2340 endif
2350 rem Nested IF
2360 if 1 then
2370    if 1 then
2380       print "Nested IF: inner true (correct)"
2390    endif
2400 endif
2410 print "Section 9 complete"
2420 print
2500 rem ============================================================
2510 rem  Section 10: FOR/NEXT loops
2520 rem ============================================================
2530 print "=== Section 10: FOR/NEXT loops ==="
2540 rem Basic FOR loop
2550 print "Count 1 to 5:";
2560 for i% = 1 to 5
2570    print " "; i%;
2580 next i%
2590 print
2600 rem FOR with STEP
2610 print "Even 2 to 10:";
2620 for i% = 2 to 10 step 2
2630    print " "; i%;
2640 next i%
2650 print
2660 rem Negative STEP
2670 print "Count 5 to 1:";
2680 for i% = 5 to 1 step -1
2690    print " "; i%;
2700 next i%
2710 print
2720 rem FOR with real variable
2730 print "Real loop 0.0 to 1.0 step 0.5:";
2740 for x = 0.0 to 1.0 step 0.5
2750    print " "; x;
2760 next x
2770 print
2780 rem Zero-trip loop (start > end with positive step)
2790 print "Zero-trip loop (should print nothing between brackets): [";
2800 for i% = 10 to 1
2810    print "ERROR";
2820 next i%
2830 print "]"
2840 print "Section 10 complete"
2850 print
2900 rem ============================================================
2910 rem  Section 11: WHILE/WEND
2920 rem ============================================================
2930 print "=== Section 11: WHILE/WEND ==="
2940 i% = 1
2950 print "While count 1 to 5:";
2960 while i% <= 5
2970    print " "; i%;
2980    i% = i% + 1
2990 wend
3000 print
3010 rem While with false condition (zero iterations)
3020 print "While false (should print nothing between brackets): [";
3030 while 0
3040    print "ERROR"
3050 wend
3060 print "]"
3070 print "Section 11 complete"
3080 print
3100 rem ============================================================
3110 rem  Section 12: REPEAT/UNTIL
3120 rem ============================================================
3130 print "=== Section 12: REPEAT/UNTIL ==="
3140 i% = 1
3150 print "Repeat count 1 to 5:";
3160 repeat
3170    print " "; i%;
3180    i% = i% + 1
3190 until i% > 5
3200 print
3210 print "Section 12 complete"
3220 print
3300 rem ============================================================
3310 rem  Section 13: GOSUB/RETURN
3320 rem ============================================================
3330 print "=== Section 13: GOSUB/RETURN ==="
3340 print "Before gosub"
3350 gosub 3400
3360 print "After gosub returned"
3370 goto 3430
3380 rem
3390 rem Subroutine
3400 print "Inside subroutine"
3410 return
3420 rem
3430 print "Section 13 complete"
3440 print
3500 rem ============================================================
3510 rem  Section 14: ON GOTO and ON GOSUB
3520 rem ============================================================
3530 print "=== Section 14: ON GOTO and ON GOSUB ==="
3540 x% = 2
3550 on x% goto 3570, 3590, 3610
3560 goto 3620
3570 print "ON GOTO selected 1 (wrong)"
3580 goto 3620
3590 print "ON GOTO selected 2 (correct)"
3600 goto 3620
3610 print "ON GOTO selected 3 (wrong)"
3620 rem
3630 x% = 1
3640 on x% gosub 3680
3650 print "After ON GOSUB returned"
3660 goto 3710
3670 rem
3680 print "ON GOSUB subroutine called (correct)"
3690 return
3700 rem
3710 print "Section 14 complete"
3720 print
3800 rem ============================================================
3810 rem  Section 15: GOTO
3820 rem ============================================================
3830 print "=== Section 15: GOTO ==="
3840 goto 3870
3850 print "ERROR: This should be skipped"
3860 print "ERROR: This should also be skipped"
3870 print "GOTO target reached (correct)"
3880 print "Section 15 complete"
3890 print
3900 rem ============================================================
3910 rem  Section 16: DATA/READ/RESTORE
3920 rem ============================================================
3930 print "=== Section 16: DATA/READ/RESTORE ==="
3940 data 10, 20, 30
3950 data "hello", "world"
3960 data 3.14, 2.72
3970 read a%
3980 read b%
3990 read c%
4000 print "Read integers:"; a%; b%; c%
4010 read a$
4020 read b$
4030 print "Read strings: "; a$; " "; b$
4040 read x
4050 read y
4060 print "Read reals:"; x; y
4070 rem Test RESTORE
4080 restore
4090 read a%
4100 print "After restore, re-read:"; a%
4110 print "Section 16 complete"
4120 print
4200 rem ============================================================
4210 rem  Section 17: DIM (arrays)
4220 rem ============================================================
4230 print "=== Section 17: DIM (arrays) ==="
4240 dim a%(10)
4250 dim b$(5)
4260 dim c(5)
4270 rem Fill and read integer array
4280 for i% = 1 to 5
4290    a%(i%) = i% * 10
4300 next i%
4310 print "Integer array:";
4320 for i% = 1 to 5
4330    print " "; a%(i%);
4340 next i%
4350 print
4360 rem String array
4370 b$(1) = "one"
4380 b$(2) = "two"
4390 b$(3) = "three"
4400 print "String array: "; b$(1); " "; b$(2); " "; b$(3)
4410 rem Real array
4420 c(1) = 1.1
4430 c(2) = 2.2
4440 c(3) = 3.3
4450 print "Real array:"; c(1); c(2); c(3)
4460 print "Section 17 complete"
4470 print
4500 rem ============================================================
4510 rem  Section 18: DEF (single-line functions)
4520 rem ============================================================
4530 print "=== Section 18: DEF (single-line functions) ==="
4540 def fna(x) = x * x
4550 def fnb(x, y) = x + y
4560 def fnc$(x$) = x$ + "!"
4570 print "fna(5) = x*x ="; fna(5)
4580 print "fnb(3, 4) = x+y ="; fnb(3, 4)
4590 print "fnc$(Hi) = x+! = "; fnc$("Hi")
4600 rem Function with no parameters
4610 def fnd = 42
4620 print "fnd = 42:"; fnd
4630 print "Section 18 complete"
4640 print
4700 rem ============================================================
4710 rem  Section 19: Multi-line FUNCTION/ENDFUNC
4720 rem ============================================================
4730 print "=== Section 19: Multi-line FUNCTION/ENDFUNC ==="
4740 function fne(x)
4750    dim t(3)
4760    t(1) = x
4770    t(2) = x * 2
4780    t(3) = x * 3
4790 endfunc t(1) + t(2) + t(3)
4800 print "fne(10) = 10+20+30 ="; fne(10)
4810 rem Multi-line function with string result
4820 function fnf$(x$, y$)
4830    z$ = x$ + " " + y$
4840 endfunc z$
4850 print "fnf$(Good, Day) = "; fnf$("Good", "Day")
4860 print "Section 19 complete"
4870 print
4900 rem ============================================================
4910 rem  Section 20: Multi-line PROCEDURE/ENDPROC
4920 rem ============================================================
4930 print "=== Section 20: Multi-line PROCEDURE/ENDPROC ==="
4940 procedure pra(x)
4950    print "Procedure called with x ="; x
4960 endproc
4970 pra(99)
4980 rem Procedure modifying global
4990 g% = 0
5000 procedure prb(x%)
5010    g% = x%
5020 endproc
5030 prb(42)
5040 print "Global g% set by procedure:"; g%
5050 print "Section 20 complete"
5060 print
5100 rem ============================================================
5110 rem  Section 21: SELECT/CASE/OTHER/ENDSEL
5120 rem ============================================================
5130 print "=== Section 21: SELECT/CASE/OTHER/ENDSEL ==="
5140 x% = 2
5150 select x%
5160 case 1
5170    print "Case 1 (wrong)"
5180 case 2
5190    print "Case 2 (correct)"
5200 case 3
5210    print "Case 3 (wrong)"
5220 endsel
5230 rem Test OTHER (default)
5240 x% = 99
5250 select x%
5260 case 1
5270    print "Case 1 (wrong)"
5280 case 2
5290    print "Case 2 (wrong)"
5300 other
5310    print "Other/default (correct)"
5320 endsel
5330 rem Test SELECT with strings
5340 a$ = "hello"
5350 select a$
5360 case "bye"
5370    print "Case bye (wrong)"
5380 case "hello"
5390    print "Case hello (correct)"
5400 endsel
5410 print "Section 21 complete"
5420 print
5500 rem ============================================================
5510 rem  Section 22: Multi-statement lines with ':'
5520 rem ============================================================
5530 print "=== Section 22: Multi-statement lines ==="
5540 a% = 1 : b% = 2 : c% = 3
5550 print "a=";a%;", b=";b%;", c=";c%
5560 print "Line 1"; : print " Line 2"
5570 print "Section 22 complete"
5580 print
5600 rem ============================================================
5610 rem  Section 23: REM and ! comments
5620 rem ============================================================
5630 print "=== Section 23: REM and ! comments ==="
5640 rem This is a REM comment
5650 ! This is a ! comment
5660 print "Comments were skipped (correct)"
5670 print "Section 23 complete"
5680 print
5700 rem ============================================================
5710 rem  Section 24: Nested FOR loops
5720 rem ============================================================
5730 print "=== Section 24: Nested FOR loops ==="
5740 for i% = 1 to 3
5750    for j% = 1 to 3
5760       print " "; i%*10+j%;
5770    next j%
5780 next i%
5790 print
5800 print "Section 24 complete"
5810 print
5900 rem ============================================================
5910 rem  Section 25: Nested GOSUB
5920 rem ============================================================
5930 print "=== Section 25: Nested GOSUB ==="
5940 gosub 5980
5950 print "Back at main level"
5960 goto 6040
5970 rem
5980 print "In outer subroutine"
5990 gosub 6020
6000 print "Back in outer subroutine"
6010 return
6020 print "In inner subroutine"
6030 return
6040 print "Section 25 complete"
6050 print
6100 rem ============================================================
6110 rem  Section 26: String expressions in conditions
6120 rem ============================================================
6130 print "=== Section 26: String expressions in conditions ==="
6140 a$ = "abc"
6150 if a$ = "abc" then print "String equality: correct"
6160 if a$ <> "xyz" then print "String inequality: correct"
6170 if a$ = "xyz" then print "ERROR: strings should not be equal"
6180 print "Section 26 complete"
6190 print
6200 rem ============================================================
6210 rem  Section 27: Expression nesting and complex expressions
6220 rem ============================================================
6230 print "=== Section 27: Complex expressions ==="
6240 print "((2+3)*4-6)/7 ="; ((2+3)*4-6)/7
6250 print "abs(sgn(-5)) ="; abs(sgn(-5))
6260 print "len(chr$(65)+chr$(66)) ="; len(chr$(65)+chr$(66))
6270 print "val(str$(42)) ="; val(str$(42))
6280 print "left$(right$(Hello World, 5), 3) = "; left$(right$("Hello World", 5), 3)
6290 print "Section 27 complete"
6300 print
6400 rem ============================================================
6410 rem  Section 28: Edge cases
6420 rem ============================================================
6430 print "=== Section 28: Edge cases ==="
6440 rem Large numbers
6450 print "Large: 1000000 * 1000 ="; 1000000 * 1000
6460 rem Negative zero
6470 print "0 * -1 ="; 0 * -1
6480 rem Empty string
6490 a$ = ""
6500 print "len of empty string ="; len(a$)
6510 rem Division yielding real
6520 print "7 / 2 ="; 7 / 2
6530 rem Multiple data reads across data statements
6540 print "Section 28 complete"
6550 print
6600 rem ============================================================
6610 rem  Section 29: WHILE with complex condition
6620 rem ============================================================
6630 print "=== Section 29: WHILE with complex condition ==="
6640 i% = 0
6650 while (i% < 10) and (i% mod 2 = 0 or i% < 5)
6660    print " "; i%;
6670    i% = i% + 1
6680 wend
6690 print
6700 print "Section 29 complete"
6710 print
6800 rem ============================================================
6810 rem  Section 30: FOR loop with string variable
6820 rem ============================================================
6830 print "=== Section 30: Miscellaneous ==="
6840 rem Test that print comma gives column tabs
6850 print "Col1","Col2"
6860 rem Test unary plus
6870 print "+5 ="; +5
6880 rem Test multiple semicolons in print
6890 print "A";"B";"C"
6900 print "Section 30 complete"
6910 print
6950 rem ============================================================
6960 rem  Section 31: Labels (symbolic goto targets)
6970 rem ============================================================
6980 print "=== Section 31: Labels ==="
6990 goto 7030
7000 print "ERROR: skipped by goto label"
7010 rem
7020 rem Target for goto
7030 print "Reached goto target (correct)"
7040 print "Section 31 complete"
7050 print
7100 rem ============================================================
7110 rem  Section 32: PRINT USING formatted output
7120 rem ============================================================
7130 print "=== Section 32: PRINT USING ==="
7140 print using "###.##"; 3.14
7150 print using "###.##"; 99.9
7160 print using "$##.##"; 5.99
7170 print using "+##"; 42
7180 print using "+##"; -42
7190 print using "-###"; -99
7200 print using "-###"; 99
7210 print using "The answer is ###."; 42
7220 print "Section 32 complete"
7230 print
7300 rem ============================================================
7310 rem  Section 33: Mixed type DATA/READ
7320 rem ============================================================
7330 print "=== Section 33: Mixed type DATA/READ ==="
7335 rem Restore to beginning of global data stream (sec 16 data: 10,20,30,
7336 rem "hello","world",3.14,2.72) and read across types
7340 restore
7350 read a%
7360 read b%
7365 read c%
7370 print "Read 3 ints:"; a%; b%; c%
7374 read a$
7376 read b$
7378 print "Read 2 strs: "; a$; " "; b$
7382 read x
7384 read y
7386 print "Read 2 reals:"; x; y
7395 print "Section 33 complete"
7400 print
7500 rem ============================================================
7510 rem  Section 34: Nested IF/ELSE/ENDIF
7520 rem ============================================================
7530 print "=== Section 34: Nested IF/ELSE/ENDIF ==="
7540 a% = 1
7550 b% = 2
7560 if a% = 1 then
7570    if b% = 2 then
7580       print "Nested: a=1, b=2 (correct)"
7590    else
7600       print "Nested: a=1, b<>2 (wrong)"
7610    endif
7620 else
7630    print "Nested: a<>1 (wrong)"
7640 endif
7650 rem Test else branch of outer
7660 if a% = 99 then
7670    print "Outer false: wrong"
7680 else
7690    if b% = 99 then
7700       print "Inner false: wrong"
7710    else
7720       print "Both else branches: correct"
7730    endif
7740 endif
7750 print "Section 34 complete"
7760 print
7800 rem ============================================================
7810 rem  Section 35: REPEAT/UNTIL with complex body
7820 rem ============================================================
7830 print "=== Section 35: REPEAT/UNTIL complex ==="
7840 s% = 0
7850 i% = 1
7860 repeat
7870    s% = s% + i%
7880    i% = i% + 1
7890 until i% > 10
7900 print "Sum 1..10 ="; s%
7910 print "Section 35 complete"
7920 print
8000 rem ============================================================
8010 rem  Section 36: SELECT with expressions
8020 rem ============================================================
8030 print "=== Section 36: SELECT with expressions ==="
8040 a% = 5
8050 select a% * 2
8060 case 8
8070    print "Case 8 (wrong)"
8080 case 10
8090    print "Case 10 = 5*2 (correct)"
8100 case 12
8110    print "Case 12 (wrong)"
8120 endsel
8130 print "Section 36 complete"
8140 print
8200 rem ============================================================
8210 rem  Section 37: Function calling function
8220 rem ============================================================
8230 print "=== Section 37: Function calling function ==="
8240 def fng(x) = x + 1
8250 def fnh(x) = fng(x) * 2
8260 print "fnh(5) = (5+1)*2 ="; fnh(5)
8270 print "Section 37 complete"
8280 print
8300 rem ============================================================
8310 rem  Section 38: Integer % suffix variables
8320 rem ============================================================
8330 print "=== Section 38: Integer suffix variables ==="
8340 x% = 100
8350 y% = 200
8360 print "x% + y% ="; x% + y%
8370 print "x% * y% ="; x% * y%
8380 print "x% div y% ="; x% div y%
8390 print "Section 38 complete"
8400 print
8500 rem ============================================================
8510 rem  Section 39: Multiple DATA statements
8520 rem ============================================================
8530 print "=== Section 39: Multiple DATA statements ==="
8535 rem Continues from global data pointer (sec 33 consumed sec 16 data via restore)
8550 data 100, 200, 300
8560 data 400, 500
8570 rem Read across DATA statement boundaries
8580 for i% = 1 to 5
8590    read v%
8600    print " "; v%;
8610 next i%
8620 print
8630 print "Section 39 complete"
8640 print
8700 rem ============================================================
8710 rem  Section 40: Nested control structures
8720 rem ============================================================
8730 print "=== Section 40: Nested control structures ==="
8740 for i% = 1 to 3
8750    if i% = 1 then
8760       print "First iteration"
8770    else
8780       if i% = 2 then
8790          print "Second iteration"
8800       else
8810          print "Third iteration"
8820       endif
8830    endif
8840 next i%
8850 print "Section 40 complete"
8860 print
8900 rem ============================================================
8910 rem  Section 41: Array with computed indices
8920 rem ============================================================
8930 print "=== Section 41: Array with computed indices ==="
8940 dim d%(10)
8950 for i% = 1 to 10
8960    d%(i%) = i% * i%
8970 next i%
8980 print "d%(3) = 3*3 ="; d%(3)
8990 print "d%(5+2) = 7*7 ="; d%(5+2)
9000 print "d%(d%(2)) = d%(4) = 16:"; d%(d%(2))
9010 print "Section 41 complete"
9020 print
9100 rem ============================================================
9110 rem  Section 42: File output and input
9120 rem ============================================================
9130 print "=== Section 42: File output and input ==="
9140 open "testfile.tmp" for output as #3
9150 print #3, "Hello from file"
9160 print #3, "Second line"
9170 close #3
9180 open "testfile.tmp" for input as #3
9190 input #3, a$
9200 input #3, b$
9210 close #3
9220 print "Line 1: "; a$
9230 print "Line 2: "; b$
9240 print "Section 42 complete"
9250 print
9300 rem ============================================================
9310 rem  Section 43: File close and reopen
9320 rem ============================================================
9330 print "=== Section 43: File close and reopen ==="
9340 open "testfile2.tmp" for output as #4
9350 print #4, "data1"
9360 close #4
9370 open "testfile3.tmp" for output as #4
9380 print #4, "data2"
9390 close #4
9400 open "testfile2.tmp" for input as #4
9410 input #4, a$
9420 close #4
9430 open "testfile3.tmp" for input as #4
9440 input #4, b$
9450 close #4
9460 print "File 1: "; a$
9470 print "File 2: "; b$
9480 print "Section 43 complete"
9490 print
9500 rem ============================================================
9510 rem  Section 44: EOF detection
9520 rem ============================================================
9530 print "=== Section 44: EOF detection ==="
9540 open "testeof.tmp" for output as #3
9550 print #3, "only line"
9560 close #3
9570 open "testeof.tmp" for input as #3
9580 c% = 0
9590 while not eof(#3)
9600    input #3, a$
9610    c% = c% + 1
9620 wend
9630 close #3
9640 print "Lines read:"; c%
9650 print "Last line: "; a$
9660 print "Section 44 complete"
9670 print
9700 rem ============================================================
9710 rem  Section 45: Multiple files open simultaneously
9720 rem ============================================================
9730 print "=== Section 45: Multiple files open simultaneously ==="
9740 open "testmf1.tmp" for output as #3
9750 open "testmf2.tmp" for output as #4
9760 print #3, "file three"
9770 print #4, "file four"
9780 close #4
9790 close #3
9800 open "testmf1.tmp" for input as #3
9810 open "testmf2.tmp" for input as #4
9820 input #3, a$
9830 input #4, b$
9840 close #3
9850 close #4
9860 print "File #3: "; a$
9870 print "File #4: "; b$
9880 print "Section 45 complete"
9890 print
9900 rem ============================================================
9910 rem  Summary
9920 rem ============================================================
9930 print "============================================================"
9940 print "All test sections completed."
9950 print "============================================================"
9960 end
