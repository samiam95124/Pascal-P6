{******************************************************************************
*                                                                             *
*                          STRING LIBRARY TESTS                               *
*                                                                             *
*                           Copyright (C) 2001                                *
*                                                                             *
*                               S. A. MOORE                                   *
*                                                                             *
* Tests for the string library.                                               *
*                                                                             *
******************************************************************************}

program strtst(output);

uses {stddef,}
     strings;

var sa:  packed array [1..20] of char;
    sb:  packed array [1..40] of char;
    spa: pstring;
    fa:  text;
    ba:  boolean;
    i:   integer;
    r:   real;

begin

   assign(fa, 'source/strtst.dat');
   reset(fa);
   writeln('1:', lcase('H'), lcase('j'), lcase('8'), ' s/b hj8');
   sa := 'ThiS iS WeRId       ';
   lcases(sa);
   writeln('48: ''', sa, '''');
   writeln('s/b ''this is werid       ');
   writeln('2: ', ucase('l'), ucase('P'), ucase('*'), ' s/b LP*');
   sa := 'ThiS iS WeRId       ';
   ucases(sa);
   writeln('47: ''', sa, '''');
   writeln('s/b ''THIS IS WERID       ''');
   sa := '12345678901234567890';
   clears(sa); writeln('3:  ''', sa, '''');
   writeln('s/b ''                    ''');
   writeln('4: ', len('hi there, joe       '):1, ' s/b 13');
   copy(spa, 'Whats up ?');
   writeln('5: ', spa^, ' s/b Whats up ?');
   copy(sa, 'what ho');
   writeln('6:  ''', sa, '''');
   writeln('s/b ''what ho             ''');
   copy(spa, 'mamba mamba        ');
   writeln('7: ''', spa^, ''' s/b ''mamba mamba''');
   spa := cat('Five dimes   ', 'make waste');
   writeln('8:  ''', spa^, '''');
   writeln('s/b ''Five dimes   make waste''');
   sa := 'What did            ';
   cat(sa, ' he say              ');
   writeln('9:  ''', sa, '''');
   writeln('s/b ''What did he say     ''');
   writeln('10: ', compc('What say', 'What say'), ' s/b true');
   writeln('11: ', compc('What Say', 'What say'), ' s/b false');
   writeln('12: ', compc('What say  ', 'What say'), ' s/b false');
   writeln('13: ', compcp('Big one', 'Big one      '), ' s/b true');
   writeln('14: ', compcp('big one', 'Big one     '), ' s/b false');
   writeln('15: ', gtrc('Abcdef', 'Abcdefg'), ' s/b true');
   writeln('16: ', gtrc('Abcdef', 'Abcd'), ' s/b false');
   writeln('17: ', gtrc('Abcd', 'abcd'), ' s/b true');
   writeln('18: ', gtrc('', 'abcd'), ' s/b true');
   writeln('19: ', gtrc('a', ''), ' s/b false');
   writeln('20: ', gtrcp('Abcdef   ', 'Abcdefg       '), ' s/b true');
   writeln('21: ', gtrcp('Abcdef    ', 'Abcd    '), ' s/b false');
   writeln('22: ', gtrcp('Abcd ', 'abcd       '), ' s/b true');
   writeln('23: ', gtrcp('        ', 'abcd            '), ' s/b true');
   writeln('24: ', gtrcp('a   ', ''), ' s/b false');
   writeln('25: ', gtr('AbCdef', 'Abcdefg'), ' s/b true');
   writeln('26: ', gtr('Abcdef', 'AbcD'), ' s/b false');
   writeln('27: ', gtr('Abcd', 'aBcD'), ' s/b false');
   writeln('28: ', gtr('', 'abCd'), ' s/b true');
   writeln('29: ', gtr('a', ''), ' s/b false');
   writeln('30: ', gtrp('AbcDef   ', 'AbcdefG       '), ' s/b true');
   writeln('31: ', gtrp('ABcdef    ', 'AbcD    '), ' s/b false');
   writeln('32: ', gtrp('AbCd ', 'abcD       '), ' s/b false');
   writeln('33: ', gtrp('        ', 'abCd            '), ' s/b true');
   writeln('34: ', gtrp('a   ', ''), ' s/b false');
   writeln('35: ', indexc('Hi there joe', 'ere j'):1, ' s/b 6');
   writeln('36: ', indexc('What price fame ?', 'what'):1, ' s/b 0');
   writeln('37: ', indexcp('Hi there joe    ', 'ere j            '):1, ' s/b 6');
   writeln('38: ', indexcp('What price fame ?   ', 'what            '):1, ' s/b 0');
   writeln('39: ', index ('Hi there Joe', 'ere j'):1, ' s/b 6');
   writeln('40: ', index('What price fame ?', 'none'):1, ' s/b 0');
   writeln('41: ', indexp('Hi there joe    ', 'ere J            '):1, ' s/b 6');
   writeln('42: ', indexp('What price fame ?   ', 'bark            '):1, ' s/b 0');
   spa := extract('The only thing I want', 5, 8);
   writeln('43: ', spa^, ' s/b only');
   extract(sa, 'Parking is not wise        ', 9, 14);
   writeln('44: ''', sa, '''');
   writeln('s/b ''is not              ''');
   spa := rep('hi ', 3);
   writeln('45: ''', spa^, ''' s/b ''hi hi hi ''');
   rep(sa, 'hi ', 3);
   writeln('46: ''', sa, '''');
   writeln('s/b ''hihihi              ''');
   reads(fa, spa, ba);
   readln(fa);
   writeln(spa^, ' ', ba, ' s/b Now is the time false');
   reads(fa, sa, ba);
   readln(fa);
   writeln('52: ''', sa, ''' ', ba);
   writeln('s/b ''for all good men    '' false');
   ints(sa, 123, 10);
   writeln('53: ''', sa, '''');
   writeln('s/b ''       123          '''); 
   spa := ints(456, 10);
   writeln('54: ''', spa^, '''');
   writeln('s/b ''       456''');
   ints(sa, 7864);
   writeln('55: ''', sa, '''');
   writeln('s/b ''7864                ''');
   spa := ints(984);
   writeln(spa^, ' s/b 984');
   reals(sa, 58.9, 15);
   writeln('56: ''', sa, '''');
   writeln('s/b '' 5.8900000e+001     ''');
   spa := reals(5001, 15);
   writeln('57: ''', spa^, '''');
   writeln('s/b '' 5.0010000e+003''');
   spa := reals(864, 10);
   writeln('58: ''', spa^, '''');
   writeln('s/b '' 8.64e+002''');
   reals(sb, 58.9);
   writeln('59: ''', sb, '''');
   writeln('s/b '' 5.89000000000000e+001                  ''');
   spa := reals(5001);
   writeln('60: ''', spa^, '''');
   writeln('s/b '' 5.00100000000000e+003''');
   reals(sa, 693.1, 10, 3);
   writeln('61: ''', sa, '''');
   writeln('s/b ''   693.100          ''');
   spa := reals(693.1, 10, 3);
   writeln('62: ''', sa, '''');
   writeln('s/b ''   693.100''');
   hexs(sa, $567, 10);
   writeln('63: ''', sa, '''');
   writeln('s/b ''       567          ');
   spa := hexs($567, 10);
   writeln('64: ''', sa, '''');
   writeln('s/b ''       567');
   hexs(sa, $123);
   writeln('65: ''', sa, '''');
   writeln('s/b ''123                 ');
   spa := hexs($832);
   writeln('66: ''', spa^, '''');
   writeln('s/b ''832''');
   write('67: ''');
   writeh(output, $8347, 10);
   writeln('''');
   writeln('s/b ''      8347''');
   write('68: ');
   writeh(output, $12934);
   writeln(' s/b 12934');
   i := intv('    193    ');
   writeln('69: ', i:1, ' s/b 193');
   r := realv('1');
   writeln('70: ', r, ' s/b  1.00000000000000e+000');
   r := realv('1.234');
   writeln('71: ', r, ' s/b  1.23400000000000e+000');
   r := realv('123.4');
   writeln('72: ', r, ' s/b  1.23400000000000e+002');
   r := realv('123.4e-3');
   writeln('73: ', r, ' s/b  1.23400000000000e+001');
   r := realv('-123.4e-3');
   writeln('74: ', r, ' s/b -1.23400000000000e+001');

end.