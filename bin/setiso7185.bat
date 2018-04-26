@echo off
rem
rem Change language extentions to ISO7185 (IE., no extensions)
rem 
sed -e 's/{$gnu-pascal}/{ GPC start !/g' -e 's/{$classic-pascal-level-0}/! GPC end }/g' source/pint.pas > temp
sed -e 's/{ Pascaline start }/{ Pascaline start !/g' -e 's/{ Pascaline end }/! Pascaline end }/g' temp > temp2
sed -e 's/{ ISO7185 start !/{ ISO7185 start }/g' -e 's/! ISO7185 end }/{ ISO7185 end }/g' temp2 > temp3
cp temp3 source/pint.pas
sed -e 's/{$gnu-pascal}/{ GPC start !/g' -e 's/{$classic-pascal-level-0}/! GPC end }/g' source/pcom.pas > temp
sed -e 's/{ Pascaline start }/{ Pascaline start !/g' -e 's/{ Pascaline end }/! Pascaline end }/g' temp > temp2
sed -e 's/{ ISO7185 start !/{ ISO7185 start }/g' -e 's/! ISO7185 end }/{ ISO7185 end }/g' temp2 > temp3
cp temp3 source/pcom.pas
