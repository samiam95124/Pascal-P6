@echo off
rem
rem Change language extentions to ISO7185 (IE., no extensions)
rem 
sed -e 's/{$gnu-pascal}/{ GPC start !/g' -e 's/{$classic-pascal-level-0}/! GPC end }/g' source/pint.pas > temp
sed -e 's/{ ISO7185 start !/{ ISO7185 start }/g' -e 's/! ISO7185 end }/{ ISO7185 end }/g' temp > temp2
cp temp2 source/pint.pas