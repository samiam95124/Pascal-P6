@echo off
rem
rem Change language extentions to GPC
rem 
sed -e 's/{ GPC start !/{$gnu-pascal}/g' -e 's/! GPC end }/{$classic-pascal-level-0}/g' source/pint.pas > temp
sed -e 's/{ ISO7185 start }/{ ISO7185 start !/g' -e 's/{ ISO7185 end }/! ISO7185 end }/g' temp > temp2
cp temp2 source/pint.pas