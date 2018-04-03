import delimited "/Users/yujunzhou/Box Sync/Research/Price_data_auto/monthly", encoding(ISO-8859-1)clear

destring ipc price, force replace
twoway (scatter rcsi  yearmo,msymbol(diamond) connect(L))(scatter price  yearmo, yaxis(2) )
twoway (scatter fcs  yearmo,msymbol(diamond) connect(L))(scatter price  yearmo, yaxis(2) )
