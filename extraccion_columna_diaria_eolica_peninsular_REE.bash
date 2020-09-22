#!/bin/bash
#anho=(2010 2011 2012 2013)
#echo ${anho[*]}

for ((mes=3; mes <=3; mes++))
do
for ((dia=1; dia <= 31; dia++))
do
if [ $dia -lt 10 ];
	then

	echo $dia
	wget https://www.ree.es/es/balance-diario/peninsula/2019/0"$mes"/0"$dia"/d
	mv d d_0"$dia"_0"$mes"
	html2text d_0"$dia"_0"$mes" |grep Eólica > eolica_0"$dia"0"$mes"2019.txt
	more eolica_0"$dia"0"$mes"2019.txt |awk -v dia=$dia -v mes=$mes '{print "0"dia" 0"mes" 2019 " $2}' >> eolica_diaria_"$mes"_2019.txt

	else

	echo $dia
	wget https://www.ree.es/es/balance-diario/peninsula/2019/0"$mes"/"$dia"/d
	mv d d_"$dia"_0"$mes"
	html2text d_"$dia"_0"$mes" |grep Eólica > eolica_"$dia"0"$mes"2019.txt
	more eolica_"$dia"0"$mes"2019.txt |awk -v dia=$dia -v mes=$mes '{print dia" 0"mes" 2019 " $2}' >> eolica_diaria_"$mes"_2019.txt
fi
	echo "DIA=" $dia, "MES=" $mes

done

	gnuplot -e "set terminal png; set output 'eolica_diaria_"$mes"_2019.png'; set title 'Producción eólica diaria peninsular (GWh) - Mes "$mes" 2019' font 'Verdana,15'; plot 'eolica_diaria_"$mes"_2019.txt' using 1:4 with linespoints notitle"
	display eolica_diaria_"$mes"_2019.png

done



