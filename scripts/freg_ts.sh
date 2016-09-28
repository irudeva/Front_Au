:
prog=freg_ts
gfortran -o ${prog} ${prog}.f
echo $prog compiled
:
d=erain
HS=( "0S" "0N" )
lev=10
Din=~/work/Projects/Front/FrontTrk/output/$d/
for nh in 0
do
:
fy=1979
y=1979
ey=2016
#Region
reg="SA"
lon=(135 160)
lat=(-45 -25)   #from south to north!!!

ssn=( "djf" "mam" "jja" "son" )
for sn in 0 1 2 3
do
 fres[$sn]=~/work/Projects/Front/Australia/output/freq_ts/${d}${lev}.${ssn[$sn]}$y-$ey.${reg}.dat
 echo ${fres[$sn]}
 if [ -f "${fres[$sn]}" ]; then
  rm ${fres[$sn]}; fi
done


while [ $y -le $ey ]
do
for sn in  0 1 2 3
#for sn in  0
do
if [[ $sn -eq 0 ]];then
#cy=$[$fy+1]
cy=$[$y+1]
py=$[$cy-1]
sdate=${py}1201
echo `expr $cy % 400`
if [ `expr $cy % 400` -eq 0 ]; then
  edate=${cy}0229
elif [ `expr $cy % 100` -eq 0 ]; then
  edate=${cy}0229
elif [ `expr $cy % 4 ` -eq 0 ]; then
  edate=${cy}0229
else
  edate=${cy}0228
  if [ ${cy} -gt $ey ];then
    continue
  fi
fi
#edate=${cy}0229
elif [ $sn -eq 1 ];then
cy=$y
sdate=${cy}0301
edate=${cy}0531
elif [[ $sn -eq 2 ]];then
cy=$y
sdate=${cy}0601
edate=${cy}0831
elif [[ $sn -eq 3 ]];then
cy=$y
sdate=${cy}0901
edate=${cy}1130
fi
echo ${ssn[$sn]}
fout=~/work/Projects/Front/Australia/output/freq_ts/${d}${lev}.${ssn[$sn]}$cy.${reg}.dat
:
echo $fout
./$prog <<mark
${Din}/fcyc2trk$lev.$d.
${HS[$nh]}
$sdate
$edate
${ssn[$sn]}
$fout
${lon[0]} ${lon[1]}
${lat[0]} ${lat[1]}
mark

cat $fout >>  "${fres[$sn]}"
rm $fout
:
done
y=$[$y+1]
done
:
done
rm $prog
