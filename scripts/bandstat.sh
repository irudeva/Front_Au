:
prog=bandstat
gfortran -o ${prog} ${prog}.f
echo $prog compiled
:
d=erain
lev=10
HS=( "0S" "0N")
Din=~/work/Projects/Front/FrontTrk/output/$d/$lev/
#for nh in 0
#do
:
fy=1979
sy=1979
ey=2016

for nh in 0
do

reg=( "SH" "SA" "SEP")

ssn=( "djf" "mam" "jja" "son" )



#Region
#reg="SA"
#lon=(135 160)
#lat=(-45 -25)   #from south to north!!!

#reg="SH"
#lon=(0 360)
#lat=(-70 -20)   #from south to north!!!

for nr in 0 1 2
do
  echo ${reg[$nr]}

if [ ${reg[$nr]} = "SH" ]; then
  lon=(0 360)
  echo 'still SH...'
  #lat=(-70 -20)   #from south to north!!!
elif [ ${reg[$nr]} = "SA" ]; then
  lon=(105 165)
  #lat=(-70 -20)   #from south to north!!!
elif [ ${reg[$nr]} = "SEP" ]; then
  lon=(140 200)
  #lat=(-70 -20)   #from south to north!!!
fi


for lat in $(seq -25 -5 -70)
do

  lat1=$[$lat+5]

for sn in 0 1 2 3
do
 fres[$sn]=~/work/Projects/Front/Front_Au/output/bandstat/${d}${lev}.${ssn[$sn]}$sy-$ey.${reg}.$lat-$lat1.dat
 echo ${fres[$sn]}
 if [ -f "${fres[$sn]}" ]; then
  rm ${fres[$sn]}; fi
done

y=$sy
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
fout=~/work/Projects/Front/Front_Au/output/bandstat/${d}${lev}.${ssn[$sn]}$cy.${reg}.dat
:
echo $fout
echo 'start prog'
./$prog <<mark
${Din}/fcyc2trk$lev.$d.
${HS[$nh]}
$sdate
$edate
${ssn[$sn]}
$fout
${lon[0]} ${lon[1]}
$lat $lat1
mark

cat $fout >>  "${fres[$sn]}"
rm $fout
:
done
y=$[$y+1]
done
:
done
:
done
:
done #HS
rm $prog
