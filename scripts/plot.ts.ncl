; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

d="erain"
lev="10"

;reg="SA"
;reg_lon=(/135, 160/)+"E"
;reg_lat=(/"25","45"/)+"S"

reg="SH"
reg_lon=(/0, 360/)+"E"
reg_lat=(/"20","70"/)+"S"

col = (/"black","red","orange","blue","paleturquoise","green","olivedrab"/)
mon = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
ssn = (/"djf","mam","jja","son"/)
var = (/"lat","lon"/)
var_title = (/"latitude","longitude"/)
varn = (/2,3/)

y1=1979
y2=2016

data=new((/y2-y1+1,dimsizes(varn)/),float)
data@_FillValue = -9999.0
listyr=data(:,0)
pltarry=new((/2,dimsizes(data(:,0))/),typeof(data),data@_FillValue)

 do iv = 0,1
  do is  = 0,3
   fout = "../output/freq_ts/"+var(iv)+"."+d+lev+"."+ssn(is)+y1+"-"+y2+"."+reg

   data=data@_FillValue

; ***  Plotting  *******************************************************************
  wks  = gsn_open_wks("ps",fout)

  res                    = True

  ;res@tiMainString       = "Ray Path"
  ;res@tiMainFontHeightF  = 0.013
  ;res@gsnCenterString    = "("+lat(loc)+"N; "+lon(loc)+"E)      period = "+p+"days     root = "+root
  ;res@gsnCenterStringFontHeightF = 0.008

  yr = ispan(y1,y2,1)
  yr1 = ispan(1,y2-y1+1,1)

; *** read data ******************************************************************


      fin = "../output/freq_ts/"+d+lev+"."+ssn(is)+y1+"-"+y2+"."+reg+".dat"
      if (fileexists(fin)) then
      print("Data from "+fin)
       getvar = readAsciiTable (fin,4,"float",0)

       listyr(:dimsizes(getvar(:,1))-1) = getvar(:,0)
       data(:dimsizes(getvar(:,1))-1,iv)   = getvar(:,varn(iv))

       delete(getvar)

       end if  ; if filein exist

       ;print(listyr)
       ;print(data(:,iv))


;*** XY plot *************************************************************************

; regression
      rc = regline(yr-y1+1, data(:,iv))
      rc@units = "NPF/yr"
      pltarry(0,:)=data(:,iv)
      pltarry(1,:)=rc*((yr-y1+1)-rc@xave)+rc@yave

      res@tiMainString = var_title(iv)+"  "+ssn(is)+" "+y1+" - "+y2

      res@gsnCenterString    = "("+reg_lat(0)+" - "+reg_lat(1)+"; "+reg_lon(0)+" - "+reg_lon(1)+")"


      plot = gsn_csm_xy(wks,yr,pltarry,res)
;---Drawing the plot will draw the attached polylines and markers.
;      draw(plot)
;      frame(wks)



; ***  End plotting  ***************************************************************
  end do ; loc
 end do  ;var

end
