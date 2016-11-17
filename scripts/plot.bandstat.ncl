begin

d="erain"
lev="10"


reg=(/"SH","SA","SEP"/)

col = (/"black","red","orange","blue","paleturquoise","green","olivedrab"/)
mon = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
ssn = (/"djf","mam","jja","son"/)
var = (/"freq","lat","length"/)
var_title = (/"frequency","latitude","frontal length"/)
varn = (/1,2,4/)

latrange = ispan(-25,-70,5)

plot = new((/dimsizes(latrange),dimsizes(ssn)/),graphic)


do nr = 1,1 ;dimsizes(reg)-1
 print("new nr"+reg(nr))

 if (reg(nr) .eq. "SH" ) then
  reg_lon=(/0, 360/)+"E"
  print("still SH...")
 else if (reg(nr) .eq. "SA" ) then
  reg_lon=(/105, 165/)+"E"
 else if (reg(nr) .eq. "SEP" ) then
  reg_lon=(/140, 200/)+"E"
  end if
  end if
  end if

;reg="SA"
;reg_lon=(/135, 160/)+"E"
;reg_lat=(/"25","45"/)+"S"

;reg="SH"
;reg_lon=(/0, 360/)+"E"
;reg_lat=(/"20","70"/)+"S"


y1=1979
y2=2016
yr  = ispan(y1,y2,1)
yr1 = ispan(1,y2-y1+1,1)

data=new((/dimsizes(ssn),y2-y1+1,dimsizes(varn)/),float)
data@_FillValue = -9999.0
listyr=data(0,:,0)
pltarry=new((/2,dimsizes(data(0,:,0))/),typeof(data),data@_FillValue)


 do iv = 0,dimsizes(var)-1
  print("iv="+var(iv))

 ;fout = "../output/bandstat/"+var(iv)+"."+d+lev+"."+ssn(is)+y1+"-"+y2+"."+reg
 fout = "../output/bandstat/"+var(iv)+"."+d+lev+"."+y1+"-"+y2+"."+reg(nr)

 ; ***  Plotting  *******************************************************************
   wks  = gsn_open_wks("ps",fout)

   res                     = True
   res@gsnDraw             = False           ; don't draw
   res@gsnFrame            = False           ; don't advance frame
   ;res@tiMainString       = "Ray Path"
   ;res@tiMainFontHeightF  = 0.013
   ;res@gsnCenterString    = "("+lat(loc)+"N; "+lon(loc)+"E)      period = "+p+"days     root = "+root
   ;res@gsnCenterStringFontHeightF = 0.008

   data=data@_FillValue

  do is  = 0,3
   do nlat = 0,dimsizes(latrange)-1

    lat=(/latrange(nlat),latrange(nlat)+5/)
    print(lat(0)+" - "+lat(1))


; *** read data ******************************************************************

      fin = "../output/bandstat/"+d+lev+"."+ssn(is)+y1+"-"+y2+"."+reg(nr)+"."+lat(0)+"-"+lat(1)+".dat"
      if (fileexists(fin)) then
       print("Data from "+fin)
       getvar = readAsciiTable (fin,5,"float",0)
       getvar@_FillValue = data@_FillValue

       listyr(:dimsizes(getvar(:,1))-1) = getvar(:,0)
       data(is,:(dimsizes(getvar(:,varn(iv)))-1),iv) = getvar(:,varn(iv))
       do i=0,dimsizes(getvar(:,varn(iv)))-1
        if(isnan_ieee(data(is,i,iv))) then
         data(is,i,iv)=data@_FillValue
        end if
       end do
       if(is.eq.2.or.is.eq.3)then
        data(is,ind(listyr.eq.2016),iv) = data@_FillValue
       end if
       ;print(data(is,:(dimsizes(getvar(:,varn(iv)))-1),iv))

       delete(getvar)

      end if  ; if filein exist

       ;print(listyr)
       ;print(data(is,:,iv))

;*** XY plot *************************************************************************

; regression
      rc = regline(yr1, data(is,:,iv))
      rc@units = "NPF/yr"
      pltarry(0,:)=data(is,:,iv)
      pltarry(1,:)=rc*((yr1)-rc@xave)+rc@yave

      ;res@tiMainString = var_title(iv)+"  "+lat(0)+" - "+lat(1)+ssn(is)+" "+y1+" - "+y2
      res@gsnLeftString = ssn(is)
      res@gsnRightString    = "("+lat(0)+" - "+lat(1)+"; "+reg_lon(0)+" - "+reg_lon(1)+")"
      res@gsnStringFontHeightF = .03

      print("create plot"+ssn(is)+" "+reg(nr)+" "+lat(0)+"-"+lat(1)+" "+var(iv))
      ;print(pltarry)
      ;print(rc)
      ;delete(rc)
      plot(nlat,is) = gsn_csm_xy(wks,yr,pltarry,res)

;---Drawing the plot will draw the attached polylines and markers.
;      draw(plot(nlat,is))
;      frame(wks)

   end do  ;lat
  end do ; ssn

;************************************************
; create panel
;************************************************
  resP                     = True

  resP@txString = var_title(iv)+ "  "+reg(nr)
  ;for ncl >= 6.4
  ;resP@gsnPanelMainString = var_title(iv)+ "  "+reg(nr)

  resP@gsnMaximize      = True

  print("create panel plot")
   gsn_panel(wks,ndtooned(plot),dimsizes(plot),resP)

 end do  ;var
end do ; reg

end
