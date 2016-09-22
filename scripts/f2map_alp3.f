        module grid
	real,parameter:: grad=111.2263,rs1=10.,rs2=2.5
	integer,parameter:: nlon=240,nlat=61,maxN=10000
	real glon(nlon+1),glat(nlat)

	real, dimension(nlon,nlat):: sw
	real svw(nlon,nlat)
	integer nk(nlon,nlat)

	end module grid


        program f2map
	use grid
	
 	implicit none
        include '/usr/local/include/netcdf.inc'


	character*100 fin,Din,Dintrk,ftrk,fout,fcdl,fnc,HS*2
	integer	sy,ey,sm,em,sd,ed,y,m,d,scy,scm,scd,sch,osy,osm,osd
	integer,dimension(1000):: cy,cm,cd,ch,k
	real,dimension(1000):: lon,lat,flen,mdv,sdv,nrd 

	real vout(nlon,nlat)
	real r
        real*8 dist

	integer ciy,ociy,cim,cid,cih,iy,iiy,id,im,ih,in	
	real rd
	real,dimension(2500,100,1000):: flon,flat,alp
	integer ird,nn,ii,ij,tt,ntt
	
        real w,ny
	integer i,j,n,num,v

	character*80 fmt

!netcdf	
      integer retval,ncid
      integer lon_dimid,lat_dimid,time_dimid
      integer lon_varid,lat_varid,var_varid

C     We will create two netCDF variables, one each for temperature and
C     pressure fields.
      character*(*) VAR_NAME
      parameter (VAR_NAME='alpha')
      integer,parameter:: ndims=3
      integer rec,dimids(ndims), count(ndims),start(ndims)

C     We recommend that each variable carry a "units" attribute.
      character*(*) UNITS
      parameter (UNITS = 'units')
      character*(*) VAR_UNITS, LAT_UNITS, LON_UNITS
      parameter (VAR_UNITS = 'degrees')
      parameter (LAT_UNITS = 'degrees_north')
      parameter (LON_UNITS = 'degrees_east')
!end netcdf	

	vout(1,1)=1.    !no reason for this line

	read(*,'(a100)') Din
	read(*,'(a100)') Dintrk
	read(*,'(a)') HS
	read(*,'(i4,2i2)') sy,sm,sd
	read(*,'(i4,2i2)') ey,em,ed
	read(*,'(a)') fout
	read(*,'(a)') fcdl
	osy=sy;osm=sm;osd=sd

	if (sm.ge.11)then
	 sy=sy+1
	 sm=sm-13
        endif
	ny=float(ey-sy+1)
!--------------------------------------------------------------
!creat lon-lat grid

	do j=1,nlat
!for NH
	if(HS=='0N')then
	 glat(j)=90.-1.5*(j-1)
!for SH
	elseif(HS=='0S')then
	 glat(j)=-90.+1.5*(j-1)
	else
	 print*, "ERROR: hemisphere???"
	endif
	enddo

	do i=1,nlon+1
	 glon(i)=1.5*(i-1)
	enddo

	write(*,'("Grid done")')
	nk=0
	svw=0.;sw=0.
!--------------------------------------------------------------

	do 1 y=sy,ey;print*, y
	write(ftrk,'(a,i4,".",a,".dat")') trim(Dintrk),y,HS
	open(11,file=ftrk,action='read')
	print*,ftrk
	
	do
        
         read(11,*,end=333)
	 read(11,'(11x,3i2,i5,28x,i4)')iy,im,id,ih,in
	 if(y.lt.1999)then
	  iiy=iy+1900
	 elseif(y.ge.1999.and.y.le.2001)then
	  iiy=iy+1980
	 else
	  iiy=iy+2000
	 endif

	 call convtime(iiy,im,id,ih,tt,osy,osm,osd)
	
	 read(11,*)
	 read(11,*)
	 read(11,*)
	
        do ii=1,in
	 read(11,'(i3,43x,f8.3)')nn,rd

         if(nn/=ii)then
	  write(*,'("!ERROR: ii.ne.nn")')
	  stop
	 endif
 
	 ird=int(rd)
	 do ij=1,ird
	 if(tt>0)then
	  read(11,'(14x,2f8.2,9x,f9.3)')
     &         flat(tt,ii,ij),flon(tt,ii,ij),alp(tt,ii,ij)
	 else
	  read(11,*)
	 endif
	 enddo
	 enddo
	 enddo

333     close (11)
!----------------------------------------------------------------
	write(fin,'(a,i4,".",a,".trk")') trim(Din),y,HS
	open(10,file=fin,action='read')
	print*,HS,sy,y, fin

	do 
	read(10,*, end=999)

	read(10,'(62x,i4,i5,2i2,i4)')num,scy,scm,scd,sch
	if(scy==y-1)then
	 scm=scm-13
	 scy=y	
	elseif(scy==y+1)then
	 scm=scm+12
	 scy=y
	elseif(scy/=y)then
	 write(*,'("Error: check years!")')
	endif

	read(10,*)
	read(10,*)
	do 4 i=1,num
	read(10,1000) cy(i),cm(i),cd(i),ch(i),k(i),lon(i),lat(i),flen(i)
     &               ,mdv(i),sdv(i),nrd(i)
1000	format(12x,i4,2i2,i5,5x,i5,11x,6f9.3)
4	enddo
	print*, num,scy,scm,scd,sch


	do 2 n=1,num;

	ociy=cy(n)
	if(y<1999)then
	ciy=cy(n)-1900
	elseif(y.ge.1999.and.y<=2000)then
	ciy=cy(n)-1980
	else
	ciy=cy(n)-2000
	endif

	cim=cm(n)
 	cid=cd(n)
	cih=ch(n)
	
	if(cy(n)==y-1)then
	 cm(n)=cm(n)-13
	 cy(n)=y	
	elseif(cy(n)==y+1)then
	 cm(n)=cm(n)+12
	 cy(n)=y
	elseif(cy(n)/=y)then
	 write(*,'("Error: check years!",3i4)')cy(n),cm(n),cd(n)
	 print*, lon(n),lat(n)
	endif


	if(cm(n)<=em.and.cm(n)>=sm)then
	 if((cm(n)==sm.and.cd(n)>=sd)
     &      .or.(cm(n)==em.and.cd(n)<=ed))then
 !    &      .or.any([sm,ed]/=cm(n)))then
 !        if(n==1)    print*,scy,scm,scd,sch,num	

	call convtime(ociy,cim,cid,cih,ntt,osy,osm,osd)
	if(ntt<1)then
	 write(*,'("!!!!CHECK ntt")')
	 stop
	endif
 
	 
	   ird=int(nrd(n))
           call dat2map(ird,flon(ntt,k(n),1:ird),flat(ntt,k(n),1:ird)
     &                  ,alp(ntt,k(n),1:ird),-999.999)

	endif  !cyclones within days
	endif  !cyclone within months
2	enddo  !tracks
!	endif  !cyclones start within days
!	endif  !cyclone start within months

	enddo

999	close(10)
    1   continue


	print*, "start writing out"
!	open(20,file=fout,action='write')
	 do j=1,nlat
	 do i=1,nlon
	 if(sw(i,j)>0.)then
!	  write(20,1001)glon(i),glat(j),svw(i,j)/sw(i,j),nk(i,j)
	  vout(i,j)=svw(i,j)/sw(i,j)
	 else
!	  write(20,1001)glon(i),glat(j),-999.9,nk(i,j)
	  vout(i,j)=-999.9
	 endif
	 enddo
	 enddo
1001     format (2f10.3,f10.3,i5)



c     -----------------------------------------------------------------
c   Create the CDL file that matches the preceding example,
c       writing the result to file `my.cdl'
c     -----------------------------------------------------------------
c
      open(unit=7, file=fcdl)
c
c       Write the netCDF file name
c
      write(7,*) 'netcdf mine{'
c
c       Write the dimensions
c
      write(7,*) 'dimensions:'
      write(7,*) 'lon=', nlon, ';'
      write(7,*) 'lat=', nlat, ';'
      write(7,*) 'time=UNLIMITED;'
c
c       Write the variable declarations along with
c       their attributes
c
      write(7,*) 'variables:'
      write(7,*) 'float lon(lon);'
      write(7,*) 'lon:units= "degrees_east";'
      write(7,*) 'float lat(lat);'
      write(7,*) 'lat:units= "degrees_north";'
      write(7,*) 'float alpha(time,lat,lon);'
      write(7,*) 'alpha:long_name= "alpha";'
      write(7,*) 'alpha:units= "degrees";'
      write(7,*) 'alpha:missing_value= -999.9f ;'
!90    format(1x,2x,A,':missing_value = ',F7.1,'f ;') ! Note 'f' KK

c
c       Write the global attributes
c
!      write(7,*) ':x_min=-180.f;'
!      write(7,*) ':x_max=180.f;'
!      write(7,*) ':x_units="degrees_east";'
!      write(7,*) ':x_label="longitude";'
!      write(7,*) ':y_min=-90.f;'
!      write(7,*) ':y_max=90.f;'
!      write(7,*) ':y_units="degrees_north";'
!      write(7,*) ':y_label="latitude";'
!      write(7,*) ':z_label="level";'
!      write(7,*) ':t_label="time";'
c
c       Begin writing the data
c
      write(*,*) 'data:'
      write(*,*) 'lon='
      do i = 1, nlon-1
         write(*, *) glon(i), ','
      enddo
      write(*, *) glon(nlon), ';'
      write(7,*) 'data:'
      write(7,*) 'lon='
	write(fmt,106)nlon-1
106     format('(',I3,'(E14.6,'',''),E14.6,'';'')') 
        write(7,fmt)(glon(i),i=1,nlon)
      
      write(7,*) 'lat='
      do i = 1, nlat-1
         write(7, *) glat(i), ','
      enddo
      write(7, *) glat(nlat), ';'

      write(7,*) 'alpha='
      do j=1,nlat-1
        write(fmt,103)nlon
103     format('(',I3,'(E14.6,'',''))')
        write(7,fmt)(vout(i,j),i=1,nlon)
      enddo
      
	write(fmt,106)nlon-1
	print*, fmt
        write(7,fmt)(vout(i,nlat),i=1,nlon)

      write(7,*)'}'

      close (unit=7)

	stop  !skip netcdf writing

!write netcdf
      retval = nf_create(fnc, NF_CLOBBER, ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)
	print*, ncid

C     Define the dimensions. NetCDF will hand back an ID for each.
      retval = nf_def_dim(ncid, "lon", nlon, lon_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_dim(ncid, "lat", nlat, lat_dimid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_dim(ncid, "time", NF_UNLIMITED, time_dimid)

      retval = nf_def_var(ncid, "lon", NF_FLOAT, 1, lon_dimid,
     +     lon_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_def_var(ncid, "lat", NF_DOUBLE, 1, lat_dimid,
     +     lat_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)


C     Assign units attributes to coordinate variables.
      retval = nf_put_att_text(ncid, lat_varid, UNITS, len(LAT_UNITS),
     +     LAT_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_put_att_text(ncid, lon_varid, UNITS, len(LON_UNITS),
     +     LON_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     The dimids array is used to pass the dimids of the dimensions of
C     the netCDF variables. Both of the netCDF variables we are creating
C     share the same four dimensions. In Fortran, the unlimited
C     dimension must come last on the list of dimids.
      dimids(1) = lon_dimid
      dimids(2) = lat_dimid
      dimids(3) = time_dimid

C     Define the netCDF variables for the pressure and temperature data.
      retval = nf_def_var(ncid, VAR_NAME, NF_REAL, NDIMS, dimids,
     +     var_varid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Assign units attributes to the netCDF variables.
      retval = nf_put_att_text(ncid, var_varid, UNITS, len(VAR_UNITS),
     +     VAR_UNITS)
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = nf_put_att_text(ncid, var_varid, "lon_name",
     +     5,"alpha")
      retval = nf_put_att_text(ncid, var_varid, "missing_value",
     +     NF_FLOAT,1,-999.9)
      if (retval .ne. nf_noerr) call handle_err(retval)
	print*, retval

C     End define mode.
      retval = nf_enddef(ncid)
      if (retval .ne. nf_noerr) call handle_err(retval)

C     Write the coordinate variable data. This will put the latitudes
C     and longitudes of our data grid into the netCDF file.
!      call ncvput(ncid, lat_varid,1,181, glat,retval)
      retval = nf_put_vara_double(ncid, lat_varid,1,181, glat)
	print*, retval,ncid,lat_varid,glat
      if (retval .ne. nf_noerr) call handle_err(retval)
      retval = NF_PUT_VAR_real(ncid, lon_varid,1,nlon
     &      ,glon(1:nlon))
	print*, retval,ncid,lon_varid,glon
       if (retval .ne. nf_noerr) call handle_err(retval)

C     These settings tell netcdf to write one timestep of data. (The
C     setting of start(4) inside the loop below tells netCDF which
C     timestep to write.)
       count(1) = NLON
       count(2) = NLAT
       count(3) = 1
       start(1) = 1
       start(2) = 1
       start(3) = 1

C     Write the pretend data. This will write our surface pressure and
C     surface temperature data. The arrays only hold one timestep worth
C     of data. We will just rewrite the same data for each timestep. In
C     a real appli1ation, the data would change between timesteps.
       do rec = 1, 1
         retval = nf_put_var_real(ncid, var_varid, start, count
     &                          , vout)
         if (retval .ne. nf_noerr) call handle_err(retval)
       end do


C     Close the file. This causes netCDF to flush all buffers and make
C     sure your data are really written to disk.
!      retval = nf_close(ncid)
!      if (retval .ne. nf_noerr) call handle_err(retval)

      print *,'*** SUCCESS writing example file', fnc, '!'


!end netcdf writing

        stop

888     write(*,'("!!!ERROR: 888 stop")')
        stop
        end



!***********************************************************************
      subroutine handle_err(errcode)
      implicit none
        include '/usr/local/include/netcdf.inc'
      integer errcode

      print *, 'Error: ', nf_strerror(errcode)
      stop 2
      end


!***********************************************************************

	real(8) function dist(k,lon,lat)
	implicit none

	real,parameter::R=6372.8
	real,parameter::pi=3.14156265
	integer n,k
	real, dimension(k)::lat,lon
	real*8, dimension(k)::dlat,dlon

	dist=0.
	dlat=lat*pi/180.
	dlon=lon*pi/180.


!	print*, lon
!	print*, lat

	do n=1,k-1
	if(lon(n)==lon(n+1))then
	 dist=dist+abs(lat(n+1)-lat(n))*pi/180.
	elseif(lat(n+1)==lat(n))then
	 dist=dist+acos(cos(dlat(n))**2*(cos(dlon(n+1)-dlon(n))-1.)+1.)
        else
 	 dist=dist+acos(sin(dlat(n))*sin(dlat(n+1))+
     &         cos(dlat(n))*cos(dlat(n+1))*cos(dlon(n+1)-dlon(n)))
	endif
	enddo

	dist=R*dist  

! OR
! dist=R*acos(...) - angle in radians * earth's radius (6372.8 km)
!	dist=111.18*dist  !111.18 - length of 1deg of longitude (km)

	return
	end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	subroutine dat2map(num,lon,lat,var,miss)
	use grid
	implicit none

	integer i,j,n,num,v
	real lon(num),lat(num),var(num),r,w,miss
	real*8 dist
	real minlat,minlon
	integer ilon,ilat

	integer ii,dlon,dlat,tlon,tlat
	
	 do 1 n=1,num-1
	 if(var(n)/=miss)then

	  minlat=90.;ilat=-999
	  minlon=360.;ilon=-999
	  do j=1,nlat  	     	     
	    if (abs(lat(n)-glat(j)).lt.minlat)then
	    minlat=abs(lat(n)-glat(j)); ilat=j
	    elseif(abs(lat(n)-glat(j))>90.)then
	    print*, lat(n),glat(j),n
	    write(*,'("!!ERROR: check hemispher!")')
	    endif	  
	  enddo  
	  do i=1,nlon  	     	     
	    if (abs(lon(n)-glon(i)).lt.minlon)then
	    minlon=abs(lon(n)-glon(i)); ilon=i
	    endif
	  enddo

!	 print*, "grid loc   ",ilon,ilat
!	 print*, "grid point ",glon(ilon), glat(ilat)	
!	 print*, "front point  ",lon(n), lat(n)
	
	 dlat=0
	 loop_c: do;!print*,"dlat=", dlat
	 tlat=0
	 do 3 j=ilat-dlat,ilat+dlat
	 if(j==ilat-dlat.or.j==ilat+dlat)then

	 if (j>0.and.j<nlat)then
	 dlon=0
	 loop_b: do;!print*, "dlon=",dlon
	 tlon=0
	 do 31 ii=ilon-dlon,ilon+dlon
	 if(ii==ilon-dlon.or.ii==ilon+dlon)then	 

	 if(ii<=0)then
	  i=ii+nlon
	 elseif(ii>nlon)then
	  i=ii-nlon
	 else
	  i=ii
	 endif
	 !print*,i,j, glon(i),glat(j)
	 !print*,n, lon(n),lat(n)

	  
	  r=dist(2,[glon(i),lon(n)],[glat(j),lat(n)])
	  !print*,"r=",r 
	
	  if(r<=111.*rs1)then
	  tlon=1;tlat=1
	   !print*,"!!!!",r,i,j
           nk(i,j)=nk(i,j)+1
	    if(nk(i,j)>maxN)then
	     	write(*,'("Large number: nk(i,j)")') 
		stop
	    endif
	  r=r/grad
	  w=(rs1*rs1-r*r)/(rs1*rs1+r*r*(rs1*rs1/(rs2*rs2)-1))
	  svw(i,j)=svw(i,j)+w*var(n)
	  sw(i,j)=sw(i,j)+w
	  endif   !r<111.
	 endif
31	 enddo
	 
	  !print*, tlon 
	  if (tlon==0)exit loop_b
	  dlon=dlon+1
	  if(dlon>nlon/2)exit loop_b
	  enddo loop_b

	 elseif(j==nlat)then
         r=dist(2,[0.,lon(n)],[90.,lat(n)])

	  if(r<=111.*rs1)then
           nk(1:nlon,j)=nk(1:nlon,j)+1
	    if(nk(1,j)>maxN)then
	     	write(*,'("Large number: nk(i,j)")') 
		stop
	    endif
	  r=r/grad
	  w=(rs1*rs1-r*r)/(rs1*rs1+r*r*(rs1*rs1/(rs2*rs2)-1))
	  svw(1:nlon,j)=svw(1:nlon,j)+w*var(n)
	  sw(1:nlon,j)=sw(1:nlon,j)+w
	  endif
	 endif  !!!lat<90 and lat>equator
	 endif 
3	 enddo

	if(tlat==0)exit loop_c
	dlat=dlat+1
	if(dlat>nlat)exit loop_c
        enddo loop_c
	endif
1	 enddo

	return
	end

!----------------------------------------------------------------------


	subroutine convtime(y,m,d,h,tt,sy,sm,sd)

	integer t,tt
	integer y,m,d,h,sy,sm,sd,hour(4),cy,cm,cd,ch
	integer, dimension(12)::dm,dm_reg,dm_leap

	data hour/0,600,1200,1800/
	data dm_reg/31,28,31,30,31,30,31,31,30,31,30,31/
	data dm_leap/31,29,31,30,31,30,31,31,30,31,30,31/


	if(sy>y.or.(sy==y.and.sm>m).or.(sy==y.and.sm==m.and.sd>d))then
	 write(*,'("!!!CHECK DATES_0:")') 
	 write(*,'("Starting date ",i4,2i2)')sy,sm,sd
	 write(*,'("Current date ",i4,2i2)')y,m,d
!	 stop
	tt=-1
	return
	endif
	 


	cy=sy
	  if(mod(cy,100).ne.0.and.mod(cy,4).eq.0)then
		dm=dm_leap
	  elseif(mod(cy,400).eq.0)then
		dm=dm_leap
	  endif
	dm=dm_reg
	cm=sm
	cd=sd
	ch=1
	tt=1

	do
	if(cy==y.and.cm==m.and.cd==d.and.hour(ch)==h)then
	return
	endif

	tt=tt+1

	ch=ch+1
	if(ch>4)then
	 ch=1
	 cd=cd+1
	 if(cd>dm(cm))then
	  cd=1
	  cm=cm+1
	  if(cm>12)then
	   cm=1
	   cy=cy+1
	   dm=dm_reg
	   if(mod(cy,100).ne.0.and.mod(cy,4).eq.0)then
		dm=dm_leap
	   elseif(mod(cy,400).eq.0)then
		dm=dm_leap
	   endif
	  endif
	 endif
	endif
	
	if(tt>2500)then
	 write(*,'("!!!CHECK DATES:",i10)') tt
	 write(*,'("Starting date ",i4,2i2)')sy,sm,sd
	 write(*,'("Current date ",i4,2i2,i4)')cy,cm,cd,ch
	 write(*,'("Looking for  ",i4,2i2,i4)')y,m,d,h
	 stop
	endif

	enddo

	end
	
	


