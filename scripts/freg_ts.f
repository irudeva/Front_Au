        module grid
	       real,parameter:: grad=111.2263,rs1=10.,rs2=2.5
	       integer,parameter:: nlon=240,nlat=61,maxN=10000
	       real glon(nlon+1),glat(nlat)

	       real, dimension(nlon,nlat):: sw
	       real svw(nlon,nlat)
	       integer nk(nlon,nlat)

	      end module grid


        program freg_TS
	       use grid

      	 implicit none
c        include '/usr/local/include/netcdf.inc'

	       character*100 fin,Din,ftrk,fout,fcdl,fnc,HS*2,ssn*3
	       integer	sy,ey,sm,em,sd,ed,y,m,d,scy,scm,scd,sch,osy,osm,osd
	       integer,dimension(1000):: cy,cm,cd,ch,k
	       real,dimension(1000):: lon,lat,flen,mdv,sdv,nrd

	       real r,reglon(2),reglat(2), flngth
         real*8 dist

	       integer iy,iiy,id,im,ih,in
	       real rd
	       real,dimension(2500,100,1000):: flon,flat
	       integer ird,nn,ii,ij,tt,ntt,ett

         real w,ny
	       integer i,j,n,num,v

	       character*80 fmt


         write(*,*) 'freq_ts start'

	       read(*,'(a100)') Din
	       read(*,'(a)') HS
	       read(*,'(i4,2i2)') sy,sm,sd
	       read(*,'(i4,2i2)') ey,em,ed
         read(*,'(a)') ssn
	       read(*,'(a)') fout
	       read(*,*) reglon
	       read(*,*) reglat
! Some info
         write(*,*) "Start date", sd,"/",sm,"/",sy
         write(*,*) 'End date', ed,'/',em,'/',ey
         write(*,*) 'Region: lon ',  reglon
         write(*,*) 'Region: lat ',  reglat
         write(*,*) 'Output: ', fout

         osy=sy;osm=sm;osd=sd

	       if (sm.ge.11)then
	        sy=sy+1
	        sm=sm-13
         endif
	       ny=float(ey-sy+1)

         call convtime(ey,em,ed,1800,ett,osy,osm,osd)


!-------------------------------------------------------
	       do 1 y=sy,ey
          write(ftrk,'(a,i4,".",a,".dat")') trim(Din),y,HS
	        open(11,file=ftrk,action='read')
	        write(*,'(" Input: ",a)')ftrk

          do
           read(11,*,end=333)
           read(11,'(11x,3i2,i5,28x,i4)')iy,im,id,ih,in
           print *, iy,im,id,ih

           if(y.lt.1999)then
            iiy=iy+1900
           elseif(y.ge.1999.and.y.le.2001)then
            iy=iy+1980
           else
            iiy=iy+2000
           endif

	         call convtime(iiy,im,id,ih,tt,osy,osm,osd)
           print*, tt

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
             if(tt>0.and.tt.le.ett)then
              read(11,'(14x,2f8.2,9x,f9.3)')
     &         flat(tt,ii,ij),flon(tt,ii,ij)
             else
              read(11,*)
             endif

             if(tt>0.and.tt.le.ett)then
                flngth=
     &         real(dist(ird,flon(tt,ii,1:ird),flat(tt,ii,1:ird)))
                print*, rd, flngth
                print*, flat(tt,ii,1:ird)
             end if
            end do
           end do
          end do

333       close (11)

1	       end do

       write(*,*) 'Output: ', fout
       open(21, file=fout,action='write')
       write(21,'(a," ",i4, "  read")')ssn,ey
       close(21)

	     end


       !----------------------------------------------------------------------


       	subroutine convtime(y,m,d,h,tt,sy,sm,sd)

       	integer t,tt
       	integer y,m,d,h,sy,sm,sd,hour(4),cy,cm,cd,ch
       	integer, dimension(12)::dm,dm_reg,dm_leap

       	data hour/0,600,1200,1800/
       	data dm_reg/31,28,31,30,31,30,31,31,30,31,30,31/

        dm_leap = dm_reg
        dm_leap(2)=29
       	!data dm_leap/31,29,31,30,31,30,31,31,30,31,30,31/


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
        	 dist=dist+
     &      acos(cos(dlat(n))**2*(cos(dlon(n+1)-dlon(n))-1.)+1.)
                else
         	 dist=dist+
     &      acos(sin(dlat(n))*sin(dlat(n+1))+
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
