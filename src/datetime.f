c Time computation subroutines.  Rewritten by GRH/DTM 12 May 91.
c GETTDAY obtains the total number of days since 0 Jan 80
c from year, month, day input information
c Example: 81,3,13  gives  438
c Notes:  idy can be in Julian days if imon is set to 1 
c         itday can be negative
      subroutine GETTDAY(iyr,imon,idy,itday)
      integer iyr,imon,idy,itday
      itday = julday(imon,idy,iyr) - julday(1,0,1980)
      return
      end
c
c
c TIMEDIF finds station time minus quake time (minutes)
      subroutine TIMEDIF(eqyr,eqmon,eqdy,eqhr,eqmn,eqsc,
     &                   styr,stmon,stdy,sthr,stmn,stsc,timdif)
      integer eqyr,eqmon,eqdy,eqhr,eqmn,styr,stmon,stdy,sthr,stmn
      integer ieqtday, isttday
      real eqsc,stsc,timdif
      double precision dt
      call GETTDAY(eqyr,eqmon,eqdy,ieqtday)
      call GETTDAY(styr,stmon,stdy,isttday)
      dt=60.d0*dble(sthr-eqhr)+dble(stmn-eqmn)+
     &  (dble(stsc)-dble(eqsc))/60.d0
      timdif=dt+1440.d0*dble(isttday-ieqtday)
      return
      end
c
c
c DIFTIME finds time differences in seconds (double precision) for SEED BTIMEs
      double precision function DIFTIME(eq,st)
      integer eq(6), st(6)
      integer eqyr,eqjdy,eqhr,eqmn,eqsc,eqth
      integer styr,stjdy,sthr,stmn,stsc,stth
      integer ieqtday, isttday
      call GETTDAY(eq(1),1,eq(2),ieqtday)
      call GETTDAY(st(1),1,st(2),isttday)
      diftime=
     &   86400.d0*dble(isttday-ieqtday) +
     &    3600.d0*dble(st(3)-eq(3)) +
     &      60.d0*dble(st(4)-eq(4)) +
     &      dble(st(5))-dble(eq(5)) + 
     &      1d-4*(st(6)-eq(6))
      end
c
c
c ADDTIME adds time (in minutes) to yr,mn,dy,hr,mn,sc time
      subroutine ADDTIME(qyr,qmon,qdy,qhr,qmn,qsc,toff,
     &                   syr,smon,sdy,shr,smn,ssc)
      integer qyr,qmon,qdy,qhr,qmn,syr,smon,sdy,shr,smn
      syr=qyr
      smon=qmon
      sdy=qdy
      shr=qhr
      smn=qmn      
      ssc=qsc+toff*60.
      call CLEANTIME(syr,smon,sdy,shr,smn,ssc)
      return
      end
c
c
c CLEANTIME cleans up oddball yr,mn,dy,hr,mn,sc times
      subroutine CLEANTIME(yr,mon,dy,hr,mn,sc)
      integer yr,mon,dy,hr,mn,dmn,dhr,ddy
c
      dmn=int(sc/60.)
      sc=sc-60.*float(dmn)
      if (sc.lt.0.) then
         sc=sc+60.
         dmn=dmn-1
      end if
      mn=mn+dmn
c      
      dhr=mn/60
      mn=mn-60*dhr
      if (mn.lt.0) then
         mn=mn+60
         dhr=dhr-1
      end if
      hr=hr+dhr
c
      ddy=hr/24
      hr=hr-24*ddy
      if (hr.lt.0) then
         hr=hr+24
         ddy=ddy-1
      end if
c
      call GETTDAY(yr,mon,dy,itday)
      itday=itday+ddy
      call GETYMD(itday,yr,mon,dy)
c
      return
      end
c
c
c GETJDAY obtains the Julian day from year, month, day input information
c Example: 81,3,13  gives 72
      subroutine GETJDAY(iyr,imon,idy,jday)
      integer iyr,imon,idy,jday
      jday = julday(imon,idy,iyr) - julday(1,0,iyr)
      return
      end
c
c
c GETDAY obtains the month and day from Julian day and year input information
c Example: 81,72, gives 3,13 
      subroutine GETDAY(iyr,jday,imon,idy)
      integer iyr,jday,imon,idy,iystrt,julday,i,imostrt
      iystrt = julday(1,0,iyr)
      do 10 i=1,11
	 imostrt = julday(i+1,1,iyr)
	 if (imostrt - iystrt .gt. jday) go to 15
10    continue
15    continue
      imon = i
      idy = jday + iystrt - julday(imon,0,iyr)
      return
      end

c      
c
c GETYMD obtains year,month,day from total days
c Example:  438  gives  81,3,13      
      subroutine GETYMD(itday,iyr,imon,idy)
      integer itday,iyr,imon,idy,jday,julday,iend,iinc,jyr,jybeg
      jday = itday + julday(1,0,1980)
      if (itday .le. 0) then
C        No seismograms prior to first seismograph.
	 iend = 1896
	 iinc = -1
      else
C        Seems like forever to me, but software has a habit of hanging around.
	 iend = 2500
	 iinc = 1
      endif
      do 10 jyr=1980,iend,iinc
	 jybeg = julday(1,0,jyr)
	 if (iinc*(jday-jybeg) .le. 0) go to 15
10    continue
      write(0,*) ' **GETYMD: Called with ',itday,iyr,iend,iinc
      pause ' **GETYMD: Can''t find year!'
15    continue
      iyr = jyr-(iinc+1)/2
      if (jday .eq. jybeg .and. itday .le. 0) iyr = iyr - 1
      call getday(iyr,jday-julday(1,0,iyr),imon,idy)
      return
      end

      INTEGER FUNCTION JULDAY(MM,ID,IYYY)
C     Return Julian day number given MM DD YY.
      INTEGER MM,ID,IYYY,IGREG,JY,JM,JA
      PARAMETER (IGREG=15+31*(10+12*1582))
      IF (IYYY.LT.0) IYYY=IYYY+1
      IF (MM.GT.2) THEN
        JY=IYYY
        JM=MM+1
      ELSE
        JY=IYYY-1
        JM=MM+13
      ENDIF
      JULDAY=INT(365.25*JY)+INT(30.6001*JM)+ID+1720995
      IF (ID+31*(MM+12*IYYY).GE.IGREG) THEN
        JA=INT(0.01*JY)
        JULDAY=JULDAY+2-JA+INT(0.25*JA)
      ENDIF
      RETURN
      END

