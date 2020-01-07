C     Time subroutines.  Time is generally encoded as a two-word quantity.
C        1 integer yyyyddd - 4 digit year and 3 digit day of year
C        1 real seconds - seconds into day from midnight
C
C     By George Helffrich, U. Bristol, 1994-2006

C     timdif  --  Return difference in seconds between two string-encoded
C                 days.
C     Called via:
C        dif = timdif(timend, timbeg)
C
C     Assumes:
C        timend, timbeg - SEED variable length strings encoding at time.
C
C     Returns:
C        dif - difference between values, in seconds.
      real*8 function timdif(timend,timbeg)
      character timend*(*), timbeg*(*)
      integer daybeg, dayend
      real*8 secbeg, secend, tdsdif

      call timcvt(daybeg,secbeg,timbeg)
      call timcvt(dayend,secend,timend)
      timdif = tdsdif(dayend,secend,daybeg,secbeg)
      end

C     tdsdif  --  Return difference in seconds between to canonically
C                 encoded times.
C
C     Called via:
C        dif = tdsdif(endyd, endsec, begyd, begsec)
C
C     Assumes:
C        endyd - ending year and day, yyyyddd
C        endsec - ending second within day
C        begyd - beginning year and day, yyyyddd
C        begsec - beginning second with day
C
C     Returns:
C        dif - difference between times in seconds.
      real*8 function tdsdif(endyd, endsec, begyd, begsec)
      integer endyd, begyd, daybeg, dayend
      real*8 endsec, begsec, secdif
      parameter (secday = 60*60*24)

      daybeg = julday(1,0,begyd/1000) + mod(begyd,1000)
      dayend = julday(1,0,endyd/1000) + mod(endyd,1000)
      secdif = endsec - begsec
      if (secdif .lt. 0) then
	 secdif = secdif + secday
	 dayend = dayend-1
      endif
      tdsdif = dble(dayend-daybeg)*secday + secdif
      end

C     timcvt  --  Convert SEED time string to canonical format.
C
C     Called via:
C        call timcvt(day, sec, timstr)
C
C     Assumes:
C        timstr - SEED variable length time string
C
C     Returns:
C        day - integer value encoding yyyyddd
C        sec - real value encoding seconds since midnight within the day
      subroutine timcvt(day, sec, timstr)
      integer day
      real*8 sec
      character timstr*(*)
      character str*22

C     Pad string to maximum length, then overlay as much of the string
C        as we were given.
      str = '0001,001,00:00:00.0000'
      l = index(timstr,'~')
      if (l .eq. 0) l = min(len(timstr),len(str))+1
      str(1:l-1) = timstr
      read(str,'(i4,1x,i3,1x,2(i2,1x),f7.4)',iostat=ios)
     &   iyy,idd,ihh,imm,rsec
      if (ios .ne. 0) then
	 if (str(18:18) .eq. ':') then
	    read(str,'(i4,1x,i3,1x,2(i2,1x),i2,1x,i4)',iostat=ios)
     &         iyy,idd,ihh,imm,isec,ith
	    if (ios .eq. 0) then
	       rsec=isec+1e-4*ith
	    endif
	 endif
      endif
      if (str .eq. ' ') then
         iyy = 2500
	 idd = 365
	 ios = 0
      endif
      if (ios .ne. 0) pause '**TIMCVT:  Invalid time!'
      day = iyy*1000 + idd
      sec = dble(rsec) + dble(60*(imm + 60*ihh))
      end

C     getday  --  Given year and day number in year, return month and day
C                 of month.
C
C     Called via:
C        call getday(iyr,jday,imon,idy)
C
C     Assumes:
C        iyr - year (19xx)
C        jday - day number in year (jan 1 is 001)
C
C     Returns:
C        imon - month (1-12)
C        idy - day number

      subroutine getday(iyr,jday,imon,idy)
      iystrt = julday(1,0,iyr)
      do 10 i=1,11
	 imostrt = julday(i+1,1,iyr)
	 if (imostrt - iystrt .gt. jday) go to 15
10    continue
15    continue
      imon = i
      idy = jday + iystrt - julday(imon,0,iyr)
      end

C     julday -- Return julian day number given month, day year.
C
      FUNCTION JULDAY(MM,ID,IYYY)
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
