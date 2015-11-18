C     Program to check out station info (lat, lon).  As input, give
C     either:
C        4 character station ids
C     or
C        integer station number, the GDSN station number.
C     Give these 1 per line.  Prints lat, lon, station type
C     listing as result.

      character sta*4, org*4, stacd*4
      integer id*2
      logical first

      first = .true.
10    continue
         read(5,'(a4)',end=999,err=999) sta
	 if (first) write(6,1001)
	 read(sta,*,iostat=ios) id
	 if (ios .eq. 0) then
C           Assume a GDSN station number.
	    call srosta2(id,stacd,org,ierr)
	    if (ierr .eq. 0) go to 15
	    write(6,1003) '?',id,' isn''t a valid GDSN station number.'
	 else
C           Assume a station name, just call STALOC
            id = 0
            stacd = sta
15          continue
	    call staloc(stacd,th,ph,ierr)
	    if (ierr .ne. 0) then
	       write(6,1003) stacd,id,' location isn''t known by STALOC.'
	    else
	       write(6,1004) stacd,id,90.0-th,ph,360.0-ph
	    endif
	 endif
	 first = .false.
      go to 10
999   continue

1001  format(1x,'ID   GDSN#   Lat.     Lon.E   Lon.W')
1003  format(1x,a4,1x,i4,1x,a)
1004  format(1x,a4,1x,i4,1x,f9.5,2(1x,f10.5))
      end
