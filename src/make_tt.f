C     Make travel time table suitable for CDSEIS using the IASPEI91 travel
C     time model.
C
C     This program calls a subroutine to return travel times for generic
C     phases to build up a table of travel times that CDSEIS then queries to
C     determine whether the phase arrival time is in the available data
C     interval.
C
C     -model x - To indicate specific model wanted
C
C     G Helffrich/Carnegie/DTM 2 Mar 1992
C     G Helffrich/U. Bristol/18 Feb. 1999 - modified to include diffraction
C            extensions of main phases

      parameter (ntdeep = 15, ntt=50)
      real tttab(ntdeep), tt(ntt), dtdh(ntt), dtdd(ntt)
      character phgen*8, phspec*8, phid(ntt)*8, arg*32, tables*32
      logical first, odiff, omin

      data first /.true./, tables/' '/, odiff/.true./, omin/.false./

      iskip = 0
      do 3 i=1,iargc()
	 if (i .le. iskip) go to 3
	 call getarg(i,arg)
	 if (arg .eq. '-model') then
	    call getarg(i+1,tables)
	    iskip = i+1
	 else if (arg .eq. '-nodiff') then
	    odiff = .false.
	 else if (arg .eq. '-nomaj') then
	    omin = .true.
	 else
	    write(0,*) '**Don''t understand "',arg(1:lenb(arg)),
     +         '", skipping.'
	 endif
3     continue
      if (tables .ne. ' ') call tpmod(tables)

      write(0,*) 'Enter generic phase name and specific phase ',
     +   'e.g., PKP PKPdf'
      read(5,*,end=900,err=900) phgen,phspec
      write(0,*) 'Enter first depth, last depth, increment:'
      read(5,*,end=900,err=900) h0, hn, hinc
      write(0,*) 'Enter first distance, last distance, increment:'
      read(5,*,end=900,err=900) d0, dn, dinc
      lph = lenb(phspec)

      do 15 delta = d0,dn,dinc
	 k = 0
	 do 10 depth = h0,hn,hinc
	    k = k + 1
	    if (k .gt. ntdeep) stop '**Too many distances given.'
	    n = mtptt(phgen,delta,depth,ntt,phid,tt,dtdd,dtdh)
	    tttab(k) = 0.0
	    do 5 i=1,n
	       if (phid(i) .eq. phspec .or.
     +               (phid(i)(1:lph)  .eq. phspec(1:lph) .and.
     +                phid(i)(lph+1:lph+3) .eq. 'dif' .and. odiff)
     +            ) then
                  if (dtdd(i).gt.0.0 .or. .not.omin) then
		     tttab(k) = tt(i)/60.0
		     go to 10
		  endif
	       endif
5           continue
10       continue

C        Write a line of the table.  If first line, write header too.
	 if (first) then
	    write(6,*) phspec(1:index(phspec,' ')),' travel time table.'
	    write(6,'(2i4)') 1+ifix((dn-d0)/dinc),1+ifix((hn-h0)/hinc)
	    write(6,990) (depth,depth=h0,hn,hinc)
	    first = .false.
	 endif
	 write(6,991) delta,(tttab(i),i=1,k)
15    continue

990   format(8x,20(1x,f8.3))
991   format(21(f8.3,1x))

900   continue
      end

      integer function lenb(string)
c     finds index of last nonblank character in string
      character*(*) string
      integer i,n
      n=len(string)
      do 10 i=n,1,-1
      if(string(i:i).ne.' ')then
        lenb=i
        return
      endif
   10 continue
      lenb=0
      return
      end
