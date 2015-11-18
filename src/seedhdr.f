C     Seed reading utility subroutines
C        G. Helffrich/U. Bristol

      function seedhd(sta,cdfil)
      character sta*(*),cdfil*(*)

      include 'seed.io.com'
      include 'seed.sta.com'
      integer getbkt, getvol

      character bkette*(MAXBL),bkid*3
      equivalence (bkette(1:3),bkid)
      integer pos(2), possta(2)
      integer bklen, diadd, gtstr, rdadd
      character timbeg*23, timend*23, vloc*2, vchan*3, spname*5
      character v20*21, v30*31, v50*51, v60*61, v70*71
      character effbeg*23, effend*23, tkeys*6
      integer tnch,tnid

      logical valid

C     Read volume header to determine time span of recorded data.  Save
C     for bounds checking against hypocentral information.
      sn = 0
      if (getvol(cdfil) .ne. 0) then
	 write(0,*) '**SEEDHD:  Unable to open CD SEED file ',
     &      cdfil(1:lenb(cdfil)),'.'
	 return
      endif

      call posset(pos,1,8)
      bklen = getbkt(pos,bkette)
      if (bklen .le. 0) then
	 write(0,*) '**SEEDHD:  Can''t read vol. hdr.'
	 seedhd = -1
	 return
      endif
      j = gtstr(timbeg,bkette(14:))
      j = gtstr(timend,bkette(14+j:))

C     Read in station header index
10    continue
	 call posadd(pos,bklen)
	 bklen = getbkt(pos,bkette)
	 if (bkid .ne. '011') go to 19
	 read(bkette(8:10),'(i3)',iostat=ios) nsta
	 if (ios .ne. 0) go to 11
	 j = 11
	 do 15 i=1,nsta
	    read(bkette(j:j+10),'(a5,i6)',iostat=ios) spname,k
	    if (ios .ne. 0) go to 11
	    j = j + 11
	    if (sta .eq. spname) then
	       sn = sn + 1
	       if (sn .ge. MAXSTA) then
		  write(0,*) '**SEEDHD:  No space for station info.'
		  seedhd = -2
		  return
	       endif
	       sname(sn) = spname
	       shdr(sn) = k
	    endif
15       continue
      go to 10

11    continue
      write(0,*) '**SEEDHD:  Corrupted 011 block.'
      seedhd = -1
      return

C     Read in dictionary information
19    continue
      if (sn .le. 0) return

      call diinit
20    continue
	 call posadd(pos,bklen)
	 bklen = getbkt(pos,bkette)
	 if (bkid .eq. '012') then
	    continue
	 else if (bkid .eq. '030') then
	    continue
	 else if (bkid .eq. '031') then
90311       format(3x,4x,i4,1x,a71)
	    read(bkette,90311,iostat=ios) k,v70
	    if (ios .ne. 0) go to 25
	    v70(71:71) = '~'
	    ln = diadd(ncd,ixcd,lncd,kycd,k,v70)
	 else if (bkid .eq. '032') then
	    continue
	 else if (bkid .eq. '033') then
90331       format(3x,4x,i3,a51)
	    read(bkette,90331,iostat=ios) k,v50
	    if (ios .ne. 0) go to 25
	    v50(51:51) = '~'
	    ln = diadd(nga,ixga,lnga,kyga,k,v50)
	 else if (bkid .eq. '034') then
90341       format(3x,4x,i3,a21)
	    read(bkette,90341,iostat=ios) k,v20
	    if (ios .ne. 0) go to 25
	    v20(21:21) = '~'
	    ln = diadd(nua,ixua,lnua,kyua,k,v20)
	 else if (bkid .eq. '035') then
	    continue
	 else if (bkid .eq. '041') then
	    read(bkette(8:),'(i4)',iostat=ios) ikey
	    if (ios .ne. 0) go to 26
	    if (0 .eq. rdadd(ikey,041,0)) go to 27
	 else if (bkid .eq. '043') then
90431       format(1x,3x,3x,f12.0,f12.0,i3)
90432       format(30(2f12.0,12x,12x))
C           043 - Poles & Zeroes.
C           Layout is: 1- 043; 2- A0; 3- Freq; 4- number of zeroes; 
C             5- number of poles; 6- zero values (real, imag pairs);
C             5+2*(4)- pole values

C           First determine whether there is any space.
	    j = 12 + gtstr(v30,bkette(12:))
	    read(bkette(j:),90431,iostat=ios) ra0,rnf,nzero
	    if (ios .ne. 0) go to 28
	    read(bkette(j+4+3+12+12+3+48*nzero:),'(i3)',end=28,err=28)
     +         npole
	    read(bkette(8:),'(i4)',end=26,err=26) ikey
	    ix = rdadd(ikey,043,4+2*(nzero+npole))
	    if (0 .eq. ix) go to 27

C           Have space, now fill in entries.
            call rdset(commrd(ix+1),ra0)
            call rdset(commrd(ix+2),rnf)
            call rdset(commrd(ix+3),nzero)
            call rdset(commrd(ix+4),npole)
	    read(bkette(j+4+3+12+12+3:),90432,end=28,err=28)
     +         (commrd(ix+3+2*i),commrd(ix+4+2*i),i=1,nzero)
	    ix = ix + 2*nzero
	    read(bkette(j+4+3+12+12+3+48*nzero+3:),90432,end=28,err=28)
     +         (commrd(ix+3+2*i),commrd(ix+4+2*i),i=1,npole)
	 else if (bkid .eq. '044') then
	    read(bkette(8:),'(i4)',end=26,err=26) ikey
	    if (0 .eq. rdadd(ikey,044,0)) go to 27
	 else if (bkid .eq. '045') then
	    read(bkette(8:),'(i4)',end=26,err=26) ikey
	    if (0 .eq. rdadd(ikey,045,0)) go to 27
	 else if (bkid .eq. '046') then
	    read(bkette(8:),'(i4)',end=26,err=26) ikey
	    if (0 .eq. rdadd(ikey,046,0)) go to 27
	 else if (bkid .eq. '047') then
	    read(bkette(8:),'(i4)',end=26,err=26) ikey
	    if (0 .eq. rdadd(ikey,047,0)) go to 27
	 else if (bkid .eq. '048') then
	    read(bkette(8:),'(i4)',end=26,err=26) ikey
C           048 - Sensitivity/Gain.
C           Layout is: 1- 048; 2- sensitivity/gain; 3- frequency
	    ix = rdadd(ikey,048,2)
	    if (0 .eq. ix) go to 27
	    j = 11 + gtstr(v30,bkette(11:))
	    read(bkette(j:),'(2f12.0)',iostat=ios) (commrd(ix+i),i=1,2)
	    if (ios .ne. 0) write(0,*) '**SEEDHD:  Corrupted 048 blk'
	 else
	    if (bkid .eq. '050') go to 29
	    write(0,*) '**SEEDHD:  Unrecognized blockette ',bkid
	 endif
      go to 20

25    continue
      write(0,*) '**SEEDHD:  Invalid comment format.'
      go to 20

26    continue
      write(0,*) '**SEEDHD:  Corrupted ',bkid,' blockette.'
      go to 20
27    continue
      write(0,*) '**SEEDHD:  Response dictionary overflow.'
      go to 20

28    continue
      write(0,*) '**SEEDHD:  Corrupted 043 blk'
      go to 20

29    continue

C     Finished reading abbreviation information, now get station information.

      do i=1,sn
C        Fill in using station header info.  Reset channel
C           count.  Definition of number of channels is imprecise, so
C           it isn't used.
	 call posset(possta,shdr(i),8)
30       continue
	    bklen = getbkt(possta,bkette)
	    if (bkid .ne. '050') go to 31
90501       format(3x,4x,a5,f10.0,f11.0,f7.0,i4,3x,a61)
90502       format(i3,a6)
	    read(bkette,90501,iostat=ios)
     &         spname,tlat,tlon,tel,tnch,v60
	    if (ios .ne. 0) go to 39
	    if (spname .ne. sta) go to 35
	    j = 48 + gtstr(v60,bkette(48:))
	    read(bkette(j:),90502,iostat=ios) tnid,tkeys
	    if (ios .ne. 0) go to 39
	    j = j + 9 + gtstr(timbeg,bkette(j+9:))
	    j = j + gtstr(timend,bkette(j:))
	    if (timend(1:1) .eq. ' ') timend = '2500,365'
	    if (valid(timbeg,timend)) then
               slat(i) = tlat
	       slon(i) = tlon
	       sel(i) = tel
	       snch(i) = tnch
	       snid(i) = tnid
	       snetcd(i) = bkette(j+1:j+2)
	       skeys(i) = tkeys
	       sst(i) = timbeg
	       sse(i) = timend
	       call poscpy(possta,pos)
	    endif
	    call posadd(possta,bklen)
	 go to 30
C        Zero subchannel count
31       continue
	 do j=1,MAXCHA
	    snsch(i,j) = 0
         enddo
C        Reposition at 050 for station definition
	 call poscpy(pos,possta)
	 bklen = getbkt(possta,bkette)
         nch = 0
32       continue
	    call posadd(possta,bklen)
	    bklen = getbkt(possta,bkette)
	    if (bklen .le. 0) go to 35
	    if (bkid .eq. '050') then
	       read(bkette,'(3x,4x,a5)',iostat=ios) spname
	       if (ios .ne. 0) pause '**050 OK before, not now?!'
	       if (spname .ne. sta) go to 35
	    else if (bkid .eq. '051') then
	       continue
	    else if (bkid .eq. '052') then
C              Fill in subchannel info.  Some fields here update the
C                 station information.
90521          format(7x,a2,a3,i4,3x,a31)
90522          format(3x,3x,f10.0,f11.0,f7.0,3f5.0,i4,i2,2e10.0)
	       read(bkette,90521,iostat=ios) vloc,vchan,ich,v30
	       if (ios .ne. 0) then
		  write(0,*) '**Corrupted 052 block.'
		  go to 32
	       endif
C              Skip those that aren't seismometers/geophones.
	       if (0 .eq. index('HLGMP',vchan(2:2))) go to 32

C              Extract effective time range, skip if outside.
	       j = index(v30,'~') + 20
	       k = gtstr(v50,bkette(j+79:))
	       j = k + j + 79
	       k = gtstr(effbeg,bkette(j:))
	       k = gtstr(effend,bkette(j+k:))
	       if (.not.valid(effbeg,effend)) go to 32

C              Check for appearance of new channel.
	       if (ich .gt. 3) then
		  write(0,*) '**SEEDHD: 052 Multiplexed data count ',
     &               '> 3, setting to zero; was ',ich
		  ich = 0
	       endif
	       if (ich .le. 1) then
		  nch = nch + 1
		  if (nch .gt. MAXCHA) then
		     write(0,*) '**SEEDHD:  Too many channels!'
		     sn = 0
		     return
		  endif
		  schid(i,nch) = vchan // vloc
	       endif
	       snsch(i,nch) = snsch(i,nch) + 1
	       j = index(v30,'~') + 20
	       k = snsch(i,nch)
	       read(bkette(j:),90522,iostat=ios)
     &            slat(i),slon(i),sel(i),sdep(i),
     &            schaz(i,nch,k),schdip(i,nch,k),sform(i,nch,k),
     &            sdatl(i,nch,k),srate(i,nch,k),sdrift(i,nch,k)
	       if (ios .ne. 0) then
		  write(0,*) '**Corrupted 052 block.'
		  go to 32
	       endif
	    else if (bkid .eq. '053') then
	       continue
	    else if (bkid .eq. '054') then
	       continue
	    else if (bkid .eq. '055') then
	       continue
	    else if (bkid .eq. '056') then
	       continue
	    else if (bkid .eq. '057') then
	       continue
	    else if (bkid .eq. '058') then
	       continue
	    else if (bkid .eq. '059') then
	       continue
	    else if (bkid .eq. '060') then
	       continue
	    else if (bkid .eq. '061') then
	       continue
	    else 
	       go to 35
	    endif
	 go to 32

39       continue
	 write(0,*) '**SEEDHD:  Corrupted station header!'
	 sn = 0
	 return

C        Come here if finished reading blockettes for this station.
35       continue
         snch(i) = nch
      enddo

      end
