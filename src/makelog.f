C     Program to scan GEOSCOPE SEED CDs and build a log file for the data
C        contained therein.
C
C     makelog [-s] [-d] [-cat [cmt | std]] cd-dir [ hypo-file ]
C
C     Options:
C        -d - debug mode
C        -s - dump station information only (for aux program use)
C        -cat cmt - CMT catalog format
C        -cat std - standard, free-format catalog format
C
C     Program takes one or two arguments on the command line following the
C     command:
C        CD directory (e.g., /cdrom)
C        Hypocenter file name (not required if volume is event-oriented)
C     Standard input is a list of file names (without the CD directory name)
C     of files to put into the assembled log file:
C        CD file (e.g. seed.dat, *not* /cdrom/seed.dat)
C
C     This program assumes that the station information does not change through
C     the duration of the data present in the SEED volume it is reading from.
C     This may be wrong if the dataset's span is long.  To fix this would
C     require all the versions of the station information to be accumulated
C     and keyed by time, and when any info is wanted, the time period of the
C     query presented along with what's wanted.  The program doesn't do this
C     yet (if it ever will), so caveat emptor.
C
C     At present, there are two choices for the hypocenter file:
C        -cat cmt is assumed to be in CMT dataset (dek-file) format
C        -cat std is assumed to be free-format lines with
C           year mo day hr mn sec   lat       lon      depth  mb  other mag
C           1998 07 25  02 39 23.30 -13.608   166.867   44.0  5.9  6.0  
C
C     Last updated:
C        4 Feb. 2011 G. Helffrich/U. Bristol.
C
C        Handle multiple channel versions with different effective dates.
C        gfortran syntax conformance.

      program makelog
      parameter (iuhf=10, ewindo = 45.0*60, tsslop=11.0)
      integer getvol, getbkt, datspan
      logical poseq
      real*8 tdsdif, timdif
      include 'cdscan.io.com'
      include 'cdscan.sta.com'
      logical exists, verify, evtvol, keep, evtfst, nchmsg
      logical help, osonly, exist, valid
      character cddir*256, cdname*256, flname*256, hypofl*256, line*80
      character bkette*(MAXBL),bkid*3
      equivalence (bkette(1:3),bkid)
      integer pos(2), possta(2), postim(2), bklen, debug
      integer ydbeg, ydend, chbeg, chend, edate, gtstr
      real*8 secbeg, secend, esec, chsbeg, chsend
      character timbeg*23, timend*23, effbeg*23, effend*23
      character vsta*5, vchan*3, spname*5
      character v22*23, v30*31, v50*51, v60*61, cid*8, cymd*8, chms*10
      character cidold*8, catfmt*3, loc*2, base62*62

C     Station quantities.
      integer sn, snch(MAXSTA), snsch(MAXSTA,MAXCHA),shdr(MAXSTA)
      real slat(MAXSTA), slon(MAXSTA), sel(MAXSTA), sdep(MAXSTA)
      real srate(MAXSTA,MAXCHA,MAXSCH), sdrift(MAXSTA,MAXCHA,MAXSCH)
      real schaz(MAXSTA,MAXCHA,MAXSCH), schdip(MAXSTA,MAXCHA,MAXSCH)
      integer sdatl(MAXSTA,MAXCHA,MAXSCH), sform(MAXSTA,MAXCHA,MAXSCH)
      character schid(MAXCHA,MAXSTA)*5
      character sname(MAXSTA)*5, skeys(MAXSTA)*6, snet(MAXSTA)*2
      character sst(MAXSTA)*23, sse(MAXSTA)*23

C     Time series quantities.  
      parameter (MAXSEG=20000)
      integer tn, tnall
      integer tsix(MAXSEG),tssort(MAXSEG)
      integer tstchn(MAXSEG), tsblok(MAXSEG), tsdy(MAXSEG)
      real*8 tssec(MAXSEG), tsdur(MAXSEG)

C     Event record quantities.
      parameter (MAXSPN = 5)
      real spaz, spbaz, spdel
      real spbeg(MAXSPN), spdur(MAXSPN), spsrat(MAXSPN)
      integer spsta, splcmp, spnbrk(MAXSPN), spncmp(MAXSPN)
      character spctyp(MAXSPN)*2

C     Event information quantities.
      parameter (MAXEVT=16, MAXMAG = 5)
      character magtyp(MAXMAG,MAXEVT)*11
      integer magsrc(MAXMAG,MAXEVT), enm(MAXEVT)
      real magmag(MAXMAG,MAXEVT)
      character etime(MAXEVT)*23
      real elat(MAXEVT), elon(MAXEVT), edepth(MAXEVT)

C     Set verify to true if you wish to ignore the time index blockettes
C        and want to read the data blocks to determine the start times
C        and durations of each data span.
      data verify /.false./, evtvol /.false./, evtfst/.false./
      data debug /0/, catfmt /'std'/, help/.false./
      data osonly /.false./
      data base62 /
     &'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
     &/

      cddir = ' '
      hypofl = ' '
      iskip = 0
      do 5 i=1,iargc()
         if (i .le. iskip) go to 5
         call getarg(i,flname)
	 if (flname(1:2) .eq. '-d') then
	    debug = debug + 1
	 else if (flname .eq. '-cat') then
	    call getarg(i+1,catfmt)
	    iskip = i+1
	 else if (flname(1:2) .eq. '-s') then
	    osonly = .true.
	 else if (flname .eq. '-verify') then
	    verify = .true.
	 else if (flname .eq. '-help') then
	    help = .true.
	 else if (cddir .eq. ' ') then
	    cddir = flname
	 else if (hypofl .eq. ' ') then
	    hypofl = flname
	 else
	    write(0,*) '**Unrecognized parameter, ',
     +         flname(1:index(flname,' ')-1),', skipping.'
	 endif
5     continue
      if (help .or. cddir .eq. ' ') then
         if (cddir .eq. ' ') write(*,*) '**Missing CD directory name.'
	 write(*,*) 'Usage: makelog [-cat cmt|std] cddir [catfile]'
	 write(*,*) '  std. input is list of SEED volumes to index'
	 write(*,*) '  std. output is cdseis log file'
	 write(*,*) '  no catalog needed if volume is event-oriented'
	 stop
      endif

      open(7,file='makelog.log')

C     Big loop to read in next file on CD.
      cidold = ' '
1000  continue
      read(5,'(a)',iostat=ios) cdname
      if (ios .ne. 0) stop

      flname = cddir(1:index(cddir,' ')-1)//'/'//cdname
      if (getvol(flname) .ne. 0) then
	 write(0,*) '**Unable to open CD SEED file ',
     &      flname(1:index(flname,' ')-1),', skipping.'
	 go to 1000
      endif

C     Read volume header to determine time span of recorded data.  Save
C     for bounds checking against hypocentral information.
      call posset(pos,1,8)
      bklen = getbkt(pos,bkette)
      if (bklen .le. 0) stop '**Can''t read vol. hdr.'
      timbeg = bkette(14:14+22)
      timend = bkette(14+index(timbeg,'~'):)
      write(7,9001) cdname(1:index(cdname,' ')),
     +   bkette(1:3),bkette(8:11),
     +   lrecl,
     +   timbeg(1:index(timbeg,'~')-1),
     +   timend(1:index(timend,'~')-1)
      call timcvt(ydbeg,secbeg,timbeg)
      call timcvt(ydend,secend,timend)

C     Read in station header index
      sn = 0
10    continue
	 call posadd(pos,bklen)
	 bklen = getbkt(pos,bkette)
	 if (bkid .ne. '011') go to 19
	 read(bkette(8:10),'(i3)',iostat=ios) nsta
	 if (ios .ne. 0) stop '**Corrupted 011 block.'
	 if (sn+nsta .gt. MAXSTA) then
	    write(0,*) '**Too many stations.  Using ',MAXSTA
	    nsta = MAXSTA - sn
	 endif
	 j = 11
	 do 15 i=1,nsta
	    read(bkette(j:j+10),'(a5,i6)',iostat=ios)
     +         sname(sn+i),shdr(sn+i)
	    if (ios .ne. 0) stop '**Corrupted 011 block.'
	    j = j + 11
15       continue
	 sn = sn + nsta
      go to 10

C     Read in station headers
19    continue
      write(7,*) sn,' stations on this volume.'
      if (sn .le. 0) go to 1000
      write(7,*) ' Reading station headers...'
      do 20 i=1,sn
C        Fill in using station header info.  Reset channel
C           count.  Definition of number of channels is imprecise, so
C           it isn't used.
	 call posset(possta,shdr(i),8)
	 bklen = getbkt(possta,bkette)
	 if (bkid .ne. '050') stop '**No station header!'
90501    format(3x,4x,5x,f10.0,f11.0,f7.0,i4,3x,a61)
90502    format(3x,a6)
24       continue
	 read(bkette,90501,iostat=ios)
     &      slat(i),slon(i),sel(i),snch(i),v60
	 if (ios .ne. 0) stop '**Corrupted 050 block.'
	 j = index(v60,'~') + 51
	 skeys(i) = bkette(j:j+5)
	 sst(i) = bkette(j+6:)
	 j = j+6+index(sst(i),'~')
	 sse(i) = bkette(j:)
	 j = j+index(sse(i),'~')
	 snet(i) = bkette(j+1:j+2)
C        Zero subchannel count
	 do 21 j=1,MAXCHA
	    snsch(i,j) = 0
21       continue
         nch = 0
	 nchmsg = .true.
22       continue
	    call posadd(possta,bklen)
	    bklen = getbkt(possta,bkette)
	    if (bklen .le. 0) go to 25
	    if (bkid .eq. '050') then
C              Some volumes written that repeat 050 block for different epochs
C              for the same station.  If more found, proceed past them
C              unless we're into the next stations' bunch (which might have
C              the same name as the previous one), otherwise declare info for
C              this station finished.
	       if (sname(i) .eq. bkette(8:12)) then
		  call posset(postim,shdr(i+1),8)
		  if (.not. poseq(possta,postim)) go to 24
	       endif
	       go to 25
	    endif
	    if (bkid .eq. '051') then
	       continue
	    else if (bkid .eq. '052') then
C              Fill in subchannel info.  Some fields here update the
C                 station information.
90521          format(7x,a2,a3,i4,3x,a31)
90522          format(3x,3x,f10.0,f11.0,f7.0,3f5.0,i4,i2,2e10.0)
	       read(bkette,90521,iostat=ios) loc,vchan,ich,v30
	       if (ios .ne. 0) stop '**Corrupted 052 block.'
C              Extract effective time range, skip if outside.
               j = index(v30,'~') + 20
	       k = gtstr(v50,bkette(j+79:))
	       j = k + j + 79
	       k = gtstr(effbeg,bkette(j:))
	       k = gtstr(effend,bkette(j+k:))
               call timcvt(chbeg,chsbeg,effbeg)
               call timcvt(chend,chsend,effend)
	       exist = valid(ydbeg,secbeg,ydend,secend,
     &                       chbeg,chsbeg,chend,chsend)
               if (debug.gt.1) then
	          write(0,*) vchan,loc,exist,effbeg,effend
	       endif
	       if (.not.exist) go to 22
	       if (ich .gt. 3) then
		  write(7,*) '***052 ',sname(i),
     &               ' Multiplexed data count > 3,',
     &               ' setting to zero; was ',ich
		  ich = 0
	       endif
C              Check for appearance of new channel
	       if (ich .le. 1) then
		  iidchn = idchan(sname(i),vchan,loc,
     &               1,MAXCHA,sname(i),snch,schid(1,i))
		  if (iidchn .ne. 0) then
C                    Channel exists for this station already, just replace
C                    values, updating any previous epoch.
		     nch = mod(iidchn,100)
		     snsch(i,nch) = 0
		  else
		     nch = nch + 1
		  endif
		  if (nch .gt. MAXCHA) then
		     stop '**Too many channels!'
		  endif
		  if (nch .gt. snch(i)) then
C                    This seems to be violated so often that it isn't worth
C                    complaining about.  Uncomment if you want to see a lot
C                    of harmless messages.  (GRH 11 Mar/98)
C		     if (nchmsg) write(0,*) 
C    &                  '**More channels than 050 indicates',
C    &                  ' for ',sname(i),' (ignoring 050 info).'
		     nchmsg = .false.
		     snch(i) = nch
		  endif
		  schid(nch,i) = vchan // loc
	       endif
	       snsch(i,nch) = snsch(i,nch) + 1
	       j = index(v30,'~') + 20
	       k = snsch(i,nch)
	       read(bkette(j:),90522,iostat=ios)
     &            slat(i),slon(i),sel(i),sdep(i),
     &            schaz(i,nch,k),schdip(i,nch,k),sform(i,nch,k),
     &            sdatl(i,nch,k),srate(i,nch,k),sdrift(i,nch,k)
	       if (ios .ne. 0) stop '**Corrupted 052 block.'
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
	       go to 25
	    endif
	 go to 22

C        Come here if finished reading blockettes for this station.
25       continue
         if (nch .gt. snch(i)) then
            write(7,'(a,i3,a,i3,a)') '*** '//sname(i)//':',snch(i),
     &         ' channels in Station ID block., but ',nch,' given',
     &         ' in Channel ID block.  Using ',nch,'.'
	 endif
	 snch(i) = nch
20    continue

C     Write station and time span information to log file
      do 100 i=1,sn
	 write(7,*) '0Station: ',sname(i),', valid ',
     &      sst(i)(1:index(sst(i),'~')-1),'-',
     &      sse(i)(1:index(sse(i),'~')-1),'.'
	 write(7,9100) slat(i),slon(i),sel(i),sdep(i),snch(i)
	 write(7,9101) (
     &      (schid(j,i)(1:3),schid(j,i)(4:5),
     &       srate(i,j,k),schaz(i,j,k),schdip(i,j,k),
     &       sdrift(i,j,k),k=1,snsch(i,j)),j=1,snch(i))
100   continue

C     If only dumping station information, do it and quit.

9401  format(a,1x,a,1x,2f13.7,2f7.1,1x,i2,10(1x,a,1x,a,2(1x,f5.1)))

      if (osonly) then
	 do i=1,sn
	    nevt = 0
	    do j=1,snch(i)
	       nevt = nevt + snsch(i,j)
	    enddo
	    write(*,9401) sname(i),snet(i),
     &         slat(i),slon(i),sel(i),sdep(i),nevt,
     &         (schid(j,i)(1:3),schid(j,i)(4:5),
     &         (schaz(i,j,k),schdip(i,j,k),k=1,snsch(i,j)),j=1,snch(i))
	 enddo
	 stop
      endif

C     Now read in the time span information.  The last volume blockette
C        read was the 011 type.  We look for 012 types now, and use
C        the indices given as places to search for specific time spans.

30    continue
	 bklen = getbkt(pos,bkette)
	 if (bklen .le. 0) stop '**No time span index!'
      if (bkid .ne. '012') go to 30

      tn = 0
32    continue
90121    format(3x,4x,i4)
90122    format(i6)
         read(bkette,90121,iostat=ios) nspan
	 if (ios .ne. 0) stop '**Corrupted 012 header!'
	 if (nspan+tn .gt. MAXTIM) stop '**Too many time spans!'
	 j = 12
	 do 35 i=1,nspan
	    v22 = bkette(j:)
	    j = index(v22,'~') + j
	    v22 = bkette(j:)
	    j = index(v22,'~') + j
	    read(bkette(j:),90122,iostat=ios) tsix(tn+i)
	    if (ios .ne. 0) stop '**Corrupted 012 blockette!'
	    j = j + 6
35       continue
         tn = tn + nspan
         call posadd(pos,bklen)
	 bklen = getbkt(pos,bkette)
      if (bklen .gt. 0 .and. bkid .eq. '012') go to 32
      write(7,*) tn,' time spans.'

C     Have a list of time series indicators.  Now plow through them to
C     figure out where the start/stop times for each channel are.
      nevt = 0
      tnall = 0
      do 40 i=1,tn
	 call posset(postim,tsix(i),8)
	 bklen = getbkt(postim,bkette)
	 if (bklen .le. 0 .or. bkid .ne. '070') 
     &      stop '**No time span ID!'
45       continue
            call posadd(postim,bklen)
	    bklen = getbkt(postim,bkette)
	    if (bklen .le. 0 .or. bkid .eq. '070') go to 40
	    if (bkid .eq. '071') then
C              Parse subset of hypocentral location.
90711          format(i2,f10.6,f11.6,f7.2,i2)
90712          format(f5.2,a11)
               evtvol = .true.
	       nevt = nevt + 1
               if (nevt .gt. MAXEVT) pause '**Too many hypocenters...'
	       etime(nevt) = bkette(8:)
	       j = index(etime(nevt),'~') + 8
	       read(bkette(j:),90711,end=41,err=41) 
     &            is,elat(nevt),elon(nevt),edepth(nevt),enm(nevt)
               enm(nevt) = min(enm(nevt),MAXMAG)
	       j = j+32
	       do 44 k=1,enm(nevt)
		  read(bkette(j:),90712,end=41,err=41)
     &               magmag(k,nevt),magtyp(k,nevt)
		  j = j+index(magtyp(k,nevt),'~')+7
		  read(bkette(j-2:),90711,end=41,err=41) magsrc(k,nevt)
44             continue
	    else if (bkid .eq. '072') then
	       continue
	    else if (bkid .eq. '073') then
	       pause '**073 blockette found!'
	    else if (bkid .eq. '074') then
90741          format(3x,4x,a5,a2,a3,a23)
90742          format(i6,i2,a23)
               tnall = tnall + 1
	       if (tnall .gt. MAXSEG) stop '**Too many time segments!'
	       read(bkette,90741,end=47,err=47) vsta,loc,vchan,timbeg
	       j = index(timbeg,'~') + 18
	       read(bkette(j:),90742,end=47,err=47)
     &            tsblok(tnall),i2,timend
	       j = index(timend,'~') + j + 8
	       read(bkette(j:),90742,end=47,err=47) iblk,i2
	       tsdur(tnall) = timdif(timend,timbeg)
	       call timcvt(tsdy(tnall),tssec(tnall),timbeg)
	       iidchn = idchan(vsta,vchan,loc,sn,
     &                         MAXCHA,sname,snch,schid)
	       tstchn(tnall) = iidchn
	       write(7,'(1x,a5,1x,a3,1x,a2,2(1x,a),2(1x,i7))')
     +            vsta,vchan,loc,
     +            timbeg(1:index(timbeg,'~')-1),
     +            timend(1:index(timend,'~')-1),
     +            tsblok(tnall),iblk
	       if (iidchn .eq. 0) then
                  write(0,*) '**Can''t find station & channel for ',
     +               vsta,vchan,' data at ',tsblok(tnall),'.'
		  tnall = tnall - 1
	       endif
	    else
	       go to 40
	    endif
	 go to 45
41       continue
	 stop '**Corrupted 071 blockette!'
47       continue
	 stop '**Corrupted 074 blockette!'
40    continue
      write(7,'(1x,i6,a)') tnall,' time indices.'
      if (evtvol) then
	 write(7,*) ' Event information:'
	 do 43 i=1,nevt
	    write(7,9043)
     +      i,etime(i)(1:index(etime(i),'~')-1),
     +      elat(i),elon(i),edepth(i),
     +      (magmag(j,i),magtyp(j,i)(1:index(magtyp(j,i),'~')-1),
     +         j=1,enm(i))
43       continue
9043     format(1x,i2,1x,a,1x,f8.4,1x,f9.4,1x,f7.2,10(1x,f4.1,a))
      endif

C     All the time spans have been accumulated.  If paranoid, we can read the
C     data blocks to which they point to figure out the length of each
C     sequence of data.  This (like all paranoid behavior) takes a long time
C     (about 1.5 hours for a 700 Mb CD) so sane people wouldn't do it.

      if (.not. verify) go to 59
      i = 0
50    continue
	 i = i + 1
	 if (i .gt. tnall) go to 59
	 ist = tstchn(i)/100
	 ich = mod(tstchn(i),100)
	 iblk = tsblok(i)
	 ilen = 2**sdatl(ist,ich,1)
	 inext = datspan(tsdy(i),tssec(i),tsdur(i),ilen,
     &      sdrift(ist,ich,1),skeys(ist)(1:4),skeys(ist)(5:6),iblk)

C        Check that first block following break is indexed.  If not,
C        and it is the last time segment, don't worry about it.
	 do 51 j=1,tnall
	    if (inext .eq. tsblok(j)) go to 50
51       continue
         if (i .eq. tnall) go to 50
	 write(7,'(a,i6,a)') '**Block ',inext,
     &      ': time tear that isn''t indexed.'

C        It isn't -- index it to include in span.
         call dathead(bkette,ilen,inext,skeys(ist)(1:4),skeys(ist)(5:6),
     &                vsta,vchan,loc,edate,esec)
	 iidch = idchan(vsta,vchan,loc,sn,MAXCHA,sname,snch,schid)
	 if (iidch .eq. 0) then
	    write(7,*) '  Ignoring.  Can''t find station & channel.'
	 else
	    if (iidch/100 .ne. ist) then
	       write(7,*) '  Ignoring.  Not in station sequence.'
	    else
C              Make room for new entry and then process it like a normal one.
	       if (tnall .lt. MAXSEG) then
		  write(7,'(a,1x,i7,1x,f12.4,1x,i6)')
     &               '  Added '//vsta//' '//vchan,edate,esec,inext
		  do 52 j=tnall,i,-1
          	     tstchn(j+1) = tstchn(j)
		     tsdy(j+1) = tsdy(j)
		     tssec(j+1) = tssec(j)
		     tsdur(j+1) = tsdur(j)
		     tsblok(j+1) = tsblok(j)
52                continue
                  tnall = tnall + 1
		  tstchn(i) = iidch
		  tsblok(i) = inext
		  i = i - 1
		  go to 50
	       endif
	       write(7,*) '  Ignoring.  Too many segments.'
	    endif
	 endif
      go to 50

59    continue

9000  continue
      close(iucd)

C     Sort data segments into order
      call mlsort(tnall,tssort,tstchn,tsdy,tssec)
  
      if (debug.gt.0) then
	 write(7,*)
	 write(7,*) tnall,' discrete data segments.'
	 write(7,9201) (
     &      tstchn(tssort(i)),
     &      tsdy(tssort(i)),
     &      tssec(tssort(i)),
     &      tsdur(tssort(i)),
     &      tsblok(tssort(i)),
     &   i=1,tnall)
      endif

C     Read hypocentral file to determine events for which we will build up an
C     index.
      if (.not. evtvol) then
C        No hypo info in file -- have to get it from external file
         exists = .false.
	 if (hypofl .ne. ' ') inquire(file=hypofl,exist=exists)
	 if (.not. exists) then
	    write(0,*) '**Not an event volume.',
     &         '  Unable to find hypo file ',
     &         hypofl(1:index(hypofl,' ')-1),', quitting.'
	    stop
	 endif
	 inquire(iuhf,opened=exists)
	 if (exists) close(iuhf)
	 open(iuhf,file=hypofl)
      endif
      n = 0
      nspan = 0
200   continue
C        Check if we found any records of prior event before we move on to
C           next.
	 if (nspan .ne. 0) then
	    write(6,9302) sname(spsta),spdel,spaz,spbaz,
     &         (spsrat(j),spncmp(j),spbeg(j),spdur(j),min(spnbrk(j),9),
     &         j=1,nspan)
	    nspan = 0
	 endif
	 if (.not. evtvol) then
	    if (catfmt .eq. 'cmt') then
	       read(iuhf,'(a)',end=299) line
	       if (line(10:10).ne.'/' .or. line(13:13).ne.'/') then
C                 Old .dek format
	          read(line,9310,iostat=ios) cid,cymd,chms,
     &               eclat,eclon,ecdpth,emb,ems
                  read(iuhf,9320) elat(1),elon(1),edepth(1)
	          read(cymd,'(i2,1x,i2,1x,i2)',iostat=ios) imm,idd,iyy
	          if (iyy .lt. 60) iyy=iyy+100
	          edate = (1900+iyy)*1000 + 
     &               julday(imm,idd,1900+iyy) - julday(1,0,1900+iyy)
	          read(chms,'(i2,1x,i2,1x,f4.1)',iostat=ios) ihh,imm,ss
	          cid = cid(2:)
	       else
C                 New .ndk format.  Event ID is formed from the @yymmdd*
C                 where * is event minute character
C                 (00-25->a-z, 26-51->A-Z, 52-60->0-7) and @ is hour char
C                 and duplicate event character (00->a, 23->x and A...Z)
	          read(line,9410,iostat=ios) iyy,imo,idd,ihh,imm,ss,
     &               eclat,eclon,ecdpth,emb,ems
                  if (ios.ne.0) go to 298
		  edate = iyy*1000+julday(imo,idd,iyy)-julday(1,0,iyy)
	          read(iuhf,9420,iostat=ios) v22
		  read(v22(10:13),'(2i2)',iostat=ios) ii,jj
		  j = max(0,ichar(v22(14:14))-ichar('A'))
		  ii = ii+1+j + 26*min(1,j)
		  cid = v22(4:9) // base62(ii:ii) // base62(jj+1:jj+1)
	          write(chms,'(2(i2.2,1x),f4.1)',iostat=ios) ihh,imm,ss
	       endif
	    else if (catfmt .eq. 'std') then
               read(iuhf,*,end=299,iostat=ios) iyy,imo,idd,ihh,imm,ss,
     &            eclat,eclon,ecdpth,emb,ems
               write(chms,'(i2,1x,i2,f5.1)') ihh,imm,ss
	       write(cid,'(3i2.2,a)') mod(iyy,100),imo,idd,'a'
	       if (cid(1:6) .eq. cidold(1:6)) then
	          cid(7:7) = char(ichar(cidold(7:7))+1)
	       endif
	       edate = iyy*1000+julday(imo,idd,iyy)-julday(1,0,iyy)
	    else
	       stop '**Invalid catalog format!'
	    endif
	    if (ios .ne. 0) then
	       pause '**Invalid date in hypo file!'
	       go to 200
	    endif
	    esec = dble(ss) + dble(60*(imm + 60*ihh))
	 else
C           Convert hypo info in file to form we want for event file.
	    if (n .ge. nevt) go to 299
	    n = n + 1
	    call timcvt(edate,esec,etime(n))
	    eclat = elat(n)
	    eclon = elon(n)
	    ecdpth = edepth(n)
	    chms = etime(n)(10:)
	    if (index(chms,'~') .ne. 0) chms(index(chms,'~'):) = ' '
	    emb = 0.0
	    ems = 0.0
	    do 185 i=1,enm(n)
	       if (index('MBmbMbmB',magtyp(i,n)(1:2)) .ne. 0)
     &            emb = magmag(i,n)
	       if (index('MSmsMsmS',magtyp(i,n)(1:2)) .ne. 0)
     &            ems = magmag(i,n)
185         continue
	    iyy = edate/1000
	    ijdy = mod(edate,1000)
	    call getday(iyy,ijdy,imm,idd)
	    do 187 i=0,25
	       write(cid,'(3i2.2,a)') mod(iyy,100),imm,idd,
     &           char(ichar('a')+i)
	       if (cid(1:6) .ne. cidold(1:6)) go to 188
	       if (ichar(cid(7:7)) .gt. ichar(cidold(7:7))) go to 188
187         continue
188         continue
	 endif
C        Eliminate hh:mm:ss punctuation so output is purely numeric
	 chms(3:3) = ' '
	 chms(6:6) = ' '

C        Quick check if event is within time window advertised in volume
C        header.
	 if (.not. evtvol .and.
     &       (tdsdif(ydbeg,secbeg,edate,esec+ewindo) .gt. 0.0 .or.
     &       tdsdif(edate,esec,ydend,secend) .gt. 0.0)) go to 200

C        Scan station/channel start/stop times to see if there is anything
C        interesting to index.  Remember if any traces found for this event.
         exists = .false.
	 spsta = 0
	 spname = ' '
	 do 210 ii=1,tnall
	    i = tssort(ii)
	    edif = tdsdif(edate,esec,tsdy(i),tssec(i))
	    keep = 
     &         (evtvol .and. edif .le. tsdur(i) .and.
     &            -edif/60.0 .le. 999.9 ) .or.
     &         (.not.evtvol .and.
     &            edif .lt. tsdur(i) .and. -edif .le. ewindo)
	    if (keep) then
C              Yes, found something.  Check if same station as before.  Since
C              some seed volumes duplicate station info (if a new sensor is
C              installed at a station, for example) the station name is checked
C              as well.  If that is the same, we treat it as the same station.
               newsta = tstchn(i)/100
               if (spsta .ne. newsta .and. 
     &            spname .ne. sname(newsta)) then
		  if (nspan .ne. 0) then
		     write(6,9302) 
     &                  sname(spsta),spdel,spaz,spbaz,
     &                  (spsrat(j),spncmp(j),spbeg(j),spdur(j),
     &                   min(spnbrk(j),9),j=1,nspan)
		  endif
		  nspan = 0
		  spsta = newsta
		  spname = sname(newsta)
		  splcmp = 0
                  call mygrt(eclat,eclon,slat(spsta),slon(spsta),
     &                       spdel,spaz,spbaz,gc)
	       else
		  spsta = newsta
	       endif
	       if (.not. exists) then
		  if (.not. evtvol) n = n + 1
		  iyy = edate/1000
		  ijdy = mod(edate,1000)
		  call getday(iyy,ijdy,imm,idd)
		  if (evtfst) write(6,*)
		  write(6,9301) iyy,imm,idd,chms,
     &               eclat,eclon,ecdpth,
     &               emb,ems,'seed '//cdname,cid
		  exists = .true.
		  evtfst = .true.
		  cidold = cid
	       endif
C              Check existing spans for occurrence of same sample rate.
               ichn = mod(tstchn(i),100)
	       isix = 0
               do 205 j=1,nspan
		  if (spctyp(j) .eq. schid(ichn,spsta)(1:2) .and.
     &                spsrat(j) .eq. srate(spsta,ichn,1)) then
C                    Same data type.  If channel differs from previous one,
C                    then this represents another component or subchannel.
C                    If it is the same comp. or subchannel, it is counted as a
C                    gap for the same event.
C                    Formerly, the logic was based on a comparison of
C                    the start times of the channels.  However, different
C                    start times for different components of the same
C                    3-component set renders this unsatisfactory.
C                    This case is recognized by limiting the number of
C                    components found to 3.
C                    The longest time span represented is kept.  
C                    Gaps are only counted for the vertical component, or
C                    the only component when multiplexed subchannels are
C                    present in the data.  The assumption is that the gaps
C                    are the same for all components of a multicomponent
C                    station.  CDSEIS does not use the gap information, so
C                    this approximation suffices.
                     beg = max(-edif,0.0)/60.0
		     if (splcmp .ne. ichn) then
C                    if (abs(spbeg(j)-beg) .le. tsslop) then
			ncnew = spncmp(j) + snsch(spsta,ichn)
			if (ncnew .le. 3) then
			   spncmp(j) = ncnew
			else
			   write(7,'(1x,a,1x,i7,1x,f5.1)') 
     &                         '**More than 3 comps:'//spname,edate,
     &                         srate(spsta,ichn,1)
			endif
		     else
                        if (0 .ne. index(' Z',schid(ichn,spsta)(3:3)))
     & 		           spnbrk(j) = spnbrk(j) + 1
		     endif
		     dur = tdsdif(tsdy(i),tssec(i)+tsdur(i),
     &                            edate,esec)/60.0
		     spdur(j) = max(spdur(j),min(999.9,dur))
		     go to 208
		  endif
C                 Keep track of insertion point of new sample rate info.
		  if (srate(spsta,ichn,1) .gt. spsrat(j)) isix = j
205            continue
C              New data type.  Keep track of it if it represents seismometer
C                 data. HGLMP codes are: H, L - High/Low gain seismometer;
C                 G - gravimeter; M - mass position seismometer; P - geophone
               if (0 .ne. index('ESHBMLVUR',schid(ichn,spsta)(1:1))
     &             .and.
     &             0 .ne. index('HLGMP',schid(ichn,spsta)(2:2))) then
		  if (nspan .ge. MAXSPN) then
		     write(7,*) '**Too many spans of data for ',cid
		  else
C                    Insert new sample rate info so that things will be sorted
C                       into ascending order of sample rate.
		     do 207 j=nspan,isix+1,-1
			spbeg(j+1) = spbeg(j)
			spdur(j+1) = spdur(j)
			spsrat(j+1) = spsrat(j)
			spnbrk(j+1) = spnbrk(j)
			spncmp(j+1) = spncmp(j)
			spctyp(j+1) = spctyp(j)
207                  continue
		     nspan = nspan + 1
		     isix = isix+1
		     spbeg(isix) = abs(min(edif,0.0))/60.0
		     dur = tdsdif(tsdy(i),tssec(i)+tsdur(i),
     &                         edate,esec)/60.0
		     spdur(isix) = min(dur,999.9)
		     spsrat(isix) = srate(spsta,ichn,1)
		     spnbrk(isix) = 0
		     spncmp(isix) = snsch(spsta,ichn)
		     spctyp(isix) = schid(ichn,spsta)(1:2)
	          endif
	       endif
208            continue
	       splcmp = ichn
            endif
210      continue
      go to 200

299   continue
      write(7,*) ' ',cdname(1:index(cdname,' ')-1),':',
     +   n,' recorded events.'
      go to 1000

C     Error reading hypo file
298   continue
      write(0,*) '**Error reading hypocenter information file!'

9001  format(1x,a,'Vol. header: Type: ',a3,' Version ',a,' lrecl ',i5,
     +   /,1x,'Time span: ',2(a,1x))
9002  format(1x,a,' type ',i3,' length ',i4)
9003  format(1x,'**Funny blockette format (',a,'): ',a)
9100  format(2x,'Lat, Lon: ',2f12.7,' El., depth: ',2f7.1,' Chans: ',
     +   i2)
9101  format((2x,(a3,1x,a2,1x,0pf6.2,2(1x,f5.1),1x,1pe11.4)))
9201  format((1x,2i8,2(f12.4,1x),i6))
9300  format(a8,1x,a8,1x,a10,1x,f6.2,1x,f7.2,1x,f5.1,2f3.1,/,
     +   t45,f6.2,6x,f7.2,6x,f5.1,/,/)
9301  format(1x,i5,2(1x,i2),2x,a10,2x,f8.3,1x,f9.3,1x,f6.1,2(1x,f4.1),
     +       2x,a25,6x,a8)
9302  format(a5,4x,3(1x,f5.1),9(1x,f5.1,1x,i1,2(1x,f5.1),1x,i1))
9310  format(a8,1x,a8,1x,a10,1x,f6.2,1x,f7.2,1x,f5.1,2f3.1)
9320  format(t45,f6.2,6x,f7.2,6x,f5.1,/,/)
9410  format(5x,i4,1x,4(i2,1x),f4.1,f7.2,f8.2,f6.1,2f4.1)
9420  format(a14,/,/,/)
      end

C     idchan  --  Search station id and channel list to find a given id
C                 and channel.
C
C     Called via:
C        i = idchan(id,chan,loc,n,nmax,sname,snch,schid)
C
C     Assumes:
C        id - station id
C        chan - channel id
C        loc - location id
C        n - number of stations
C        nmax - max number of stations
C        sname - station name array
C        snch - number of channels for each station
C        schid - channel id array
C
C     Returns:
C        i - encoded id and channel (100*id + channel) or zero if not found
C
C     Notes:
C        We assume that the SEED volume does not have extraneous data.
C        Thus, if an ID is given that does not match a known name, and
C        the station has only a single channel anyway, we match it.

      integer function idchan(id,chan,loc,n,nmax,sname,snch,schid)
      character id*5,chan*3,loc*2,sname(n)*5,schid(nmax,*)*5
      character chanloc*5
      integer snch(n)

      chanloc = chan//loc
      do 2 j=1,n
	 if (id .eq. sname(j)) then
	    do 4 k=1,snch(j)
	       if (chanloc .eq. schid(k,j)) go to 9
4           continue
C           k = 1
C           if (snch(j) .eq. 1) go to 9
	 endif
2     continue
      j = 0
      k = 0
9     continue
      idchan = 100*j + k
      end

C     mygrt  --  Compute great circle information given two lat, lon
C                coordinates
C
C     Called via:
C       call mygrt(alat1,alon1,alat2,alon2,disd,az12,az21,gc)
C
C     Assumes:
C       alat1, alon1 - lat, lon of point 1
C       alat2, alon2 - lat, lon of point 2
C
C     Returns:
C       disd - distance between points in degrees
C       az12 - azimuth from point 1 to point 2 (degrees)
C       az21 - azimuth from point 2 to point 1 (degrees)
C       gc - great circle distance (km)
C
C     Hacked from code written by Emile Okal (don't blame me for an absence
C     of comments, in other words!  It works as if by magic.)
        subroutine mygrt(alat1,alon1,alat2,alon2,disd,az12,az21,gc)
        pi=3.1415926535897
        ath=6378.388
        bth=6356.912
        rad=pi/180.
        h = 1. - bth*bth/ (ath*ath)
        p = h/(1.-h)
        gr=alon1*rad
        tr=alat1*rad
        sintr=sin(tr)
        costr=cos(tr)
        if(sintr.eq.0.) sintr=0.000001
        if(costr.eq.0.) costr=0.000001
        r1=ath/sqrt(1.-h*sintr*sintr)
        z1=r1*(1.-h)*sintr
        g=alon2*rad
        t=alat2*rad
        if(t.eq.0.) t=0.00001
        sint=sin(t)
        cost=cos(t)
        if(cost.eq.0.) cost=0.000001
        r2=ath/sqrt(1.-h*sint*sint)
        dg=g-gr
        cosdg=cos(dg)
        sindg=sin(dg)
        dgr=gr-g
        dt=t-tr
        q=sint*costr/((1.+p)*cost*sintr) + h*r1*costr/(r2*cost)
        x=r2*cost*cosdg
        y=r2*cost*sindg
        z=r2*(1.-h)*sint
        az12=atan2(sindg,(q-cosdg)*sintr)
2       q=sintr*cost/(costr*sint*(1.+p))+h*r2*cost/(r1*costr)
        az21=atan2(sin(dgr),sint*(q-cos(dgr)))
        cos12=cos(az12)
        cta2=costr*costr*cos12*cos12
        p0=p*(cta2+sintr*sintr)
        b0= (r1/(1.+p0))*sqrt(1.+p*cta2)
        e0=p0/(1.+p0)
        ezosl=1.-e0*(.25+e0*(3./64.+5.*e0/256.))
        gc=2.*pi*b0*sqrt(1.+p0)*ezosl
        c0=1.+ p0*(.25 - p0*(3./64.-5.*p0/256.))
        c2=p0*(-.125 + p0*(1./32.-15.*p0/1024.))
        c4=(-1./256. + 3.*p0/1024.)*p0*p0
        u0=atan2(sintr,costr*cos12*sqrt(1.+p0))
        ezosl=(x*cos12-y*sintr*sin(az12))*sqrt(1.+p0)
        u =atan2(r1*sintr +(1.+p0)*(z-z1),ezosl)
        disd=u-u0
        if(u.lt.u0) disd=pi+pi+disd
        dist=b0*(c0*disd + c2*(sin(2.*u)-sin(2.*u0)))
        dist=dist+b0*c4*(sin(4.*u)-sin(4*u0))
        dist=dist/111.195
        disd=disd/rad
        if(disd.le.181.) go to 1
        az12=az12-pi
        go to 2
1       az12=az12/rad
        az21=az21/rad
        if(az12.lt.0.) az12=az12+360.
        if(az21.lt.0.) az21=az21+360.
        return
        end

      SUBROUTINE MLSORT(n,indx,ra,rb,rc)
C     MLSORT -- Return index to rearrange list in sorted order.
C        Uses a heapsort.
C
C     Assumes:
C        ra - integer station id/channel id array
C        rb - integer start time (yyyyddd)
C        rc - real seconds in day of start time
C
C     Returns:
C        ix - array of indices to rearrange data in ascending order of
C             ascending station/channel and start time.
         
      INTEGER n,indx(n),M,NSTACK
      INTEGER ra(n),rb(n)
      DOUBLE PRECISION rc(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      INTEGER a
      LOGICAL mfscmp
      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=ra(indxt)
          do 12 i=j-1,1,-1
	    indxi=indx(i)
            if(ra(indxi).lt.a)goto 2
            if(ra(indxi).eq.a) then
	       if (rb(indxi).lt.rb(indxt)) goto 2
	       if (rb(indxi).eq.rb(indxt)) then
		 if (rc(indxi).le.rc(indxt)) goto 2
	       endif
	    endif
            indx(i+1)=indxi
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
C       if(ra(indx(l+1)).gt.ra(indx(ir)))then
        if(mfscmp(indx(l+1),indx(ir),n,ra,rb,rc))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
C       if(ra(indx(l)).gt.ra(indx(ir)))then
        if(mfscmp(indx(l),indx(ir),n,ra,rb,rc))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
C       if(ra(indx(l+1)).gt.ra(indx(l)))then
        if(mfscmp(indx(l+1),indx(l),n,ra,rb,rc))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=ra(indxt)
3       continue
          i=i+1
        if(ra(indx(i)).lt.a)goto 3
        if(ra(indx(i)).eq.a)then
	  if (rb(indx(i)).lt.rb(indxt))goto 3
	  if (rb(indx(i)).eq.rb(indxt))then
	     if (rc(indx(i)).lt.rc(indxt))goto 3
	  endif
	endif
4       continue
          j=j-1
        if(mfscmp(indx(j),indxt,n,ra,rb,rc))goto 4
C       if(ra(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
      
      logical function mfscmp(ix1,ix2,n,ra,rb,rc)
C     MFSCMP -- Do 3-key comparison of ix1 to ix2, return true if
C        ix1 is greater than ix2
      integer ix1,ix2,ra(n),rb(n)
      double precision rc(n)

C     if(ra(indx(l+1)).gt.ra(indx(ir)))then
      mfscmp = .true.
      if (ra(ix1) .gt. ra(ix2)) return
      if (ra(ix1) .eq. ra(ix2)) then
	 if (rb(ix1) .gt. rb(ix2)) return
	 if (rb(ix1) .eq. rb(ix2)) then
	    if (rc(ix1) .gt. rc(ix2)) return
	 endif
      endif
      mfscmp = .false.
      end

      integer function gtstr(oustr,instr)
C     gtstr -- Get a SEED variable string and return its value and length
C
C     Assumes:
C        instr - SEED input string, terminated with '~'
C
C     Returns:
C        Function result - length of string (including '~')
C        oustr - string value

      character instr*(*), oustr*(*)

      ix = index(instr,'~')
      if (ix .eq. 0) then
         write(0,*) '**STRGET:  Unterminated string.'
         gtstr = 0
         return
      endif
      if (ix .gt. 1) then
         oustr = instr(1:ix-1)
      else
         oustr = ' '
      endif
      gtstr = ix
      end

      logical function valid(volbyd,volbsc,voleyd,volesc,
     &                       stabyd,stabsc,staeyd,staesc)
C     valid -- Determine whether a time range lies within another.
C
C     Assumes:
C        volbyd, volbsc - volume start time
C        voleyd, volesc - volume end time
C        stabyd, stabsc - station start time
C        staeyd, staesc - station end time
C
C     Returns:
C        Function result - whether time spans overlap.

      integer volbyd, voleyd, stabyd, staeyd
      double precision volbsc, volesc, stabsc, staesc

      valid = voleyd .ge. stabyd
      if (valid) then
         if (voleyd .eq. stabyd) valid = volesc .ge. staesc
      endif
      if (valid) then
         valid = volbyd .le. staeyd
	 if (volbyd .eq. staeyd) valid = volbsc .le. staesc
      endif
      end
