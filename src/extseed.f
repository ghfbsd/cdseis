c subroutine EXTSEED gets data from CD which is a seed dataset.  Present
c version used rdseed program to get it.
c
c    G. Helffrich 27/6/95 original version
c                 21/7/95 add response information, time breaks
c                  1/8/95 parse station information, time breaks
c                 14/8/96 expand GEOSCOPE time corrections
c                 7/11/98 handle arbitrary channel names (UVW, Z12, 123, ugh)
c                 21/7/05 use sample rate from rdseed rather than header
c                 28/7/05 fix bugs in gap handling in data
c                 03/8/05 add mean value to gap in data, not zero
c                 16/9/07 return station elevation properly
c                 12/1/08 decode SEED directly without using rdseed
c                 02/2/11 fix for gfortran compile, remove use of debug dir,
c                         fix bug in response handling if response data
c                         time dependent.
c                 03/2/11 increase max data blocks.
c                 04/2/11 workaround for gfortran i/o lib bug, add to debug
c                         output.
c                11/11/15 add LOCID to output
c
c     Inputs:   cdfile  = (char*25) file name on CD
c               cddir   = (char*80) directory name for CD
c               snam    = (char*4) station name
c               sr      = desired sample rate (Hz)
c               srtol   = sample rate tolerance
c               qyr     = (integer) event year
c               qmon    = (integer) event month
c               qdy     = (integer) event day
c               qhr     = (integer) event hour
c               qmn     = (integer) event minute
c               qsc     = (real) quake second
c               qazi    = event azimuth at station (deg. E of N)
c               t1      = start time (minutes relative to event)
c               t2      = end time
c               kk      : 1=(z,n,e)  2=(z,r,t)  3=(n,e)  4(r,t)
c                         5=z  6=n  7=e  8=r  9=t 10=any
c               debug   = if>=1 Write verbose output 
c     Returns:  sr      = actual sample rate (Hz)
c               stlat   = station latitude
c               stlon   = station longitude
c               stelev  = station elevation
c               stype   = (char*8) station type
c               stcomm  = (char*(*)) station log comments
c               chan*4(3)=channel names using SEED convention (eg. BHZ for 
c                         broad-band,high gain, vertical)
c               npts    = number of points extracted
c               timebuf = array with time points
c               zbuf    = vertical component data array
c               buf2    = n-s or transverse component if data are rotated
c               buf3    = e-w or radial component if data are rotated
c               co(1)   = dip of vertical component from horizontal
c               co(2)   = azimuth of n-s (t) component
c               co(3)   = azimuth of e-w (r) component
c               yamp(1) = maximum amplitude of vertical component (zbuf)
c               yamp(2) = maximum amplitude of n-s (t) component (buf2)
c               yamp(3) = maximum amplitude of e-w (r) component (buf3)
c               gain(3) = gain of each component
c               units*20= character string containing units of gain
c               a0(3)   = normalization for each component
c               np(3)   = number of poles in instrument response
c               poles(30,3)= complex array containing poles for three components
c               nz(3)   = number of zeros
c               zeros(30,3)= complex array containing zeros forthree components
c               ncor    = number of time corrections
c               timcor(12)= (char*12) character array containing year, julian day,
c                         hour, minute, of a reported time correction.
c               cor(12) = time correction (s) to be added to the time in timbuf.
c
c
c Notes: 1) rotated components are returned in the Kanamori sign convention:
c      transverse is positive in a counterclockwise direction as viewed from
c      the source, radial is positive for motions away from the source
c      Caution: this is different from the convention given in Aki and Richards
c
c      2) for instrument response: assumes the same number of poles and zeros
c      for the 2 horizontal components.  Rotation assumes same instrument
c      response, including normalization (but not necessarily the same gain) 
c      for the 2 horizontal components.
c
c      3) data are output in counts except when rotated horizontals are requested.
c      In this case all components are divided by gain. 
c
      subroutine EXTSEED(cdfile,cddir,snam,sr,srtol,
     &      kk,qyr,qmon,qdy,qhr,qmn,qsc,qazi,t1,t2,
     &      stlat,stlon,stelev,stype,stcomm,chan,
     &      npts,ndim,timebuf,zbuf,buf2,buf3,co,
     &      yamp,gain,units,a0,np,poles,nz,zeros,ncor,timcor,cor,debug)
      integer debug
      integer qyr,qmon,qdy,qhr,qmn
      integer kk
      real qsc,stsec,ensec,cor(12)
      integer nn(3),np(3),nz(3),ncor
      character timcor(12)*(*)
      character chan(3)*(*),units*(*)
      character stname*8,locname*8,snam*(*),stype*(*)
      character cdfile*(*),header*25,cdfil*128,cddir*(*),file*128
      character cdfprv*128
      character stcomm*(*),diget*70
      character sttime*22,entime*22,stntin*256
      character cst*22,cet*22,cstp*22,cetp*22
      real timebuf(ndim),zbuf(ndim),buf2(ndim),buf3(ndim)
      real yamp(3),gain(3),a0(3),co(3),stgain(3)
      parameter (npzmx=30)
      complex poles(npzmx,3),zeros(npzmx,3)
      logical vert,rot,exists,geotc,snsmsg,pzmsg,inspan,fixgeo
      logical lgap(3),lovr(3),fststg,etemsg,ckgain,sprev,sthis,snext
      logical valid
      real stlat,stlon,stelev,qazi
      real qazirad,cosphi,sinphi,y_e,y_n,sr,t1,t2
      integer i,ii,npts,bklen,possta(2)
      integer lendir,lenfil,lenb,tsamp(6)
      integer rseed,getbkt,getdat,gtstr,rdget,rdint
      character zne23*6,rsfx(3)*3,chgot(3)*5,chnow*5,stcomm0*70
      character vsta*5,vloc*2,vchan*3,timbeg*23,timend*23,v22*23,v1*1
      equivalence (nn(1),n1),(nn(2),n2),(nn(3),n3)
      parameter (nssmax=8, ndbmax=256, ndfmax=20)
      integer stgix(nssmax), dfid(ndfmax)
      integer dbbeg(ndbmax), dbend(ndbmax), dbent(ndbmax)
      double precision diff
      character dfname(ndfmax)*50, skey*50, dbuf*(32768)

      include 'seed.sta.com'
      include 'seed.io.com'
      include 'extseed.com'
      parameter (ndlim=MAXBL)
      real samp(ndlim)
      character bkette*(MAXBL),bkid*3,v30*31,v50*51
      character effbeg*23, effend*23
      equivalence (bkette(1:3),bkid), (bkette,samp)
      external rename,stat

      parameter (gtol=0.01)

      data zne23 /'ZNE23 '/, cdfprv /' '/

      pi = 4.*atan(1.0)
      geotc=.false.
      snsmsg=.false.
      etemsg=.false.
      pzmsg=.false.
c    -if (vert) get vertical component; if (rot) rotate horizontals
      if (kk.eq.1.or.kk.eq.2.or.kk.eq.5.or.kk.eq.10) then
         vert=.true.
      else
         vert=.false.
      endif
      if (kk.eq.2.or.kk.eq.4.or.kk.eq.8.or.kk.eq.9) then
         rot=.true.
      else
         rot=.false.
      endif
      ncor=0
      npts=0
      nlstct=0
      do 19 ii=1,3
	 gain(ii) = 0.0
	 yamp(ii) = 0.0
	 a0(ii) = 0.0
	 poles(1,ii) = (0.,0.)
	 zeros(1,ii) = (0.,0.)
	 np(ii) = 0
         nz(ii) = 0
	 chgot(ii) = ' '
	 chan(ii) = ' '
	 stgain(ii) = 1.0
19    continue
      stcomm = '$'

c  Construct file name, assumed to start with 'seed ...'

      call rename(cdfile, file, header)
      lendir=lenb(cddir)
      lenfil=lenb(cdfile)
      if (lendir.gt.0)then
         cdfil=cddir(1:lendir)//'/'//cdfile(6:lenfil)
      else
         cdfil=cdfile(6:)
      endif

c     Calculate time start and end wanted in the form of a SEED time string.
c        This goes into common storage and is implicitly used by the VALID
c        function.
      call addtime(qyr,qmon,qdy,qhr,qmn,qsc,t1,
     +            styr,stmon,stdy,sthr,stmn,stsec)
      call addtime(qyr,qmon,qdy,qhr,qmn,qsc,t2,
     +            enyr,enmon,endy,enhr,enmn,ensec)
      call getjday(styr,stmon,stdy,stjdy)
      call getjday(enyr,enmon,endy,enjdy)
      stsc = int(stsec)
      ensc = int(ensec)
      stth = int((stsec-stsc)*10000)
      enth = int((ensec-ensc)*10000)
      write(sttime,1000) styr,stjdy,sthr,stmn,stsec
      write(entime,1000) enyr,enjdy,enhr,enmn,ensec
1000  format(i4,',',i3.3,',',2(i2.2,':'),f7.4)
      write(stname,'(i3)') debug
      if (sttime(16:16) .eq. ' ') sttime(16:16) = '0'
      if (entime(16:16) .eq. ' ') entime(16:16) = '0'

c     Read in station information for this CD, then find out which version
c        of the station is being referred to.
      if (cdfprv .ne. cdfil .or. snam .ne. sname(1)) then
	 call seedhd(snam,cdfil)
	 cdfprv = cdfil
      endif

c     Debug > 1 gets a report of what was found for the station in the SEED
c        volume.
      if (debug.gt.1) then
90001    format('Station: ',a5,' ch: ',i2,' sch:',15(1x,i2))
90002    format('  id: ',a3,1x,a2,' sch: ',i2,6(1x,f6.2))
         do i=1,sn
	    write(*,90001) sname(i),snch(i),(snsch(i,j),j=1,snch(i))
	    do j=1,snch(i)
	       write(*,90002) schid(i,j)(1:3),schid(i,j)(4:5),
     +                        snsch(i,j),(srate(i,j,k),schaz(i,j,k),
     +                                    schdip(i,j,k),k=1,snsch(i,j))
            enddo
         enddo
      endif

      ixst = 0
      do i=1,sn
	 exists = inspan(sst(i),sse(i),
     +                   styr,stmon,stdy,sthr,stmn,stsec,
     +                   enyr,enmon,endy,enhr,enmn,ensec)
	 if (exists .or. sn .eq. 1) ixst = i
      enddo
      if (ixst .eq. 0) then
	 write(0,*) '**EXTSEED:  Can''t find station/channel - '//
     +     'log file info right?'
	 return
      endif

c     Read in time span information for this seed volume to seek data blocks.
c       Find time span index, see if it matches the time range of interest,
c       and compile data blocks pertaining to that range.
      ixts = 0
      ixdf = 0
      call posset(possta,1,8)
      do
         bklen = getbkt(possta,bkette)
	 if (bklen .le. 0) then
	    write(0,*) '**EXTSEED:  No time span index in volume.'
	    return
	 endif
	 if (bkid .eq. '012') then
90121       format(3x,4x,i4)
90122       format(i6)
            read(bkette,90121,iostat=ios) nspan
            if (ios .ne. 0) then
               write(0,*) '**EXTSEED:  Corrupted 012 header!'
	       return
            endif
	    j = 12
	    do i=1,nspan
	       j = j + gtstr(timbeg,bkette(j:))
	       j = j + gtstr(timend,bkette(j:))
	       read(bkette(j:),90122,iostat=ios) ix
	       if (ios .ne. 0) then
	          write(0,*) '**Corrupted 012 blockette!'
		  return
	       endif
	       exists = inspan(timbeg,timend,
     +                   styr,stmon,stdy,sthr,stmn,stsec,
     +                   enyr,enmon,endy,enhr,enmn,ensec)
               if (exists) then
	          ixts = ixts + 1
		  if (ixts.eq.nssmax+1) then
		     write(0,*) '**EXTSEED:  Too many time spans -',
     +                  nssmax,' - some skipped.'
                  endif
		  if (ixts.le.nssmax) stgix(ixts) = ix
	       endif
	       j = j + 6
	    enddo
	 else if (bkid .eq. '030') then
c           030 - extract name and format id; translate name to upper case
90301       format(i4)    
            ixdf = ixdf + 1
	    if (ixdf.eq.ndfmax+1) then
	       write(0,*) '**EXTSEED:  Too many data formats -',
     +            ndfmax,' - some skipped.'
            endif
	    if (ixdf.le.ndfmax) then
	       j = 8 + gtstr(dfname(ixdf),bkette(8:))
	       read(bkette(j:),90301,iostat=ios) dfid(ixdf)
	       if (ios .ne. 0) then
	          write(0,*) '**Corrupted 030 blockette!'
		  return
	       endif
	       j = lenb(dfname(ixdf))
	       do i=1,j
	          v1 = dfname(ixdf)(i:i)
		  if (ichar(v1).ge.ichar('a') .and.
     +                ichar(v1).le.ichar('z')) then
                     dfname(ixdf)(i:i) =
     +                  char(ichar(v1)-ichar('a')+ichar('A'))
                  endif
	       enddo
	       if (debug.gt.1) then
	          write(*,*) ixts,ixdf,
     +               dfname(ixdf)(1:lenb(dfname(ixdf)))
	       endif
	    endif
	 else
	    if (ixts.gt.0 .and. ixdf.gt.0) exit
	 endif
	 call posadd(possta,bklen)
      enddo

      ndb = 0
      do n=1,ixts
	 call posset(possta,stgix(n),0)
         bklen = 8
         do
	    call posadd(possta,bklen)
	    bklen = getbkt(possta,bkette)
	    if (bklen .le. 0) go to 50
	    if (bkid .eq. '070') then
	       continue
	    else if (bkid .eq. '071') then
	       continue
	    else if (bkid .eq. '072') then
	       continue
	    else if (bkid .eq. '073') then
	       continue
	    else if (bkid .eq. '074') then
90010          format('Data for ',a3,1x,a2,' fmt ',a,' in blocks',
     &                2(1x,i6))
90741          format(3x,4x,a5,a2,a3,a23)
90742          format(i6,i2,a23)
               read(bkette,90741,end=37,err=37) vsta,vloc,vchan,v22
	       j = 18 + gtstr(timbeg,v22)
	       read(bkette(j:),90742,end=37,err=37)
     &            ixdbeg,i2,v22
               j = j + 8 + gtstr(timend,v22)
	       read(bkette(j:),90742,end=37,err=37) ixdend,i2
	       exists = inspan(timbeg,timend,
     +                   styr,stmon,stdy,sthr,stmn,stsec,
     +                   enyr,enmon,endy,enhr,enmn,ensec)
               if (exists .and. vsta .eq. snam) then
c                 Check if station name is desired, then check if sample
c                    rate is wanted.
                  i = ixctbl(vsta,sn,sname)
		  if (i .gt. 0) then
		     do j=1,snch(i)
			if (vchan .eq. schid(i,j)(1:3) .and.
     +   		    vloc  .eq. schid(i,j)(4:5)) then
			   do k=1,snsch(i,j)
			      if (abs(srate(i,j,k)-sr)/sr
     +                            .le. srtol) then
c                                Sample rate ok -- add data to blocks list
				 ix = ixtbl(sform(i,j,k),ixdf,dfid)
				 if (ix.gt.0) then
				    v22 = dfname(ix)
				 else
				    v22 = '(unknown)'
				 endif
				 ix = lenb(v22)
				 if (debug.gt.0) then
				    write(*,90010) vchan,vloc,
     +                                 v22(1:ix),
     +                                 ixdbeg,ixdend
				 endif
				 ndb = ndb + 1
				 if (ndb.eq.ndbmax+1) then
	                            write(0,'(2a,i3,4a)')
     +                                 '**EXTSEED:  Too many data ',
     +                                 'blocks (>',ndbmax,
     +                                 ') for ',vsta,vloc,';'
	                            write(0,'(a)')
     +                                 '**some data omitted.'
				 endif
				 if (ndb.le.ndbmax) then
				    dbbeg(ndb) = ixdbeg
				    dbend(ndb) = ixdend
				    dbent(ndb) = 10000*i + 100*j + k
				 endif
			      endif
			   enddo
			endif
		     enddo
		  endif
	       endif
	    else
	       exit
	    endif
         enddo
      enddo
      go to 50

37    continue
      write(0,*) '**Corrupted 074 blockette -- skipping.'
      return

c     Prepare to read data.  Read data blockettes identified and extract
c        data.

50    continue

c     Retrieve station location information; set network part of info
      stntin = diget(snid(ixst),nga,ixga,lnga,kyga)
      stntin = snetcd(ixst) // '~'
      stlat = slat(ixst)
      stlon = slon(ixst)
      stelev = sel(ixst)
c     Use vertical component sample rate from station.  May differ from
c        requested one if non-integral and can't be represented in log file
c        exactly.
      do i=1,snch(ixst)
         do j=1,snsch(ixst,i)
	    if (abs(schdip(ixst,i,j)) .ge. 60.0) then
	       if (abs(srate(ixst,i,j)-sr)/sr .le. srtol) then
	          sr=srate(ixst,i,j)
	       endif
	    endif
	 enddo
      enddo

      ndmx = min(nint((t2-t1)*60*sr),ndim)

c     gain=0 indicates no data in buffer in case kk=10
      do i=1,3
         gain(i) = 0.0
	 nn(i) = 0
	 lgap(i) = .false.
	 lovr(i) = .false.
      enddo
      do i=1,ndmx
         timebuf(i) = 0.0
      enddo

c     Look over all sets of data blockette ranges.
      do n=1,ndb
         ixs = dbent(n)/10000
	 jxs = mod(dbent(n)/100,100)
	 kxs = mod(dbent(n),100)
	 ix = ixtbl(sform(ixs,jxs,kxs),ixdf,dfid)
	 if (ix .le. 0) then
	    skey = '(unknown)'
	 else
	    skey = dfname(ix)
	 endif

C        Read channel info to determine what to call channel.  Since
C           non-standard names are, well, non-standard, we use the following
C           heuristics to assign channels:
C           - if orientation within 30 degrees of vertical (up or down), call
C             it a vertical channel (component = 1)
C           - otherwise, if it is within 45 degrees of north, call it north
C             (component = 2)
C           - otherwise call it east (component = 3)
	 if (abs(schdip(ixs,jxs,kxs)) .ge. 60.0) then
	    ic = 1
	    co(1) = -schdip(ixs,jxs,kxs)
	 else
	    dtheta = 180./pi*
     &         acos(abs(cos(schaz(ixs,jxs,kxs)*pi/180.) *
     &                  cos(schdip(ixs,jxs,kxs)*pi/180.)))
	    if (dtheta .le. 45.0) then
	       ic = 2
	    else
	       ic = 3
	    endif
	    co(ic) = schaz(ixs,jxs,kxs)
	 endif
	 chan(ic) = schid(ixs,jxs)(1:3)
	 if (snsch(ixs,jxs) .gt. 1) chan(ic)(3:3) = zne23(kxs:kxs)
	 chgot(ic) = chan(ic)(1:3) // schid(ixs,jxs)(4:5)

	 nadd = 2**(ic-1)
	 do 100 i=dbbeg(n),dbend(n)
            in = getdat(i, lrecl, dbuf)
	    if (in.ne.0) then
	       write(0,*) '**EXTSEED: Read err block ',i,' for ',sdkey
	       go to 100
	    endif

	    ixb = 1
	    do while (ixb.lt.lrecl)
	       is = rseed(skey,kxs,dbuf(ixb:lrecl),ndlim,samp,in,tsamp)
	       ixb = ixb + is
               diff = diftime(sttim,tsamp)
	       is = 1 + int(diff*sr)
	       if (debug .gt. 1) then
	          write(0,*) schid(ixs,jxs),' read ',in,' points ',
     &               is,' to ',is+in-1
	       endif

	       ix = 1
	       if (is .le. 0) then
		  in = max(in + is - 1,0)
		  ix = 1 - is
		  is = 1
	       endif
	       if (is+in-1 .gt. ndmx) in = max(ndmx-is,0)
	       if (in .le. 0) cycle

	       nn(ic) = nn(ic) + in

C              Check for clock drift that might cause sample start to be
C              in wrong bin.  Shift data index as appropriate and copy.
	       sthis = mod(int(timebuf(is))/nadd,2) .ne. 0
	       if (is .ge. 2) then
	          sprev = mod(int(timebuf(is-1))/nadd,2) .ne. 0
	       else
	          sprev = .false.
	       endif
	       if (is .lt. ndmx) then
	          snext = mod(int(timebuf(is+1))/nadd,2) .ne. 0
	       else
	          snext = .false.
	       endif
	       if (sthis) then
C                 Check for one sample overlap
                  if (sprev .and. .not.snext) is = is+1
	       else
C                 Check for one sample gap
                  if (.not.sprev) then
	             if (is.eq.2) then
	                is = 1
		     else if (is.gt.2) then
		        sprev = mod(int(timebuf(is-2))/nadd,2) .ne. 0
		        if (sprev) is = is - 1
		     endif
	          endif
	       endif
	       do j=0,in-1
	          if (ic .eq. 1) zbuf(is+j) = samp(ix+j)
	          if (ic .eq. 2) buf2(is+j) = samp(ix+j)
	          if (ic .eq. 3) buf3(is+j) = samp(ix+j)
	       enddo

C              Check if there are gaps or (gasp) overlaps in the data.
               do j=is,is+in-1
	          if (mod(int(timebuf(j))/nadd,2) .ne. 0) then
	             lovr(ic) = .true.
                  else
	             timebuf(j) = timebuf(j) + nadd
	          endif
	       enddo
	    enddo
100      continue
      enddo

C     Report data overlaps and check for data gaps.
      do ic=1,3
         if (nn(ic).gt.0) then
	    nadd = 2**(ic-1)
	    ixb = 0
	    ixe = 0
	    do n=0,ndmx-1
	       if (ixb.eq.0) then
		  if (mod(int(timebuf(1   +n))/nadd,2) .ne. 0) then
		     ixb = n+1
		     if (ixe .gt. 0) go to 101
		  endif
	       endif
	       if (ixe.eq.0) then
		  if (mod(int(timebuf(ndmx-n))/nadd,2) .ne. 0) then
		     ixe = ndmx-n
		     if (ixb .gt. 0) go to 101
		  endif
	       endif
	    enddo
101         continue
	    if (ixb.le.0 .or. ixe.le.0) pause '**EXTSEED:  Logic error.'

	    nn(ic) = ixe - ixb + 1
	    nd = 0
	    sum = 0.0
	    do n=ixb,ixe
	       if (mod(int(timebuf(n))/nadd,2) .ne. 0) then
	          if (ic .eq. 1) sum = sum + zbuf(n)
	          if (ic .eq. 2) sum = sum + buf2(n)
	          if (ic .eq. 3) sum = sum + buf3(n)
		  nd = nd + 1
	       else
	          lgap(ic) = .true.
	       endif
	    enddo
	    sum = sum/nd
            if (lgap(ic)) then
	       do n=ixb,ixe
	          if (mod(int(timebuf(n))/nadd,2) .eq. 0) then
		     if (ic .eq. 1) zbuf(n) = sum
		     if (ic .eq. 2) buf2(n) = sum
		     if (ic .eq. 3) buf3(n) = sum
		  endif
	       enddo
	    endif
c           Pad front of data if needed
	    do n=1,ixb-1
	       if (ic .eq. 1) zbuf(n) = sum
	       if (ic .eq. 2) buf2(n) = sum
	       if (ic .eq. 3) buf3(n) = sum
	    enddo
c           Pad end of data if needed
	    do n=ixe+1,ndmx
	       if (ic .eq. 1) zbuf(n) = sum
	       if (ic .eq. 2) buf2(n) = sum
	       if (ic .eq. 3) buf3(n) = sum
	    enddo
	    nn(ic) = ndmx
	 endif
         if (lgap(ic)) then
	    n = index(stcomm,'$')
            stcomm(n:) = zne23(ic:ic) // ' component data gap; $'
	 endif
         if (lovr(ic)) then
	    n = index(stcomm,'$')
            stcomm(n:) = zne23(ic:ic) // ' component data overlap; $'
	 endif
      enddo

C     At the end of this segment, all traces will begin with time t1 and
C        will be suitably padded so that differences in actual start times
C        and t1 will be padded with the mean trace value.
      if (n2 .eq. 0 .and. n3 .eq. 0 .and. n1 .ne. 0) then
	 ncmp = 1
	 npts = n1
      else if (n1+n2+n3 .gt. 0) then
	 ncmp = 3
	 npts = max(n1,n2,n3)
	 if (n1*n2*n3 .eq. 0) then
	    do i=1,npts
	       if (n1 .eq. 0) zbuf(i) = 0
	       if (n2 .eq. 0) buf2(i) = 0
	       if (n3 .eq. 0) buf3(i) = 0
	    enddo
	    do i=1,3
	       if (chan(i) .ne. ' ') n=i
	    enddo
	    do i=1,3
	       if (chan(i) .eq. ' ') chan(i)=chan(n)(1:2)//zne23(i:i)
	    enddo
	 endif
      else
	 ncmp = 0
	 npts = 0
      endif

c     Report time correction for GEOSCOPE CD data LH* components.
      if (cdfile .eq. 'seed seed.dat') then
C        Do GEOSCOPE time corrections
	 if (fixgeo(snam,chan,
     +              styr,stjdy,sthr,stmn,stsec,
     +              enyr,enjdy,enhr,enmn,ensec,
     +              ncor,timcor,cor)) then
	    ix = index(stcomm,'$')
	    stcomm(ix:) = '1 second time correction '//
     +            'as per G. Roult/26 Jun. 1995. $'
         endif
      endif

c     Set up time buffer.  The one-sample offset is required in the mainline
c        code, it seems.
      do i=1,npts
	 timebuf(i) = t1 + i/(sr*60.0)
      enddo

c     Retrieve station comments and response information.  We only pick off
c     the information from the station records that pertain to the time window
c     of interest.  We do this through the variable exists and through the
c     subroutine called inspan.  The algorithm relies on any station update
c     being shorter span than their base span.  The heuristic chosen is that
c     if two station time spans apply to the same time window, the one which
c     is more restrictive in time prevails, and the less restrictive (one
c     that is open-ended) is superseded.
      exists = .false.
      cstp = ' '
      cetp = ' '
      chnow = ' '
      call posset(possta,shdr(ixst),0)
      bklen = 8
150   continue
	 call posadd(possta,bklen)
	 bklen = getbkt(possta,bkette)
	 if (bklen .le. 0) go to 159
	 if (bkid .eq. '050') then
	    if (chnow .ne. ' ') go to 159
	 else if (bkid .eq. '051' .or. bkid .eq. '059') then
90511       format(3x,4x,a23)
            ii = 8 + gtstr(cst,bkette(8:))
	    ii = ii + gtstr(cet,bkette(ii:))
	    read(bkette(ii:ii+3),'(i4)',iostat=ios) ix
	    if (ios .ne. 0) then
	       write(0,*) '**EXTSEED:  Invalid comment code.'
	       go to 150
	    endif
	    if (bkid .eq. '059' .and. .not. exists) go to 150
	    if (inspan(cst,cet,
     +                   styr,stmon,stdy,sthr,stmn,stsec,
     +                   enyr,enmon,endy,enhr,enmn,ensec)) then
	       stcomm0 = diget(ix,ncd,ixcd,lncd,kycd)
	       ix = lenb(stcomm0)
	       if (stcomm0(ix:ix) .ne. '.') then
		  ix = ix + 1
		  stcomm0(ix:ix) = '.'
	       endif
	       ii = index(stcomm,'$')
	       stcomm(ii:ii+ix+1) = stcomm0(1:ix)//' $'
	       if (debug.gt.1) write(*,*) bkid,' ',stcomm0(1:ix)
	    endif
	 else if (bkid .eq. '052') then
C           Read channel info to determine what to call channel.  Since
C              non-standard names are, well, non-standard, we use the following
C              heuristics to assign channels:
C              - if orientation within 30 degrees of vertical (up or down), call
C                it a vertical channel (component = 1)
C              - otherwise, if it is within 45 degrees of north, call it north
C                (component = 2)
C              - otherwise call it east (component = 3)
90521       format(7x,a2,a3,i4,i3,a31)
90522       format(3x,3x,f10.0,f11.0,f7.0,3f5.0,i4,i2,2e10.0)
	    read(bkette,90521,iostat=ios)
     &         chnow(4:5),chnow(1:3),isc,iin,v30
	    if (ios .ne. 0) pause '**EXTSEED: 052 now wrong?!'
            is = ixctbl(snam,sn,sname)
	    if (is .eq. 0) pause '**EXTSEED: Station now unknown?!'
C           Extract effective time range, skip if outside.
	    j = index(v30,'~') + 20
	    k = gtstr(v50,bkette(j+79:))
	    j = k + j + 79
	    k = gtstr(effbeg,bkette(j:))
	    k = gtstr(effend,bkette(j+k:))
	    if (.not.valid(effbeg,effend)) then
C              Defeat header re-read heuristic if some channel defs are
C                 time dependent
               exists = .false.
	       cdfprv = ' '
	       go to 150
	    endif
	    if (snch(is) .le. 0) go to 150
	    do j=1,snch(is)
	       if (chnow .eq. schid(is,j)) go to 157
	    enddo
            pause '**EXTSEED:  052 channel now unknown?!'
157         continue
            ic = j
            if (snsch(is,ic).gt.1 .and. chnow(3:3).ne.' ') then
	       write(0,*) '**EXTSEED:  Multiplex subch. ID nonblank'
	    endif
	    isc = max(isc,1)
            if (isc .gt. snsch(is,ic))
     &         pause '**EXTSEED: 052 subch. bad'
	    j = 20 + gtstr(stcomm0,bkette(20:))
	    read(bkette(j:),90522,iostat=ios)
     &         xlat,xlon,xel,xdep,
     &         xchaz,xchdip,iform,idatl,xrate,xdrift
	    if (ios .ne. 0) pause '**EXTSEED: 052 now wrong?!'
	    if (90.0-abs(xchdip) .le. 30.0) then
	       ich = 1
	    else
	       dtheta = 180./pi*
     &            acos(abs(cos(xchaz*pi/180.)*cos(xchdip*pi/180.)))
	       if (dtheta .le. 45.0) then
		  ich = 2
	       else
		  ich = 3
	       endif
	    endif
	    if (snsch(is,ic).gt.1) chnow(3:3) = zne23(ich:ich)
	    exists = .false.
	    do j=1,ncmp
	       exists = exists .or. chnow .eq. chgot(j)
            enddo
	    if (.not.exists) go to 150
C           Add or replace instrument information: Network & loc ID
	    ii = index(stntin,'~')
	    if (ii.eq.0) ii = lenb(stntin)+1
	    stntin(ii:ii) = '~'
	    stntin(ii+1:) = diget(iin,nga,ixga,lnga,kyga)
	    ii = lenb(stntin)
	    if(stntin(ii:ii) .ne. '~') then
	       stntin(ii+1:) = '~' // chnow(4:5)
	    else
	       stntin(ii+1:) = chnow(4:5)
	    endif
	    j = j + 79 + gtstr(stcomm0,bkette(j+79:))
	    j = j + gtstr(cst,bkette(j:))
	    j = j + gtstr(cet,bkette(j:))
C           if (cet(1:1) .eq. ' ') cet = '2500,365'
	    exists = inspan(cst,cet,
     +                   styr,stmon,stdy,sthr,stmn,stsec,
     +                   enyr,enmon,endy,enhr,enmn,ensec)
C           Don't accept if this ending was unspecified and previous wasn't
 	    if (exists .and. cet .eq. ' ' .and. cetp .ne. ' ')
     +         exists = .false.
 	    if (exists) then
 	       cstp = cst
 	       cetp = cet
 	    endif
	 else if (bkid .eq. '053') then
C           Read pole-zero blockettes, accumulating the poles and zeros into
C              a single, longer response, and amalgamating the gains.
90531       format(3x,4x,1x,i2,i3,i3,e12.5,e12.5,i3)
            if (.not. exists) go to 150
	    read(bkette,90531,iostat=ios)
     +         ix,iin,iou,azero,fnorm,ii
	    if (ios .ne. 0) then
151            continue
	       write(0,*) '**EXTSEED:  Invalid pole-zero blockette.'
	       go to 150
	    endif
	    ix = nz(ich)
	    if (ix+ii .gt. npzmx) then
	       write(0,*) '**EXTSEED:  Too many zeros: ',ix+ii,
     +            ' max ',npzmx,' ignoring rest.'
                  goto 150
	    endif
	    nz(ich) = nz(ich) + ii
	    do 153 i=44,43+48*ii,48
	       read(bkette(i:),'(2e12.5)',iostat=ios) rr,ri
	       if (ios .ne. 0) go to 151
	       ix = ix + 1
	       zeros(ix,ich) = CMPLX(rr,ri)
153         continue
            read(bkette(i:i+2),'(i3)',iostat=ios) ii
	    if (ios .ne. 0) go to 151
	    ix = np(ich)
	    if (ix+ii .gt. npzmx) then
	       write(0,*) '**EXTSEED:  Too many poles: ',ix+ii,
     +            ' max ',npzmx,' ignoring rest.'
                  goto 150
	    endif
	    np(ich) = np(ich) + ii
	    do 155 i=i+3,i+2+48*ii,48
	       read(bkette(i:),'(2e12.5)',iostat=ios) rr,ri
	       if (ios .ne. 0) go to 151
	       ix = ix + 1
	       poles(ix,ich) = CMPLX(rr,ri)
155         continue
	    if (a0(ich) .eq. 0.0) then
	       a0(ich) = azero
	    else
	       a0(ich) = a0(ich)*azero
	    endif
	 else if (bkid .eq. '054') then
            if (.not.exists) go to 150
	    pzmsg = .true.
	 else if (bkid .eq. '055') then
	    continue
	 else if (bkid .eq. '056') then
	    continue
	 else if (bkid .eq. '057') then
	    continue
	 else if (bkid .eq. '058') then
90581       format(3x,4x,i2,e12.5,e12.5,i2)
            if (.not.exists) go to 150
	    read(bkette,90581,iostat=ios) ii,sgain,sfreq,ix
	    if (ios .ne. 0) then
	       write(0,*) '**EXTSEED:  Invalid gain/sens. blockette.'
	       go to 150
	    endif
	    if (ii .eq. 0) then
	       if (gain(ich) .ne. 0.0) then
		  if (ckgain(gain(ich),sgain,gtol)) then
		     if (.not.snsmsg) write(0,*)
     +               '***EXTSEED -- Sensitivity different '//
     +               'in response & header, response value used.'
		     snsmsg=.true.
		  endif
	       endif
	       gain(ich) = rvgain(sgain,ich,xchaz,xchdip)
	    else
	       stgain(ich) = stgain(ich)*sgain
	    endif
	 else if (bkid .eq. '060') then
90601       format(3x,4x,i2)
	    if (.not.exists) go to 150
	    read(bkette,90601,iostat=ios) i
	    if (ios .ne. 0) then
156            continue
	       write(0,*) '**EXTSEED:  Invalid resp. ref. blockette.'
	       go to 150
	    endif
	    fststg = .true.
	    ii = 10
	    do 158 i=1,i
	       read(bkette(ii:),'(2i2,100i4)',end=156,err=156)
     +            nstg,nsstg,(stgix(min(j,nssmax)),j=1,nsstg)
	       ii = ii + 2+2+4*nsstg
	       do 154 ix=1,min(nsstg,nssmax)
		  j = rdget(stgix(ix))
		  if (j .eq. 0) then
		     write(0,*) '**EXTSEED:  Missing response ' //
     +                  'dictionary info, key ',stgix(ix)
		  else
		     irblk = rdint(commrd(j))
		     if (irblk .eq. 043) then
			azero = commrd(j+1)
			if (a0(ich) .eq. 0.0) then
			   a0(ich) = azero
			else
			   a0(ich) = a0(ich)*azero
			endif
			ixz = rdint(commrd(j+3))
			ixp = rdint(commrd(j+4))
			jz = j+5
			jp = j+5+2*ixz
			do 152 k=1,max(ixz,ixp)
			   if (k .le. ixp) poles(np(ich)+k,ich) =
     +                        CMPLX(commrd(jp),commrd(jp+1))
			   if (k .le. ixz) zeros(nz(ich)+k,ich) =
     +                        CMPLX(commrd(jz),commrd(jz+1))
			   jp = jp + 2
			   jz = jz + 2
152                     continue
			nz(ich) = nz(ich) + ixz
			np(ich) = np(ich) + ixp
                     else if (irblk .eq. 048) then
C                       Only accept gain/sensitivity if this is the earliest
C                          in a set of stages, and if it has an associated
C                          frequency (which eliminates FIR stages, mostly).
			sgain = commrd(j+1)
			if (fststg .and. commrd(j+2) .ne. 0.0) then
			   if (gain(ich) .ne. 0.0) then
			      if (ckgain(gain(ich),sgain,gtol)) then
		                 if (.not.snsmsg) write(0,*) 
     +               '***EXTSEED -- Sensitivity different '//
     +               'in response & header, response value used.'
			         snsmsg = .true.
			      endif
			   endif
			   gain(ich) = rvgain(sgain,ich,xchaz,xchdip)
			   fststg = .false.
			else
			   stgain(ich) = stgain(ich)*sgain
			endif
		     else
			pzmsg = .true.
		     endif
		  endif
154            continue
158         continue
	 else if (bkid .eq. '061') then
	    if (.not.exists) go to 150
	    pzmsg = .true.
	 else
	    go to 159
	 endif
      go to 150

159   continue
C     Check that all gains specified, and bitch if there are discrepancies
C        between end-for-end gain and accumulated gains.  If stage gain is
C        1.0, we assume we weren't ever given any stage gains and so don't
C        bother to compare.
      do i=1,3
         if (nn(i) .gt. 0) then
	    if (gain(i) .ne. 0) then
	       if (ckgain(gain(i),stgain(i),2e-2) .and.
     +             stgain(i) .ne. 1.0) then
	          if (.not.etemsg) write(0,*) 
     +               '***EXTSEED -- End-to-end and stage-by-stage ' //
     +               'sensitivities differ by > 2%, ' //
     +               'end-to-end value used.'
	          etemsg = .true.
	       endif
	    else
	       gain(i) = stgain(i)
	    endif
	 endif
      enddo

c     Set up for rotation if necessary
      qazirad=(qazi-co(2))*3.1415927/180.
      cosphi=cos(qazirad)
      sinphi=sin(qazirad)

c     Check if same number of points read for all components; if so,
c     do component rotations if required.
      if (n1 .eq. n2 .and. n1 .eq. n3) then
	 npts = n1
	 do 350 i=1,npts
	    if (vert) then
c              -get vertical component
	       if (rot) zbuf(i)=zbuf(i)/gain(1)
	       yamp(1)=max(yamp(1),abs(zbuf(i)))
	    endif
	    if (kk.ne.5) then
c              -get horizontal components
	       if (rot) then
c                 -rotate horizontals 
		  y_n=buf2(i)
		  y_e=buf3(i)
		  y_n=y_n/gain(2)
		  y_e=y_e/gain(3)
		  buf2(i)= y_e*cosphi - y_n*sinphi        
		  buf3(i)= -(y_n*cosphi + y_e*sinphi)     
	       endif
	       yamp(2)=max(yamp(2),abs(buf2(i)))
	       yamp(3)=max(yamp(3),abs(buf3(i)))
	    endif
350      continue
      else if (ncmp.gt.0) then
	 npts = max(n1,n2,n3)
	 if (rot) then
	    write(0,*) '***WARNING:  COMPONENT DATA POINTS '//
     +      'UNEQUAL, ROTATION SKIPPED.'
	    ix = index(stcomm,'$')
	    stcomm(ix:) = 'COMPONENT DATA POINTS UNEQUAL, '//
     +         'ROTATION SKIPPED. $'
	    rot = .false.
	 endif
      endif

c     Flag incomplete response if encountered.
      if (pzmsg) then
	 ix = index(stcomm,'$')
	 stcomm(ix:) = 'Non-pole-zero stages present in response. $'
      endif
 
900   continue
901   if (rot) then
         do 902 ii=1,3
902      gain(ii)=1.0
         chan(2)(3:3)='T'
         chan(3)(3:3)='R'
	 co(2)=qazi + 90.0
	 co(3)=qazi + 180.0
      else
c        Check for and fix gain reversal.
	 if (gain(1) .lt. 0.0 .or. gain(2) .lt. 0.0 .or.
     +      gain(3) .lt. 0.0) then
	    do 903 ii=1,npts
	       if (gain(1) .lt. 0.0) zbuf(ii) = -zbuf(ii)
	       if (gain(2) .lt. 0.0) buf2(ii) = -buf2(ii)
	       if (gain(3) .lt. 0.0) buf3(ii) = -buf3(ii)
903         continue
	    stcomm0 = '   '
            do 904 ii=1,3
	       if (gain(ii) .lt. 0.0) then
		  gain(ii) = -gain(ii)
		  stcomm0(ii:ii) = zne23(ii:ii)
	       endif
904         continue
	    ii = lenb(stcomm0)
	    ix = index(stcomm,'$')
	    stcomm(ix:) = 'Gain reversal fixed: '//stcomm0(1:ii)//'.$'
	 end if
      end if
      stype = stntin
      stcomm(index(stcomm,'$'):) = ' '
      end

      logical function ckgain(g1,g2,tol)
C     CKGAIN -- Check whether gain values are materially different.
C
C     Assumes:
C        g1, g2 - Gain values to check.  Only absolute value is used.
C        tol - fractional gain tolerance.
C
C     Returns:
C        Function result - gains different.

      ckgain = abs((abs(g1)-abs(g2))/abs(g1)) .gt. tol
      end

      function rvgain(g,icmp,cazi,cdip)
C     RVGAIN -- Set gain, accounting for possible component reversal.
C
C     Assumes:
C        g - Gain value to set.
C        icmp - component index: 1 - z, 2 - n, 3 - e
C        cazi - azimuth (degrees from N)
C        cdip - dip (degrees from horizontal, +ve downward)
C
C     Returns:
C        Function result - gain multiplied by -1 to account for component
C           reversal.
      parameter (rad = 3.14159/180)
      logical rev

      if (icmp .eq. 1) then
         rev = cdip .ge. 0
      else if (icmp .eq. 2) then
         rev = cos(cazi*rad) .lt. 0
      else
         rev = sin(cazi*rad) .lt. 0
      endif

      if (rev) then
         rvgain = -g
      else
         rvgain = g
      endif
      end
     
      logical function valid(tst,tet)
C     VALID -- Is event time within data request interval?
C
C     Assumes:
C        tst, tet - Starting, ending time for relevant action,
C           SEED character format
C        extseed.com - contains present request time window
C
C     Returns:
C        Function result - if data time span overlaps action time span.

      character tst*(*), tet*(*)
      logical inspan
      include 'extseed.com'

      valid = inspan(tst,tet,
     +                   styr,stmon,stdy,sthr,stmn,stsc+1e-4*stth,
     +                   enyr,enmon,endy,enhr,enmn,ensc+1e-4*etth)
      end

      logical function inspan(cst,cet,
     +                   styr,stmon,stdy,sthr,stmn,stsc,
     +                   enyr,enmon,endy,enhr,enmn,ensc)
C     INSPAN -- Is event time within relevance interval?
C
C     Assumes:
C        cst, cet - Starting, ending time for relevant action,
C           SEED character format
C        styr, stmon, stdy, sthr, stmn, stsc -
C           year, month, day, hour, minute, second starting time for data
C           span
C        enyr, enmon, endy, enhr, enmn, ensc -
C           data ending time
C
C     Returns:
C        Function result - if data time span overlaps action time span.

      character cst*22,cet*22
      integer styr,stmon,stdy,sthr,stmn
      integer enyr,enmon,endy,enhr,enmn
      real stsc,ensc

9000  format(i4,1x,i3,1x,i2,1x,i2,1x,f7.4)
C     Following read stmt is to fix gfortran compiler bug in 20090604 release
C       Without it, following read will yield +Infinity values for esst when
C       string is valid numerical data.
      read(cst,'(15x,f7.4)',iostat=ios1) esst
      read(cst,9000,iostat=ios1) iyst,idst,ihst,imst,esst
      read(cet,9000,iostat=ios2) iyet,idet,ihet,imet,eset
      if (ios1+ios2 .ne. 0) then
         write(0,*) '**EXTSEED:  Invalid comment expiry.'
	 inspan = .true.
	 return
      endif
      if (cet .eq. ' ') then
	 iyet = 2500
	 idet = 365
      endif
      call timedif(iyst,1,idst,ihst,imst,esst,
     +             enyr,enmon,endy,enhr,enmn,ensc,dt)
      if (dt .lt. 0.0) then
         inspan = .false.
      else
	 call timedif(iyet,1,idet,ihet,imet,eset,
     +                styr,stmon,stdy,sthr,stmn,stsc,dt)
	 inspan = dt .le. 0.0
      endif
      end

      logical function fixgeo(snam,chan,
     +   styr,stjday,sthr,stmn,stsc,
     +   enyr,enjday,enhr,enmn,ensc,
     +   ncor,timcor,cor)
C     FIXGEO -- Return time correction for GEOSCOPE errors
C
C     GEOSCOPE time corrections are incomplete, see April 1996 message from
C     G. Roult (with CDs 19+20)
C
C     Called with:
C        styr - start year
C        stjday - start julian day
C        sthr - start hour
C        stmn - start minute
C        stsc - start second
C        enyr - end year
C        enjday - end julian day
C        enhr - end hour
C        enmn - end minute
C        ensc - end second
C        snam - station name
C        chan - channel name
C
C     Returns:
C        function result - true if correction made, false otherwise
C        ncor - new number of corrections
C        timcor - time correction times
C        cor - time correction value

      real cor(12)
      integer styr, stjday, sthr, stmn
      integer enyr, enjday, enhr, enmn
      integer dbeg, dend
      character snam*(*), chan(3)*(*), timcor(12)*(*)

      fixgeo = .false.
      dbeg = 1000*styr+stjday
      dend = 1000*enyr+enjday
      sbeg = stsc + 100*(stmn + 100*sthr)
      send = ensc + 100*(enmn + 100*enhr)
C     Do time correction for GEOSCOPE CD data LH* components. Vols 1-14
      if (chan(1)(1:2) .eq. 'LH'
     +    .and. dbeg .ge. 1988001
     +    .and. dbeg .le. 1990174) then
	 if (snam .eq. 'CAN' .or.
     +       snam .eq. 'HYB' .or.
     +       snam .eq. 'INU' .or.
     +       snam .eq. 'KIP' .or.
     +       snam .eq. 'SSB' .or.
     +       snam .eq. 'TAM' .or.
     +       snam .eq. 'UNM' .or.
     +       snam .eq. 'WUS') then
	    ncor = ncor + 1
	    write(timcor(ncor),'(i4,i4,2i2)') styr,stjday,sthr,stmn
	    cor(ncor) = 1.0
	    fixgeo = .true.
	 endif
      endif
C     Do time correction on all KIP data 1989169 - 1989289/22:00. Vols 7-8
      if (snam .eq. 'KIP' .and.
     +    dbeg .ge. 1989169 .and.
     +    dbeg .le. 1989289 .and. sbeg .le. 220000.0) then
	 ncor = ncor + 1
	 write(timcor(ncor),'(i4,i4,2i2)') styr,stjday,sthr,stmn
	 cor(ncor) = 1.0
	 fixgeo = .true.
      endif
C     Do time correction on CAN and KIP data vols 5-6
      if ((snam .eq. 'KIP' .and.
     +    dbeg .ge. 1989065 .and. dbeg .le. 1989153) .or.
     +   (snam .eq. 'CAN' .and.
     +    dbeg .ge. 1989065 .and. dbeg .le. 1989171)) then
	 ncor = ncor + 1
	 write(timcor(ncor),'(i4,i4,2i2)') styr,stjday,sthr,stmn
	 cor(ncor) = 1.0
	 fixgeo = .true.
      endif
C     Do time correction on CAN, INU, WFM data vol. 4
      if (dbeg .ge. 1989001 .and. dbeg .lt. 1989067) then
	 if ((snam .eq. 'CAN' .and. dbeg .lt. 1989067) .or.
     +       (snam .eq. 'INU' .and. dbeg .lt. 1989011
     +                        .and. sbeg .le. 022200.0) .or.
     +       (snam .eq. 'WFM' .and. dbeg .lt. 1989044
     +                        .and. sbeg .le. 165900.0) .or.
     +       (snam .eq. 'KIP' .and. dbeg .lt. 1989066
     +                        .and. sbeg .le. 235900.0)) then
	    ncor = ncor + 1
	    write(timcor(ncor),'(i4,i4,2i2)') styr,stjday,sthr,stmn
	    cor(ncor) = 1.0
	    fixgeo = .true.
	 endif
      endif
      end

      function ixtbl(key,n,keytbl)
C     IXTBL -- Look up an integer key in a list
C
C     Assumes:
C        key - value to be sought
C        n - number of items in table
C        keytbl - table of keys to search
C
C     Returns:
C        function result - index in list (1-n) or 0 if not found

      integer keytbl(n)

      do i=1,n
         if (key .eq. keytbl(i)) then
	    ixtbl = i
	    return
	 endif
      enddo
      ixtbl = 0
      end

      function ixctbl(key,n,keytbl)
C     IXCTBL -- Look up a character key in a table
C
C     Assumes:
C        key - value to be sought
C        n - number of items in table
C        keytbl - table of keys to search
C
C     Returns:
C        function result - index in list (1-n) or 0 if not found

      character keytbl(n)*(*), key*(*)

      do i=1,n
         if (key .eq. keytbl(i)) then
	    ixctbl = i
	    return
	 endif
      enddo
      ixctbl = 0
      end
