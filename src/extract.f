c subroutine EXTRACT gets data from CD
c
c    modified from P. Shearer version by D. W., 8/90
c    modified by K. Creager 9/7/90
c    modified by G. Helffrich to parse network-day tape log 10/26/92
c    modified by G. Helffrich to decode GEOSCOPE 24 bit format 7/26/95
c
c     Inputs:   cdfile  = (char*25) file name on CD
c               cddir   = (char*80) directory name for CD
c               nrec    = number of records in file
c               snam    = (char*5) station name
c               irecnum = record number to begin reading file
c               samrat  = desired sample rate (Hz)
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
c               debug   = if =1 Write verbose output 
c     Returns:  stlat   = station latitude
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
      subroutine EXTRACT(cdfile,cddir,nrec,snam,irecnum,samrat,
     &      kk,qyr,qmon,qdy,qhr,qmn,qsc,qazi,t1,t2,
     &      stlat,stlon,stelev,stype,stcomm,chan,
     &      npts,timebuf,zbuf,buf2,buf3,co,
     &      yamp,gain,units,a0,np,poles,nz,zeros,ncor,timcor,cor,debug)
      integer*2 ibuf2(1000),ib(10),ih(11)
      integer ier,debug,dim
      parameter (dim=100000)
      integer qyr,qmon,qdy,qhr,qmn,styr,stmon,stdy,sthr,stmn,kk,sgid
      real qsc,stsc,cor(12),cor0(12)
      integer np(3),nz(3),ncor,ncor0
      character*12 timcor(12),timcor0(12)
      character*4 chan(3),units*20,cunits*20
      character stname*4,snam*5,chr2*2,stype*8
      character cdfile*25,header*25,cdfil*128,cddir*80,file*25
      character schan(3)*4,statyp*8,styp*8,stcomm*(*)
      character ibuf3(660)*3
      equivalence (ibuf2(11),ibuf3(1))
      real timebuf(dim),zbuf(dim),buf2(dim),buf3(dim),yamp(3),gain(3)
      real a0(3),cgain(3),co(3)
      complex poles(30,3),zeros(30,3)
      logical sryes,vert,rot,sryn,nars
      real stlat,stlon,stelev,slat,slon,selev,samrat,qazi,samr
      real qazirad,cosphi,sinphi,y_e,y_n,sr,t,t1,t2,tbeg,tend,tlen 
      real ganran
      integer i,ii,kkk,nchan,ichan,cichan
      integer irec,nrec,irecnum,npts
      integer lendir,lenfil,lenb,i1
      character*80 chr80,idfmt,stcomm0*500
      logical byteswap
      integer*4 ibuf4(500)
      common /btswap/ byteswap
      parameter (nstamx=50)
      character nlstnm(nstamx)*4,nlsttp(nstamx)*8
      character stnm1*4,stnm2*4,sttyp1*8,sttyp2*8
      integer nlstct,nlstid(nstamx),nlfmt(nstamx)
      logical stagid
      external rename

      sryes=.false.
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
      npts=0
      nlstct=0
      do 19 ii=1,3
19    yamp(ii)=0.

c  open data file

      call rename(cdfile, file, header)
      if (header(1:4).eq.'NARS') then
        nars=.true.
      else
        nars=.false.
      endif
      lendir=lenb(cddir)
      lenfil=lenb(file)
      if (lendir.gt.0)then
         cdfil=cddir(1:lendir)//'/'//file(1:lenfil)
      else
         cdfil=file
      endif
      open (8,file=cdfil,access='direct',recl=2000,status='old',err=15)

c     Parse log to get station names, ID (the important part) and
c     the instrument type.  Loop over successive log records, each
c     with max 38 stations
      nlrec = 0
      kkk = 0
100   continue
         nlrec = nlrec + 1
	 if (byteswap.and.cdfile(1:4).eq.'5033') then
	    read (8,rec=1+nlrec,err=899) ibuf4
	    call SWAP32(ibuf4,ibuf2,500)
	 else
	    read (8,rec=1+nlrec,err=899) ibuf2
	 endif
	 write(chr80,'(40a2)',err=199) (ibuf2(i),i=1,40)
	 if (chr80(1:2) .ne. 'GL') then
	    write(0,*) '**',cdfil(1:index(cdfil,' ')-1),
     +      ': Incomplete network-day tape log!'
	    go to 899
	 endif

c        Format of this table changes depending on the CD.
c        The difference is in the position of the second column of
c        station entries - some CDs (the 1st ORFEUS one) has the
c        entries shifted by one column.  Detect these by the position
c        of the second 'ID' in the header line.  Early entries on the
c        NARS CD have the station ID field shifted right and the inst.
c        type field shifted left.  Ugh.
       write(chr80,'(40a2)',err=199) (ibuf2(i+5*40),i=1,40)
       if (chr80(19:) .eq. 'STATIONS ON THIS TAPE:') then
          read(chr80,'(13x,i4)',err=199) ii
          write(chr80,'(40a2)',err=199) (ibuf2(i+6*40),i=1,40)
          if (chr80(42:44) .eq. 'ID ' .or. chr80(41:) .eq. ' ') then
             idfmt = '(2(i3,2x,a4,3x,a8,1x,i4,15x))'
          else if (chr80(43:45) .eq. 'ID ') then
             idfmt = '(2(i3,2x,a4,3x,a8,1x,i4,16x))'
          else
             write(0,*) '**',cdfil(1:index(cdfil,' ')-1),
     +         ': Can''t find ID line for parsing station info.'
             go to 899
          endif
       else if (chr80(18:) .eq. 'STATIONS ON THIS TAPE:') then
          read(chr80,'(13x,i3)',err=199) ii
          idfmt = '(2(1x,i3,1x,a4,2x,a8,1x,i3,15x))'
	 else
	    write(0,*) '**',cdfil(1:index(cdfil,' ')-1),
     +      ': Can''t find STATION line for parsing station info.'
	    go to 899
	 endif

	 nlmax = 1 + (ii-1)/36
	 do 105 i=1,18
	    write(chr80,'(40a2)',err=199) (ibuf2(j+(6+i)*40),j=1,40)
	    read(chr80,idfmt,end=199,err=106) 
     +      nst1,stnm1,sttyp1,nfmt1,nst2,stnm2,sttyp2,nfmt2
	    if (stnm1 .ne. ' ') then
	       kkk = kkk + 1
	       call stains(stnm1,nst1,sttyp1,nfmt1,
     +            nlstct,nstamx,nlstnm,nlstid,nlsttp,nlfmt)
	    endif
	    if (stnm2 .ne. ' ') then
	       kkk = kkk + 1
	       call stains(stnm2,nst2,sttyp2,nfmt2,
     +            nlstct,nstamx,nlstnm,nlstid,nlsttp,nlfmt)
	    endif
105      continue
106      continue
      if (nlrec .lt. nlmax) go to 100

c     check that we parsed all the stations
      if (kkk .ne. ii) then
199      continue
	 write(0,*) '**NETWORK-DAY TAPE LOG FORMAT ERROR.'
	 go to 899
      endif
      go to 16
15       write(*,'(a,a)') ' CD file not found.  Trying to open :',
     .                     cdfil(1:lenfil+lendir+1)
         return
16    irec=irecnum-1

20    irec=irec+1
c      write(*,*)'in extract: reading record # ',irec
c !past end of file
      if (irec.gt.nrec) go to 900     
c kluge to skip record mostly full of backspace characters on day 85 11 8
      if (cdfile(1:19).eq.'5571:1985:NOV:85312'.and.irec.eq.3)irec=4
      if (byteswap.and.cdfile(1:4).eq.'5033') then
        read (8,rec=irec,err=899) ibuf4
        call SWAP32(ibuf4,ibuf2,500)
      else
        read (8,rec=irec,err=899) ibuf2
      endif
c     get first two characters
      write(chr2,'(a2)') ibuf2(1)        
c      write(*,*)chr2
c
c --- beginning of major if-then-else, branch on first two characters
c --- GDSN NETWORK-DAY TAPE LOG ------------------------------------
      if (chr2.eq.'GL') then
          go to 900
c
c ------- EVENT LOG -----------------------------------------------------
      else if (chr2.eq.'EV') then
         print *,'***ERROR in EXTRACT, EV header found'
c
c -------- STATION LOG ---------------------------------------------------
      else if (chr2.eq.'ST') then

         call stalog(ibuf2,stname,styp,sgid,slat,slon,selev,
     .               stcomm0,ncor0,timcor0,cor0,ier)

         if (stname.ne.snam) go to 900
         if (debug.eq.1) call dumpbuf(ibuf2,'SLOG')
         statyp=styp
         stelev=selev
         stlat=slat
         stlon=slon
	 stcomm=stcomm0
         ncor=ncor0
         do 321 i=1,12
            timcor(i)=timcor0(i)
            cor(i)=cor0(i)
321      continue
         if (debug .eq. 1) then
	    do 41 ii=13,16
	       i1=ii*40-39
	       write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
	       write(*,'(2a)') chr80,stname
41          continue
	    do 42 ii=1,ncor
42          write(*,'(a4,1x,a12,f8.3)')stname,timcor(ii),cor(ii)
         endif
	 co(1) = 90.0
	 co(2) = 0.0
	 co(3) = 90.0
	 if (nars) then
	    i = index(stcomm,'AZIMUTH CORRECTION:')
	 else
	    i = index(stcomm,'HORIZONTALS MISALIGNED BY')
	 endif
	 if (i .ne. 0) then
c           Horizontal component misalignment, parse and note in file header
            if (nars) then
c              Find first blank following text after key string
               i = i+20
	       sryn = .false.
	       do 45 ii=i,len(stcomm)
		  if (.not. sryn) then
		     if (stcomm(ii:ii) .ne. ' ') sryn=.true.
		  else
		     if (stcomm(ii:ii) .eq. ' ') go to 46
		  endif
45             continue
46             continue
	    else
	       ii = index(stcomm,'DEGREES (CLOCKWISE)')
	       i = i+25
	    endif
	    if (ii .ne. 0) then
	       ii = lenb(stcomm(i:ii-1))
	       if (ii .lt. 10) stcomm0(1:10-ii) = ' '
	       stcomm0(11-ii:10) = stcomm(i:)
	       read(stcomm0,'(f10.0)',iostat=ios) co(2)
	       if (ios .ne. 0) then
                  write(0,*) ' EXTRACT WARNING, unable to get '//
     +            'component misorientation.'
		  co(2) = 0.0
	       endif
	       co(3) = co(2) + 90.0
	    endif
	 endif
	 qazirad=(qazi-co(2))*3.1415927/180.
	 cosphi=cos(qazirad)
	 sinphi=sin(qazirad)
c
c -------- DATA LOG -------------------------------------------------------
      else if (chr2.eq.'DA') then

c  determine whether sample rate matches requested sample rate, read the gain
c  gain=1 if error reading it.

         call datlog1(ibuf2,snam,nars,samrat,sryn,samr,cichan,schan,
     .                cgain,cunits)
         if (sryn) then
c         -read instrument response only if sample rates match
c          we can safely copy the gain now since we know it is for the
c          proper sample rate
           do 52 i=1,3
	      gain(i) = cgain(i)
              chan(i) = schan(i)
52         continue
           units=cunits
           ichan=cichan
           if (debug.eq.1) call dumpbuf(ibuf2,snam)
           irec=irec+1
           if (irec.gt.nrec) go to 900
           if (byteswap.and.cdfile(1:4).eq.'5033') then
             read (8,rec=irec,err=901) ibuf4
             call SWAP32(ibuf4,ibuf2,500)
           else
             read (8,rec=irec,err=901) ibuf2
           endif
           if (debug.eq.1) call dumpbuf(ibuf2,snam)

c  get the normalization, poles and zeros of the instrument response

           call datlog2(ibuf2,snam,a0,np,nz,poles,zeros,ier)
c          write(*,*)'gain,normalization= ',(gain(jjj),a0(jjj),jjj=1,3)
         else
            irec=irec+1
         endif
         irec=irec+1
         if (irec.gt.nrec) go to 900
         if (byteswap.and.cdfile(1:4).eq.'5033') then
           read (8,rec=irec,err=899) ibuf4
           call SWAP32(ibuf4,ibuf2,500)
         else
           read (8,rec=irec,err=899) ibuf2
         endif
c
c --------- DATA ----------------------------------------------------------
      else
         do 60 i=1,10
60          ib(i)=ibuf2(i)
         call GDSNHD(ib,ih,sr)
         if (samrat.ne.sr) then
            if (sryes) go to 900
            go to 20
         end if
         sryes=.true.
c  !convert 1984 to 84
         styr=ih(3)
c  !kluge for Julian days
         stmon=1
         stdy=ih(4)
         sthr=ih(5)
         stmn=ih(6)
         stsc=float(ih(7))+float(ih(8))/1000.
         nchan=ih(9)
c         write(*,*)'nchan = ',nchan
         if (ichan.ne.nchan)
     &      print*,'***WARNING:  ichan.ne.nchan',ih(1),ichan,nchan
c   horizontals were requested but do not exist
         if ((nchan.ne.3).and.(kk.ne.5).and.(kk.ne.10)) go to 20
c         write(*,*)'in extract: reading data'
         call TIMEDIF(qyr,qmon,qdy,qhr,qmn,qsc,
     &                styr,stmon,stdy,sthr,stmn,stsc,tbeg)
         if (tbeg.gt.t2) go to 20
c   !length in minutes
         tlen=float(ih(11))/(float(nchan)*sr*60.)   
c   !predicted time for next block
         tend=tbeg+tlen                      
         if (tend.lt.t1) go to 20
c
         if (nchan.ne.3) then
c           gain=0 indicates no data in buffer in case kk=10
	    gain(2) = 0.0
	    gain(3) = 0.0
	 endif
         i = ih(1)
         if (.not.stagid(i,stnm1,stype,kkk,
     .                   nlstct,nlstnm,nlstid,nlsttp,nlfmt)) then
            print *,' EXTRACT ERROR - station code ',ih(1),
     .              ' not in network-day tape list'
            print *,' station, type, lat, lon, elev are: ', 
     .              snam,statyp,stlat,stlon,stelev
            return
         end if
	 if (stnm1 .ne. snam) then
c           JAS changes its name from JAS to JAS1 in 8513400.evt, but it isn't
c           an error.  Generate warning if name change occurs.
            print *,' EXTRACT WARNING - station code ',ih(1),
     .              ' changed its name from ',stnm1,' to ',snam
            print *,' station, type, lat, lon, elev are: ', 
     .              snam,statyp,stlat,stlon,stelev
	 end if
 
	 j = 1 - nchan
         do 70 i=11,10+ih(11),nchan
            ii=(i-11)/nchan
            t=tbeg+float(ii)/(sr*60.)
            if (t.lt.t1.or.t.gt.t2) go to 70
c            write(*,*)'reading data, npts =',npts
            npts=npts+1
            if (npts.gt.dim) then
             print*,'***ERROR in EXTRACT,',dim,' point limit exceeded'
              return
            end if
            timebuf(npts)=t
c
            j = j + nchan
            if (vert) then
c             -get vertical component
               zbuf(npts)=GANRAN(ibuf2(i),ibuf3(j),kkk)
               if (rot) zbuf(npts)=zbuf(npts)/gain(1)
               yamp(1)=max(yamp(1),abs(zbuf(npts)))
            endif
            if (kk.ne.5) then
c             -get horizontal components
               y_n=GANRAN(ibuf2(i+1),ibuf3(j+1),kkk)
               y_e=GANRAN(ibuf2(i+2),ibuf3(j+2),kkk)
               if (rot) then
c                -rotate horizontals 
                  y_n=y_n/gain(2)
                  y_e=y_e/gain(3)
                  buf2(npts)= y_e*cosphi - y_n*sinphi        
                  buf3(npts)= -(y_n*cosphi + y_e*sinphi)     
               else
                  buf2(npts)= y_n
                  buf3(npts)= y_e
               endif
               yamp(2)=max(yamp(2),abs(buf2(npts)))
               yamp(3)=max(yamp(3),abs(buf3(npts)))
            endif
70       continue
80       format (f10.4,2x,e12.5,i10)
c    ! end of major if-then-else
      end if                        
c
      go to 20
c
899   print*,'***error in CD read'
      go to 901
c
900   close (8)
901   if (rot) then
         do 902 ii=1,3
902      gain(ii)=1.0
         chan(2)(3:3)='T'
         chan(3)(3:3)='R'
	 co(2)=qazi + 90.0
	 co(3)=qazi + 180.0
      end if
c     !do not try to close file if end= or err= branch
      return           
      end


c
c
c  GDSNHD decodes the 10 16-bit word bcd header of the new gdsn tapes into integers
c  (modified into DCM from Woodward Apollo routine)
c  uses DCM ibits subroutine to extract bits
c  bit 15 =  left most bit
c  bit  0 = right most bit
      subroutine GDSNHD(ib,ih,sr)
      integer*2 ib(10),ih(11),ibits
      real*4 sr
c  station id
      ih(1)=ibits(ib(1),12,4)*100+ibits(ib(1),8,4)*10
     &      +ibits(ib(1),4,4)
c  sample rate
      sr   =ibits(ib(6),0,4)*10+ibits(ib(7),12,4)
     &      *10.**float(5-ibits(ib(6),4,4))
c  year
      ih(3)=1900+ibits(ib(1),0,4)*10+ibits(ib(2),12,4)
c  day
      ih(4)=ibits(ib(2),8,4)*100+ibits(ib(2),4,4)*10+ibits(ib(2),0,4)
c  hour
      ih(5)=ibits(ib(3),12,4)*10+ibits(ib(3),8,4)
c  minute
      ih(6)=ibits(ib(3),4,4)*10+ibits(ib(3),0,4)
c  second
      ih(7)=ibits(ib(4),12,4)*10+ibits(ib(4),8,4)
c  millisecond
      ih(8)=ibits(ib(4),4,4)*100+ibits(ib(4),0,4)*10
c  number of channels
      ih(9)=ibits(ib(6),12,4)*10+ibits(ib(6),8,4)
c  status (1=cal)
      ih(10)=ibits(ib(5),7,1)
c  number of samples
      ih(11)=ibits(ib(7),8,4)*100+ibits(ib(7),4,4)*10+ibits(ib(7),0,4)
      return
      end
c
c
c  GANRAN returns a floating point value translated from the 16
c  bit gain-range format of the various formats
c  (modified into DCM from Woodward Apollo routine)
c  (modified back to apollo (f77)) from Shearer by Creager)
c  (Why are all numbers divided by 1000?)
c  (Divide by 1000 removed Creager 9/11/90)
      function GANRAN(ii,iii,ifmt)
      character iii*3,iiii*4
      integer*2 i,ii,ibits,i2,j
      integer*4 i4,mant,itab(4),ifmt,itabgeo(16)
      equivalence (i4,iiii)
      save itab,itabgeo,i2
      data itab/1,4,16,128/
      data itabgeo/1,2,4,8,16,32,64,128,256,512,1024,2048,
     &             4096,8192,16384,32768/
      data i2/2/
      goto (1,2,3,4,5,6,7),ifmt
c sro + asro
1     i=ii
      mant=ibits(i,0,12)
      if(mant.ge.2048) mant=mant-4096
      i4=10-ibits(i,12,4)
      ganran=(lshift(mant,i4))
      return
c dwwssn
2     ganran=ii
      return
c rstn
3     i=ii
      j=lshift(i,i2)
      mant=ibits(j,2,14)-8191
      ganran=(mant*itab(ibits(i,14,2)+1))
      return
c  graefenberg
4     continue
      mant = ibits(ii,4,12)
      if (mant .ge. 2048) mant = mant-4096
      i4 = 12-ibits(ii,0,4)
      if (i4 .ge. 0) then
	 ganran = mant*float(itabgeo(1+i4))
      else
	 ganran = mant/float(itabgeo(1-i4))
      endif
      return
c  geoscope
5     i=ii
      ig=ibits(i,12,4)
c g = 2**ig
      g=float(itabgeo(ig+1))
      im=ibits(i,0,12)
      ganran=float(im-2048)/g
      return
c  echery -- different from sro/asro?
6     continue
      mant = ibits(ii,0,12) - 2048
      ganran = mant/float(itabgeo(1+ibits(ii,12,3)))
      return
c  geoscope -- 24 bit
7     iiii=char(0)//iii
      if (i4 .le. 8388608) then
	 ganran = float(i4)
      else
	 ganran = float(i4 - 16777216)
      endif
      return
      end
c
c
c  SWAP32 converts VAX I*4 into regular I*4
c  array is an array of 32-bit integers
c  n is the number of points in the integer*4 array
      subroutine SWAP32(array4,array2,n)
      integer*4 array4(n),i4
      integer*2 array2(2*n),i2(2),iscr
      equivalence (i4,i2)
      do 100 i=1,n
         i4=array4(i)
         iscr=ishftc(i2(1),8,16)
         i2(1)=ishftc(i2(2),8,16)
         i2(2)=iscr
         array2(2*i-1)=i2(1)
         array2(2*i)=i2(2)
100   continue
      return
      end


      subroutine repair(np,nz,poles,zeros)
c     makes sure that poles and zeros are complex conjugate
c     pairs
      integer np(3),nz(3)
      complex poles(30,3),zeros(30,3),cc
      real ac,di,rc,dr,cflag,apz,tol,rpz
      integer i,j,k,l,ip,iz
      tol=1.E-06
c     loop over channels   
      do 100 i=1,3
c        poles
         k=0
         ip=np(i)
c        loop over poles to find conjugate
         do 50 j=1,ip
            apz=aimag(poles(j,i))
            if (abs(apz).lt.tol) goto 50
c
            cc=conjg(poles(j,i))
            rc=real(cc)
            ac=aimag(cc)
            cflag=0
            do 60 l=1,ip
               rpz=real(poles(l,i))
               apz=aimag(poles(l,i))
               dr=rpz-rc
               di=apz-ac
c              conjugate found
               if ((abs(dr).lt.tol).and.(abs(di).lt.tol))
     &              cflag=1
60          continue
c
c           if conjugate not found add the extra pole
            if (cflag.eq.0) then
               k=k+1
               poles(ip+k,i)=cc
c               write(*,*)'conjugate pole added in routine REPAIR'
c               write(*,*)'pole =',cc
            endif
50       continue
         np(i)=np(i)+k
c        zeros
         k=0
         iz=nz(i)
c        loop over zeros to find conjugate
         do 150 j=1,iz
            apz=aimag(zeros(j,i))
            if (abs(apz).lt.tol) goto 150
c
            cc=conjg(zeros(j,i))
            rc=real(cc)
            ac=aimag(cc)
            cflag=0
            do 160 l=1,iz
               rpz=real(zeros(l,i))
               apz=aimag(zeros(l,i))
               dr=rpz-rc
               di=apz-ac
c              conjugate found
               if ((abs(dr).lt.tol).and.(abs(di).lt.tol))
     &                  cflag=1
160         continue
c
c           if conjugate not found add the extra zero
            if (cflag.eq.0) then
               k=k+1
               zeros(iz+k,i)=cc
c               write(*,*)'conjugate zero added in routine REPAIR'
c               write(*,*)'zero =',cc
            endif
150      continue
         nz(i)=nz(i)+k
c
100   continue
      return
      end

      subroutine dumpbuf(ibuf2,string)
c     interpret entire 2000 character buffer as
c     character data and dump it to the screen
      integer*2 ibuf2(1000)
      integer i,iii,i1,i2
      character chr80*80, string*4
      do 1 iii=1,25
         i2=iii*40
         i1=i2-39
         write(chr80,'(40a2)') (ibuf2(i),i=i1,i2)
         write(*,'(a,a,i3)') chr80,string,iii
1     continue
      return
      end

      subroutine stains(stname,stid,sttype,stfmt,
     +                  n,nmax,nlnm,nlid,nltype,nlfmt)
c     stains  --  Insert another station name into the list of stations
c                 in this file, checking for duplicates.
c
c     assumes:
c        stname - new station name
c        stid - new station id number
c        sttype - new station type
c        n - number of stations in list so far (updated on return)
c        nmax - max size of list
c        nlnm - list of names so far
c        nlid - list of ids so far
c        nltype - list of types so far
      character stname*(*),nlnm(nmax)*(*)
      character sttype*(*),nltype(nmax)*(*)
      integer n,nmax,stid,stfmt,nlid(nmax),nlfmt(nmax)
      logical fndnm,fndid

c     check list for duplicate names and id numbers
      fndnm = .false.
      fndid = .false.
      do 10 i=1,n
	 if (stname .eq. nlnm(i)) then
	    fndnm = .true.
	    if (stid .eq. nlid(i) .and. sttype .eq. nltype(i) .and.
     +          stfmt .eq. nlfmt(i)) then
	       write(0,*) '**Duplicate entry for ',stname
	    else
	       write(0,*) '**Conflicting entries for ',stname
	       write(0,101) 'old',nlid(i),nltype(i),nlfmt(i)
	       write(0,101) 'new',stid,sttype,stfmt
101            format('**  ',a,'id, type, fmt:',i4,1x,a,1x,i4)
	    endif
	 endif
	 if (stid .eq. nlid(i)) then
	    fndid = .true.
	    write(0,*) '**Conflicting station ID numbers for ',stname,
     +         ' and ',nlnm(i)
	    write(0,102) stname,stid,sttype,stfmt
	    write(0,102) nlnm(i),nlid(i),nltype(i),nlfmt(i)
102         format('**  ',a,':',i4,1x,a,1x,i4)
	 endif
10    continue
      if (.not. (fndnm .or. fndid)) then
	 if (n .ge. nmax) then
	    write(0,*) '**Too many stations in day tape log!'
	 else
	    n = n + 1
	    nlnm(n) = stname
	    nlid(n) = stid
	    nltype(n) = sttype
	    nlfmt(n) = stfmt
	 endif
      endif
      end

      logical function stagid(id,name,type,fmt,
     +   n,nlnm,nlid,nltype,nlfmt)
c     stagid  --  return station name, type and data format given GDSN
c                 station ID
c
c     assumes:
c        id - integer GDSN station id
c        n - number of stations in list
c        nlnm - station name array
c        nlid - station id number array
c        nltype - station type array
c     returns:
c        name - station name
c        type - station type
c        fmt - data format for decoding of digital data
c     function result:
c        true or false depending on outcome of search.
      character*(*) name,type,nlnm(n),nltype(n)
      integer fmt,nlid(n),nlfmt(n)

      do 10 i=1,n
	 if (id .eq. nlid(i)) then
	    name = nlnm(i)
	    type = nltype(i)
	    fmt = nlfmt(i)
	    stagid = .true.
	    return
	 endif
10    continue
      stagid = .false.
      end
