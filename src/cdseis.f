c CDSEIS 2.0
c  PROGRAM CDSEIS reads NEIC CDROM seismic wavefrom data and writes 
c  output to AH or ASC format files.  It currently runs on a SUN4,
c  and on an Apollo (operating system sr10.2), but because Apollo 
c  does not support XDR, only the ASC format can be written on the
c  Apollo.  This code was written by Peter Shearer to run on a 
c  Macintosh and modified by Ken Creager to run in a UNIX environment 
c  and output AH files.  9/20/90
c  Modified to read instrument response and clock corrections K. Creager 10/10/90
c  Modified to read 5033 correctly if bytes are swapped. If your first
c  NEIC disk is labeled '5033' then change the data statement below to
c  make byteswap=.true. if you have '5054' then change the data 
c  statement so byteswap=.false. 1/15/91
c
c   Modified to make a log.type output by L. Astiz 12/6/92
c
c  Modified to handle SEED volumes by G. Helffrich/U. Bristol 1992-2008
c  version 001: 27/6/1995
c      read seed volumes
c  version 002: 12/1/2008
c      native reading of seed data; don't use rseed as co-process
c  version 003: 04/1/2011
c      handle separately byte-swapped mseed headers and data
c      handle time-dependent, multiple responses in station records
c  version 004: 21/2/2023
c      output EVALRESP responses
c      improve error message on bad command inputs

c  Please report bugs, problems or comments to:
c      Ken Creager
c      Geophysics Program AK-50
c      University of Washington
c      Seattle, WA 98195
c      kcc@geops.geophys.washington.edu
c      (206) 685-2803

      program cdseis
      integer yr1,mon1,dy1,hr1,mn1,yr2,mon2,dy2,hr2,mn2,ier
      integer qyr,qmon,qdy,qhr,qmn,syr,smon,sdy,shr,smn,sjday
      integer myr,mmon,mdy,mhr,mmn,ncor
      real timest,cor(12),tcor,msc,ttref,delref,co(3)
      integer exyr,exmon,exdy,exhr,exmn,exyr1,exyr2,exmon1,exmon2
      real qcolat1,qcolon1,qcolat2,qcolon2,qdep1,qdep2,qmag1,qmag2
      real scolat1,scolat2,scolon1,scolon2,del1,del2,mtime
      real mcolat,mcolon,mcolat1,mcolat2,mcolon1,mcolon2,binmax
      real pcolat,pcolon,pdello,pdelhi
      real qazi1,qazi2,sazi1,sazi2,time1,time2,srat1,srat2,bin0,bininc
      integer nbin,ndim,debug,readresp
      parameter (ndim=100000)
      real xdata(10),timebuf(ndim),data1(ndim),data2(ndim),data3(ndim)
      real stnrat(3),yamp(3)
      parameter (nsrat=4, lbhdr=29, lbpsr=22, lbsiz=lbhdr+nsrat*lbpsr)
      character linebuf*(lbsiz),lineseg*(lbpsr-1)
      character code*4,line*116,cdfile*25,cddir*80
      character logdir*80,logfile*120,phsdir*80
      character stnamecur*5,stype*128,logname*40,scomm*500
      character phase*40,ttfile(10)*40,phsfil(10)*80,exfilenm*40,exst*5
      character*20 filecode(20),chdata(20),statid*5,chanid*4,phsid(10)*6
      character stname(10)*5,phasid*6,evntid*8,chan(3)*4,units*20
      character outfil*128,outdir*40,fname*128,wtype*2,outtyp*4,rtype*4
      character*12 timcor(12)
      integer nfilec,lenb,lendir,plflag,nsta
      logical quake_ok,needstinfo,readnext,gotresp
      integer np(3),nz(3)
      real gain(3),a0(3)
      complex poles(30,3),zeros(30,3)
      real slat,slon,selev,qlat,qlon,qdep,sazi,qazi,del,qmb,qms,qsc,ssc
      real qcolat,qcolon,scolat,scolon,sr,stnmin,gapmax
      real timebeg,timeend,toff,tt1,tt2,tdif1,tdif2,tday1,tday2,tt
      integer irecnum,kk,nrec,ierr,nerr,jtot,iextftype,iphase,nphase
      integer lenfil,npts,ntrace,nfound,icount,nsloss,iflag,nchan,nchar
      integer i,ii,iii,j,i1,i2,j1,j2,k1,k2
      integer istncol,ttwrite
      logical byteswap
      common /btswap/ byteswap

c
c default values follow
      data byteswap/.false./,needstinfo/.false./
      data yr1,mon1,dy1,hr1,mn1/1980,1,1,0,0/
      data yr2,mon2,dy2,hr2,mn2/2100,1,1,0,0/
      data qcolat1,qcolat2,qcolon1,qcolon2/0.,180.,0.,360./
      data scolat1,scolat2,scolon1,scolon2/0.,180.,0.,360./
      data mcolat1,mcolat2,mcolon1,mcolon2/0.,180.,0.,360./
      data pcolat,pcolon,pdello,pdelhi/0.,0.,0.,180./
      data qdep1,qdep2,qmag1,qmag2/0.,999.,0.0,9.9/
      data del1,del2,qazi1,qazi2,sazi1,sazi2/0.,180.,0.,360.,0.,360./
      data time1,time2,srat1,srat2/0.,20.,1.0,1.0/
      data gapmax/0.1/
      data stname(1),phase,nsta,nphase,nfilec/'all  ','none',1,1,1/
      data logname,exfilenm/'log5161','none'/
      data cddir,outdir/'/cdrom',' '/
      data logdir/'/data4/kcc/cdseis'/
      data phsdir/'/data4/kcc/cdseis'/
c     data outtyp/'ah  '/, wtype/'a '/
      data outtyp/'sac '/, wtype/'n '/, rtype/'RESP'/
      data ttwrite/0/
      data bin0,bininc,binmax,nbin/90.,5.,90.,0/
      data plflag,kk,debug/0,10,0/
      data stol/0.005/
      filecode(1)='ah.data'
c
c     call ieeeset('environment')
      print *,'CDSEIS 2.0 (grh-004)'
10    print *,'Enter commands'
      call ASK(code,line,nfound,xdata,nerr)
      if (nerr.eq.0) go to 11
      if (code.eq.'snam') go to 11
      if (code.eq.'logf') go to 11
      if (code.eq.'odir') go to 11
      if (code.eq.'file') go to 11
      if (code.eq.'phas') go to 11
      if (code.eq.'extf') go to 11
      if (code.eq.'cdir') go to 11
      if (code.eq.'ldir') go to 11
      if (code.eq.'pdir') go to 11
      if (code.eq.'otyp') go to 11
      if (code.eq.'wtyp') go to 11
      if (code.eq.'stol') go to 11
      if (code.eq.'resp') go to 11
      if (code.eq.'comm' .or. code(1:1).eq.'*') go to 11
         print *,'***ERROR in input, nerr= ',nerr
         go to 10
c
11    continue
      if (code.eq.'comm' .or. code(1:1).eq.'*') then
c        Comment command -- does nothing.
	 go to 10
      else if (code.eq.'dmin') then
53       format('***',a,' - Wrong number of input parameters')
         if (nfound.ne.5) then
            print 53,'DMIN'
            go to 10
         end if
         yr1=nint(xdata(1))
         mon1=nint(xdata(2))
         dy1=nint(xdata(3))
         hr1=nint(xdata(4))
         mn1=nint(xdata(5))
      else if (code.eq.'dmax') then
         if (nfound.ne.5) then
            print 53,'DMAX'
            go to 10
         end if
         yr2=nint(xdata(1))
         mon2=nint(xdata(2))
         dy2=nint(xdata(3))
         hr2=nint(xdata(4))
         mn2=nint(xdata(5))
      else if (code.eq.'qloc') then
         if (nfound.ne.4) then
            print 53,'QLOC'
            go to 10
         end if
         qcolat1=xdata(1)
         qcolon1=xdata(3)
         qcolat2=xdata(2)
         qcolon2=xdata(4)
      else if (code.eq.'qdep') then
         if (nfound.ne.2) then
            print 53,'QDEP'
            go to 10
         end if
         qdep1=xdata(1)
         qdep2=xdata(2)
      else if (code.eq.'qmag') then
         if (nfound.ne.2) then
            print 53,'QMAG'
            go to 10
         end if
         qmag1=xdata(1)
         qmag2=xdata(2)
      else if (code.eq.'sloc') then
         if (nfound.ne.4) then
            print 53,'SLOC'
            go to 10
         end if
         scolat1=xdata(1)
         scolon1=xdata(3)
         scolat2=xdata(2)
         scolon2=xdata(4)
         needstinfo=.true.
      else if (code.eq.'mloc') then
         if (nfound.ne.4) then
            print 53,'MLOC'
            go to 10
         end if
         mcolat1=xdata(1)
         mcolon1=xdata(3)
         mcolat2=xdata(2)
         mcolon2=xdata(4)
         needstinfo=.true.
      else if (code.eq.'resp') then
         call parse(line,chdata,iii)
         if (iii.ne.1) then
            print 53,'RESP'
            go to 10
         end if
         if (chdata(1).eq.'RESP'.or.chdata(1).eq.'resp') then
            rtype='RESP'
         elseif (chdata(1).eq.'PZ'.or.chdata(1).eq.'pz') then
            rtype='PZ'
         else
            print *,'***RESP - must be RESP or PZ--retry'
            go to 10
         endif
      else if (code.eq.'snam') then
         if ((line(1:3).eq.'ALL').or.(line(1:3).eq.'all')) then
            stname(1)='all'
            nsta=1
         else
            call parse(line,chdata,nsta)
            nsta=min(10,nsta)
            do 429 iii=1,nsta
429         stname(iii)=chdata(iii)
         endif
      else if (code.eq.'rang') then
         if (nfound.ne.2) then
            print 53,'RANG'
            go to 10
         end if
         del1=xdata(1)
         del2=xdata(2)
      else if (code.eq.'prng') then
	 if (nfound.ne.4) then
            print 53,'PRNG'
            go to 10
         end if
	 pcolat=xdata(1)
	 pcolon=xdata(2)
	 pdello=xdata(3)
	 pdelhi=xdata(4)
      else if (code.eq.'qazi') then
         if (nfound.ne.2) then
            print 53,'QAZI'
            go to 10
         end if
         qazi1=xdata(1)
         qazi2=xdata(2)
      else if (code.eq.'sazi') then
         if (nfound.ne.2) then
            print 53,'SAZI'
            go to 10
         end if
         sazi1=xdata(1)
         sazi2=xdata(2)
      else if (code.eq.'wind') then
         if (nfound.ne.2) then
            print 53,'WIND'
            go to 10
         end if
         time1=xdata(1)
         time2=xdata(2)
      else if (code.eq.'srat') then
         if (nfound.ne.2) then
            print 53,'SRAT'
            go to 10
         end if
         srat1=xdata(1)
         srat2=xdata(2)
      else if (code.eq.'sync') then
         if (nfound.ne.1) then
            print 53,'SYNC'
            go to 10
         end if
         gapmax=xdata(1)
      else if (code.eq.'stol') then
         if (nfound.ne.1) then
            print 53,'STAT'
            go to 10
         end if
         stol=0.01*xdata(1)
      else if (code.eq.'comp') then
         if (nfound.ne.1) then
            print 53,'COMP'
            go to 10
         end if
         kk=nint(xdata(1))
         if ((kk.lt.1).or.(kk.gt.10)) then
            print *,'***COMP - unkown value'
            goto 10
         endif
      else if (code.eq.'ttwr') then
         if (nfound.ne.1) then
            print 53,'TTWR'
            go to 10
         else if (xdata(1).ne.1..and.xdata(1).ne.0.) then
            print *,'***TTWR must be 0 or 1--retry'
            goto 10
         else
            ttwrite=nint(xdata(1))
         endif
      else if (code.eq.'stat') then
         print *,'Current parameter list follows'
         print 20,yr1,mon1,dy1,hr1,mn1
20       format ('DMIN: Minimum year,month,day,hour,minute:    ',i5,4i3)
         print 21,yr2,mon2,dy2,hr2,mn2
21       format ('DMAX: Maximum year,month,day,hour,minute:    ',i5,4i3)
         print 22,qcolat1,qcolat2
22       format ('QLOC: Minimum, maximum quake colatitude:     ',2f8.2)
         print 23,qcolon1,qcolon2
23       format ('    : Minimum, maximum quake colongitude:    ',2f8.2)
         print 24,qdep1,qdep2
24       format ('QDEP: Minimum, maximum quake depth (km):     ',2f8.2)
         print 25,qmag1,qmag2
25       format ('QMAG: Minimum, maximum quake magnitude:      ',2f8.2)
         print 26,scolat1,scolat2
26       format ('SLOC: Minimum, maximum station colatitude:   ',2f8.2)
         print 27,scolon1,scolon2
27       format ('    : Minimum ,maximum station colongitude:  ',2f8.2)
         print 29,del1,del2
29       format ('RANG: Minimum, maximum quake-station range:  ',2f8.2)
         print 38,pcolat,pcolon,pdello,pdelhi
38       format ('PRNG: Ref. point colatitude, colongitude:    ',2f8.2,
     +         /,'    : Minimum, maximum range from ref. point:',2f8.2)
         print 30,qazi1,qazi2
30       format ('QAZI: Minimum, maximum quake azimuth:        ',2f8.2)
         print 31,sazi1,sazi2
31       format ('SAZI: Minimum, maximum station azimuth:      ',2f8.2)
         print 40,mcolat1,mcolat2
40       format ('MLOC: Minimum, maximum midpoint colatitude:  ',2f8.2)
         print 41,mcolon1,mcolon2
41       format ('    : Minimum ,maximum midpoint colongitude: ',2f8.2)
         print 32,time1,time2
32       format ('WIND: Min, max minutes in data window:       ',2f8.2)
         print 33,srat1,srat2
33       format ('SRAT: Minimum, maximum sample rate (Hz):     ',2f8.2)
         print 39,gapmax
39       format ('SYNC: Maximum timing error in data (sec):    ',f8.4)
         print 58,100*stol
58       format ('STOL: Requested/actual sample rate tol. (%): ',f7.3)
         print 34,kk
34       format ('COMP: 1=zne 2=zrt 3=ne 4=rt 5=z 6=n 7=e 8=r 9=t',
     +     ' 10=any: ',i2)
         print 28,(stname(iii),iii=1,nsta)
28       format ('SNAM: Station names:                 ',10(a5,1x))
         print 35,logname(1:lenb(logname))
35       format ('LOGF: Log file name:                 ',a)
         print 48,logdir(1:lenb(logdir))
48       format ('LDIR: Log directory:                 ',a)
         print 43,cddir(1:lenb(cddir))
43       format ('CDIR: CD directory:                  ',a)
         print 44,outdir(1:lenb(outdir))
44       format ('ODIR: Output directory name:         ',a)
         print 36,(filecode(i)(1:lenb(filecode(i))), i=1,nfilec)
36       format ('FILE: Output file name:              ',10(a,1x))
         print 52,rtype
52       format ('RESP: Response type:                 ',a)
         print 49,plflag
49       format ('PLTS: Write plot.sh line to stnd out:',i4,' ')
         print 45,outtyp
45       format ('OTYP: Output format (sac, ah or asc):',a4,' ')
         print 46,wtype
46       format ('WTYP: a,w,n,x (apnd,ovrwrt,new,none):',a2,' ')
         print 47,bin0,binmax,bininc
47       format ('BINS: Bin min, max, increment:       ',3f8.2,' ')
         print 42,exfilenm(1:lenb(exfilenm))
42       format ('EXTF: External event list:           ',a)
         print 37,phase(1:lenb(phase))
37       format ('PHAS: Phase selection:               ',a)
         print 56,phsdir(1:lenb(phsdir))
56       format ('PDIR: Phase file directory:          ',a)
         print 55,ttwrite
55       format ('TTWR: Model time to extra 20 if=1:   ',i1,' ')
         print 51,debug
51       format ('DEBU: Write verbose output if =1:    ',i1,' ')
         print 57,nscan
57       format ('SCAN: Write scan.out if  nscan=1:    ',i1,' ')
      else if (code.eq.'help') then
         print *,'Available commands are:'
         print *,'DMIN DMAX QLOC QDEP QMAG SLOC RANG QAZI SAZI MLOC'
         print *,'WIND SRAT SYNC COMP SNAM LOGF LDIR FILE PLTS OTYP'
         print *,'WTYP BINS PHAS PDIR TTWR STAT HELP EXTF SCAN READ'
         print *,'PRNG RESP COMM *    QUIT'
      else if (code.eq.'logf') then
          logname=line
      else if (code.eq.'odir') then
          outdir=line
      else if (code.eq.'cdir') then
          cddir=line
      else if (code.eq.'ldir') then
          logdir=line
      else if (code.eq.'pdir') then
          phsdir=line
      else if (code.eq.'otyp') then
          if (line(1:3).ne.'asc'.and.line(1:2).ne.'ah'.and.
     &        line(1:3).ne.'sac') then
             print *,'***OTYP options are sac, asc or ah; retry'
          else
             outtyp=line(1:4)
          endif
      else if (code.eq.'wtyp') then
          if (line(1:1).ne.'a'.and.line(1:1).ne.'w'.and.
     .        line(1:1).ne.'n'.and.line(1:1).ne.'x') then
             print *,'WTYP options are a (append), w (overwrite) ',
     .               'n (new name), x (no data output); retry'
          else
             wtype=line(1:1)
          endif
      else if (code.eq.'file') then
         call parse(line,filecode,nfilec)
      else if (code.eq.'plts') then
         if (nfound.ne.1) then
            print 53,'PLTS'
            go to 10
         end if
         plflag=int(xdata(1))
      else if (code.eq.'bins') then
         if (nfound.ne.3) then
            print 53,'BINS'
            go to 10
         end if
         bin0=  xdata(1)
         binmax=xdata(2)
         bininc=xdata(3)
         nbin=int((binmax+.01-bin0)/bininc)
      else if (code.eq.'debu') then
         if (nfound.ne.1) then
            print 53,'DEBU'
            go to 10
         end if
         debug=xdata(1)
      else if (code.eq.'phas') then
         phase=line(1:40)
         icount=0
         i1=1
         do 50 i=2,39
            if (phase(i:i).ne.' ') go to 50
            if (phase(i-1:i-1).eq.' ') go to 50
            if (phase(i+1:i+1).eq.' ') go to 50
            icount=icount+1
            i2=i-1
            nchar=i2-i1+1
            ttfile(icount)='                                        '
            ttfile(icount)(1:nchar)=phase(i1:i2)
            i1=i+1
50       continue
         icount=icount+1
         ttfile(icount)=phase(i1:40)
         nphase=icount
      else if (code.eq.'extf') then
         if (exfilenm.ne.'none') close (9)
         exfilenm=line(1:40)
         open (9,file=exfilenm)
         print *,'Enter yr,mon minimum in log file'
         read *,exyr1,exmon1
         print *,'Enter yr,mon maximum in log file'
         read *,exyr2,exmon2
         print *,'Enter:  (1) ratout format  or  (2) stn format'
         read *,iextftype
         if (iextftype.eq.2) then
            print *,'Enter istncol,stnmin'
            read *,istncol,stnmin
         end if

      else if (code.eq.'scan'.or.code.eq.'read') then
         if (code.eq.'scan') then
            if (nfound.gt.1) then
               print 53,code
               go to 10
            endif
            if (nfound .ne. 1) then
               nscan = 0
            else
               nscan=xdata(1)
            end if
         else
            nscan = 0
         end if
 
         if(nscan.eq.1) then
            open(22,file='scan.out',status='UNKNOWN')
         endif
c      read and search log file

         ntrace=0
         readnext=.true.
c        call GETTDAY(yr1,mon1,dy1,tday1)
c        call GETTDAY(yr2,mon2,dy2,tday2)
         lendir=lenb(logdir)
         lenfil=lenb(logname)
         if (lendir.gt.0)then
            logfile=logdir(1:lendir)//'/'//logname(1:lenfil)
            lenfil=lenfil+lendir+1
         else
            logfile=logname
         endif
         open (12,file=logfile,status='old',iostat=ier)
         if (ier.ne.0) then
           write(*,'(a,a)') ' WARNING Could not open logfile :',
     .                        logfile(1:lenfil)
           go to 10
         endif
         call bins(0.0,'first   ',bin0,bininc,nbin)
         write(*,'(a,a)')
     .    '  date        time        lat      lon    dep    mb   ??   s'
     .   ,'ta       del    az    bakaz  srat cmp start end   phase tt'

c       -set up absolute phase file names from file names and directories
c       -set phase id = part of file name after 'tt_' and directory
c        names are removed
         do 86 iphase=1,nphase
           if (phase.eq.'none') then
             phsid(iphase)='none'
           else
             lendir=lenb(phsdir)
             lenfil=lenb(ttfile(iphase))
             if (lendir.gt.0) then
               phsfil(iphase)=
     .         phsdir(1:lendir)//'/'//ttfile(iphase)(1:lenfil)
               lenfil=lenfil+lendir+1
             else
               phsfil(iphase)=ttfile(iphase)
             endif
             do 84 ii=lenfil,1,-1
               if (phsfil(iphase)(ii:ii).eq.'/') goto 85
84           continue
             ii=0
85           if (phsfil(iphase)(ii+1:ii+3).eq.'tt_') then
               phsid(iphase)=phsfil(iphase)(ii+4:lenfil)
             else
               phsid(iphase)=phsfil(iphase)(ii+1:lenfil)
             endif
           end if
86       continue


ccccccccccccccccccccccccccccccccccccccccccccccccc
c    begin main loop that reads the log file    c
ccccccccccccccccccccccccccccccccccccccccccccccccc

100      quake_ok=.false.
         read (12,110) qyr,qmon,qdy,qhr,qmn,qsc,qlat,qlon,
     &                qdep,qmb,qms,cdfile,nrec,evntid
	 if (qyr .lt. 100) qyr = qyr+1900
110      format (i6,2i3,i4,1x,i2,1x,f5.2,f9.3,f10.3,f7.1,2f5.1,
     &           2x,a25,i5,1x,a8)
         if ((qyr*12+qmon).lt.(yr1*12+mon1)) goto 115
         call TIMEDIF(qyr,qmon,qdy,qhr,qmn,0.,
     &                yr1,mon1,dy1,hr1,mn1,0.,tdif1)
c         write(*,'(f20.3)')tdif1
         if (tdif1.gt.0.) go to 115
         call TIMEDIF(qyr,qmon,qdy,qhr,qmn,0.,
     &                yr2,mon2,dy2,hr2,mn2,0.,tdif2)
c following quakes will be later so quit
         if (tdif2.lt.0.) go to 160
         qcolat=-qlat+90.
         if (qcolat.lt.qcolat1.or.qcolat.gt.qcolat2) go to 115
         qcolon=qlon
         if (qcolon.lt.0.) qcolon=qcolon+360.
         if (qcolon1.lt.qcolon2) then
            if (qcolon.lt.qcolon1.or.qcolon.gt.qcolon2) go to 115
         else
            if (qcolon.gt.qcolon2.and.qcolon.lt.qcolon1) go to 115
         end if
         call stadis(qcolat,qcolon,pcolat,pcolon,del,az)
	 if (del .lt. pdello .or. del .gt. pdelhi) go to 115
         if (qdep.lt.qdep1.or.qdep.gt.qdep2) go to 115
         if (qmb.lt.qmag1.or.qmb.gt.qmag2) go to 115
         quake_ok=.true.
c
         if(nscan.eq.1) then
            write(22,111)
  111       format(/)
            write(22,110) qyr,qmon,qdy,qhr,qmn,qsc,qlat,qlon,
     &                qdep,qmb,qms,cdfile,nrec,evntid
         endif

  115    read(12,'(a99)',end=160)linebuf
         if (linebuf(1:10).eq.'          ') go to 100
         if (linebuf(1:3).eq.'   ') then
            print *,'***SYNC ERROR READING FILE'
            stop
         end if
         stnamecur=linebuf(1:5)

         if (exfilenm.ne.'none') then
            if (readnext) then
116            if (iextftype.eq.1) then
                  read (9,117,end=160) exyr,exmon,exdy,exhr,exmn,exst
117               format (4x,5i3,11x,a4)
               else
                  read (9,118,end=160) exyr,exmon,exdy,exhr,exmn,exst,
     &                             stnrat(1),stnrat(2),stnrat(3)
118               format (i2,4i3,1x,a4,5x,3f9.2)
                  if (stnrat(istncol).lt.stnmin) go to 116
               end if
	       if (exyr .lt. 100) exyr = exyr+1900
               if (exyr.lt.exyr1.or.exyr.gt.exyr2) go to 116
               if (exyr.eq.exyr1.and.exmon.lt.exmon1) go to 116
               if (exyr.eq.exyr2.and.exmon.gt.exmon2) go to 116
               readnext=.false.
            end if
            if (qyr.ne.exyr.or.qmon.ne.exmon.or.qdy.ne.exdy.or.
     &          qhr.ne.exhr.or.qmn.ne.exmn.or.stnamecur.ne.exst) then
               go to 115
            else
               readnext=.true.
            end if
         end if

         if (.not.quake_ok) go to 115
         if (stname(1).ne.'all') then
            do 483 iii=1,nsta
               if (stname(iii).eq.stnamecur) goto 484
483         continue
            go to 115
         endif
484      continue
         gotresp = .false.
         if (cdfile(1:5) .eq. 'seed ') then
	    read (linebuf,'(5x,i4,3f6.1)') irecnum,del,sazi,qazi
	 else
	    read (linebuf,'(4x,i5,3f6.1)') irecnum,del,sazi,qazi
	 endif
         if (del.lt.del1.or.del.gt.del2) go to 115
         if (qazi1.lt.qazi2) then
            if (qazi.lt.qazi1.or.qazi.gt.qazi2) go to 115
         else
            if (qazi.gt.qazi2.and.qazi.lt.qazi1) go to 115
         end if
         if (sazi1.lt.sazi2) then
            if (sazi.lt.sazi1.or.sazi.gt.sazi2) go to 115
         else
            if (sazi.gt.sazi2.and.sazi.lt.sazi1) go to 115
         end if
c
         if (needstinfo) then
            call STALOC(stnamecur,scolat,scolon,ierr)
            if (ierr.ne.0) then
               print *,'***WARNING:  STALOC missed ',stnamecur
               go to 115
            end if
            if (scolat.lt.scolat1.or.scolat.gt.scolat2) go to 115
            if (scolon1.lt.scolon2) then
               if (scolon.lt.scolon1.or.scolon.gt.scolon2) go to 115
            else
               if (scolon.gt.scolon2.and.scolon.lt.scolon1) go to 115
            end if
            call MIDPOINT(qcolat,qcolon,scolat,scolon,mcolat,mcolon)
c           write(*,*) qcolat,qcolon,scolat,scolon,mcolat,mcolon
            if (mcolat.lt.mcolat1.or.mcolat.gt.mcolat2) go to 115
            if (mcolon1.lt.mcolon2) then
               if (mcolon.lt.mcolon1.or.mcolon.gt.mcolon2) go to 115
            else
               if (mcolon.gt.mcolon2.and.mcolon.lt.mcolon1) go to 115
            end if
         end if
c
         do 155 iphase=1,nphase
c
         if (phase.eq.'none') then
           tt1=time1
           tt2=time2
         else
            call GET_TT(phsfil(iphase),del,qdep,tt,iflag)
            if (iflag.eq.2) go to 10
            tt1=tt+time1
            tt2=tt+time2
            delref=(del1+del2)/2.
            call GET_TT(phsfil(iphase),delref,qdep,ttref,iflag)
         end if
         phasid=phsid(iphase)
c
c loop over columns in line (different sample rates)
         do 150 i=1,nsrat
            k1=lbhdr+(i-1)*lbpsr
            k2=k1+lbpsr-2
            lineseg=linebuf(k1:k2)
            if (lineseg(1:3).eq.'   ') go to 150
            read (lineseg,130) sr,nchan,timebeg,timeend,nsloss
130         format (f5.1,i2,2f6.1,i2)
            if (sr.lt.srat1.or.sr.gt.srat2) go to 150
	    if (nchan.ne.3.and..not.(kk.eq.5.or.kk.eq.10)) go to 150
            if (timebeg.gt.tt2) go to 150
            if (timeend.lt.tt1) go to 150

               call bins(del,evntid,bin0,bininc,nbin)

               print 131, mod(qyr,100),qmon,qdy,qhr,qmn,qsc,qlat,qlon,
     &                   qdep,qmb,qms,linebuf(1:28),lineseg, tt
131            format (i2.2,2i3,i4,1x,i2,1x,f5.2,f9.3,f10.3,f7.1,2f5.1,
     &                 2x,a28,a23,f7.1)
c               print 132, tt1,tt2
132            format ('Desired tmin,tmax=',2f8.3)
               ntrace=ntrace+1

               if (code.eq.'scan'.and.nscan.eq.1) then
                   write(22,133)linebuf
 133               format(a99)
                   go to 150
               endif
               if(code.eq.'scan'.and.nscan.eq.0) go to 150

               irok = -1
	       if (cdfile(1:5) .eq. 'seed ') then
                  if (.not.gotresp) then
                     irok = readresp(cddir(1:lenb(cddir)) // '/' //
     &                               cdfile(6:lenb(cdfile)))
                     gotresp = .true.
                  endif
		  call EXTSEED(cdfile,cddir,stnamecur,sr,stol,
     &                      kk,qyr,qmon,qdy,qhr,qmn,qsc,qazi,tt1,tt2,
     &                      slat,slon,selev,stype,scomm,chan,
     &                      npts,ndim,timebuf,data1,data2,data3,co,
     &                      yamp,gain,units,a0,np,poles,nz,zeros,rtype,
     &                      ncor,timcor,cor,debug)
	       else
                  call EXTRACT(cdfile,cddir,nrec,stnamecur,irecnum,sr,
     &                      kk,qyr,qmon,qdy,qhr,qmn,qsc,qazi,tt1,tt2,
     &                      slat,slon,selev,stype,scomm,chan,
     &                      npts,timebuf,data1,data2,data3,co,yamp,
     &                      gain,units,a0,np,poles,nz,zeros,
     &                      ncor,timcor,cor,debug)
	       endif
               if (npts.le.1) go to 150
               print *,' Station log comments: ',scomm(1:lenb(scomm))
c               chanid=' '
c               if (sr.eq.1.) then
c                  chanid(1:1)='l'
c               else if (sr.eq.4. .or. sr.eq.5 .or. sr.eq.8. 
c     &             .or. sr.eq.10.) then
c                  chanid(1:1)='i'
c               else if (sr.eq.20. .or. sr.eq.40.) then
c                  chanid(1:1)='s'
c               else
c                  chanid(1:1)='u'
c                  write(*,'(a,f10.5)') 'sample rate=', sr
c               endif
               chanid=chan(1)(1:2)//'# '
c               chanid(2:2)='#'
               statid=stnamecur
               if (statid(4:).eq.' ') statid(4:)='_'
               lendir=lenb(outdir)
               if (lendir.gt.0) then
                 outfil=outdir(1:lendir)//'/'//
     .                fname(evntid,statid(1:lenb(statid)),
     .                chanid,phasid,filecode,nfilec)
               else
                 outfil=fname(evntid,statid(1:lenb(statid)),
     .                 chanid,phasid,filecode,nfilec)
               endif
               j1=1
               do 135 j=2,npts
                  toff=abs((timebuf(j)-timebuf(j-1))*60.-1./sr)
c  sync loss criterion
                  if (toff.gt.gapmax) then
c                   -time is out of sync
                     j2=j-1
                     jtot=j2-j1+1
c  determine the clock correction appropriate for the midpoint of the seismogram and 
c  apply it to the entire time series.
                     mtime=(timebuf(j2)+timebuf(j1))/2.
                     call ADDTIME(qyr,qmon,qdy,qhr,qmn,qsc,
     &                      mtime,myr,mmon,mdy,mhr,mmn,msc)
                     call FIXTIM(ncor,timcor,cor,myr,mmon,mdy,
     &                           mhr,mmn,msc,tcor)
                     timest=timebuf(j1)+tcor/60.
                     call ADDTIME(qyr,qmon,qdy,qhr,qmn,qsc,
     &                            timest,syr,smon,sdy,shr,smn,ssc)
                     call GETJDAY(syr,smon,sdy,sjday)
                     call datout
     &                   (outfil,wtype,outtyp,evntid,chan,phasid,
     &                    ttwrite,plflag,qyr,qmon,qdy,qhr,qmn,qsc,
     &                    qlat,qlon,qdep,qmb,qms,stnamecur,stype,scomm,
     &                    slat,slon,selev,sr,kk,del,qazi,sazi,sjday,
     &                    syr,smon,sdy,shr,smn,ssc,
     &                    co,gain,units,a0,np,poles,nz,zeros,rtype,
     &                    tt,timest, tcor,delref,ttref,
     &                    jtot,data1(j1),data2(j1),data3(j1))
                     print 134,timebuf(j1)+tcor,timebuf(j2)+tcor,jtot
134                  format ('BLOCK WRITTEN:',f7.2,' to ',f7.2,
     &                       ' npts = ',i6)
                     j1=j
                  end if
135            continue
               j2=npts
               jtot=j2-j1+1
c  determine the clock correction appropriate for the midpoint of the seismogram and
c  apply it to the entire time series.
               mtime=(timebuf(j2)+timebuf(j1))/2.
               call ADDTIME(qyr,qmon,qdy,qhr,qmn,qsc,
     &                      mtime,myr,mmon,mdy,mhr,mmn,msc)
               call FIXTIM(ncor,timcor,cor,myr,mmon,mdy,
     &                     mhr,mmn,msc,tcor)
               timest=timebuf(j1)+tcor/60.
               call ADDTIME(qyr,qmon,qdy,qhr,qmn,qsc,
     &                      timest,syr,smon,sdy,shr,smn,ssc)
               call GETJDAY(syr,smon,sdy,sjday)
               call datout
     &                   (outfil,wtype,outtyp,evntid,chan,phasid,
     &                    ttwrite,plflag,qyr,qmon,qdy,qhr,qmn,qsc,
     &                    qlat,qlon,qdep,qmb,qms,stnamecur,stype,scomm,
     &                    slat,slon,selev,sr,kk,del,qazi,sazi,sjday,
     &                    syr,smon,sdy,shr,smn,ssc,
     &                    co,gain,units,a0,np,poles,nz,zeros,rtype,
     &                    tt,timest,tcor,delref,ttref,
     &                    jtot,data1(j1),data2(j1),data3(j1))
               print 134,timebuf(j1)+tcor/60.,timebuf(j2)+tcor/60.,jtot

c loop over columns in line (different sample rates)
150      continue
c loop over phase files
155      continue
         if (gotresp) call freeresp
c
         go to 115
160      close (12)
         call bins(0.0,'last    ',bin0,bininc,nbin)
         if (exfilenm.ne.'none') close (9)
         exfilenm='none'
         print *,'Number of traces = ',ntrace
c
         if (nscan .eq. 1) write(22,136)ntrace
136      format(/,'Number of traces = ',i5)
c
      else if (code.eq.'quit') then
         close(22)
         go to 900
      else
         print '(a,a)','Command not found: ',code
      end if
      go to 10
c
900   stop
      end


      subroutine bins(delta, event, bin0, bininc, nbin)

c     bin waveforms by delta and output
c     a histogram summary for each event
c     there are nbin bins starting at bin0 and going to bininc.
c     also output are the number of waveforms at delta less
c     bin0 and greater than bin0+nbin*bininc.
c     output is appended to file bin.out
c     total number of bins cannot exceed 50
c     for n waveforms, this routine is called n+2 times, starting with
c     event='first', followed by n waveforms, ending with event='last'

      real delta, bin0, bininc, abin(0:50), temp
      integer nbin, i, bin(0:51),bintot(0:51)
      character*8 event, last
      logical update
      if (nbin.le.0) return
      nbin=min(50,nbin)
      if (event.eq.'first') then
c      -initialize binning: open file, write header, no data are entered here
        open (23,file='bin.out',status='UNKNOWN')
        do 1 i=0,nbin
    1   abin(i)=bin0+real(i)*bininc
        write(23,'(a10,50i5)') 'eventid   ',(nint(abin(i)), i=0,nbin)
        update=.false.
      else if (event.eq.'last') then
c      -last time through loop: output summary and close, no data are entered he
        write(23,'(a7,52i5)') 'totals ', (bintot(i),i=0,nbin+1)
        close(23)
        update=.false.
      else if (last.eq.'first') then
c      -first time through with data: initialize and update array
        do 2, i=0,nbin+1
        bintot(i)=0
    2   bin(i)=0
        update=.true.
      else if (event.eq.last) then
c      -same event as last time: update array
        update=.true.
      else
c      -new event: write last event, then initialize and update array
        write(23,'(a,1x,52i5)') last, (bin(i),i=0,nbin+1)
        do 3, i=0,nbin+1
    3   bin(i)=0
        update=.true.
      endif
      if (update) then
        temp=(delta-bin0)/bininc
        if (temp.ge.0.) then
           i=int(temp)+1
        else
           i=int(temp)
        endif
        i=max(0,i)
        i=min(nbin+1,i)
        bin(i)=bin(i)+1
        bintot(i)=bintot(i)+1
      endif
      last=event
      return
      end

      subroutine FIXTIM(ncor,timcor,cor,myr,mmon,mdy,mhr,mmn,msec,tcor)
c     determine the clock correction for the input time by linear interpolation
c     of the available clock corrections.  If the input time lies outside the
c     range of times at which corrections are available, use the nearest correction.
c Input parateters:
c   nocr  (I*4)  number of clock corrections
c   timcor char*12(12) character array containing the absolute times of each 
c                      correction (year(1:4), julian day(5:8), hour(9:10), minute(11:12)
c                      () represent character positions.
c   cor  R(12)  clock corrections in seconds
c   myr,mmon,mdy,mhr,mmn (I*4) msec (R*4) year, month, day, hour, minute and second 
c               of absolute time for desired clock correction. 
c
c  Output parameter:
c    tcor  (R*4) clock correction (to be added to time) in seconds.
c
      integer ncor,myr,mmon,mdy,mhr,mmn,mjday
      integer tyr,tjday,thr,tmn,tmon,tdy,i
      character*12 timcor(12)
      real cor(12),msec,tcor,tsec,timdif(12)
      if (ncor.eq.0) then
        tcor=0.
      else if (ncor.eq.1) then
        tcor=cor(1)
      else
        do 10 i=1,ncor
          read(timcor(i),'(i4,1x,i3,i2,i2)') tyr,tjday,thr,tmn
          call GETDAY(tyr,tjday,tmon,tdy)
          tsec=0.
          call TIMEDIF(tyr,tmon,tdy,thr,tmn,tsec,
     &                 myr,mmon,mdy,mhr,mmn,msec,timdif(i))
10      continue
        if (timdif(1).le.0.) then
          tcor=cor(1)
        else if (timdif(ncor).ge.0.) then
          tcor=cor(ncor)
        else
          do 20 i=2,ncor
            if (timdif(i).le.0.) then
              tcor=cor(i-1)+(cor(i)-cor(i-1))*timdif(i-1)
     &             /(timdif(i-1)-timdif(i))
              goto 30
            endif
20        continue
        endif
      endif
30    continue
      call GETJDAY (myr,mmon,mdy,mjday)
      write(*,'(2i4,2i3,f8.3)') myr,mjday,mhr,mmn,tcor
      return
      end

      function dtheta(th1,th2)
C     DTHETA - Find difference between angles expressed in degrees
C
C     Assumes:
C       th1, th2 - angles
C     Returns:
C       difference th2-th1 - in degrees

      parameter (rad=3.14159/180.)

      theta1 = rad*th1
      theta2 = rad*th2
      dsin = sin(theta1)*cos(theta2) - cos(theta1)*sin(theta2)
      dcos = cos(theta1)*cos(theta2) + sin(theta1)*sin(theta2)
      dtheta = atan2(dsin,dcos)/rad
      end
