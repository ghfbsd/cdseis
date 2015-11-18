c Program MAKELOG makes an ascii index file for GDSN CDs
c Written in DCM Fortran for a MacIntosh by Peter Shearer
c This version modified to run on a SUN or Apollo, and to read
c station corrections and instrument response by Ken Creager 9/90
c Modified to collect station information and dump when through
c by George Helffrich 9/92
c *** currently 20 events/day maximum
c Also reads all station logs for station name, type, lat,
c long, elevation, and station time correction, and all 
c data logs for the gain, normalization and poles and zeros
c of the instrument response.  These are not kept, but error
c and warning messages are written to standard output.  (Search
c for ERROR or WARNING).  The logs are reread in evt2ah.  Each
c CDROM has new formats and errors in the station and data 
c logs, and the subroutines stalog, datlog1,datlog2, and 
c respons (which are used by makelog and evt2ah) should
c be modified for each CD to read the logs properly.
c The NARS and first 6 cdroms are currently read properly 
c except for a handful of errors reported in the errors.log file.

      program makelog

      integer dim
      parameter (dim=1000,maxsta=200)
      integer*2 ibuf2(1000),ib(10),ih(11)
      character infile*25,chr2*2,chr80*80,stname*4,units*20
      character linebuf(dim)*50,stnamebuf(dim)*4,infileold*25,styp*8
      character stnamecur*4,linecur*99,lineblank*99,cscr*21,infileany*32
      character outfile*25,file*25,header*25,cddir*25,cdfil*128,log*500
      integer lenb,lendir,lenfil
      integer qnum(dim),irecnumbuf(dim)
      integer*4 eqyr(20),eqmon(20),eqdy(20),eqhr(20),eqmn(20)
      integer*4 styr,stmon,stdy,sthr,stmn,sgid
      real eqsc(20),eqlat(20),eqlon(20),eqdep(20),eqmb(20),eqms(20)
      real del(20),az1(20),az2(20)
      real stsc,stlat,stlon,eqclat,eqclon,stclat,stclon,stelev
      character slid(maxsta)*4, slerr(maxsta)*1
      integer slgid(maxsta)
      real sllat(maxsta), sllon(maxsta)
      real timebeg,timenew,timdif,timlen,sr,scr
      integer irecnum,irecnum_st,nrec,menu,nsyncloss,nquake
      integer ilinebuf,itdaymin,itdaymax,itday,ii
      logical newrecord,dataread,nars
      character*4 snam
      integer i,j,i1,i2,i3,k1,nchan,index,indexcur
      logical sryn
      real samrat,samr,gain(3),a0(3),cor(12)
      integer ichan,debug,np(30),nz(30),ier,ncor
      character*12 timcor(12)
      complex poles(30,3),zeros(30,3)
      character*4 schan(3)
      common /long/ linebuf
      external rename

      pi = 4.0*atan(1.0)
      degrad = pi/180.0
      nsta = 0
      open (24,file='output')
c !not currently reading data blocks
      dataread=.false.
      lineblank=' '
c
      print *,'Enter output file name'
      read (*,'(a)',end=999,err=999) outfile
      open (7,file=outfile,status='new',form='formatted')

      print *,'Enter CDROM directory name'
      read (*,'(a)',end=999,err=999) cddir

      print *,'Enter 1 for verbose data log output'
      read (*,*,end=999,err=999) debug
c
10    print *,'Enter: (1) GDSN  or  (2) NARS'
      read (*,*,end=999,err=999) menu
      nars=.false.
      if (menu.eq.2) nars=.true.
c
      print *,'Enter min year,month,day (e.g. 84,3,1)'
      read (*,*,end=999,err=999) i1,i2,i3
      if (i1.eq.0) go to 999
      call GETTDAY(i1,i2,i3,itdaymin)
      print *,'Enter max year,month,day (e.g. 84,10,31)'
      read (*,*,end=999,err=999) i1,i2,i3
      call GETTDAY(i1,i2,i3,itdaymax)
      print *,'itdaymin,itdaymax= ',itdaymin,itdaymax
c      
      do 200 itday=itdaymin,itdaymax
c    naming convention for the MAC is         infileany
c    name is reconstructed for the SUN into   file
         call GETFILENM(itday,infile)
         if (nars) then
            infileany(1:11)='ORFEUS_NARS'
            infileany(12:32)=infile(5:25)
            infile(1:4)='NARS'
         else
            infileany(1:25)=infile
c !later CDs have ';1' at end of file names
            if (itday.ge.1827) infileany(26:27)=';1'      
         end if
         write  (*,14) infileany(1:27)
14       format ('infileany = ',a27)
         call rename(infile, file, header)
         lendir=lenb(cddir)
         lenfil=lenb(file)
         if (lendir.gt.0)then
            cdfil=cddir(1:lendir)//'/'//file(1:lenfil)
         else
            cdfil=file
         endif

         open (8,file=cdfil,access='direct',recl=2000,
     .         status='old',err=15)
         go to 16
15       write(*,'(a,a)') ' CD file not found.  Trying to open :',
     .                     cdfil(1:lenfil+lendir+1)
         go to 200
16       print *, ' '
c
         irecnum=1
c !read and discard initial header
         read (8,rec=irecnum,end=190,err=191) ibuf2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  
c                                                                              c
c    begin loop reading each 2000 byte record                                  c
c    checking to see if record is a header of data                             c
c                                                                              c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  
20       irecnum=irecnum+1
c    kluge to skip record mostly full of backspace characters
c    on day 85 11 8
         if (itday.eq.2139.and.irecnum.eq.3) irecnum=4
         read (8,rec=irecnum,end=190,err=191) ibuf2
c  !first CD is byte swapped! On the SUN it is not byte swapped K.Creager, 8/9/90)
c         if (infile(1:4).eq.'5033') call SWAP32(ibuf2,500)     
c  !get first two characters
         write(chr2,'(a2)') ibuf2(1)        
c              print *,'XXXXXXXXXXXXXXXXXXX'
c              do 231 iii=1,25
c               i2=iii*40
c               i1=i2-39
c               write(chr80,'(40a2)') (ibuf2(i),i=i1,i2)
c               print *, chr80
c231           continue

c
c The following section finishes data for the last record if the
c end of a data block is reached 
         if (dataread.and.(chr2.eq.'GL'.or.chr2.eq.'EV'.or.
     &       chr2.eq.'ST'.or.chr2.eq.'DA')) then
            dataread=.false.
c            print 22, index,stname,sr,nchan,timebeg,timenew,nsyncloss
22          format (i5,2x,a4,f5.1,i2,2f6.1,i2)
            ilinebuf=ilinebuf+1
            qnum(ilinebuf)=index
            stnamebuf(ilinebuf)=stname
            irecnumbuf(ilinebuf)=irecnum_st
            write (linebuf(ilinebuf),23) del(indexcur),
     &            az1(indexcur),az2(indexcur),sr,nchan,
     &            timebeg,timenew,nsyncloss                 
23          format (3f6.1,1x,f5.1,i2,2f6.1,i2)
         end if
c
c ------- GDSN NETWORK-DAY TAPE LOG ------------------------------------           
         if (chr2.eq.'GL') then 
c            print *,chr2         
c
c ------- EVENT LOG -----------------------------------------------------
         else if (chr2.eq.'EV') then
c !get info for up to 20 quakes
            do 30 j=1,20              
               i1=281+40*(j-1)
               i2=i1+39
               write(chr80,'(40a2)') (ibuf2(i),i=i1,i2)
c !blank line ends list
               if (chr80(7:7).eq.' ') go to 35    
c               print *,chr80
               read(chr80,28) eqyr(j),eqmon(j),eqdy(j),eqhr(j),
     &                eqmn(j),eqsc(j),eqlat(j),eqlon(j),eqdep(j),
     &                eqmb(j),eqms(j)
28             format (i9,2i3,i4,1x,i2,1x,f5.2,f9.3,f10.3,f7.1,
     &                 2f5.1)
               eqyr(j)=eqyr(j)-1900
C              Month of 28 Mar 1988 event is wrong in event log on ORFEUS/NARS disk.
	       if (itday .eq. 3010 .and. eqyr(j).eq.88 .and. 
     &             eqmon(j).eq.2 .and. eqdy(j).eq.28)
     &             eqmon(j) = 3
30          continue
            print *,'***20 quakes in this file'
            j=21
35          nquake=j-1
c            do 40 j=1,nquake
c               print 28, eqyr(j),eqmon(j),eqdy(j),eqhr(j),
c     &                eqmn(j),eqsc(j),eqlat(j),eqlon(j),eqdep(j),
c     &                eqmb(j),eqms(j),infile
c40          continue
            ilinebuf=0
            infileold=infile
c
c -------- STATION LOG ---------------------------------------------------               
         else if (chr2.eq.'ST') then
            irecnum_st=irecnum

            call stalog(ibuf2,stname,styp,sgid,stlat,stlon,stelev,
     .                  log,ncor,timcor,cor,ier)
c          add station to list of stations encountered
	   do 43 i=1,nsta
	      if (stname .eq. slid(i)) then
c                already there, check consistency
		 if (slgid(i) .ne. sgid .or.
     &               sllat(i) .ne. stlat .or.
     &               sllon(i) .ne. stlon) then
		     write(24,*) '**Station info differs for ',stname
		     write(24,*) '**Old, New (GDSN#, lat, lon):', 
     &                  slgid(i),sllat(i),sllon(i),sgid,stlat,stlon
		     slerr(i) = '*'
		 endif
		 go to 44
	      endif
43         continue
c          didn't find so far, add to list
	   if (nsta .eq. maxsta) 
     &        write(24,*) '**Too many stations, skipping the rest.'
	   nsta = nsta + 1
	   if (nsta .le. maxsta) then
	      slid(nsta) = stname
	      slgid(nsta) = sgid
	      sllat(nsta) = stlat
	      sllon(nsta) = stlon
	      slerr(nsta) = ' '
	   endif
44         continue
           if (debug.eq.1) call dumpbuf(ibuf2,stname)
c          -write lines 13-19 of station log (time correction)
            do 41 ii=13,16
               i1=ii*40-39
               write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
               write(24,'(2a)') chr80,stname
41          continue
            do 42 ii=1,ncor
42          write(24,'(a4,1x,a12,f8.3)')stname,timcor(ii),cor(ii)
            stclat=-stlat+90.
            stclon=stlon
            if (stclon.lt.0.) stclon=stclon+360.
            do 50 i=1,nquake
               eqclat=-eqlat(i)+90.
               eqclon=eqlon(i)
               if (eqclon.lt.0.) eqclon=eqclon+360.
               call STADIS(eqclat,eqclon,stclat,stclon,del(i),az1(i))
               call STADIS(stclat,stclon,eqclat,eqclon,scr,az2(i))
c               print *,i,del(i),az1(i),az2(i)
50          continue
c            
c -------- DATA LOG -------------------------------------------------------
         else if (chr2.eq.'DA') then
         snam=stname

c  determine whether sample rate matches requested sample rate, read the gain
c  gain=1 if error reading it.

         samrat=0.
         call datlog1(ibuf2,snam,nars,samrat,sryn,samr,ichan,schan,
     .                gain,units)
         if (sryn) then
c         -read instrument response only if sample rates match or samrat=0
           if (debug.eq.1) call dumpbuf(ibuf2,snam)
           irecnum=irecnum+1
           read (8,rec=irecnum,end=190,err=191) ibuf2
c          if (cdfile(1:4).eq.'5033') call SWAP32(ibuf2,500)
           if (debug.eq.1) call dumpbuf(ibuf2,snam)

c  get the normalization, poles and zeros of the instrument response

           call datlog2(ibuf2,snam,a0,np,nz,poles,zeros,ier)
c          write(*,*)'gain,normalization= ',(gain(jjj),a0(jjj),jjj=1,3)
         else
           irecnum=irecnum+1
         endif
         irecnum=irecnum+1
         read (8,rec=irecnum,end=190,err=191) ibuf2
         newrecord=.true.
         dataread=.true.
         nsyncloss=0
c
c --------- DATA ----------------------------------------------------------
         else
            do 60 i=1,10
60             ib(i)=ibuf2(i)
            call GDSNHD(ib,ih,sr)
            if (debug) print 70,ih(1),sr,(ih(i),i=3,11)
c            if (ih(3) .lt. 1980) then
c              do 232 iii=1,25
c               i2=iii*40
c               i1=i2-39
c               write(chr80,'(40a2)') (ibuf2(i),i=i1,i2)
c               print *, chr80
c232           continue
c            endif
70          format (i7,f8.3,9i7)
c !convert 1984 to 84
            styr=ih(3)-1900     
c !kluge for Julian days
            stmon=1             
            stdy=ih(4)
            sthr=ih(5)
            stmn=ih(6)
            stsc=float(ih(7))+float(ih(8))/1000.
            nchan=ih(9)
c            if (snam.eq.'NWAO') 
c     &      write(*,*) styr,stdy,sthr,stmn,stsc,nchan
            call FINDEQ(eqyr,eqmon,eqdy,eqhr,eqmn,eqsc,nquake,
     &                  styr,stmon,stdy,sthr,stmn,stsc,index,timdif)            
c            print *,index,timdif,ih(11)
            if (newrecord) then
               newrecord=.false.
               timebeg=timdif
               indexcur=index
            else
c !sync loss (0.1 s criterion)
               if (abs(timdif-timenew).gt.0.0017) then    
                  nsyncloss=nsyncloss+1
c                  print *,'***SYNC LOSS FOUND'
               end if
c !new event
               if (index.ne.indexcur) then       
                  nsyncloss=nsyncloss-1
c                  print 22, indexcur,stname,sr,nchan,timebeg,
c     &                     timenew,nsyncloss
                  ilinebuf=ilinebuf+1
                  qnum(ilinebuf)=indexcur
                  stnamebuf(ilinebuf)=stname
                  irecnumbuf(ilinebuf)=irecnum_st
                  write (linebuf(ilinebuf),23) del(indexcur),
     &                az1(indexcur),az2(indexcur),sr,nchan,
     &                timebeg,timenew,nsyncloss     
                  timebeg=timdif
                  indexcur=index
                  nsyncloss=0
               end if
            end if
c !length in minutes
            timlen=float(ih(11))/(float(nchan)*sr*60.)   
c !predicted time for next block
            timenew=timdif+timlen                      
         end if
c -------------------------------------------------------------------------
c
         go to 20
c
191      print *,' ***error in read, irecnum=',irecnum
190      close (8)
c
c The following section finishes data for the last record
         dataread=.false.
         ilinebuf=ilinebuf+1
         qnum(ilinebuf)=index
         stnamebuf(ilinebuf)=stname
         irecnumbuf(ilinebuf)=irecnum_st
         write (linebuf(ilinebuf),23) del(indexcur),
     &          az1(indexcur),az2(indexcur),sr,nchan,
     &          timebeg,timenew,nsyncloss                 
c
c The following section outputs linebuf 
c !loop over quakes in this day
         do 198 i=1,nquake     
            print *,' '
            nrec=irecnum-1
            print 192, eqyr(i),eqmon(i),eqdy(i),eqhr(i),
     &         eqmn(i),eqsc(i),eqlat(i),eqlon(i),eqdep(i),
     &         eqmb(i),eqms(i),infileold,nrec
            write (7,'(10x)')
            write (7,192) eqyr(i),eqmon(i),eqdy(i),eqhr(i),
     &         eqmn(i),eqsc(i),eqlat(i),eqlon(i),eqdep(i),
     &         eqmb(i),eqms(i),infileold,nrec
192            format (i6,2i3,i4,1x,i2,1x,f5.2,f9.3,f10.3,f7.1,
     &                 2f5.1,2x,a25,i5)     
            stnamecur='XXXX'
            linecur=lineblank
c !loop over buffer
            do 196 j=1,ilinebuf     
               if (qnum(j).ne.i) go to 196
               if (stnamecur.ne.stnamebuf(j)) then
                  if (linecur.ne.lineblank) then
                     print 195,linecur
195                  format (a99)
                     write (7,195) linecur
                  end if
                  linecur=lineblank
                  write (linecur,'(i9)') irecnumbuf(j)
                  linecur(1:4)=stnamebuf(j)
                  stnamecur=stnamebuf(j)
                  linecur(10:49)=linebuf(j)(1:40)
                  k1=51
               else
                  linecur(k1:k1+20)=linebuf(j)(20:40)
                  k1=k1+22
                  if (linecur(53:55).eq.'1.0') then
                     cscr(1:21)=linecur(51:71)
                     linecur(51:71)=linecur(29:49)
                     linecur(29:49)=cscr(1:21)
                  end if
               end if
196         continue
            if (linecur.ne.lineblank) then
               print 195,linecur(1:99)
               write (7,195) linecur(1:99)
            end if                  
198      continue
c      
c !end loop over day files
200   continue        
c      
900   go to 10
c
999   close (7)
c     dump out station information before finishing up  
c     this is useful for building new entries for srosta2 and stalist
      if (nsta .gt. 0) then
	 write(24,*) ' Stations encountered (* means info conflict):'
	 write(24,'((1x,a4,a1,i3,2(1x,f9.4,1x,f10.4)))')
     +      (slid(i),slerr(i),slgid(i),sllat(i),sllon(i),
     +      (90.0-sllat(i))*degrad,mod(360.0+sllon(i),360.0)*degrad,
     +       i=1,nsta)
      endif
c      close(24)
      stop
      end
c
c
c FINDEQ finds earthquake with closest time to given time
c        Inputs:  eqyr(*)  =  integer array with earthquake years
c                 eqmon(*) =  integer array with earthquake months
c                 eqdy(*)  =  integer array with earthquake days
c                 eqhr(*)  =  integer array with earthquake hours
c                 eqmn(*)  =  integer array with earthquake minutes
c                 eqsc(8)  =  real array with earthquake seconds
c                 nquake   =  number of quakes in array
c                 styr     =  integer for station year
c                 stmon    =  etc.
c        Returns: index    =  array index of closest quake time
c                 tdifmin  =  minimum station time minus quake time (minutes)
c
      subroutine FINDEQ(eqyr,eqmon,eqdy,eqhr,eqmn,eqsc,nquake,
     &                  styr,stmon,stdy,sthr,stmn,stsc,index,tdifmin)
      integer*4 eqyr(10),eqmon(10),eqdy(10),eqhr(10),eqmn(10)
      integer*4 styr,stmon,stdy,sthr,stmn
      real eqsc(10),stsc,tdifmin,tdifmin1,timdif
      integer index1,nquake,i,index

c                               !find closest event before or after
      tdifmin=9.e12
      index=999
      do 10 i=1,nquake
         if (eqyr(i).gt.95 .or. eqyr(i).lt.80) 
     &      print *, ' before, eqyr(i)=', eqyr(i)
         if (styr.gt.95 .or. styr.lt.80) print *, ' before, styr=', styr
         call TIMEDIF(eqyr(i),eqmon(i),eqdy(i),eqhr(i),eqmn(i),eqsc(i),
     &                styr,stmon,stdy,sthr,stmn,stsc,timdif)
         if (abs(timdif).lt.abs(tdifmin)) then
            tdifmin=timdif
            index=i
         end if
10    continue
c                                !find closest event before 
      tdifmin1=9.e12
      index1=999
      do 20 i=1,nquake
         if (eqyr(i).gt.95 .or. eqyr(i).lt.80) 
     &      print *, ' after, eqyr(i)=', eqyr(i)
         if (styr.gt.95 .or. styr.lt.80) print *, ' after, styr=', styr
         call TIMEDIF(eqyr(i),eqmon(i),eqdy(i),eqhr(i),eqmn(i),eqsc(i),
     &                styr,stmon,stdy,sthr,stmn,stsc,timdif)
         if (timdif.lt.0.) go to 20
         if (abs(timdif).lt.abs(tdifmin1)) then
            tdifmin1=timdif
            index1=i
         end if
20    continue
c                                !favor earlier event if in 90% of gap
      if (index1.eq.999.or.index.eq.index1) go to 50
      if (abs(tdifmin1).lt.abs(tdifmin)*9.) then
         tdifmin=tdifmin1
         index=index1
      end if
50    return
      end 
c
c GETFILENM obtains the CD file name
c Input:  total number of days since 0 Jan 80
      subroutine GETFILENM(itday,filenm)
      character*25 filenm
      character*3 monchr(12),chr3
      character*2 chr2
      integer i,jday,imon,idy,iyr,itday     
      save monchr
      data (monchr(i),i=1,12)/'JAN','FEB','MAR','APR','MAY','JUN',
     &                        'JUL','AUG','SEP','OCT','NOV','DEC'/
      call GETYMD(itday,iyr,imon,idy)
      filenm(1:25)='    :19  :   :     00.EVT'
      if (itday.lt.640) then
         filenm(1:4)='5033'
      else if (itday.lt.1187) then
         filenm(1:4)='5159'
      else if (itday.lt.1522) then
         filenm(1:4)='5160'
      else if (itday.lt.1827) then
         filenm(1:4)='5161'
      else if (itday.lt.2101) then
         filenm(1:4)='5461'
      else if (itday.lt.2344) then
         filenm(1:4)='5571'
      else if (itday.lt.2467) then
         filenm(1:4)='4386'
      else if (itday.lt.2558) then
         filenm(1:4)='4438'
      else if (itday.lt.2648) then
         filenm(1:4)='4050'
      end if
      write(chr2,'(i2)') iyr
      filenm(8:9)=chr2
      filenm(11:13)=monchr(imon)
      write(chr2,'(i2)') iyr
      filenm(15:16)=chr2
      call GETJDAY(iyr,imon,idy,jday)
      write(chr3,'(i3)') jday
      filenm(17:19)=chr3
      if (filenm(17:17).eq.' ') filenm(17:17)='0'
      if (filenm(18:18).eq.' ') filenm(18:18)='0'
      return
      end
