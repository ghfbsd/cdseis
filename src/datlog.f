      subroutine stalog(ibuf2,stname,styp,sgid,slat,slon,selev,
     .                  log,ncor,timcor,cor,ier)

c   read station name, instrument type (eg. DWWSSN, NARS, etc) 
c   latitude(deg), longitude(deg), elevation(m), 
c   and time correction(s) from the station log.  There are typically
c   0 to 3 time corrections reported.  Return the first correction
c   if one or two are given, otherwise return the second one.  Write
c   a warning message if the corrections differ by more than 0.1 s.

      integer*2 ibuf2(1000)
      character chr80*80,chrns*1,chrew*1,chrm*1,stname*(*),styp*8
      character*2 cflag(12)
      character*12 timcor(12)
      character*20 chdata(20)
      character log*(*)
      real slat,slon,selev,cor(12)
      logical corr
      integer i,ii,i1,j,j1,j3,jj,ier,ierr,year,year0,day,time,n
      integer line(12),ncor,ncorr,sgid

      i1=161
      write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
c      print *,chr80
      read(chr80,45) sgid,stname,styp,slat,chrns,slon,chrew,selev,chrm
9     format(5a)
45    format (15x,i3,2x,a4,2x,a8,4x,f8.4,a1,2x,f9.4,a1,2x,f8.2,a1)
46    format ( 4x,i4,2x,i3,2x,i4,2x,f7.3,1x,a1)
47    format (44x,i4,2x,i3,2x,i4,2x,f7.3,1x,a1)
      if (chrns.eq.'S') slat=-slat
      if (chrew.eq.'W') slon=-slon
      if (chrm.ne.'M') write(*,9) chr80, stname,
     .' STALOG WARNING units on station elevation is ',chrm,' not M'

c Read comments out of station log so they may be returned.  Scan
c lines 13-25 for the word, 'COMMENT:'
      log = ' '
      do 70 i=13,25
       write(chr80,'(40a2)') (ibuf2(j),j=1+(i-1)*40,40+(i-1)*40)
       ii = index(chr80,'COMMENTS:')
       if (ii .ne. 0) then
          log = chr80(ii+9:ii+9+lenb(chr80(12:)))
          do 72 ii=i+1,25
             write(chr80,'(40a2)')
     .            (ibuf2(j),j=1+(ii-1)*40,40+(ii-1)*40)
             log = log(1:lenb(log)) // ' ' // chr80(1:lenb(chr80))
72          continue
          go to 79
       endif
70    continue
79    continue

c Read lines 13 through 21 and attempt to interpret each line as a station 
c time correction.  A time correction is identified by the
c characters /0 or /- in columns 29:30, and vallues interpretable by the
c formatted read using format 46 above, with the value of year exceeding 1900.
c Once a correction has been found, continue reading until a line not 
c containg a correction is found.  Then go back and search the lines that
c contained corrections for more corrections in the same way, but shifted over 40
c characters to the right.  
      ncor=0
      do 5 j=1,12
        cor(j)=0.
        timcor(j)=' '
5     continue
      j=1
      do 20 j1=13,21
        i1=j1*40-39
        write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
        if (chr80(29:30).eq.'/-' .or. chr80(29:30).eq.'/0')then 
          corr=.true.
        else
          corr=.false.
        endif
        read(chr80,46,iostat=ierr)year,day,time,cor(j),cflag(j)
        if (corr.and.year.gt.1900.and.ierr.eq.0) then
          line(j)=j1
          year0=year
          write(timcor(j),'(3i4)')year,day,time
          j=j+1
        else if (j.gt.1) then
c        -At least one correction was found, but this line does not contain a 
c         corection.  Go back and check the lines containing corrections for 
c         a second column of corrections.
          do 10 j3=line(1),line(j-1)
            i1=j3*40-39
            write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
            if (chr80(69:70).eq.'/-' .or. chr80(69:70).eq.'/0') then
              read(chr80,47,iostat=ierr) year,day,time,cor(j),cflag(j)
              if (year.gt.1900.and.ierr.eq.0) then
                year0=year
                write(timcor(j),'(3i4)')year,day,time
                j=j+1
              endif
            endif
10        continue
c        -done reading corrections
          ncor=j-1
          if (ncor.gt.6) then
            write(*,9) stname, 'STALOG WARNING more than six',
     .      ' time corrections found'
            ncor=6
          endif
c replace all time adjustments by two time corrections at the same time, the first
c is the value given, the second is 0.    
          ncorr=ncor
          do 17 jj=ncor,1,-1
            if (cflag(jj).eq.'0') then
              do 15 ii=ncorr,jj,-1
                timcor(ii+1)=timcor(ii)
                cor(ii+1)=cor(ii)
                cflag(ii+1)=cflag(ii)
15            continue
              cor(jj+1)=0.0
              ncorr=ncorr+1
            endif
17        continue
          ncor=ncorr
          ier=0
          return
        endif
20    continue 

c    -no time corrections were found: 
c     search line #13 for the standard line that comes before the time corrections,
c     then check the next line for nonblank characters.  Sometimes
c     this line is blank, which presumably means no station corrections
c     are available (hopefully that the time is correct).  Also check line #13
c     for a message that says TIME CORRECTIONS:  0.  Write an error message
c     if neither of these are found.

      i1=13*40-39
      write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
      call parse (chr80,chdata,n)
      if (n.gt.3) then
        if(chdata(1).eq.'YEAR'.and.chdata(2).eq.'DAY'.and.
     .     chdata(3).eq.'TIME') then
          i1=i1+40
          write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
          if (chr80(1:30).eq.'                              ')then
            ier=0 
            return
          endif
        endif
      else if (n.eq.3) then
        if (chdata(1).eq.'TIME'.and.chdata(3).eq.'0') then
          ier=0
          return
        endif  
      endif
      do 55 ii=13,15
        i1=ii*40-39
        write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
        write(*,9) chr80,stname,' STALOG ERROR reading time corretions'
55    continue
      ier=-1
      return
      end
 

      subroutine datlog1(ibuf2,snam,nars,samrat,sryn,samr,ichan,schan,
     .                   gain,units)

c input parameters:
c   ibuf2  I*2(1000) 
c   snam   C*4        station name
c   nars   L          =.true. for NARS CDROM
c   samrat R*4        requested sample rate
c 
c output parameters:
c   sryn   L          = true if sample rates match or samrat=0. 
c                     = false is sample do not match : skip data log
c   samr   R*4        sample rate read from data log
c   ichan  I*4        number of channels
c   schan  C*4(3)     channel names
c   gain   R*4(3))    gain (set to 1 if it cannot be read)
c   units  C*20       units of gain

      integer*2 ibuf2(1000)
      character*80 chr80,snam*4,schan(3)*4
      character*20 chr20,chn*1,orient*1,units*20
      logical nars,sryn
      integer samrat,samr
      real    gain(3),ds,scale
      integer i1,ierr,i,ichan,ichn,istart,iend

c obtain name of frequency band pass from first line of data log
   
      i1=1
      write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
      ichn=index(chr80,'CHANNEL')
      chr20=chr80(ichn+6:ichn+27)
      if     (index(chr20,'VERY LONG') .ne. 0) then
        chn='V'
      elseif (index(chr20,'ULTRA LONG') .ne. 0) then
        chn='U'
      elseif (index(chr20,'LONG') .ne. 0) then
        chn='L'
      elseif (index(chr20,'SHORT') .ne. 0) then
        chn='S'
      elseif (index(chr20,'INTER') .ne. 0) then
        chn='I'
      elseif (index(chr20,'BROAD') .ne. 0) then
        chn='B'
      else
        write(0,*) 'DATLOG1 ERROR reading channel ',chr80
        chn='N'
      endif

c check sample rate against requested sample rate
c only read the data log if they match or if requested rate is 0

9     format(4a)
      if (nars) then
c       -NARS has only one sampling rate
         sryn=.true.
      else
         i1=81
         write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
         read(chr80,'(16x,f6.1)',iostat=ierr) samr
c        blacknest data seems to have sample rate field shifted
c        try groping for rate after 'SAMPLE RATE:' label
         if (ierr.ne.0) read(chr80,'(14x,f6.0)',iostat=ierr) samr
         if (ierr.ne.0) then
         write(*,9)chr80,snam,' DATLOG1 WARNING cannot read sample rate'
            sryn=.true.
         else if (samrat.eq.0) then
            sryn=.true.
         else if (samr.eq.samrat) then
            sryn=.true.
         else
            sryn=.false.
         endif
      endif

c  number of channels of data are assumed to equal the number of
c  times the character '=' occurs in line 4 of the data log.
c  channel names are stored in schan

      i1=121
      write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
      ichan=0
      do 10 i=1,70
         if (chr80(i:i).eq.'=') then
            ichan=ichan+1
            orient=chr80(i+1:i+1)
            if (orient.eq.'V') orient='Z'
            schan(ichan)=chn//'H'//orient//' '
         end if
10    continue

c   read the units for the digital sensitivity from the columns
c   47:70 of line 8.  It is assumed to appear between parantheses

      i1=281
      write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
      istart=index(chr80(47:70),'(')
      iend  =index(chr80(47+istart:70),')')
      iend  =min(iend,istart+19)
      units=chr80(47+istart:47+istart+iend)
      if (units(1:17).eq.'counts/micrometer') then
        scale=1.e6
      elseif (units(1:17).eq.'COUNTS/MICROMETER') then
        scale=1.e6
      else
        scale=1.
        write(0,*) 'ERROR in DATLOG1 reading units of DS ', units,
     .             chr80,istart,iend
      endif

c   read gain (average cal value) for each component, using the last
c   value given for each component from lines 9-14.  If gain is not 
c   given for horizontals, assign the same value as for the vertical.

      do 15 i=1,3
15       gain(i)=1.
      do 20 i1=321,521,40
         write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
         if (chr80(51:60).ne.'          ') then
c          -sometimes gain is blank
            read (chr80,'(5x,i5,40x,e15.4)',iostat=ierr) i,ds
            if (ierr.eq.0) then
               if (i.eq.1.or.i.eq.2.or.i.eq.3) gain(i)=ds
            endif
         endif
20    continue
      if (gain(1).eq.1.) then
         write(*,*)  ' DATLOG1 ERROR reading gain, setting gain=1'
         do 30 i1=321,521,40
            write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
            write(*,9) ' DATLOG1 ERROR :',chr80,snam
30       continue
      endif
      if (gain(2).eq.1.) gain(2)=gain(1)
      if (gain(3).eq.1.) gain(3)=gain(1)

      return
      end


      subroutine datlog2(ibuf2,snam,a0,np,nz,poles,zeros,ier)

c input parameters:
c   ibuf2  I*2(1000) 
c   snam   C*4        station name

c Output parameters:
c   a0(3)      = normalization for each component
c   np(3)      = number of poles in instrument response
c   poles(30,3)= complex array containing poles for three components
c   nz(3)      = number of zeros
c   zeros(30,3)= complex array containing zeros forthree components
c   ier        = 0 if values returned properly


      integer*2 ibuf2(1000)
      integer np(3),nz(3),ier
      real a0(3),values(6)
      complex poles(30,3),zeros(30,3)

      character*80 chr80,snam*4
      integer nzeros(3),iread(3),icopy(3)
      integer iii,jjj,kkk,i1,i,j,ierr,iii1,iii2,istart
      real f1,f2,f3
      character polzer*2


c  initialize instrument response arrays and flags
9     format(4a)
      do 10 jjj=1,3
        iread(jjj)=0
        icopy(jjj)=0
        nzeros(jjj)=-1
        np(jjj)=0
        nz(jjj)=0
        a0(jjj)=1.
        do 10 iii=1,30
        poles(iii,jjj)=cmplx(0.,0.)
        zeros(iii,jjj)=cmplx(0.,0.)
10    continue
      ier=0
c
c    -read normalization
c     search lines 26-29 of data log for either A0 or AO starting in columns 2-5.
c     From this line, obtain the 3 normilizations, setting the horizontals equal to the
c     vertical if horizontals are not readable.  If vertical normalization is not
c     readable, set it to 1 set number of poles and zeros to 0.

      do 20 i1=1,121,40
        write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
        do 15 i=2,5
          if (chr80(i:i+1).eq.'A0'.or.chr80(i:i+1).eq.'AO') then
            read (chr80,'(6x,3(g20.8,3x))',iostat=ierr)f1,f2,f3
            if (ierr.ne.0) then
              read (chr80,'(6x,3(g20.8,3x))',iostat=ierr)f1
              if (ierr.ne.0) then
                write(*,9) chr80,snam,' DATLOG2 ERROR reading ',
     .          'normalization (f1), setting f1=f2=f3=1.'
                ier=-7
                f1=1.
                f2=1.
                f3=1.
              else 
                do 12 j=20,50
                  if(chr80(j:j).eq.'N') then
                    if (chr80(j:j+37).eq.
     .              'NOMINAL TRANSFER FUNCTION ALL CHANNELS')then
c                    -this line occurs as many as 300 times per cdrom.  when encountered
c                     set horizontal normalizations to that of the vertical component.
                      f2=f1
                      f3=f1
                      go to 13
                    endif
                  endif
12              continue
                write(*,9) chr80,snam,
     .          ' DATLOG2 ERROR cannot read f2,f3; setting them=f1'
                f2=0.
                f3=0.
              endif
            endif
13          if (f2.eq.0.) f2=f1           
            if (f3.eq.0.) f3=f1
            a0(1)=f1
            a0(2)=f2
            a0(3)=f3
            goto 30
          end if
15      continue
20    continue
      write(*,9)  snam,' DATLOG2 ERROR A0 not found'
      ier=-7

30    if (ier.lt.0) return

c  Read poles and zeros of instrument response for up to three components.
c  K. Creager 9/25/90

      kkk=0
      istart=i1+40
      do 80 i1=istart,1000,40
        kkk=kkk+1

c Read a line and interpret it (see documentation in routine respone)

        write(chr80,'(40a2)') (ibuf2(i),i=i1,i1+39)
        call respons (chr80,snam,polzer,values,nzeros,ier)

c If there is a reading error exit with number of poles and zeros set to 0

        if (ier.ne.0) then
          do 40 jjj=1,3
            np(jjj)=0
            nz(jjj)=0
40        continue
          return
        endif

c If an S is found set number of zeros with values of zero and set done reading flag

         do 50 jjj=1,3
          if (nzeros(jjj).gt.0) then
            iread(jjj)=1
            do 45 iii=1,nzeros(jjj)
               nz(jjj)=nz(jjj)+1
               zeros(nz(jjj),jjj)=cmplx(0.,0.)
45          continue
            nzeros(jjj)=-1
          endif
50      continue

c  On first pass if pole for second component equals zero then 
c  set flags to copy first component instrument response to second and third components
c  and set flags for reading done on second and third components

        if (kkk.eq.1.and.values(3).eq.0..and.values(4).eq.0.) then
          iread(2)=1
          iread(3)=1
          icopy(2)=1
          icopy(3)=1
        endif

c  Assign zeros or poles read from this line to appropriate components 
c  only if their complex values are not zero.

        do 60 iii=1,3
          iii1=iii*2-1
          iii2=iii1+1
          if (values(iii1).ne.0..or.values(iii2).ne.0.) then
            if (polzer.eq.'p') then
               np(iii)=np(iii)+1
               poles(np(iii),iii)=cmplx(values(iii1),values(iii2))
            else if (polzer.eq.'z') then
               nz(iii)=nz(iii)+1
               zeros(nz(iii),iii)=cmplx(values(iii1),values(iii2))
            endif
          endif
60      continue

c  If done reading all three components then copy horizontals if needed and exit

        if (iread(1)*iread(2)*iread(3).eq.1) then
          do 70 iii=2,3
            if (icopy(iii).eq.1) then
              np(iii)=np(1)
              nz(iii)=nz(1)
              do 64 jjj=1,np(1)
64            poles(jjj,iii)=poles(jjj,1)
              do 66 jjj=1,nz(1)
66            zeros(jjj,iii)=zeros(jjj,1)
            endif
70       continue
         go to 90
        endif
80    continue

c------------------------------------------------------   
c
90    call repair(np,nz,poles,zeros)
c      write(*,*)'found all poles and zeros'
c      write(*,*)'Z component np, nz =',np(1),nz(1)
c      write(*,*)'poles -- ',(poles(iii,1),iii=1,np(1))
c      write(*,*)'zeros -- ',(zeros(iii,1),iii=1,nz(1))   
c      write(*,*)'N component np, nz =',np(2),nz(2)
c      write(*,*)'poles -- ',(poles(iii,2),iii=1,np(2))
c      write(*,*)'zeros -- ',(zeros(iii,2),iii=1,nz(2))   
c      write(*,*)'E component np, nz =',np(3),nz(3)
c      write(*,*)'poles -- ',(poles(iii,3),iii=1,np(3))
c      write(*,*)'zeros -- ',(zeros(iii,3),iii=1,nz(3))
c
      return
      end
      subroutine respons(str,snam,polzer,values,nzeros,ier)

c interpret one 80 character long string to obtain the poles and zeros of up to three components.
c
c K. Creager Sept 25, 1990
c 
c input parameters:
c     str   C*80    string to be interpreted
c     snam  C*4     station name used only for error messages
c
c output parameters:
c     polzer C*2    = 'p' if returning poles
c                   = 'z' if returning zeros
c     values R(6)   = real and imaginary parts of the instrument response for three components
c     nzeros I(3)   = number of zeros equal to the value zero for each of the three components
c     ier    I      = 0 no error
c                   = -i if an error was encountered in the ith step described below
c                                                                                   
c PROCEDURE
c 
c 1) parse string                  
c       { if string blank write error(-1) and exit  }
c 
c 2) find P or Z in first string   
c       { if no P or Z write error(-2) and exit }
c 
c 3) Find position of all E's :  each E must lie within one the the 6 column ranges set by elim:
c                                13-17  23-28  36-40  46-51  59-63  or  69-74
c                                Assign each E to a position (1-6) using array 'place'
c       { if an E is found outside these regions write error(-3) and exit }
c 
c 4) Find position of all S's :  each S must lie within one the the 3 column ranges set by slim:
c                                9-17  32-40  or  55-63  
c                                Each S must be followed by '**' and then an integer
c                                assign the integer to the number of zeros for the ith component
c                                where the S lies in the ith column range
c        { if an S is found and any of the conditions is not met write error(-4) and exit }
c
c 5) Interpret each string containing an E as a number and put values into 'number'
c    For each string 2-N
c       determine its length
c       if it contains an E then
c          if its length is 8-11 and it is interpretable as a real number
c             assign it to the array 'number'
c          else if it does not contain an S
c             write warning but keep going
c           endif
c       endif
c   { if string contains E but is not interpretable as a real number write error(-5) and exit }
c 
c 6) Assign numbers to appropriate components based on column number of 'E'
c    If number of values read in (5) equals number of E's in (3) then
c       assign the values in (5) to the appropriate components from (3)
c       { set error flag to 0 }
c    else
c      { write error(-6) and exit }
c    endif


      character*20 chdata(20),str*80,snam*4,polzer*2
      real values(6), number(40)
      integer nstrng,ier,ierr,nzeros(3)
      integer i,ii,k,place(80),nume,numn,ndumy,len,lenb
      integer elim1(6),elim2(6),slim1(3),slim2(3)
      save elim1,elim2,slim1,slim2
      data elim1/13,23,36,46,59,69/, elim2/17,28,40,51,63,74/
      data slim1/9,32,55/,           slim2/17,40,63/

9     format(4a)
c STEP 0: The second pole for the response of ALQ,AFI,BER,SCP,TAU in 
c         1981 contains a control character ^H (back space). Remove 
c         this character and shift all following characters back two 
c         spaces.  This occurs 55 times on cdrom #1.
c         Also, the complex part of the second pole for station GUMO on 
c         cdrom #5 contains the character '_' where the sign belongs.  This
c         error occurs 10 times from April 11 to 25, 1988.  '_' is changed to '-'.

      if (str(12:12).eq.char(8)) then
        str(12:12)=' '
        do 20 i=11,78
          str(i:i)=str(i+2:i+2)
20      continue
      endif
      if (str(20:20).eq.'_') str(20:20)='-'

c STEP 1: parse string using blanks and commas as delimiters     
      do 30 i=1,6
30    values(i)=0.
      call parse(str, chdata, nstrng)
      if (nstrng.eq.0) then
        write(*,9) ' RESP ERROR poles/zeros line is blank', snam
        ier=-1
        return
      endif

c STEP 2: find P or Z in first string  

      if (chdata(1)(1:1).eq.'P') then
        polzer='p '
      else if (chdata(1)(1:1).eq.'Z') then
        polzer='z '
      else
        polzer='n '
        write(*,9) str, snam, ' RESP ERROR P/Z not found'
        ier=-2
        return
      endif

c STEP 3: Find position of all E's

      k=0
      do 2 i=1,80
        if (str(i:i).eq.'E') then
          do 1 ii=1,6
            if (i.ge.elim1(ii).and.i.le.elim2(ii)) then
              k=k+1
              place(k)=ii
              goto 2
            endif
1         continue
          write(*,9) str, snam, ' RESP ERROR E found in wrong place'
          ier=-3
          return 
        endif
2     continue
      nume=k  

c STEP 4: Find position of all S's

      do 4 i=1,80
        if (str(i:i).eq.'S') then
          do 3 ii=1,3
            if (i.ge.slim1(ii).and.i.le.slim2(ii)) then
              if (str(i+1:i+2).eq.'**') then
                read(str(i+3:i+3),'(i1)',iostat=ierr) ndumy
                if (ierr.eq.0) then
                  nzeros(ii)=ndumy
                else
                  write(*,9) str,snam,
     .                 ' RESP ERROR number of zeros not readable'
                  ier=-4
                  return
                endif
              else
                write(*,9) str,snam,' RESP ERROR S found in wrong place'
                ier=-4
                return 
              endif
              goto 4
            endif
3         continue
          write(*,9) str, snam, ' RESP ERROR S found in wrong place'
          ier=-4
          return 
        endif
4     continue

c STEP 5: Interpret each string containing an E as a number and put values into 'number'

      k=1
      do 10 i=2,nstrng
        len=lenb(chdata(i))
c        write(*,*) chdata(i),i,len
        do 5 ii=1,len
          if (chdata(i)(ii:ii).eq.'E') goto 7
5       continue
        do 6 ii=1,len
          if (chdata(i)(ii:ii).eq.'S') goto 10
6       continue
        write(*,9) str,snam,' RESP WARNING string found without S or E'
        go to 10
7       if (len.ge.8.and.len.le.11) then
          read(chdata(i)(1:len),*,iostat=ierr)number(k)
          if (ierr.eq.0) then
            k=k+1
          else
            write(*,'(3a,i4,a)') str,snam,
     .      ' RESP ERROR Could not read ,',i,'th number'
            ier=-5
            return
          endif
        endif
10    continue
      numn=k-1

c STEP 6: Assign numbers to appropriate components based on column number of 'E'

      if (numn.eq.nume) then
        do 11 i=1,numn
          values(place(i))=number(i)
11      continue
        ier=0
        return
      else
        write(*,9) str,snam,' RESP ERROR number of values not equal',
     .  ' to number of strings containing E'
        ier=-6
        return
      endif
      end
