      subroutine datout
     .      (outfil,wtype,outtyp,evntid,chanid,phasid,
     .       ttwrite,plflag,eyear,emonth,eday,ehour,emin,esec,
     .       elat,elon,edep,emb,ems,
     .       sta,stype,scomm,slat,slon,selev,srate,
     .       kk,del,eazi,sazi,sjday,
     .       syear,smonth,sday,shour,smin,ssec,
     .       co,gain,units,a0,np,poles,nz,zeros,
     .       tt,timebuf,tcor,delref,ttref,ndata,data1,data2,data3)

c  write waveform data in AH, ASC or SAC format depending on 'outtyp'
c  append, overwrite, or create new filename depending on 'wtype'
c  write one, two, or three components depending on 'kk'
c  write a line to standard output to run an xyplot shell script depending on 'p

c   outfil       C*(*) = output file name (#) is replaced by z,n,e,r, or t
c   wtype        C*2   = 'w' overwrite existing file
c                      = 'a' append to existing file
c                      = 'n' open new file if file already exists
c                      = 'x' do not write output file
c   outtyp       C*4   = 'asc' to write asc output file
c                      = 'ah' to write ah output file
c                      = 'sac' to write SAC output file
c   evntid       C*8   = unique event identifier
c   chanid       C*4(3)= channel identifier using SEED convention (eg BHZ)
c   phasid       C*6   = phase identifier
c   ttwrite      I*4   = flag for writing travel time info to extras
c                        (1 if want written, 0 else)
c   plflag       I*4   = 0 do not write plot.sh line to standard output
c                      = 1 write line in format #1, ...
c                        this line can subsequently be run to start a
c                        plotting shell script
c   eyear        I*4   = origin time year
c   emonth       I*4   = origin time month
c   eday         I*4   = origin time day
c   ehour        I*4   = origin time hour
c   emin         I*4   = origin time minute
c   esec         R*4   = origin time seconds
c   elat         R*4   = event latitude (deg)
c   elon         R*4   = event latitude (deg)
c   edep         R*4   = event depth (km)
c   emb          R*4   = event body wave magnitude
c   ems          R*4   = event surface wave magnitude
c   sta          C*5   = station name
c   stype        C*(*) = network + '~' + station type (eg. DWWSSN, NARS, SRO, ...)
c   scomm        C*(*) = station log comments
c   slat         R*4   = station latitude
c   slon         R*4   = station latitude
c   selev        R*4   = station elevation
c   srate        R*4   = sample rate
c   kk           I*4   : 1=(z,n,e)  2=(z,r,t)  3=(n,e)  4(r,t)
c                        5=z  6=n  7=e  8=r  9=t 10=any
c   del          R*4   = epicentral distance
c   eazi         R*4   = earthquake back azimuth
c   sazi         R*4   = station azimuth
c   sjday        I*4   = start julian day
c   syear        I*4   = start time year
c   smonth       I*4   = start time month
c   sday         I*4   = start time day
c   shour        I*4   = start time hour
c   smin         I*4   = start time minute
c   ssec         R*4   = start time seconds
c   co           R*4(3)    = orientation of each component
c   gain         R*4(3)    = gain of each component (zero if comp. absent)
c   units        C*20      = units of gain
c   a0           R*4(3)    = normalization of each component
c   np           I*4(3)    = number of poles in instrument responses
c   poles        C*4(30,3) = complex array containing poles for three components
c   nz           I*4(3)    = number of zeros in instrument responses
c   zeros        C*4(30,3) = complex array containing zeros for three components
c   tt           R*4       = travel time of phase of interest (min)
c   timebuf      R*4       = start time of data with respect to origin time of e
c   tcor         R*4       = clock correction (s) applied to start time
c   delref       R*4       = reference distance (middle of delta range)
c   ttref        R*4       = travel time to reference distance
c   ndata        R*4       = number of data
c   data1        R*4(dim)  = vertical component data array
c   data2        R*4(dim)  = n-s or transverse component if data are rotated
c   data3        R*4(dim)  = e-w or radial component if data are rotated

c   type         I*4   = data type (1=scalar)  (for asc)
c   type2        I*2   = data type (1=scalar)  (for ah)
c   maxamp       R*4   = maximum amplitude of data
c   maxamp_plot  R*4   = maximum amplitude of demeaned data
c   absmin       R*4   = abscissa minimum ??
c   extra(0:20)  R*4   = extra reals, 18,19,20 contain delref,ttref,and tt if ttwrite=1

      character*(*) outfil, chanid(3)*4, phasid*6, units*20, fpfx*35
      real      elat, elon, edep, esec, maxamp, absmin, extra(0:20)
      real      emb, ems, del, eazi, sazi, timebuf, tt, tcor
      real      slat,slon,selev,ssec,ttref,delref
      character sta*5, stype*(*), wtype*2, wrtype*2, outtyp*4, evntid*8
      integer   kk, sjday, ndata, dim, ttwrite
      parameter (dim=100000)
      real      data1(dim),data2(dim),data3(dim)
      integer   plflag, eyear, emonth, eday, ehour, emin
      integer   syear, smonth, sday, shour, smin
      character code*6, chan*6, llog*202, scomm*(*)
      integer   np(3),nz(3)
      real      gain(3),a0(3),co(3),emag(2)
      complex   poles(30,3),zeros(30,3)

c   norm         R*4  = normalization constant
c   meands       R*4  = average ds
c   poler(30)    R*4  = poles (real)
c   polei(30)    R*4  = poles (imaginary)
c   zeror(30)    R*4  = zeros (real)
c   zeroi(30)    R*4  = zeros (imaginary)
c   dmin         R*4 minimum value of data
c   dmax         R*4 maximum value of data

      character oldfil*128
      save oldfil
      external stat

      real      poler(30),polei(30),zeror(30),zeroi(30)
      integer   ifile, lenb, lenfil, lenlog, ndat,kkk,iii,ii,i,lentemp
      real      srate, dmin, dmax, norm, meands, delta, mean, rms, var
      real      maxamp_plot, y_shift
      logical   op
      character ecoment*80,rcoment*80,ot*20,st*20,filename*128
      character comp(3)*1,temp_str*80
      integer   type
      integer*2 type2
      integer write_ah, write_asc, write_sac
      data fpfx /'123456789abcdefghijklmnopqrstuvwxyz'/
c    -determine requested components based on kk
      if (wtype.eq.'x')return
      comp(1)=' '
      comp(2)=' '
      comp(3)=' '
      if (kk.eq.1.or.kk.eq.2.or.kk.eq.5.or.kk.eq.10) comp(1)='z'
      if (kk.eq.1.or.kk.eq.3.or.kk.eq.6.or.
     +    (kk.eq.10.and.gain(2).ne.0.0)) comp(2)='n'
      if (kk.eq.1.or.kk.eq.3.or.kk.eq.7.or.
     +    (kk.eq.10.and.gain(3).ne.0.0)) comp(3)='e'
      if (kk.eq.2.or.kk.eq.4.or.kk.eq.8) comp(3)='r'
      if (kk.eq.2.or.kk.eq.4.or.kk.eq.9) comp(2)='t'

c     loop over three components.  If no data are requested for a
c     component (ie. comp(kkk) = ' ') skip it

      do 100 kkk=1,3
        if (comp(kkk).ne.' ') then

          chan=chanid(kkk)//'  '

c        -if the last character of filename is '#' or filename contains '#.'
c        -replace '#' by channel id (Z,N,E,R, or T)
          filename=outfil
          lenfil=lenb(filename)
          if (filename(lenfil:lenfil).eq.'#') then
             filename(lenfil:lenfil)=comp(kkk)
          else
             do 1 ii=1,lenfil-1
                if (filename(ii:ii+1).eq.'#.') then
                   filename(ii:ii)=comp(kkk)
                   go to 2
                endif
1            continue
          endif
2         continue
          wrtype=wtype
c        -if appending to an AH file of the same root name
c         as the last write then append onto the existing open file
c         otherwise open into new file with -1,...,-9 
          if (outtyp.eq.'ah' .and. wrtype.eq.'a') then
             if (filename.eq.oldfil) then
                wrtype='a'
             else
                wrtype='n'
             endif
          endif
          oldfil = filename
  
c         if wrtype='n' and file already exists then
c         create a new file called file-1
c         if that exists try -2 ... up to whatever.
c         after that append to last name if possible. If not possible
c         then do not write data.

          if (wrtype .eq. 'n') then
             inquire (file=filename, exist=op)
             if (op) then
                lenfil=lenb(filename)
                do 3 ifile=1,len(fpfx)
                 filename=filename(1:lenfil)//'-'//fpfx(ifile:ifile)
                   inquire (file=filename, exist=op)
                   if (.not. op) then
                      wrtype='w'
                      go to 4
                   endif
3               continue
                write(*,*) ' ERROR:  Files ',fpfx(1:1),' to ',
     .                     fpfx(len(fpfx):len(fpfx)),
     .                     ' are all used, data are not written.'
                wrtype='x'
                return
             else 
                wrtype='w'
             endif
          endif
c        -set real part of first zero and first pole
c        -to number of zeros and poles
c        -shift remaining zeros and poles by one
4         poler(1)=np(kkk)
          zeror(1)=nz(kkk)
          polei(1)=0.0
          zeroi(1)=0.0
          do 5 i=2,30
             poler(i)= real(poles(i-1,kkk))
             zeror(i)= real(zeros(i-1,kkk))
             polei(i)=aimag(poles(i-1,kkk))
             zeroi(i)=aimag(zeros(i-1,kkk))
5         continue
          absmin=0.
          meands=gain(kkk)
          norm=a0(kkk)
          delta=1./srate
          do 6, i=0,20
6         extra(i)=0.

c        -don't let station log comment get too long.  If too long, truncate.
          llog    = 'Comments: ' //
     .       scomm(1:min(lenb(scomm),len(llog)-50)) // ';'
          if (tcor.ne.0.) then
            write(llog(lenb(llog)+1:),'(a,f7.3,a)',iostat=ios) 
     .      ' clock correction applied :',tcor,';'
          endif
          if (comp(kkk).eq.'r' .or. comp(kkk).eq.'t') then
             lenlog=lenb(llog)
             llog(lenlog+1:lenlog+9)=' rotated;'
          endif
          if (ttwrite.eq.1.and.phasid(1:4).ne.'none')then
             lentemp=lenb(phasid)
             write(temp_str, '(3A)') ' travel time of ',
     .          phasid(1:lentemp),' model written to extra 20;'
             lentemp=lenb(temp_str)
             rcoment(1:lentemp)=temp_str(1:lentemp)
             extra(18) = delref
             extra(19) = ttref
             extra(20) = tt
          else
             rcoment = 'none'
          endif

          type   = 1
          type2  = 1

          if (kkk.eq.1) then
             call stat (data1, ndata, mean, var, rms, dmin, dmax)
          else if (kkk.eq.2) then
             call stat (data2, ndata, mean, var, rms, dmin, dmax)
          else if (kkk.eq.3) then
             call stat (data3, ndata, mean, var, rms, dmin, dmax)
          endif

          maxamp=amax1(abs(dmax), abs(dmin))
          maxamp_plot = amax1(abs(dmax - mean), abs(dmin - mean))
          if (maxamp_plot .eq. 0.0) maxamp_plot = 1.0
	  y_shift=-mean/maxamp_plot

9001      format(i4,4i2,f8.5)
          write(ot,9001) eyear, emonth, eday, ehour, emin, esec
          write(st,9001) syear, smonth, sday, shour, smin, ssec
          ecoment='none'
          code=sta

          lenfil=lenb(filename)
c          write(*,'(a,9g13.5,a40)') ' plot.sh ',del,eazi,sazi,
c     .       mean,rms,maxamp,1./maxamp,delta/60.,timebuf-tt,filename
c          write(*,'(a,8(g13.5,1x),a40)') ' plot.sh ',del,eazi,sazi,
c     .    rms,maxamp,1./maxamp,delta/60.,timebuf-tt,filename(1:lenfil+1)

c        -write info for xyplot shell script to automatically create hardcopy
c         of individual waveforms and of record sections.

          if (plflag.eq.1) then
55          format(4(a,1x),2f9.2,i4,f5.1,i5,f7.1,i5,6(g13.5,1x),a)
            write(*,55) ' plot.sh ',sta,chan(1:3),evntid,elat,elon,
     .      int(edep),emb,int(eazi),del,int(sazi),maxamp_plot,
     .      1./maxamp_plot,mean,y_shift,delta/60.,
     .      timebuf-tt,filename(1:lenfil+1)
          else if (plflag.eq.2) then
            write(*,'(a,8(g12.5,1x),a40)') ' plot.sh ',
     .      del,eazi,sazi,mean,maxamp,1./maxamp,delta/60.,timebuf-tt,
     .      filename(1:lenfil+1)
          else if (plflag.eq.3) then
            do 56 iii=lenfil,1,-1
56          if (filename(iii:iii).eq.'/') goto 57
            iii=0
57          write(*,'(a,1x,2f5.1,1x,a,5f9.3)') 'eq_info ',del,emb,
     .      filename(iii+1:lenfil),elat,elon,edep,slat,slon
          endif

          if (outtyp .eq. 'asc' .or. outtyp .eq. 'ah') then
c           for ah output convert units to counts/meter (displacement)
            if (units(1:17).eq.'counts/micrometer') then
              scale=1.e6
            elseif (units(1:17).eq.'COUNTS/MICROMETER') then
              scale=1.e6
            else
              scale=1.
              write(0,*) 'ERROR in DATOUT decoding units of DS ', units
            endif
            meands=meands*scale
          endif
          if (outtyp .eq. 'asc') then

c    write asc output file

            if (kkk.eq.1) then
              ndat= write_asc
     .        (filename,wrtype,code,chan,stype,slat,slon,selev,co(1),
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,ot,
     .        ecoment,type,ndata,delta,maxamp,st,absmin,rcoment,
     .        llog,extra(0),data1)
            else if (kkk.eq.2) then
              ndat= write_asc
     .        (filename,wrtype,code,chan,stype,slat,slon,selev,co(2),
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,ot,
     .        ecoment,type,ndata,delta,maxamp,st,absmin,rcoment,
     .        llog,extra(0),data2)
            else if (kkk.eq.3) then
              ndat= write_asc
     .        (filename,wrtype,code,chan,stype,slat,slon,selev,co(3),
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,ot,
     .        ecoment,type,ndata,delta,maxamp,st,absmin,rcoment,
     .        llog,extra(0),data3)
            endif

          else if (outtyp .eq. 'ah') then

c    write ah output file

            if (kkk.eq.1) then
              ndat= write_ah
     .        (filename,wrtype,code,chan,stype,slat,slon,selev,
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,ot,
     .        ecoment,type2,ndata,delta,maxamp,st,absmin,rcoment,
     .        llog,extra(0),data1)
            else if (kkk.eq.2) then
              ndat= write_ah
     .        (filename,wrtype,code,chan,stype,slat,slon,selev,
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,ot,
     .        ecoment,type2,ndata,delta,maxamp,st,absmin,rcoment,
     .        llog,extra(0),data2)
            else if (kkk.eq.3) then
              ndat= write_ah
     .        (filename,wrtype,code,chan,stype,slat,slon,selev,
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,ot,
     .        ecoment,type2,ndata,delta,maxamp,st,absmin,rcoment,
     .        llog,extra(0),data3)
            endif

	else if (outtyp .eq. 'sac') then

c    write SAC output file

	    emag(1)= emb
	    emag(2)= ems
            if (kkk.eq.1) then
              ndat= write_sac
     .        (evntid,
     .        filename,wrtype,code,chan,stype,slat,slon,selev,co(1),
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,emag,
     .        ot,ecoment,type2,ndata,delta,eazi,st,absmin,rcoment,
     .        llog,extra(0),data1)
            else if (kkk.eq.2) then
              ndat= write_sac
     .        (evntid,
     .        filename,wrtype,code,chan,stype,slat,slon,selev,co(2),
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,emag,
     .        ot,ecoment,type2,ndata,delta,eazi,st,absmin,rcoment,
     .        llog,extra(0),data2)
            else if (kkk.eq.3) then
              ndat= write_sac
     .        (evntid,
     .        filename,wrtype,code,chan,stype,slat,slon,selev,co(3),
     .        meands,norm,poler,zeror,polei,zeroi,elat,elon,edep,emag,
     .        ot,ecoment,type2,ndata,delta,eazi,st,absmin,rcoment,
     .        llog,extra(0),data3)
            endif

	  else
	    write(*,'(a,a)') ' **Don''t understand file type ',outtyp
	    ndat = -1

          endif

          if (ndat.eq.-1) then
             lenfil=lenb(filename)
             write(*,'(2a)') ' File writing error, cannot open file: ',
     .                       filename(1:lenfil+1)
          else if (ndat.eq.0) then
             write(*,'(a)')
     .         ' Error writing file header or data, insufficient space'
          else if (ndat .ne. ndata) then
             write(*,'(a,2i8)')' File writing error,ndata,ndat=',
     .         ndata,ndat
          end if
	

        end if
100   continue
      return
      end


      integer function write_asc
     .  (filename, wtype, code, chan, stype, slat, slon, selev, cad,
     .  meands, norm, poler, zeror, polei, zeroi, elat, elon, edep, ot,
     .  ecoment, type, ndata, delta, maxamp, st, absmin, rcoment,
     .  llog, extra, data)

c  write one waveform header and data record in asc format.
c  this ascii format can be read by asc2ah to convert to ah format.
c  if file 'filename' exists then overwrite it if wtype = 'w' and
c  append to it if wtype = 'a'.
c
c  function returns write_asc = ndata if successful
c                             = -1 if could not open file,
c                                  or file is of wrong type

c   filename     C*(*) = output file name
c   wtype        C*2   = 'w' overwrite existing file
c                      = 'a' append to existing file
c   code         C*6   = station code
c   chan         C*6   = station channel
c   stype        C*8   = network name (eg. DWWSSN, NARS, SRO, ...)
c   slat         R*4   = station latitude
c   slon         R*4   = station latitude
c   selev        R*4   = station elevation
c   cad          R*4   = component azimuth/dip
c   meands       R*4   = average ds
c   norm         R*4   = normalization constant
c   poler(30)    R*4   = poles (real part)
c   polei(30)    R*4   = poles (imaginary part)
c   zeror(30)    R*4   = zeros (real part)
c   zeroi(30)    R*4   = zeros (imaginary part)
c   elat         R*4   = event latitude (deg)
c   elon         R*4   = event latitude (deg)
c   edep         R*4   = event depth (km)
c   ot           C*20  = event origin time (YYYYMMHHMMSS.SSSSS)  (i4,4i2,f8.5)
c   ecoment      C*80  = event comment
c   type         I*4   = data type (1 for real data)
c   ndata        I*4   = number of data
c   delta        R*4   = sampling interval
c   maxamp       R*4   = maximum amplitude
c   st           C*20  = data start time (YYYYMMHHMMSS.SSSSS)  (i4,4i2,f8.5)
c   absmin       R*4   = abscissa minimum ??
c   rcoment      R*4   = record comment
c   llog         C*202 = log
c   data         R*4   = data

      character*(*) filename
      real      slat,slon,selev,elat,elon,edep,meands,norm
      real      delta,maxamp,absmin
      real      poler(30),polei(30),zeror(30),zeroi(30),extra(0:20)
      character wtype*2,code*6,chan*6,stype*(*),ot*20,st*20
      character ecoment*80,rcoment*80,llog*202,file*128
      integer   type,ndata,iout,i,dim
      parameter (dim=100000)
      real data(dim)
      logical op

      iout=17

c    -if unit is open, close it.

      if (wtype(1:1).eq.'w' .or. wtype(1:1).eq.'W') then
c       -close file if open, open file and overwrite if it already exists
         inquire (unit=iout, opened=op)
         if (op) close(iout)
         open(iout,file=filename,err=99)
         rewind(iout)
      else
c       -if no file is open, open filename
c       -if file is open to the correct file, append to it
c       -if file is open to an incorrect file, close it, open
c           the correct file and append to it
c       -NOTE write_ah can not open a file and append to it
         file=' '
         inquire (unit=iout, opened=op, name=file)
         if (op) then
           if (file.ne.filename) then
             close(iout)
             open(iout,file=filename,status='UNKNOWN',err=99)
           endif
         else
           open(iout,file=filename,status='UNKNOWN',err=99)
         endif
      endif

      write (iout,'(a)')          'station information'
      write (iout,'(a,1x,a)')     'code:',         code
      write (iout,'(a,1x,a)')     'channel:',      chan
      write (iout,'(a,1x,a)')     'type:',         stype(1:lenb(stype))
      write (iout,'(a,1x,f11.6)') 'latitude:',     slat
      write (iout,'(a,1x,f11.6)') 'longitude:',    slon
      write (iout,'(a,1x,f11.6)') 'elevation:',    selev
      write (iout,'(a,1x,f11.6)') 'azimuth/dip:',  cad
      write (iout,'(a,1x,e12.3)') 'gain:',         meands
      write (iout,'(a,1x,e12.3)') 'normalization:',norm
      write (iout,'(a)')          'calibration information'
      write (iout,'(a)')
     .       '   pole.re      pole.im      zero.re      zero.im'
      do 5, i=1,30
         write (iout,'(3(e12.3,1x),e12.3)')
     .   poler(i), polei(i), zeror(i), zeroi(i)
    5 continue
      write (iout,'(a)')          'event information'
      write (iout,'(a,1x,f11.6)') 'latitude:',     elat
      write (iout,'(a,1x,f11.6)') 'longitude:',    elon
      write (iout,'(a,1x,f11.6)') 'depth:',        edep
      write (iout,'(a,3x,5(a2,1x),a8)') 'origin_time:',
     .       ot(1:4),ot(5:6),ot(7:8),ot(9:10),ot(11:12),ot(13:20)
      write (iout,'(a,1x,a)')     'comment:',      ecoment
      write (iout,'(a)')          'record information'
      write (iout,'(a,1x,i2)')    'type:',         type
      write (iout,'(a,1x,i8)')    'ndata:',        ndata
      write (iout,'(a,1x,e14.6)') 'delta:',        delta
      write (iout,'(a,1x,e14.6)') 'max_amplitude:',maxamp
      write (iout,'(a,3x,5(a2,1x),a8)') 'start_time:',
     .       st(1:4),st(5:6),st(7:8),st(9:10),st(11:12),st(13:20)
      write (iout,'(a,1x,e14.6)') 'abscissa_min:', absmin
      write (iout,'(a,1x,a)')     'comment:',      rcoment
      write (iout,'(a,1x,a)')     'log:',          llog(1:80)
      write (iout,'(a)')          'extras:'
      do 6, i=0,20
         write (iout,'(i2,a,4x,e14.3))') i, ':', extra(i)
    6 continue
      write (iout,'(a)')          'data:'
      do 7,i=1,ndata
    7 write(iout,'(g14.6)') data(i)
      if (wtype(1:1).eq.'w' .or. wtype(1:1).eq.'W') then
         close(iout)
      endif
      write_asc=ndata
      return
   99 write_asc=-1
      return
      end

      character*128 function fname(evntid,statid,chanid,phasid,
     .                             filecode,nfilec)


c    construct the `file name for waveform output

c input parameters :
c    evntid  C*8   unique event identifier YYMMDDa where
c                  'a' = a for the first event, b for
c                  the second event , etc of the day
c    statid  C**   station id
c    chanid  C*4   channel identifier (eg. BHZ)
c    phasid  C*4   first 4 characters (after tt_ of file name containing
c                  a travel time curve.
c    filecode C*20(20) up to 20 character strings used to form file name
c                  if a string equals ev, st, ch, or ph then the appropriate
c                  identifier given above is inserted into the file name
c                  separated by periods.  If a string is anything else it
c                  is incorporated into the file name as is.
c    nfilec  I     number of strings in filecode
c
c output string:
c     fname c*128 output file name


      character statid*(*),chanid*4,phasid*6,evntid*8
      character*20 filecode(20), string(20),code
      integer nfilec,i,j,n,lenb,l1,lenstr(20)
      do 1 i=1,nfilec
         code=filecode(i)
         if (code.eq.'ev' .or. code.eq.'EV') then
            string(i)=evntid
         else if (code.eq.'st' .or. code.eq.'ST') then
            string(i)=statid
         else if (code.eq.'ch' .or. code.eq.'CH') then
            string(i)=chanid
         else if (code.eq.'ph' .or. code.eq.'PH') then
            string(i)=phasid
         else
            string(i)=code
         endif
         lenstr(i)=lenb(string(i))

c       -convert upper case event, station, channel and phase to lower case
         if (string(i) .ne. code) then
            do 3 j=1,lenstr(i)
              n = ichar(string(i)(j:j))
              if(n.ge.ichar('A').and.n.le.ichar('Z'))
     .            string(i)(j:j) = char(n+ichar('a')-ichar('A'))
3           continue
         endif
1     continue
      i=1
      fname=string(i)(1:lenstr(i))
      do 2 i=2,nfilec
         l1=lenb(fname)
         if (lenstr(i).gt.0)
     .   fname=fname(1:l1)//'.'//string(i)(1:lenstr(i))
2     continue
      return
      end

      subroutine stat (x, n, mean, var, stdev, xmin, xmax)

c   Compute the mean, sample variance, standard deviation, minimum, and maximim
c   of the n-dimensional, real vector x.
c
c   mean = 1/N sum i=1,N  {x(i)}
c   var  = 1/(N-1) sum i=1,N  {(x(i)-mean)**2}
c   stdev= var**1/2

      integer n, i
      real x(n), mean, var, stdev, xmin, xmax
      double precision sum, sum2, x1

      if (n .le. 1) then
         var  = 0.0
         stdev= 0.0
         if (n .eq. 1) then
            mean = x(1)
            xmin = x(1)
            xmax = x(1)
         else
            mean = 0.
            xmin = 0.
            xmax = 0.
         end if
      else
         xmax=x(1)
         xmin=x(1)
         x1=dble(x(1))
         sum=x1
         sum2=x1*x1
         do 1 i=2,n
            xmax=amax1(xmax,x(i))
            xmin=amin1(xmin,x(i))
            x1=dble(x(i))
            sum =sum +x1
            sum2=sum2+x1*x1
1        continue
         mean = real(sum)/real(n)
         var  = real(sum2 - sum*sum/dble(n)) / real(n-1)
         stdev= sqrt(var)
      end if
      return
      end

