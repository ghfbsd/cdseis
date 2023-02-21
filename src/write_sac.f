      integer function write_sac(evntid,filename,wtype,code,
     +   chan, stype, slat, slon, elev, cad, DS, A0, 
     +   rtype, rpol, rzro, ipol, izro, 
     +   lat, lon, dep, mag, ot, ecomment, 
     +   type, ndata, delta, eazi, abstime, rmin, rcomment,
     +   log, extra, data_in)
C/* write_sac:  writes out an ah file containing the data from reading the cdrom
C       	with the the routine cdseis
C
C       This routine is a FORTRAN version of the C code that was written to
C               produce AH output files.  Cloned from that code by G.
C               Helffrich/DTM 25 Aug. 91
C               Parameters are in the form of arrays of real numbers and 
C               character strings.
C       	The inputs needed by the function are as follows
C
C       (1)  evntid:    A character string containing the event id for the
C       		earthquake.
C
C       (2)  filename:  A character string containing the name of the file
C       		that the data is to be placed in.
C
C       (3)  wtype:	A character that determines the type of writing
C       		that should be done.  The user can select
C
C       	(a) wtype="a", which causes either
C       		(i) a new file to be opened if one is not already
C       			 open, and data to be written to the file or
C       		(ii) if a file is already open, the data will be
C       			written to the end of that file....
C       	(this is as close as I could get to an append mode for output,
C       	however once a file is closed, reopening it with append mode
C       	results in overwriting any data that was in the file.  This is
C       	one of the drawbacks to working with an XDR output stream,
C       	you are not allowed to reposition the pointer to the end
C       	of a file for output, repositioning of the file pointer is
C       	disregarded on output.)
C
C       		or the user can select
C
C       	(b) wtype="n", which causes a new file to be opened each time
C       		the function is called regardless of whether or not
C       		a file is currently open.  If this option is selected,
C       		any currently open files will be closed and a new file
C       		with the name "filename" will be opened.  If a file
C       		named filename already exists, then a file called
C       		"filename"-1 is created, then "filename"-2, etc. until
C       		the ninth file with that filename is opened.  When this
C       		happens, output of the data stops.
C
C       (4)  code:	A character string by 6 that contains the station code
C       		for the station that the data was recorded at
C       		(GRFO, etc.)
C
C       (5)  chan:	A character string by 6 that contains the name of the
C       		data channel that the data was recorded on (VERT, etc.)
C
C       (6)  stype:	A character string*(*) that contains the network and
C       		instrument type of the data, separated by '~'.
C
C       (7)  slat:	A float (real*4) that gives the station latitude in
C              		decimal degrees
C
C       (8)  slon:	A float (real*4) that gives the station longitude in
C       	        decimal degrees
C
C       (9)  elev:	A float (real*4) that gives the elevation of the
C       		station (in meters)
C
C       (10) cad:       A float (real*4) that gives the azimuth/orientation
C                       of the channel
C
C       (11) DS:	A float (real*4) that gives the gain of the station
C
C       (12) A0:	A float (real*4) that gives the normalization of the
C       		station
C
C       (13) rtype:     Character string 'RESP' or 'PZ' giving response type.
C
C       (14) rpol:	A float (real*4) array by 30 containing the real
C       		part of the poles of the station calibration.  The
C       		first element of this array is taken as the number
C       		of poles in the AH filter library.
C
C       (15) rzro:	A float (real*4) array by 30 containing the real
C       		part of the zeros of the station calibration.  The
C       		first element of this array is taken as the number
C       		of zeros in the AH filter library.
C
C       (16) ipol:	A float (real*4) array by 30 containing the imaginary
C       		part of the poles of the station calibration.  The
C       		first element of this array is ignored by the AH filter
C       		library.
C
C       (17) izro:	A float (real*4) array by 30 containing the imaginary
C       		part of the zeros of the station calibration.  The
C       		first element of this array is ignored by the AH filter
C       		library.
C
C       (18) lat:	a float (real*4) that gives the latitude of the
C       		earthquake in decimal degrees
C
C       (19) lon:	a float (real*4) that gives the longitude of the
C       		earthquake in decimal degrees
C
C       (20) dep:	a float (real*4) that gives the depth of the earthquake
C       		in kilometers
C
C       (21) mag:	a float (real*4) array giving log file `mb' and `ms'
C
C       (22) ot:	a character string containing the origin time of the
C       		earthquake in yyyymodyhrmmss.## format, where the yy, mo,
C       		dy, hr, and mm are converted to short (integer*2)
C       		format, and as such should be of an i2 format (format
C       		in this sense is that of the FORTRAN output formats)
C       		while the ss.## is converted to a float (real*4)
C       		number, so any number of decimal places could be
C       		included in the seconds field of the character string.
C       		Note:  this character string should be of length one
C       		more than the length necessary to define the string
C       		being passed.  As with all C functions, the character
C       		string variables must end with a null character here.
C       		For example, if 5 decimal places are needed in the
C       		accuracy of the seconds variable, then a format of
C       		f8.5 would be needed to define the seconds field.  The
C       		resulting FORTRAN character string would need to be
C       		at least a character by 18, so it should be defined
C       		as a character by 19 in order to avoid overwriting
C       		other data points that are being passed as part of
C       		the argument list when placing the null character at
C       		the end of the string ot.
C
C       (23) ecomment:	a character string by 80 that contains any special
C       		comments that the user wishes to make about the
C       		earthquake
C
C       (24) type:	a short (integer*2) number that tells the program
C       		the type of data represented in the data_in array
C       		(see below).  The choices are;
C
C       	(a)  1 = float data		(b)  2 = complex data
C       	(c)  3 = vector (2-d) data	(d)  4 = tensor (2-d symmetric)
C       	(e)  6 = double precision data
C
C       	this program has been written explicitly for float data, as
C       	can be seen in the definition of data_in below, but some
C       	relatively minor changes could be made to the code to support
C       	other data types if desired.
C
C       (25) ndata:	a long (integer*4) variable that gives the number of
C       		data points in the input routine.  Note:  if the data
C       		is complex, then it should appear as multiplexed data
C       		in the data_in array (real1, imaginary1, real2, etc.)
C       		but ndata would be the number of complex data points,
C       		not the total number of real and imaginary numbers
C       		in the array.
C
C       (26) delta:	a float (real*4) that gives the sampling rate
C       		(in seconds per sample) of the data
C
C       (27) eazi:    	a float(real*4) that gives the earthquake back-azimuth
C       		from the station (in degrees)
C
C       (28) abstime:	a character string (see ot above for format) that
C       		gives the start time of the record
C
C       (29) rmin:	a float (real*4) that gives the minimum value
C       		of the abscissa
C
C       (30) rcomment:	a character string by 80 that contains comments about
C       		the record
C
C       (31) log:	a character string by 202 that contains a record of the
C       		operations that have been performed on the data in
C       		the record
C
C       (32) extra:	a float (real*4) array by 21 that contains space for
C       		any extra real numbers that the user might want to save
C       		(essentially yours to do with as you please)
C
C       (33) data_in:	a variable length float (real*4) array of the
C       		amplitudes that are to be saved in the record.  As
C       		mentioned above, complex and vector data should be
C       		contained in the array in a multiplexed manner, as
C       		should the tensor data type, however I have restricted
C       		the output of this routine to be real number data only.
C
C       The rest of the arguments contained in the definition of the function
C       below are simply the lengths of the character strings in the definition
C       in the order that they are included.  It is not necessary for the user
C       to include these variables in the call, they are automatically included
C       by the calling FORTRAN routine.  The user does not need to worry
C       about placing a null character at the end of all of the character
C       strings that are sent through to this function either (C needs a
C       null character at the end of every character string to define its
C       limits).  I have included a function that adds null spaces to the
C       ends of character strings as necessary (see add_null.c for further
C       description), either at the end of the strings defined length or
C       after removing white space from the end of the string (as is the case
C       with the filename).  This function should work on any operating
C       system that supports the XDR format, however the program has only
C       been tested on a Sun4 operating system to date
C
C       Upon successful completion, the function returns the value of
C       the number of data points that should have been written, allowing
C       for easy error trapping.  Errors all return error messages to
C       stderr and a negative number to the calling program.  The error
C       codes are as follows:
C
C       		(-1)	->	failed to open output file, program
C       				will terminate.
C
C       		(0)	->	error when writing to the
C       				file, program will terminate
C
C       				--- tjm 24 November, 1990
C
C       Change log:
C          Write out instrument response file for each trace.
C          GRH 18 Sep 91
C          Set DS (gain) value to SCALE value in header.
C          GRH 23 Sep 91
C          Fix bug in OMARKER setting
C          GRH 24 Sep 91
C          Add component orientation parameter
C          GRH 25 Jul 95
C          Add event id parameter
C          GRH 29 Jan 05
C          Add event mag parameter
C          GRH 16 Sep 07
C          Add network and instrument type
C          GRH 27 Dec 09
C          Add LOCID
C          GRH 11 Nov 15
C          Add RESP output
C          GRH 20 Feb 23
C       
      integer*2 type
      integer ndata
      real slat, slon, elev, cad, DS, A0, lat, lon, dep
      real delta, eazi, rmin, extra(21), data_in(ndata)
      real rpol(30), rzro(30), ipol(30), izro(30), mag(2)
      character*(*) evntid, filename, wtype, ot, abstime
      character*(*) ecomment, rcomment, log, code, chan, stype, rtype
      character fntest*256
      logical exists

C     Find I/O unit to use
      do iunit=7,99
	 inquire(iunit,opened=exists)
	 if (.not.exists) go to 10
      enddo
      write(0,*) '**WRITE_SAC:  Can''t find I/O unit to use.'
      write_sac = -1
      return

10    continue
     

	call newhdr

C       Station information.  Pick apart subfields of stype (inst, net, locid)
        isf1 = index(stype,'~')
        isf2 = index(stype(isf1+1:),'~')
	call setkhv('KSTNM',code,nerr)
	call setkhv('KCMPNM',chan,nerr)
	call setkhv('KINST',stype(isf1+1:isf1+isf2-1),nerr)
	call setkhv('KEVNM',evntid,nerr)
	call setkhv('KNETWK',stype(1:isf1-1),nerr)
	if (stype(isf1+isf2+1:isf1+isf2+2) .ne. '  ')
     &     call setkhv('KHOLE',stype(isf1+isf2+1:),nerr)
	if (chan(3:3) .eq. 'Z') then
	   cmpinc = 90.0 - cad
	   cmpaz = 0.0
	else
	   cmpinc = 90.0
	   cmpaz = cad
	endif
	if (cmpaz .gt. 360.0) cmpaz = cmpaz - 360.0
	call setfhv('CMPINC',cmpinc,nerr)
	call setfhv('CMPAZ',cmpaz,nerr)

C       h.station.slat = *slat;
C       h.station.slon = *slon;
C       h.station.elev = *elev;
C       h.station.DS = *DS;
C       h.station.A0 = *A0;
        call setfhv('STLA',slat,nerr)
        call setfhv('STLO',slon,nerr)
        call setfhv('STEL',elev,nerr)
	call setfhv('SCALE',ds,nerr)

C       for(i = 0; i < NOCALPTS; i++){
C       	h.station.cal[i].pole.r = *rpol++;
C       	h.station.cal[i].zero.r = *rzro++;
C       	h.station.cal[i].pole.i = *ipol++;
C       	h.station.cal[i].zero.i = *izro++;
C       }

C       h.event.lat = *lat;			/* event information */
C       h.event.lon = *lon;
C       h.event.dep = *dep;
        call setfhv('EVLA',lat,nerr)
        call setfhv('EVLO',lon,nerr)
        call setfhv('EVDP',dep,nerr)
        call setfhv('MAG',mag(1),nerr)
	call gcd(lat,lon,slat,slon,gcdel,gckm,gcaz,gcbz)
	call setfhv('GCARC',gcdel,nerr)

C       strncpy(tempc, ot, 2);			/* break up data-time */
C       h.event.ot.yr = (short) atoi(tempc);
C       strncpy(tempc, (ot+2), 2);
C       h.event.ot.mo = (short) atoi(tempc);
C       strncpy(tempc, (ot+4), 2);
C       h.event.ot.day = (short) atoi(tempc);
C       strncpy(tempc, (ot+6), 2);
C       h.event.ot.hr = (short) atoi(tempc);
C       strncpy(tempc, (ot+8), 2);
C       h.event.ot.mn = (short) atoi(tempc);
C       strcpy(tempc, (ot+10));
C       h.event.ot.sec = (float) atof(tempc);
9001    format(i4,5i2,1x,i3)
	read(ot,9001) ieyy,iemo,iedd,iehh,iemm,iess,ieth

C       strcpy(h.event.ecomment, ecomment);
	if (ecomment .ne. 'none') call setkhv('KEVNM',ecomment,nerr)

C       h.record.type = *type;			/* record information */
C       h.record.ndata = *ndata;
C       h.record.delta = *delta;
C       h.record.maxamp = *maxamp;
        if (type .ne. 1) pause '**WRITE_SAC:  Type isn''t 1!'
        call setnhv('NPTS',ndata,nerr)
        call setfhv('B',0.0,nerr)
        call setfhv('E',(delta-1)*npts,nerr)
        call setfhv('DELTA',delta,nerr)

C       len = strlen(tempc);
C       if(len > 2)
C       	for(j = 2; j < len; j++)
C       		*(tempc+j) = '\0';

C       strncpy(tempc, abstime, 2);			/* break up data-time */
C       h.record.abstime.yr = (short) atoi(tempc);
C       strncpy(tempc, (abstime+2), 2);
C       h.record.abstime.mo = (short) atoi(tempc);
C       strncpy(tempc, (abstime+4), 2);
C       h.record.abstime.day = (short) atoi(tempc);
C       strncpy(tempc, (abstime+6), 2);
C       h.record.abstime.hr = (short) atoi(tempc);
C       strncpy(tempc, (abstime+8), 2);
C       h.record.abstime.mn = (short) atoi(tempc);
C       strcpy(tempc, (abstime+10));
C       h.record.abstime.sec = (float) atof(tempc);
	read(abstime,9001) ioyy,iomo,iodd,iohh,iomm,ioss,ioth
	call getjday(ioyy,iomo,iodd,iojday)
	call setnhv('NZYEAR',ioyy,nerr)
	call setnhv('NZJDAY',iojday,nerr)
	call setnhv('NZHOUR',iohh,nerr)
	call setnhv('NZMIN',iomm,nerr)
	call setnhv('NZSEC',ioss,nerr)
	call setnhv('NZMSEC',ioth,nerr)

        call timedif(ioyy,iomo,iodd,iohh,iomm,ioss+ioth/1000.,
     &               ieyy,iemo,iedd,iehh,iemm,iess+ieth/1000.,dt)
	call setfhv('O',dt*60.0,nerr)

C       h.record.rmin = *rmin;
C       strcpy(h.record.rcomment, rcomment);
C       strcpy(h.record.log, log);

C       for(i = 0; i < NEXTRAS; i++)
C       	h.extra[i] =  *extra++;

C       Copy data to file.  Action taken depends on the wtype parameter,
C       but with present SAC capabilities, only "n" applies to SAC data.
C       if (wtype .ne. 'n') then
C	   write(*,*) 
C    &        ' **Can''t put multiple output datasets in a SAC file.'
C          go to 91
C       endif
C       See if output file exists.
	fntest = filename
	inquire(file=fntest,exist=exists)
	if (exists) go to 91
	call wsac0(fntest,data_in,data_in,nerr)
	if (nerr .ne. 0) go to 92
C       Write poles and zeroes file, if wanted.  This includes file ID, event
C          and record comments, and log of actions taken.
C       Digital sensitivity (DS or gain) is in the file header variable
C          SCALE.
        if (rtype.eq.'PZ') then
	   fntest = filename(1:index(filename,' ')-1)//'.pz'
	   inquire(file=fntest,exist=exists)
	   if (exists) go to 91
	   open(iunit,file=fntest)
	   write(iunit,1001,err=92) filename(1:index(filename,' ')-1)
	   if (ecomment .ne. 'none') 
     &        write(iunit,1001,err=92) ecomment(1:lenb(ecomment))
	   if (rcomment .ne. 'none') 
     &        write(iunit,1001,err=92) rcomment(1:lenb(rcomment))
	   if (lenb(log) .ne. 0 .and. log .ne. 'none') 
     &        write(iunit,1001,err=92) log(1:lenb(log))
	   write(iunit,1005,err=92) ds,a0
	   write(iunit,1002,err=92) ifix(rpol(1))
	   do 12 i=1,ifix(rpol(1))
	      j = i+1
	      write(iunit,1003,err=92) rpol(j),ipol(j)
12         continue
           write(iunit,1004,err=92) ifix(rzro(1))
	   do 14 i=1,ifix(rzro(1))
	      j = i+1
	      write(iunit,1003,err=92) rzro(j),izro(j)
14         continue
	   close(iunit)
        endif
	write_sac = ndata
	return
1001    format('* ',a)
1002    format('POLES ',i2)
1004    format('ZEROS ',i2)
1003    format(1p,2(e12.4,1x))
1005    format('* GAIN: ',1p,e12.4,/,'CONSTANT ',e12.4)

91      continue
	close(iunit)
	write_sac = -1
	return

92      continue
	close(iunit)
	write_sac = 0
	return
        end
