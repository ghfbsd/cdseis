#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#include <fcntl.h>      /* For open() */
#include "ahhead.h"

#define	TIMESIZE	100
#define ABORT		0

FILE *fptr = NULL;		/* keep file open until done with it */
XDR xdr_out;			/* for use in constructing xdr data stream */
int test_first = 1;

/* write_ah:  writes out an ah file containing the data from reading the cdrom
		with the the routine cdseis

	This routine is a FORTRAN callable C function that takes data
		in the form of arrays of real numbers and character strings
		and creates an XDR binary AH (ad hoc) format file.  The inputs
		needed by the function are as follows

	(1)  filename:  A character string containing the name of the file
			that the data is to be placed in.  If the filename
			is either "stdout" or "STDOUT", then the program
			will take redirect the output to standard output
			rather than opening a file for output

	(2)  wtype:	A character that determines the type of writing
			that should be done.  The user can select

		(a) wtype="a", which causes either
			(i) a new file to be opened if one is not already
				 open, and data to be written to the file or
			(ii) if a file is already open, the data will be
				written to the end of that file....
		(this is as close as I could get to an append mode for output,
		however once a file is closed, reopening it with append mode
		results in overwriting any data that was in the file.  This is
		one of the drawbacks to working with an XDR output stream,
		you are not allowed to reposition the pointer to the end
		of a file for output, repositioning of the file pointer is
		disregarded on output.)

			or the user can select

		(b) wtype="n", which causes a new file to be opened each time
			the function is called regardless of whether or not
			a file is currently open.  If this option is selected,
			any currently open files will be closed and a new file
			with the name "filename" will be opened.  If a file
			named filename already exists, then a file called
			"filename"-1 is created, then "filename"-2, etc. until
			the ninth file with that filename is opened.  When this
			happens, output of the data stops.

	(3)  code:	A character string by 6 that contains the station code
			for the station that the data was recorded at
			(GRFO, etc.)

	(4)  chan:	A character string by 6 that contains the name of the
			data channel that the data was recorded on (VERT, etc.)

	(5)  stype:	A character string by 8 that contains the type of
			station the data was recorded on (DWWSSN, RSTN, etc.)

	(6)  slat:	A float (real*4) that gives the station latitude in
			decimal degrees

	(7)  slon:	A float (real*4) that gives the station longitude in
			decimal degrees

	(8)  elev:	A float (real*4) that gives the elevation of the
			station (in meters)

	(9)  DS:	A float (real*4) that gives the gain of the station

	(10) A0:	A float (real*4) that gives the normalization of the
			station

	(11) rpol:	A float (real*4) array by 30 containing the real
			part of the poles of the station calibration.  The
			first element of this array is taken as the number
			of poles in the AH filter library.

	(12) rzro:	A float (real*4) array by 30 containing the real
			part of the zeros of the station calibration.  The
			first element of this array is taken as the number
			of zeros in the AH filter library.

	(13) ipol:	A float (real*4) array by 30 containing the imaginary
			part of the poles of the station calibration.  The
			first element of this array is ignored by the AH filter
			library.

	(14) izro:	A float (real*4) array by 30 containing the imaginary
			part of the zeros of the station calibration.  The
			first element of this array is ignored by the AH filter
			library.

	(15) lat:	a float (real*4) that gives the latitude of the
			earthquake in decimal degrees

	(16) lon:	a float (real*4) that gives the longitude of the
			earthquake in decimal degrees

	(17) dep:	a float (real*4) that gives the depth of the earthquake
			in kilometers

	(18) ot:	a character string containing the origin time of the
			earthquake in yyyymodyhrmmss.## format, where the yy, mo,
			dy, hr, and mm are converted to short (integer*2)
			format, and as such should be of an i2 format (format
			in this sense is that of the FORTRAN output formats)
			while the ss.## is converted to a float (real*4)
			number, so any number of decimal places could be
			included in the seconds field of the character string.
			Note:  this character string should be of length one
			more than the length necessary to define the string
			being passed.  As with all C functions, the character
			string variables must end with a null character here.
			For example, if 5 decimal places are needed in the
			accuracy of the seconds variable, then a format of
			f8.5 would be needed to define the seconds field.  The
			resulting FORTRAN character string would need to be
			at least a character by 18, so it should be defined
			as a character by 19 in order to avoid overwriting
			other data points that are being passed as part of
			the argument list when placing the null character at
			the end of the string ot.

	(19) ecomment:	a character string by 80 that contains any special
			comments that the user wishes to make about the
			earthquake

	(20) type:	a short (integer*2) number that tells the program
			the type of data represented in the data_in array
			(see below).  The choices are;

		(a)  1 = float data		(b)  2 = complex data
		(c)  3 = vector (2-d) data	(d)  4 = tensor (2-d symmetric)
		(e)  6 = double precision data

		this program has been written explicitly for float data, as
		can be seen in the definition of data_in below, but some
		relatively minor changes could be made to the code to support
		other data types if desired.

	(21) ndata:	a long (integer*4) variable that gives the number of
			data points in the input routine.  Note:  if the data
			is complex, then it should appear as multiplexed data
			in the data_in array (real1, imaginary1, real2, etc.)
			but ndata would be the number of complex data points,
			not the total number of real and imaginary numbers
			in the array.

	(22) delta:	a float (real*4) that gives the sampling rate
			(in seconds per sample) of the data

	(23) maxamp:	a float(real*4) that gives the (absolute value of) the
			maximum amplitude in the array data_in

	(24) abstime:	a character string (see ot above for format) that
			gives the start time of the record

	(25) rmin:	a float (real*4) that gives the minimum value
			of the abscissa

	(26) rcomment:	a character string by 80 that contains comments about
			the record

	(27) log:	a character string by 202 that contains a record of the
			operations that have been performed on the data in
			the record

	(28) extra:	a float (real*4) array by 21 that contains space for
			any extra real numbers that the user might want to save
			(essentially yours to do with as you please)

	(29) data_in:	a variable length float (real*4) array of the
			amplitudes that are to be saved in the record.  As
			mentioned above, complex and vector data should be
			contained in the array in a multiplexed manner, as
			should the tensor data type, however I have restricted
			the output of this routine to be real number data only.

	The rest of the arguments contained in the definition of the function
	below are simply the lengths of the character strings in the definition
	in the order that they are included.  It is not necessary for the user
	to include these variables in the call, they are automatically included
	by the calling FORTRAN routine.  The user does not need to worry
	about placing a null character at the end of all of the character
	strings that are sent through to this function either (C needs a
	null character at the end of every character string to define its
	limits).  I have included a function that adds null spaces to the
	ends of character strings as necessary (see add_null.c for further
	description), either at the end of the strings defined length or
	after removing white space from the end of the string (as is the case
	with the filename).  This function should work on any operating
	system that supports the XDR format, however the program has only
	been tested on a Sun4 operating system to date

	Upon successful completion, the function returns the value of
	the number of data points that should have been written, allowing
	for easy error trapping.  Errors all return error messages to
	stderr and a negative number to the calling program.  The error
	codes are as follows:

			(-1)	->	failed to open output file, program
					will terminate.

			(0)	->	error when writing ah record to the
					file, program will terminate

					--- tjm 24 November, 1990

*/
int write_ah_ (filename, wtype, code, chan, stype, slat, slon, elev, DS,
		A0, rpol, rzro, ipol, izro, lat, lon, dep, ot, ecomment, type,
		ndata, delta, maxamp, abstime, rmin, rcomment,
		log, extra, data_in, lfilename, lwtype, lcode, lchan, lstype,
		lot, lecomment, labstime, lrcomment, llog)

short *type;
long *ndata;

float *slat, *slon, *elev, *DS, *A0, *lat, *lon, *dep;
float *delta, *maxamp, *rmin, *extra, *data_in;
float *rpol, *rzro, *ipol, *izro;

char *filename, *wtype, *ot, *abstime;
char *ecomment, *rcomment, *log, *code, *chan, *stype;

int lfilename, lwtype, lot, labstime, lecomment, lrcomment;
int llog, lcode, lchan, lstype;
{
	int i, j, nbytes_write, add_null(), status;
	int xdr_putrecord(), len;
	long nelem;
	float *data;
	double atof();
	ahhed h;			/* ah header structure */
	char tempc[TIMESIZE];

	FILE *fopen();

/* add null characters to character string arguments that are used later
	in this function (remove spaces first) */

	add_null(filename, lfilename - 1, 'a');
	add_null(wtype, lwtype - 1, 'a');
	add_null(abstime, labstime - 1, 'a');
	add_null(ot, lot - 1, 'a');

/* add null characters at end of character strings for use in header */

	add_null(code, lcode - 1, 'e');
	add_null(chan, lchan - 1, 'e');
	add_null(stype, lstype - 1, 'e');
	add_null(ecomment, lecomment - 1, 'e');
	add_null(rcomment, lrcomment - 1, 'e');
	add_null(log, llog - 1, 'e');

/* null out characters in array tempc */

	for(i = 0; i < TIMESIZE; i++)
		tempc[i] = '\0';

/* stack event information into header structure for output */

	strcpy(h.station.code, code);		/* station information */
	strcpy(h.station.chan, chan);
	strncpy(h.station.stype, stype, sizeof(h.station.stype));
	h.station.slat = *slat;
	h.station.slon = *slon;
	h.station.elev = *elev;
	h.station.DS = *DS;
	h.station.A0 = *A0;
	for(i = 0; i < NOCALPTS; i++){
		h.station.cal[i].pole.r = *rpol++;
		h.station.cal[i].zero.r = *rzro++;
		h.station.cal[i].pole.i = *ipol++;
		h.station.cal[i].zero.i = *izro++;
	}
	h.event.lat = *lat;			/* event information */
	h.event.lon = *lon;
	h.event.dep = *dep;

	strncpy(tempc, ot, 4);			/* break up data-time */
	h.event.ot.yr = (short) atoi(tempc);
	strncpy(tempc, (ot+4), 2);
	h.event.ot.mo = (short) atoi(tempc);
	strncpy(tempc, (ot+6), 2);
	h.event.ot.day = (short) atoi(tempc);
	strncpy(tempc, (ot+8), 2);
	h.event.ot.hr = (short) atoi(tempc);
	strncpy(tempc, (ot+10), 2);
	h.event.ot.mn = (short) atoi(tempc);
	strcpy(tempc, (ot+12));
	h.event.ot.sec = (float) atof(tempc);

	strcpy(h.event.ecomment, ecomment);

	h.record.type = *type;			/* record information */
	h.record.ndata = *ndata;
	h.record.delta = *delta;
	h.record.maxamp = *maxamp;

	len = strlen(tempc);
	if(len > 2)
		for(j = 2; j < len; j++)
			*(tempc+j) = '\0';

	strncpy(tempc, abstime, 4);			/* break up data-time */
	h.record.abstime.yr = (short) atoi(tempc);
	strncpy(tempc, (abstime+4), 2);
	h.record.abstime.mo = (short) atoi(tempc);
	strncpy(tempc, (abstime+6), 2);
	h.record.abstime.day = (short) atoi(tempc);
	strncpy(tempc, (abstime+8), 2);
	h.record.abstime.hr = (short) atoi(tempc);
	strncpy(tempc, (abstime+10), 2);
	h.record.abstime.mn = (short) atoi(tempc);
	strcpy(tempc, (abstime+12));
	h.record.abstime.sec = (float) atof(tempc);

	h.record.rmin = *rmin;
	strcpy(h.record.rcomment, rcomment);
	strcpy(h.record.log, log);

	for(i = 0; i < NEXTRAS; i++)
		h.extra[i] =  *extra++;

/* then save header and data to filename, if there is space.  note: only
	opens a new file if is the first time through or *wtype != 'a'
	(i.e. do not want to append to previous file) */

	if(test_first && (strcmp(filename, "stdout") == 0
			|| strcmp(filename, "STDOUT") == 0)) {
		test_first == 0;
		xdrstdio_create(&xdr_out, stdout, XDR_ENCODE);
	}
	else if((fptr == NULL || *wtype != 'a') && test_first){
		if(fptr != NULL){		/* if file already open */
			fclose (fptr);
			fptr = NULL;
		}
		if ((fptr = fopen (filename, "a")) == NULL) {
			fprintf (stderr, "Failed to open output file.\n");
			perror ("write_ah_:open");
			return (-1);		 /* Failed to open the file */
		}
		xdrstdio_create(&xdr_out, fptr, XDR_ENCODE);
	}
	if(xdr_putrecord(&h,(char *)data_in, &xdr_out) != 0){
		fprintf(stderr,"Error writing ah record.\n");
		perror ("write_ah_:write");
		fclose (fptr);
		return (ABORT);
	}		/* End if error writing record */
	return(h.record.ndata);
}
