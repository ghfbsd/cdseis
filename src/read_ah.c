#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#include <fcntl.h>      /* For open() */
#include "ahhead.h"

#define	TIMESIZE	100
#define ABORT		-3

FILE *fptr1 = NULL;		/* keep file open until done with it */
XDR xdr_in;			/* for use in constructing xdr data stream */
int test_first1 = 1;
char old_filename[100];

/* read_ah:  reads a file containing ah formatted data

	This routine is a FORTRAN callable C function that reads data
		in the form of an XDR binary AH (ad hoc) format file and 
		creates arrays of real numbers and character strings that
		contain the data from the file.  The inputs
		needed by the function are as follows

	(1)  filename:  A character string containing the name of the file
			that the data are to be read from.  If the filename
			is given as "stdin" or "STDIN" then standard input
			will be used rather than a file (allows for
			i/o redirection)

	(2)  npts_max:	An int (integer*4) that indicates the maximum number
			of points that should be read from the file for
			each trace.  The function tests the number of points
			in each trace versus this limit, if it is exceeded
			the function returns an error condition of -1.

	(3)  code:	A character string by 6 containing the station code
			for the station that the data was recorded at

	(4)  chan:	A character string by 6 containing the name of the
			data channel that the data was recorded on

	(5)  stype:	A character string by 8 containing the type of
			station the data was recorded on DWWSSN, RSTN, etc.

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
			first element of this array is the number of poles.

	(12) rzro:	A float (real*4) array by 30 containing the real
			part of the zeros of the station calibration. The
			first element of this array is the number of zeros.

	(13) ipol:	A float (real*4) array by 30 containing the imaginary
			part of the poles of the station calibration.  The first
			element of this array is ignored.

	(14) izro:	A float (real*4) array by 30 containing the imaginary
			part of the zeros of the station calibration.  The first
			element of this array is ignored.

	(15) lat:	a float (real*4) that gives the latitude of the
			earthquake in decimal degrees

	(16) lon:	a float (real*4) that gives the longitude of the
			earthquake in decimal degrees

	(17) dep:	a float (real*4) that gives the depth of the earthquake
			in kilometers

	(18) ot:	a character string by 18 containing the origin time of
			the earthquake in yymodyhrmmss.#### format, where the
			first 10 characters are the yy, mo, dy, hr, and mm of
			the origin time of the event, while the seconds are
			contained in the next 7 characters (ss.####) in an
			f7.4 format (format here is in the sense of a FORTRAN
			format).  Note:  here the character string should be
			defined as character*18 or greater in length, otherwise
			the sprintf statements below will not work properly,
			needs an extra space at the end for a null character!!!

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
		other data types if desired.  If input data is of another type,
		it will be coerced to float by this routine before output.
		for types 2, 3, and 4 the data array that is returned to
		the FORTRAN calling routine is actually the multiplexed form
		of the data.

	(21) ndata:	a long (integer*4) variable that gives the number of
			data points in the input routine.  Note:  if the data
			is complex, then it will appear as multiplexed data
			in the data_in array (real1, imaginary1, real2, etc.)
			but ndata would be the number of complex data points,
			not the total number of real and imaginary numbers
			in the array.

	(22) delta:	a float (real*4) that gives the sampling rate
			(in seconds per sample)

	(23) maxamp:	a float(real*4) that gives the (absolute value of) the
			maximum amplitude in the array data_in

	(24) abstime:	a character string (see ot above for format) that
			gives the start time of the record

	(25) rmin:	a float (real*4) that gives the minimum value
			of the abscissa

	(26) rcomment:	a character string by 80 for comments about
			the record

	(27) log:	a character string by 202 that contains a record of the
			operations that have been performed on the data in
			the record

	(28) extra:	a float (real*4) array by 21 (0:20) that contains space
			for any extra real numbers that the user might want to
			save (essentially yours to do with as you please)

	(29) data_in:	a variable length float (real*4) array of the
			amplitudes that are to be read from the record.  As
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
	the total number of data points that were read into the array,
	as opposed to the number of data points read in (see ndata above
	for the distinction), allowing for easy error trapping.  All return
	error messages are sent to stderr, and a negative number is sent
	to the calling program.  The error codes are as follows:

			(-1)     ->     insufficient space in matrix to
					read the data in

			(-2)     ->     error opening file for input

			(-3)     ->     error in data (not a recognized type,
					not possible to read data as AH format,
					etc.

	The function returns a value of 0 when the EOF is reached.

					--- tjm 31 January, 1991

*/

int read_ah_ (filename, npts_max, code, chan, stype, slat, slon, elev, DS,
		A0, rpol, rzro, ipol, izro, lat, lon, dep, ot, ecomment, type,
		ndata, delta, maxamp, abstime, rmin, rcomment,
		log, extra, data_in, lfilename, lcode, lchan, lstype,
		lot, lecomment, labstime, lrcomment, llog)

long *npts_max;
short *type;
long *ndata;

float *slat, *slon, *elev, *DS, *A0, *lat, *lon, *dep;
float *delta, *maxamp, *rmin, *extra, *data_in;
float *rpol, *rzro, *ipol, *izro;

char *filename, *ot, *abstime;
char *ecomment, *rcomment, *log, *code, *chan, *stype;

int lfilename, lot, labstime, lecomment, lrcomment;
int llog, lcode, lchan, lstype;
{
	int i, test, mult, add_null();
	int xdr_gethead(), xdr_getdata();

	ahhed h;			/* ah header structure */

	FILE *fopen();

/* add null characters to character string arguments that are used later
	in this function (remove spaces first) */

	add_null(filename, lfilename - 1, 'a');
	add_null(code, lcode - 1, 'a');
	add_null(chan, lchan - 1, 'a');
	add_null(stype, lstype - 1, 'a');
	add_null(ot, lot - 1, 'a');
	add_null(ecomment, lecomment - 1, 'a');
	add_null(abstime, labstime - 1, 'a');
	add_null(rcomment, lrcomment - 1, 'a');
	add_null(log, llog - 1, 'a');

/* only opens a new file if is the first time through or the filename is
	different from the last time the function was called */

	if(test_first1 && (strcmp(filename, "stdin") == 0
			|| strcmp(filename, "STDIN") == 0)) {
		test_first1 == 0;
		xdrstdio_create(&xdr_in, stdin, XDR_DECODE);
	}
	else if((fptr1 == NULL || strcmp(filename, old_filename) != 0)
			&& test_first1) {
		if(fptr1 != NULL){	       /* if file already open */
			fclose (fptr1);
			fptr1 = NULL;
		}
		if ((fptr1 = fopen (filename, "r")) == NULL) {
			fprintf (stderr, "Failed to open input file ");
			fprintf (stderr, "'/%s'\n",filename);
			perror ("read_ah:open");
			return (-2);	     /* Failed to open the file */
		}
		strcpy(old_filename, filename);
		xdrstdio_create(&xdr_in, fptr1, XDR_DECODE);
	}

/* read header; if problem reading header, return a 0 to calling routine to
	signal the EOF..... */

	if((test = xdr_gethead(&h, &xdr_in)) != 1){
		if(test == -1){
			fclose (fptr1);
			return (0);
		}
		else{
			fprintf(stderr,"Error reading ah header.\n");
			fprintf(stderr," invalid data type.\n");
			perror ("read_ah_:read");
			fclose (fptr1);
			return (ABORT);
		}
	}	       /* End if error reading header */

/* copy origin time of the event into the date-time ot */

	sprintf(ot, "%2i%2i%2i%2i%2i%7.4f", h.event.ot.yr,h.event.ot.mo,
		h.event.ot.day,h.event.ot.hr,h.event.ot.mn,h.event.ot.sec);

/* copy time of the file start into the date-time abstime */

	sprintf(abstime, "%2i%2i%2i%2i%2i%7.4f", h.record.abstime.yr,
		h.record.abstime.mo,h.record.abstime.day,h.record.abstime.hr,
		h.record.abstime.mn,h.record.abstime.sec);

/* copy strings from header to output strings*/

	strncpy(code, h.station.code, CODESIZE-1);
	strncpy(chan, h.station.chan, CHANSIZE-1);
	strncpy(stype, h.station.stype, STYPESIZE-1);
	strncpy(ecomment, h.event.ecomment, COMSIZE-1);
	strncpy(rcomment, h.record.rcomment, COMSIZE-1);
	strncpy(log, h.record.log, LOGSIZE-1);

/* stack station and event information into variables for output */

	*slat = h.station.slat;		/* station information */
	*slon = h.station.slon;
	*elev = h.station.elev;
	*DS = h.station.DS;
	*A0 = h.station.A0;
	for(i = 0; i < NOCALPTS; i++){
		*rpol++ = h.station.cal[i].pole.r;
		*rzro++ = h.station.cal[i].zero.r;
		*ipol++ = h.station.cal[i].pole.i;
		*izro++ = h.station.cal[i].zero.i;
	}

	*lat = h.event.lat;			/* event information */
	*lon = h.event.lon;
	*dep = h.event.dep;

	*type = h.record.type;			/* record information */
	*ndata = h.record.ndata;
	*delta = h.record.delta;
	*maxamp = h.record.maxamp;
	*rmin = h.record.rmin;
	for(i = 0; i < NEXTRAS; i++)
		*extra++ = h.extra[i];

	switch (h.record.type) {
	case FLOAT:
	case DOUBLE:
		mult = 1;
		break;
	case COMPLEX:
	case VECTOR:
		mult = 2;
		break;
	case TENSOR:
		mult = 3;
		break;
	default:
		fprintf(stderr, "%s: unrecognized data type\n",filename);
		return(ABORT);
		break;
	}

	if((mult*(*ndata)) > *npts_max){	/* not enough space for input */
		return(-1);
	}

/* read data..... */

	if(xdr_getdata(&h, (char *) data_in, &xdr_in) <= 0){
			fprintf(stderr,"Error reading ah data,");
			perror ("read_ah_:read");
			fclose (fptr1);
			return (ABORT);
	}	       /* End if error reading data */

	return(mult * (*ndata));
}
