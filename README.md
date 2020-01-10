CDSEIS reads seismic data from SEED volumes and writes SAC (or AH) files for
analysis.  Unlike other SEED-reading tools, the files that it outputs are event
oriented, cut around the arrival times of specific (user-tailored) phases, and
named using event-specific names.  The files have event and station information
populating the header information for the files.  The result is a collection of
traces that is immediately available for analysis.

To select waveforms for analysis, CDSEIS uses an ascii index log file, made by
MAKELOG for each SEED volume (or CD).  The logs summarize the waveform start
and stop times and associates each waveform with an event.  The user specifies
various search parameters to identify the desired seismograms.  Once the desired
time windows are identified, they can then be read directly from the SEED volume
or CD.  The use of an index file results in greatly increased speed compared to
searching through the entire SEED volume or CD to find the desired time series.

CDSEIS uses a command language (similar to PLOTXY, etc.) to decide which
waveforms to extract.  The basic idea is to define a number of search parameters
which are used to select which seismograms to read.  The defaults defined for
most of the parameters minimize user input.  CDSEIS reads the input file and
outputs seismograms.

Originally, CDSEIS was written to extract data from GDSN CD-ROMs, but it was
expanded to work with SEED-oriented datasets.  CDSEIS was written by Peter
Shearer (1989) to run on a Macintosh.  Ken Creager added several options and
modified the code to run in a UNIX environment.  Tom McSweeney wrote the
routine write_ah.c.  Doug Wiens wrote an early version of the instrument
response reader.  George Helffrich added SAC output and implemented reading
of SEED volumes, and is responsible for continued maintenance of CDSEIS.

Please report bugs, suggestions, and comments to G. Helffrich.

HOW TO BUILD IT
===============

| ------------- | -------------------------------------------------------- |
| `./configure` | `## run configuration - to get help, ./configure --help` |
| `make`        | `## makes cdseis, makelog, make_tt` |
| `make test`   | `## extracts SAC traces from a SEED volume for testing` |
| `make install`| `## install programs in binary directory of choice` |

The configure step tries to find the SAC libraries (for cdseis) and the
Buland & Kennett tau-p routines (for make_tt).  If they are not available, or
in an unexpected place, compilation will either fail (cdseis) or will produce
no output (make_tt).  If a failure occurs, use  
`   ./configure LDFLAGS='-L<directory-with-libsacio.a>'`  
or  
`   ./configure SACAUX=<directory-with-SAC-aux-files>`  
to help the configure process find the SAC libraries.

To print the documentation to the screen or a hardcopy device by:
`   man cdseis`  
or  
`   man makelog`  
To get hard copy, use
`   man -t cdseis > cdseis.ps`  
or `groff -man -t cdseis.man > cdseis.ps`  
and view cdseis.ps with your favorite PostScript reader.  Use similar methods
to get makelog documentation.

> You may wish to change the default values of some of the 
parameters to suit your particular needs.  They all may be overridden in the
cdseis input file.  However, the default values are all set in data 
statements in the main program (cdseis.f).  In particular, expert programmers
might wish to change the default directories for the phase files, and catalog
files, and the cdroms, which are in variables phsdir, logdir, and cddir.

UNINSTALLING IT
===============

Type

`make uninstall`

to remove the programs and travel time data files.

USAGE
=====

There are three commonly used programs in the src directory: makelog, make_tt
and cdseis itself.

1) MAKELOG reads a list of SEED volumes and makes an ASCII log file of all
waveform start and stop times, associating each waveform with an event.  The
event list is either taken from the SEED volume itself (if it contains one),
or from an external catalog provided to the program.  Possible catalog formats
are the Global CMT .ndk format or a file with one entry per line (see the
file test/events.dat for an example or read the program program for format
details).

> Originally, MAKELOG read an entire cdrom and made an ASCII log file of 
all waveform start and stop times, associating each waveform with 
an event.  On a SUN sparcstation this took about 1-2 hours for each 
cdrom.  This program also read all the station corrections and instrument
response poles and zeros, and reported errors and warnings to standard
output when it does not recognize the format.  CONVERTLOG essentially tidied
up the output from this program.  (This version of MAKELOG is in the src/cd
directory for historical completeness.)

2) CDSEIS reads the modified log created by MAKELOG, accepts a 
variety of search parameters in an interactive mode, and either 
creates a list of available waveforms (without reading data) or 
retrieves requested waveforms from the SEED volume (or a CD).  

3) MAKE_TT makes up a set of approximate phase travel time tables from the
Kennett and Buland tau-p routines.  CDSEIS uses these tables to calculate
expected arrival times for extracting data around major phase arrivals.
The routine uses the subroutine interface to tau-p; you need to have a
working version of it to compile this program.

4) CONVERTLOG (in src/cd for historical completeness) reads a cdrom log created
by MAKELOG, removes trailing blanks to reduce the file size, and assigns a
unique event designation to each event.

The programs can be compiled by typing: `make all`  
The programs can be tested by typing: `make test`  
The programs can be installed by typing: `make install`  

Some example (approximate) travel time curves are given in files 
beginning with `tt_`.  They will be installed in a library directory associated
with CDSEIS (see cdseis.in.scan for the location; when `make install' is done,
you'll get a report of the location too).

You will need to use MAKELOG to create a log file for new SEED volumes that you
receive.  MAKELOG is now fairly robust in dealing with SEED volumes, and is
unlikely to fail, but it is possible that MAKELOG could crash on a subsequent
SEED volume because of some peculiarity associated with new data.  After
running MAKELOG always check the log file, makelog.log, for errors and warnings.

> Logs created by the old MAKELOG and CONVERTLOG for almost all CDs that were
ever distributed by seismological agencies are supplied in the directory
logfiles, in files named files log*.  The first CD put out by the NEIC was
distributed with integer bytes in the order that is standard for DEC computers
and has the number 5033 stamped on the front of the disk.  It was also
distributed using the UNIX convention and has the number 5054 stamped on the
front.  As far as I can tell these are identical except for the order of the
bytes in each integer.  The log file 'log5033' works for either disk.  All
subsequet CD's use the UNIX convention.  CDSEIS reads 5054 correctly if the
variable 'byteswap' in the main program is set to '.false.' and will read 5033
properly if 'byteswap' is set to '.true.'.

EXAMPLES
========

Example input for cdseis
------------------------

`COMM This test file extracts events suitable for P receiver function analysis`  
`LDIR ./`  
`PDIR ../tt`  
`CDIR ./seed`  
`ODIR /tmp`  
`DMIN 1999 06 15  0  0`  
`DMAX 1999 06 15 23 59`  
`RANG 30, 95`  
`QMAG 0.0, 10.0`  
`WIND -2 3`  
`PHAS tt_p`  
`SRAT 1 100`  
`COMP 1`  
`FILE st ev p ch`  
`LOGF CDLV-RF.log`  
`STAT`  
`READ`  
`QUIT`  

This extracts one three-component seismogram from a SEED volume named
described in the log file CDLV-RF.log for an earthquake on 15 June 1999.

`comm This extracts three-component seismograms starting before the P wave`  
`comm arrival to 20 minutes after it, for an event on 28 Aug. 1985.  It`  
`comm reads data from the CDROM labeled 5461.`  
`dmin 85 8 28 0 0`  
`dmax 85 8 28 23 59`  
`cdir /cdrom`  
`logf log5461`  
`srat 2 16`  
`phas tt_p`  
`wind -2 20`  
`file ev ch`  
`otyp sac`  
`wtyp n`  
`stat`  
`comp 1`  
`read`  
`quit`  

This gives 3 files for vertical, north and east components of intermediate-
period seismograms from a deep-focus earthquake. The vertical
component seismograms can be displayed using 'read *.ihz' if you have
SAC running.  Make sure CDROM 5461 is mounted at the directory
/cdrom. You can get a list of the seismograms by replacing 'read' command by
by 'scan'.

Example input for makelog
-------------------------

`(cd DDIR ; ls *.seed | \`  
`  makelog -cat cmt /tmp /usr/share/data/CMT/cmtdat ) > logDDIR`  

This tells makelog to scan all of the SEED volumes in the directory DDIR ending
with the name `*.seed` and to find all of the events associated with data
traces in the SEED volume with earthquakes in the copy of the CMT catalog in
`/usr/share/data/CMT/cmtdat`.  The resulting log file is named `logDDIR`.  To
extract data, you would run cdseis with `cdir DDIR` and `logf logDDIR`.  (See
[the Global CMT Project](https://www.globalcmt.org/CMTfiles.html) for CMT
catalog download information.)

CHANGES AND BUG FIXES IN CDSEIS 2.0
===================================

1.  The time corrections were given, improperly, as either the first or last
    time correction in the station log.  It now properly interpolates the 
    given time corrections to the time of the first sample in the seismogram.
2.  Fix bug where gain is returned incorrectly if 3 component SP data is 
    followed by LP.
3.  ##Data output in AH format now has gain reported in counts/meter, instead
    of counts/micrometer.##
4.  Cdseis now reads the band (short-period, long-period, etc.) from the 
    station log, and puts this in the filename if requested.  The channel
    name in the headers is converted to SEED format (eg LHZ for long-period
    high gain, vertical).  Channel used to be called VERT for vertical.
5.  SCAN will output a subset of the log file in the log-file format.
6.  Add COMM, * commands for comments in input files.
7.  Make STAT output prettier, align.
8.  Comment out some debugging code that was left active. It printed stuff on
    range/midpoint searches.
9.  New station codes.  Made changes to SROSTA2 and stalist.  In the present
    version, this code isn't used, but one of these may form the basis of the
    subroutine that Peter Shearer was asking for.  Instead, the station log is
    parsed to obtain the station names, locations and data format information
    on the CD being processed.  I'm somewhat chary of the instrument locations
    in this list simply because a lot of it is typed in by hand and there is
    a potential of screwing up the locations (thus the ckstainfo program).
    I checked the ones I added, but not the original ones.
10. Fix bug on EOF -- causes infinite loop.  Proper command is QUIT not STOP.
11. Moved nonblank length and file name subroutines into cdsubs.f -- used by
    write_sac.f
12. Omit blank first output line in CONVERTLOG.
13. Change julian date handling to eliminate 1995 restriction.
14. Obtain station info from the data log when the data is being retrieved.
    This change was needed because on some CDs the same station has different
    GDSN ID numbers, and the same GDSN ID number refers to different stations
    on different CDs.
15. OTYP=10 added and set to default:  Get any available orientations.
    If OTYPE=5 during a search, all traces (both
    horizontals & verticals) are listed.  If doing a READ, only the vertical
    component is extracted.  Thus, you can't match up a SCAN listing with the
    result of a READ if OTYPE=5.  Adding OTYPE=10 lets you see and ask for any 
    data during either READ and SCAN, and lets you get *only* vertical component
    data if OTYPE=5.  In other words, what you see is what you get whether
    you SCAN or READ.
16. Fix zero division bug if you extract data that is flatlined at zero.
17. Decode new data formats:  1) Grafenberg; 2) Echery; 3) GEOSCOPE 24-bit.
18. Make MAKELIST terminate gracefully if an EOF is given.
19. Make MAKELIST dump out a list of stations and locations encountered on
    the CD processed to the output file.
20. Name the new CDs depending on range of data.
21. SAC output format.
22. Poles and zeroes output in SAC format.
23. Comments are read from cdrom and passed to output files
24. Able to extract from SEED volumes
25. Station elevation and event magnitude in SAC output
26. Make CDSEIS exit gracefully if EOF on input given.
27. Fix pole-zero response information parsing and output.
28. Add network and instrument type to station type information.
29. Improve handling of time-dependent channel descriptions (bug fix).
30. Handle new seed data block types (M, Q, etc.)
31. Make g77 & gfortran compatible.
32. Put network name and LOCID into SAC output files


WISHLIST
- Change log file format to recognize that different LOCIDs with the same
  sample rate may be present at one station.  At present, if different LOCIDs
  have the same sample rate, they can't all be listed in the log file.  (The
  "seed" designator should also be changed at the same time to an identifier
  that takes up less space.)
- Change SAC output so that freq-amp-phase response information may be written
  out in SAC's FAP file format.
- Change output from SEED volumes so that EVALRESP response output may be
  produced for full response information.

Keep wishing for no bugs in the code.
