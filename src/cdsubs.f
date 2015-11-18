c
      SUBROUTINE SROSTA2(IB,JSTA,JSOR,IERR)
      PARAMETER (NSTAT=114)
      integer*2 ib
      INTEGER*4 IRCODE(3,NSTAT),JSTA,JSOR,XXXX,YYYY
      CHARACTER*4 CXXX,CYYY
      EQUIVALENCE (CXXX,XXXX),(CYYY,YYYY)
C     save ircode
C     DATA ((IRCODE(I,J),I=1,3),J=1,38)/
C    + 'ANMO',  30,  'SRO ',  'ANTO',  31,  'SRO ',  
C    + 'BOCO',  32,  'SRO ',  'CHTO',  33,  'SRO ',
C    + 'BGIO',  34,  'SRO ',  'GUMO',  35,  'SRO ',
C    + 'MAIO',  36,  'SRO ',  'BCAO',  37,  'SRO ',
C    + 'NWAO',  38,  'SRO ',  'GRFO',  39,  'SRO ',
C    + 'SHIO',  40,  'SRO ',  'TATO',  41,  'SRO ',
C    + 'SNZO',  42,  'SRO ',  'GAC ',  43,  'SRO ',
C    + 'CTAO',  50,  'ASRO',  'ZOBO',  51,  'ASRO',
C    + 'KAAO',  52,  'ASRO',  'MAJO',  53,  'ASRO',
C    + 'KONO',  54,  'ASRO',  'ALQ ',  60,  'DWWS',
C    + 'SCP ',  61,  'DWWS',  'COL ',  62,  'DWWS',
C    + 'LON ',  63,  'DWWS',  'JAS1',  64,  'DWWS',
C    + 'KEV ',  67,  'DWWS',  'AFI ',  69,  'DWWS',
C    + 'SLR ',  71,  'DWWS',  'TOL ',  73,  'DWWS',
C    + 'TAU ',  74,  'DWWS',  'BER ',  77,  'DWWS',
C    + 'HON ',  66,  'DWWS',  'GDH ',  70,  'DWWS',
C    + 'BDF ',  72,  'DWWS',  'LEM ',  76,  'DWWS',
C    + 'RSCP',  81,  'RSTN',  'RSNT',  82,  'RSTN',
C    + 'RSSD',  83,  'RSTN',  'RSNY',  84,  'RSTN'/
C
C     DATA ((IRCODE(I,J),I=1,3),J=39,76)/
C    + 'RSON',  85,  'RSTN',  'TLO ',   4,  'HGLP',
C    + 'EIA ',   5,  'HGLP',  'OGD ',   7,  'HGLP',
C    + 'KON ',  21,  'HGLP',  'KIP ',  22,  'HGLP',
C    + 'ALQ ',  23,  'HGLP',  'MAT ',  25,  'HGLP', 
C    + 'BJI ', 101,  'CDSN',  'LZH ', 102,  'CDSN',
C    + 'KMI ', 104,  'CDSN',  'WMQ ', 107,  'CDSN',
C    + 'HIA ', 108,  'CDSN',  'NE06', 206,  'NARS',
C    + 'NE07', 207,  'NARS',  'NE10', 210,  'NARS',
C    + 'NE14', 214,  'NARS',  'NE16', 216,  'NARS',
C    + 'NRA0', 501,  'NRSA',  'KBS ',  79,  'DWWS',
C    + 'GRA1', 701,  'GRF ',  'GRB1', 705,  'GRF ',
C    + 'GRC1', 710,  'GRF ',  'MDJ ', 109,  'CDSN',
C    + 'NE01', 201,  'NARS',  'NE02', 202,  'NARS',
C    + 'NE03', 203,  'NARS',  'NE04', 204,  'NARS',
C    + 'NE05', 205,  'NARS',  'NE08', 208,  'NARS',     
C    + 'NE09', 209,  'NARS',  'NE11', 211,  'NARS',
C    + 'NE12', 212,  'NARS',  'NE13', 213,  'NARS',     
C    + 'NE15', 215,  'NARS',  'NE17', 217,  'NARS',          
C    + 'NE18', 218,  'NARS',  'NE19', 219,  'NARS'/
C
C     DATA ((IRCODE(I,J),I=1,3),J=77,NSTAT)/
C    + 'PAF ', 253,  'GEO ',  'WFM ', 255,  'GEO ',
C    + 'SSB ', 251,  'GEO ',  'CAY ', 257,  'GEO ',
C    + 'KIP ', 262,  'GEO ',  'INU ', 265,  'GEO ',
C    + 'RER ', 252,  'GEO ',  'PPT ', 263,  'GEO ',
C    + 'ANMX',  44,  'SRO ',  'HEA ', 301,  'BLKN',
C    + 'BUW ', 302,  'BLKN',  'WOL1', 303,  'BLKN',
C    + 'BKN ', 304,  'BLKN',  'CWF ', 305,  'BLKN',
C    + 'LAM ', 307,  'BLKN',  'LLW ', 308,  'BLKN',
C    + 'MMY ', 309,  'BLKN',  'EKA ', 310,  'BLKN',
C    + 'BHM ', 311,  'BLKN',  'SBD ', 312,  'BLKN',  
C    + 'WOL2', 313,  'BLKN',  'WOL3', 314,  'BLKN',
C    + 'GRA2', 702,  'GRF ',  'GRA3', 703,  'GRF ',
C    + 'GRA4', 704,  'GRF ',  'GRB2', 706,  'GRF ',
C    + 'GRB3', 707,  'GRF ',  'GRB4', 708,  'GRF ',
C    + 'GRB5', 709,  'GRF ',  'GRC2', 711,  'GRF ',
C    + 'GRC3', 712,  'GRF ',  'GRC4', 713,  'GRF ',
C    + 'ECH1', 401,  'ECH ',  'HYB ', 250,  'GEO ',
C    + 'HDC2', 250,  'GEO ',  'BNG ', 250,  'GEO ',
C    + 'NOU ', 250,  'GEO ',  'WUS ', 250,  'GEO '/
      DATA CXXX /'XXXX'/, CYYY /'YYYY'/
C
C     DO 10 I=1,NSTAT      
C     J=I
C  10 IF (IB.EQ.IRCODE(2,I)) GOTO 20
C     GOTO 50
C  20 JSTA=IRCODE(1,J)
C     JSOR=IRCODE(3,J)
C     IERR=0
C     RETURN
C ERROR FINISH
   50 JSOR=XXXX
      JSTA=YYYY
      IERR=1
      RETURN
      END

      subroutine ask(code,line,nfound,xdata,nerr)
c$$$$ decode
c  Reads a line from the input device, splits off 1st 4 chars into
c  code , searches for beginning of next string as signalled by end
c  of blanks, then returns the string in  line .
c  Up to 10 numbers are sought in the string  line  which are put into
c  the array  xdata.  The number found is in  nfound, while an error is
c  signaled in  nerr.
c  Results are returned in common /inform/.
      character*116 line,image
      character*4 code
c      common /inout/ in,iout,idisk,idev
c      common /inform/ nchar,nfound,nerr,xdata(10)
c      common /char1/ code,line,image
c
 1000 read (*, '(a4, a116)', end=2000) code,image
      if (code(1:1) .eq. ' ') goto 1000
      ibegin=0
c  Modification to make upper-case letters in commands into lower case
c  (but not those in strings) - D. Agnew Nov 85.
      do 1050 n=1,4
        i = ichar(code(n:n))
        if(i.ge.ichar('A').and.i.le.ichar('Z'))
     &      code(n:n) = char(i+ichar('a')-ichar('A'))
 1050 continue
      do 1100 n=1,116
        if (image(n:n) .eq. ' ') ibegin=1
        if (image(n:n).ne.' ' .and. ibegin.eq.1) goto 1150
 1100 continue
      line=' '
      nfound=0
      nchar=0
      goto 1400
 1150 nchar=116-n+1
      do 1200 i=1,nchar
        k=nchar-i+1
 1200 if (image(k:k) .ne. ' ') goto 1300
 1300 continue
c  Put extra blank at end of line.
      k=min(k+1, nchar)
      line=image(n:k)
      nchar=k - n + 1
      call decode(line(1:nchar), xdata, 10, nfound)
 1400 nerr = max(0, -nfound)
      nfound=max(0,  nfound)
      return
c  Eof is interpreted as 'quit' command.
 2000 code='quit'
      nerr=0
      return
      end
c______________________________________________________________
      subroutine decode(char, values, nwant, nfound)
c$$$$ calls no other routines
c  Evaluates numbers in the string  char.  nwant  numbers are expected,
c  nfound  are actually found in the character string.  If an error is
c  discovered  nfound=-n,  where  n  is the number of numbers properly
c  decoded.
      dimension values(10)
      character*(*) char, bore(2)*1, form*8
      save bore
      data bore/'e',' '/
c
      kn=len(char)
      k1=1
c  Up to  nwant  numbers are sought.
      do 1800 nval=1, nwant
      do 1100 k=k1, kn
      if (char(k:k) .ne. ' ') goto 1200
 1100 continue
      nfound=nval-1
      return
 1200 iex=1
      do 1300 l=k, kn
        if (char(l:l).eq. ',') goto 1500
        if (char(l:l).ne. ' ') goto 1300
        if (char(l-1:l-1).ne.bore(iex)) goto 1500
        iex=2
 1300 continue
 1500 m=l - k
      k2=l+1
      if (k.eq.l-1 .and. char(k:l-1).eq.'0') then
c kluge to get around SUN FORTRAN BUG. If string contains only one character 
c and the character is '0' set value to 0.  This if statment can be removed when
c sun fixes the bug.  7/13/90
         values(nval) = 0.
      else
         write(form, '(2h(f, i3, 3h.0) )') m
         read (char(k:l-1), form, err=1900) values(nval)
      end if
 1800 k1=k2
      nval=-nwant
 1900 nfound=-nval
      return
      end
c
c-------------------------------------------------------------------
c
      subroutine staloc(snam,th,ph,ierr)
c Returns station locations for GDSN and IDA stations.
c
c  input:
c   snam = station name (character*4)
c  output:
c    th = station colatitude (degrees, 0 -> 180)
c    ph = station colongitude (degrees, 0 -> 360)
c    ierr = error code  0 -> no error
c                       1 -> error (station not in list)
c
C Note that NARS station NE10 was at La Almunia from May83 to Nov. 83
c For now, use the name NE99 to get this location of NE10. b.w.
c
      parameter (nsta=159)
      character*4 snam,name(nsta)
      dimension theta(nsta),phi(nsta)
      save rad, name
      data rad/57.29578/
      DATA (NAME(I),I=1,70) /
     +  'ANMO','ANTO','BOCO','CHTO','GUMO','MAIO','BCAO',
     +  'NWAO','GRFO','SHIO','TATO','SNZO','GAC ','CTAO',
     +  'ZOBO','KAAO','MAJO','KONO','ALQ ','SCP ','COL ',
     +  'LON ','JAS ','JAS1','HON ','KEV ','AFI ','GDH ',
     +  'SLR ','BDF ','TOL ','TAU ','LEM ','BER ','RSCP',
     +  'RSNT','RSSD','RSNY','RSON','BDF ','CAN ','CMO ',
     +  'EIC ','ERM ','ESK ','GAR ','GUA ','HAL ','KIP ',
     +  'KMY ','NNA ','PFO ','RAR ','SEY ','SPA ','SUR ',
     +  'TWO ','PAY ','ALE ','BJT ','SJG ','SSB ','PCR ',
     +  'BGIO','SSB ','PCR ','PAF ','TAM ','WFM ','AGD '
     + /
      DATA (THETA(I),I=1,70)/
     +  0.96087,0.87495,1.49074,1.24285,1.33364,0.93724,1.49342,
     +  2.14548,0.70351,1.12457,1.13489,2.29179,0.77313,1.92140,
     +  1.85476,0.96795,0.93303,0.52972,0.96093,0.85879,0.43808,
     +  0.75485,0.90850,0.90885,1.19866,0.35334,1.81356,0.36216,
     +  2.01995,1.84418,0.87473,2.31972,1.69006,0.51690,0.94946,
     +  0.48032,0.80075,0.79328,0.68314,1.84418,2.18726,0.43878,
     +  2.07970,0.83750,0.60533,0.89012,1.33451,0.79172,1.19689,
     +  1.13188,1.78003,0.98421,1.94103,1.65134,3.14159,2.13593,
     +  2.18224,0.97320,0.13120,0.87197,1.25473,0.78053,1.94074,
     +  1.01714,0.78053,1.94074,2.43214,1.17302,0.82709,1.36958
     + /
      DATA (PHI(I),I=1,70)/
     +  4.42517,0.57235,4.99089,1.72747,2.52839,1.03837,0.32350,
     +  2.04617,0.19586,1.60367,2.12037,3.04917,4.96584,2.55262,
     +  5.09418,1.20502,2.41220,0.16752,4.42515,4.92418,3.70371,
     +  4.15720,4.18113,4.18146,3.52542,0.47135,3.28511,5.34886,
     +  0.49361,5.44712,6.21253,2.57121,1.87826,0.09309,4.78973,
     +  4.28318,4.46741,4.98239,4.64777,5.44712,2.60052,3.70298,
     +  4.37320,2.49856,6.22725,1.22726,2.52919,5.17330,3.52530,
     +  1.79327,4.94204,4.25066,3.49462,0.96850,0.00000,0.36305,
     +  2.41864,4.33994,5.19410,2.02764,5.12865,0.07927,0.97002,
     +  0.61240,0.07924,0.97002,1.22545,0.09646,5.03543,0.74742
     + /
      DATA (NAME(I),I=71,140) /
     +  'CAY ','MBO ','NOC ','EIL ','TLO ','KIP ','OGD ',
     +  'CTA ','ZLP ','KON ','MAT ','CHG ','ALQ ','CRZF',
     +  'DRV ','RER ','PPT ','SCZ ','BJI ','LZH ','KMI ',
     +  'WMQ ','HIA ','MDJ ','NRA0','KBS ','NE01','NE02',
     +  'NE03','NE04','NE05','NE06','NE07','NE08','NE09',
     +  'NE10','NE99','NE11','NE12','NE13','NE14','NE15',
     +  'NE16','NE17','NE18','ANMX','GRA1','GRB1','GRC1',
     +  'GRA2','GRB2','GRC2','GRA3','GRB3','GRC3','GRA4',
     +  'GRB4','GRC4','HEA ','BUW ','WOL1','BKN ','CWF ',
     +  'LAM ','LLW ','MMY ','EKA ','BHM ','SBD ','WOL2' 
     + /
      DATA (THETA(I),I=71,140)/
     +  1.48444,1.31963,1.95774,1.05505,0.87511,1.19689,0.85368,
     +  1.92140,1.85476,0.52972,0.93303,1.24285,0.96093,2.38115,
     +  2.73432,1.94009,1.87743,0.93204,0.87196,0.94096,1.13231,
     +  0.80597,0.71093,0.79209,0.51077,0.19343,0.56198,0.58540,
     +  0.61008,0.64904,0.66169,0.69644,0.71429,0.76061,0.78798,
     +  0.81880,0.84689,0.84100,0.86146,0.89562,0.92171,0.68300,
     +  0.77208,0.87474,0.78009,0.96087,0.70351,0.70874,0.71565,
     +  0.70415,0.71086,0.71789,0.70229,0.70958,0.71750,0.70572,
     +  0.70740,0.71407,0.67443,0.67354,0.67522,0.67433,0.65034,
     +  0.66124,0.64841,0.62525,0.60505,0.67696,0.68827,0.67522
     + /
      DATA (PHI(I),I=71,140)/
     +  5.37008,5.98726,2.90170,0.60999,6.21312,3.52530,4.98124,
     +  2.55262,5.09418,0.16752,2.41220,1.72747,4.42515,0.90515,
     +  2.44364,0.97295,3.67259,4.16430,2.02764,1.81243,1.79315,
     +  1.53057,2.08989,2.26180,0.20144,0.20811,0.21174,0.16005,
     +  0.15975,0.11638,0.09027,0.08020,0.03896,0.03019,0.01712,
     +  6.27099,6.25924,6.25671,6.21067,6.21178,6.22044,0.10097,
     +  0.05416,6.21252,0.02646,4.42517,0.19586,0.20340,0.20110,
     +  0.19827,0.20366,0.19853,0.19755,0.20605,0.20220,0.19960,
     +  0.20178,0.20118,6.26112,6.26181,6.26184,6.26247,6.26037,
     +  6.21219,6.21922,6.25057,6.22805,0.02049,6.20140,6.26184
     + /
      DATA (NAME(I),I=141,nsta) /
     +  'WOL3','ECH1','HDC2','BNG ','NOU ','WUS ','INU ',
     +  'GRB5','HYB ','NE19','BOCH','SSB1','SSB2','BUG ',
     +  'ECH ','SCK ','AQU ','KHC ','NE32'
     + /
      DATA (THETA(I),I=141,nsta)/
     +  0.67522,0.72920,1.39579,1.49339,1.95653,0.85174,0.95382,
     +  0.71365,1.26676,0.82976,0.67297,0.78051,0.78051,0.67297,
     +  0.72920,0.64787,0.83158,0.71330,0.66010
     + /
      DATA (PHI(I),I=141,nsta)/
     +  6.26184,0.12497,4.81507,0.32371,2.90253,1.38261,2.39161,
     +  0.20384,1.37096,6.23950,0.12689,0.07924,0.07924,0.12689,
     +  0.12496,0.01311,0.23393,0.23698,0.10144
     + /
c
      do 10 i=1,nsta
         in=i
         if(snam.eq.name(i)) goto 20
10    continue
c
c station not found - error exit
c      print 900,nsro
c  900 format('station ',a4,' not in routine ALLSTA')
      ierr = 1
      return
c
c station found - set output variables
   20 th = theta(in) * rad
      ph = phi(in) * rad
      ierr = 0
      return
      end
c
c-------------------------------------------------------------------
c
      subroutine parse(line, chdata, nstrng)

c     parse line into substrings separated by blanks and commas

c     input:   character string 'line'

c     output:  number of substrings found: nstrng
c              array of substrings       : chdata

      character line*(*)
      character*20 chdata(20)
      integer nstrng
      integer i, ii, n, index(200) 

c     first go through entire string to find the index of 
c     the character at the beginning and end of each string

      n=len(line)
      ii=1
      do 1, i=1,n-1
         if (mod(ii,2) .eq. 1) then
            if (line(i:i) .ne. ' ' .and. line(i:i) .ne. ',') then
               index(ii)=i
               ii=ii+1
            endif
         else
            if (line(i:i) .eq. ' '  .or. line(i:i) .eq. ',') then
               index(ii)=i-1
               ii=ii+1
            endif
         endif
    1 continue
      ii=ii-1
      if (mod(ii,2) .eq. 1) then
         index(ii)=n
         nstrng=(ii+1)/2
      else
         nstrng=ii/2
      endif

c     now parse the string into substrings

      do 2 i=1,nstrng
    2 chdata(i)=line( index(2*i-1) : index(2*i) )
      return
      end

      integer function lenb(string)
c  finds index of last nonblank character in string
      character*(*) string
      integer i,n
      n=len(string)
      do 10 i=n,1,-1
      if(string(i:i).ne.' ')then
        lenb=i
        return
      endif
   10 continue
      lenb=0
      return
      end

      subroutine rename(cdfile,file,header)

c     input string   : cdfile
c     output strings : file and header
c     place characters from input string 'cdfile' up to first : into 'header'
c     and remaining characters in 'file'.
c     In 'file' convert upper case to lower case and : to / .
c     eg.  file name for MACINTOSH is cdfile='5033:1980:JAN:8000100.EVT'
c     and must be converted to file='1980/jan/8000100.evt'
c     in a UNIX environment, header='5033'

      character*25 cdfile, header, string, file
      integer i, n

c    -remove character to first :
      file=cdfile
      do 1 i=1,25
         if (file(i:i) .eq. ':') go to 2
1     continue
2     string=file(i+1:25)
      header=file(1:i-1)

c    -convert upper case to lower case and : to /
      do 3 i=1,25
        n = ichar(string(i:i))
        if(n.ge.ichar('A').and.n.le.ichar('Z'))
     .      string(i:i) = char(n+ichar('a')-ichar('A'))
        if (string(i:i).eq.':') string(i:i)='/'
3     continue
      file=string
      return
      end
