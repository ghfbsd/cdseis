C     SEED Data manipulation subroutines
C
C     By George Helffrich, U. Bristol, 1994-2006

C     dathead  --  Interpret a data block header and return station and
C                  time information.
C
C     Called via:
C        call dathead(dblok,length,block,keyw,keyh,idsta,idchan,idloc,
C                     begdy,begsec)
C
C     Assumes:
C        dblok - area into which to store data block for further use
C        length - size of this area (characters)
C        block - block number to read
C        keyw - decoder key for 4-byte quantities (4 characters)
C        keyh - decoder key for 2-byte quantities (2 characters)
C
C     Returns:
C        idsta - station ID (character)
C        idchan - channel ID (character)
C        idloc - channel location (character)
C        begdy - beginning year and day (yyyyddd, integer)
C        begsec - beginning second in day (real*8)
C
C     If a read error is encountered, a message is printed and the program
C     stops.

      subroutine dathead(dblok,length,block,keyw,keyh,
     &                   idsta,idchan,begdy,begsec)
      character dblok*(*), keyw*4, keyh*2, idsta*5, idchan*3, idloc*2
      integer length, block, begdy
      real*8 begsec
      integer*2 gthw
      integer gtw, gtbyte, getdat

      if (getdat(block,length,dblok) .ne. 0) then
	 write(0,*) '**Data read error, block ',block
	 stop
      endif

C     Decode header to determine start time
      iyear = gthw(dblok(21:22),keyh)
      ijday = gthw(dblok(23:24),keyh)
      ihh = gtbyte(dblok(25:25))
      imm = gtbyte(dblok(26:26))
      iss = gtbyte(dblok(27:27))
      ixxxx = gthw(dblok(29:30),keyh)
      begdy = iyear*1000 + ijday
      begsec = ixxxx*1d-4 + dble(iss + 60*(imm + 60*ihh))

      idsta = dblok(9:13)
      idchan = dblok(16:18)
      idloc = dblok(14:15)
      end

C     gtbyte  --  Get a byte (8 bit quantity) from a given string
C
C     Called via:
C        ival = gtbyte(string)
C
C     Assumes:
C        string - character string from which first byte is taken
C
C     Returns:
C        ival - integer (unsigned) value of byte
      integer function gtbyte(string)
      character string*(*)
      integer i
      character istr*4
      equivalence (i,istr)

      i = 0
      istr(4:4) = string(1:1)
      gtbyte = i
      end

C     gthw  --  Get a halfword from a given string
C
C     Called via:
C        ival = gthw(string, key)
C
C     Assumes:
C        string - character string from which to fetch a halfword (2 chars)
C        key - decoding key
C
C     Returns:
C        key - changed to a binary value to facilitate conversions
C        ival - value as a 2 byte integer

      integer*2 function gthw(string, key)
      character string*2
      character key*2
      integer*2 i2
      character i2str*2, str2*2, str(2)*1
      equivalence (i2,i2str), (str2,str)

C     Check if key has been converted.  If not, do it for future use.
      if (ichar(key(1:1)) .gt. 2) then
	 do 10 i=1,2
	    key(i:i) = char(2 - (ichar(key(i:i))-ichar('0')))
10       continue
      endif

C     Copy data and switch bytes
      str2 = string
      i2str(1:1) = str(ichar(key(1:1)))
      i2str(2:2) = str(ichar(key(2:2)))
      gthw = i2
      end

C     gtw  --  Get a word from a given string
C
C     Called via:
C        ival = gtw(string, key)
C
C     Assumes:
C        string - character string from which to fetch a word (4 chars)
C        key - decoding key
C
C     Returns:
C        key - changed to a binary value to facilitate conversions
C        ival - value as a 4 byte integer

      integer*4 function gtw(string, key)
      character string*4
      character key*4
      integer i4
      character i4str*4, str(4)*1, str4*4
      equivalence (i4,i4str),(str4,str)

C     Check if key has been converted.  If not, do it for future use.
      if (ichar(key(1:1)) .gt. 4) then
	 do 10 i=1,4
	    key(i:i) = char(4 - (ichar(key(i:i))-ichar('0')))
10       continue
      endif

      str4 = string
      i4str(1:1) = str(ichar(key(1:1)))
      i4str(2:2) = str(ichar(key(2:2)))
      i4str(3:3) = str(ichar(key(3:3)))
      i4str(4:4) = str(ichar(key(4:4)))
      gtw = i4
      end

C     gtnsr --  Get nominal sampling rate, based on sample rate factor
C               and sample rate multiplier
C
C     Called via
C        fac = gtnsr(nrfac,nrmul)
C
C     Assumes:
C        nrfac - sample rate factor
C        nrmul - sample rate multiplier
C
C     Returns:
C        fac - factor resulting from these two quantities.  Weird!

      function gtnsr(nrfac,nrmul)
      if (nrfac .gt. 0) then
	 if (nrmul .gt. 0) then
	    gtnsr = nrfac*nrmul
	 else
	    gtnsr = nrfac/float(-nrmul)
	 endif
      else
	 if (nrmul .gt. 0) then
	    gtnsr = nrmul/float(-nrfac)
	 else
	    gtnsr = nrmul/float(nrfac)
	 endif
      endif
      end
