C     datspan  --  Read data records starting from a given block number
C                  and determine the time span.
C
C     Called via:
C        endblock = datspan(begdy,begsec,dur,length,drift,keyw,keyh,block)
C
C     Assumes:
C        length - data block logical record length
C        block - starting block number
C        drift - per-sample time drift tolerance
C        keyw - 4-byte byte-swapping key
C        keyh - 2-byte byte-swapping key
C
C     Returns:
C        begdy - yyyyddd for data start time
C        begsec - second in day of data start time
C        dur - duration of span of data, in seconds.
C
C     Function value:
C        Integer first block past the initial one that does not contain
C        data belonging to this time sequence.
C
C     Note on time durations.  These values are kept in seconds.  They
C     can become large numbers if the station has been operating for awhile.
C     Time tolerances have to be known to 1e-4 seconds, for gap checking.
C     In order to assure enough significant bits in these values are
C     retained, they are computed as real*8 values.
C
C     By George Helffrich, U. Bristol, 1994-2006
 
      integer function datspan(begdy,begsec,dur,
     &                   length,drift,keyw,keyh,block)
      integer begdy, length, block
      real*8 begsec, oldsec, cursec, dur, olddur, curdur
      real*8 tdsdif
      character keyw*4, keyh*2
      parameter (MAXDBS = 4096)
      integer*2 gthw
      integer gtw, gtbyte, curdy, olddy, getdat
      character idsta*5, idchan*3
      real*8 align8
      character dblok*(MAXDBS)
      equivalence (align8,dblok)

      if (length .gt. MAXDBS) stop '**Data block too long!'
      call dathead(dblok,length,block,keyw,keyh,
     &             idsta,idchan,begdy,begsec)

C     Compute duration of this record, and get time correction.
      nsamp = gthw(dblok(31:32),keyh)
      nrfac = gthw(dblok(33:34),keyh)
      nrmul = gthw(dblok(35:36),keyh)
      ncorr = gtw(dblok(41:44),keyw)

      dur = nsamp/dble(gtnsr(nrfac,nrmul))
      olddif = nsamp*drift
C     if (ncorr .ne. 0) pause '**Time correction!'

C     Now read succeeding data blocks until one that is out of sequence
C     is found.
      iblk = block
      olddy = begdy
      oldsec = begsec
      olddur = dur
10    continue
         iblk = iblk + 1
	 if (getdat(iblk,length,dblok) .ne. 0 .or.
     &       idsta .ne. dblok(9:13) .or. idchan .ne. dblok(16:18))
     &      go to 19

	 iyear = gthw(dblok(21:22),keyh)
	 ijday = gthw(dblok(23:24),keyh)
	 ihh = gtbyte(dblok(25:25))
	 imm = gtbyte(dblok(26:26))
	 iss = gtbyte(dblok(27:27))
	 ixxxx = gthw(dblok(29:30),keyh)
	 curdy = iyear*1000 + ijday
	 cursec = ixxxx*1d-4 + dble(iss + 60*(imm + 60*ihh))
	 nsamp = gthw(dblok(31:32),keyh)
	 nrfac = gthw(dblok(33:34),keyh)
	 nrmul = gthw(dblok(35:36),keyh)
	 ncorr = gtw(dblok(41:44),keyw)
C        if (ncorr .ne. 0) pause '**Time correction!'
	 curdur = nsamp/dble(gtnsr(nrfac,nrmul))
	 curdif = tdsdif(curdy,cursec,olddy,oldsec)-olddur
	 if (abs(curdif) .gt. olddif) go to 19
	 dur = dur + curdur
	 olddy = curdy
	 oldsec = cursec
	 olddur = curdur
	 olddif = nsamp*drift
      go to 10

C     The duration has been accumulated as we traveled along.
19    continue
      datspan = iblk
      end
