C     iosubs -- Input and output subroutines
C     - getvol
C     - getbkt
C        G. Helffrich/U. Bristol

C     getvol  --  Open a SEED volume given a file name, and prepare it for
C                 reading.
C
C        Assumes:
C           cdname - path name of file or device to open
C
C        Function return:
C           0 - ok
C          <0 - error
      integer function getvol(cdname)
      character cdname*(*)
      include 'seed.io.com'
      logical ok

      inquire(iucd,opened=ok)
      if (ok) close(iucd)

      open(iucd,file=cdname,
     &   access='direct',
     &   form='unformatted',
     &   recl=512,
     &   iostat=ios)
      if (ios .ne. 0) then
	 write(0,901) 'Can''t open volume.'
	 go to 900
      endif

      read(iucd,rec=1,iostat=ios) inbuf(1:512)
      close(iucd)
      if (ios .ne. 0) then
         write(0,901) 'Can''t determine logical record len.'
         go to 900
      endif
      if (inbuf(1:8) .ne. '000001V ') then
	 write(0,901) 'Record isn''t a vol. hdr.'
	 go to 900
      endif
      if (inbuf(9:11) .ne. '010') then
	 write(0,901) 'Volume isn''t a station vol.'
	 go to 900
      endif
      read(inbuf(20:21),'(i2)',iostat=ios) lrecl
      if (ios .ne. 0) then
	 write(0,901) 'Undecodable lrecl in header.'
	 go to 900
      endif
      lrecl = 2**lrecl

      open(iucd,file=cdname,
     &   access='direct',
     &   form='unformatted',
     &   recl=lrecl,
     &   iostat=ios)
      if (ios .ne. 0) then
	 write(0,901) 'Can''t re-open data file!'
	 go to 900
      endif
      getvol = 0
      return

900   continue
      getvol = -1
901   format('**GETVOL: ',a)
      end

C     getbkt  --  Get blockette given at the designated position
C
C     Assumes:
C        pos - position
C     Returns:
C        bkette - blockette filled in with data.  If the blockette
C           spans a logical record, it is read completely.
C     Function value:
C        >0 - blockette was read correctly, this value is its length.
C        0  - end of file
C        <0 - error
      integer function getbkt(pos,bkette)
      include 'seed.io.com'
      integer pos(2),posnxt(2)
      integer bktype, bklen, bkoffs
      character bkette*(MAXBL), lrtype*1, lrcc*1

C     Assume the position is already in canonical form (offset < lrecl).
C        Check if near end of logical record, so there can't be a blockette
C        header present.
      if (pos(2) .gt. lrecl-7) call posset(pos,pos(1)+1,8)
C     lbfac = MAXPRL/lrecl
      lbfac = 1

C     Read proper logical record and find beginning of blockette.  If
C     it is blank, skip to the end of the logical record to the succeeding
C     one.
1     continue
	 nprec = 1 + (pos(1)-1)/lbfac
	 if (nprec .ne. recno) then
C           Physical record isn't in buffer, get it.
	    read(iucd, rec=nprec, err=98) inbuf(1:lrecl)
	    recno = nprec
	 endif

C        Find logical record in physical record.  Read record number, type,
C           and continuation indicator.
	 noffs = mod(pos(1)-1,lbfac)*lrecl
	 read(inbuf(1+noffs:),'(i6,2a1)',end=98,err=98)
     &      lrec, lrtype, lrcc
	 if (lrec .ne. pos(1)) then
	    pause '**GETBKT:  Didn''t read the right log. record!'
	 endif
         if (0 .ne. index('DRQM',lrtype)) then
            call posset(pos,pos(1),1)
            bkette(1:lrecl) = inbuf(1+noffs:)
            getbkt = lrecl
            return
         endif
         if (0 .eq. index('VAST ',lrtype)) go to 98
	 if (inbuf(1+noffs+pos(2):7+noffs+pos(2)) .ne. ' ') go to 5
	 call posset(pos,pos(1)+1,8)
      go to 1

C     Decode blockette header: type, length.
5     continue
      bkoffs = 0
      read(inbuf(1+noffs+pos(2):),'(i3,i4)',end=98,err=98) bktype, bklen
      call poscpy(pos,posnxt)
      call posadd(posnxt,bklen-1)
      if (pos(1) .eq. posnxt(1)) then
C        Blockette within this physical record.  Move it to destination.
	 bkette(1:bklen) = inbuf(1+noffs+pos(2):)
      else
C        Blockette spans logical record.  Read records in until complete
C           blockette is at hand.
         bkoffs = lrecl - mod(noffs+pos(2),lrecl)
	 bkette(1:bkoffs) = inbuf(1+noffs+pos(2):)
C        Repeat for as many portions of this blockette that span a
C           logical record.
	 do 10 i=pos(1)+1,posnxt(1)
	    nprec = 1 + (i-1)/lbfac
	    noffs = mod(i-1,lbfac)*lrecl
	    if (recno .ne. nprec) then
	       read(iucd, rec=nprec, err=98) inbuf(1:lrecl)
	       recno = nprec
	    endif

C           Check validity of new logical record.
	    read(inbuf(1+noffs:),'(i6,2a1)',end=98,err=98)
     &         lrec, lrtype, lrcc
	    if (lrec .ne. i .or. lrcc .ne. '*' .or.
     &          0 .eq. index('VAST',lrtype))
     &         pause '**GETBKT:  Continuation problem.'

	    nlen = min(lrecl-8,bklen-bkoffs)
	    bkette(1+bkoffs:bkoffs+nlen) = 
     &         inbuf(1+noffs+8:noffs+8+nlen)
	    bkoffs = bkoffs + nlen
10       continue
         if (bkoffs .ne. bklen) pause '**GETBKT:  Span logic problem.'
      endif

      getbkt = bklen
      return

C     Error reading blockette.
98    continue
      getbkt = -1
      return

C     End-of-file reading blockette.
99    continue
      getbkt = 0
      end

C     getdat  --  Get a data block.
C
C     Called via
C       ires = getdat(block, length, where)
C
C     Assumes:
C        block - integer block number
C        length - length of block (logical record length)
C        where - location to put data block
C
C     Returns:
C        where - filled with data
C        ires - 0 if read ok, nonzero otherwise
      integer function getdat(block, length, where)
      integer block, length
      character where*(*)
      include 'seed.io.com'
      character lrtype*1, lrcc*1

      if (length .gt. lrecl) 
     &   stop '**Data record longer than logical record length!'
C     lbfac = MAXPRL/lrecl
      lbfac = 1

      nprec = 1 + (block-1)/lbfac
      if (nprec .ne. recno) then
C        Physical record isn't in buffer, get it.
C        read(iucd, rec=nprec, end=99, err=98) inbuf
	 read(iucd, rec=nprec, err=98) inbuf(1:lrecl)
	 recno = nprec
      endif
      noffs = mod(block-1,lbfac)*lrecl
      read(inbuf(1+noffs:),'(i6,2a1)',end=98,err=98)
     &      lrec, lrtype, lrcc
      if (lrec .ne. block .or. 
     &    0 .eq. index('DRQM',lrtype) .or. lrcc .ne. ' ') go to 98
      where = inbuf(1+noffs:noffs+length)
      getdat = 0
      return

99    continue
      getdat = -1
      return

98    continue
      stop '**Corrupted data record.'
      end
