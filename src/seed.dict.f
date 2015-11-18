C     SEED comment dictionary subroutines.
C        G. Helffrich/U. Bristol

      subroutine diinit
      include 'seed.sta.com'

      ixfree = 1
      ncd(1) = 0
      ncd(2) = MAXCD
      nua(1) = 0
      nua(2) = MAXUA
      nga(1) = 0
      nga(2) = MAXGA
      ixrdfr = 1
      nrd(1) = 0
      nrd(2) = MAXRD
      end
      
      integer function diadd(ndict,idbeg,idlen,keytab,key,string)

      integer ndict(2)
      integer*2 idbeg(*),idlen(*),keytab(*)
      character string*(*)
      include 'seed.sta.com'

      ln = index(string,'~')
      if (ln .le. 0) then
	 write(0,*) '**DIADD:  Unterminated string.'
	 write(0,*) '**DIADD:  "',string,'"'
	 diadd = -1
	 return
      endif

      diadd = ln-1
      ln = ln-1
      if (ln .gt. 0) then
	 if (ndict(1) .ge. ndict(2)) then
99          continue
	    write(0,*) '**DIADD:  Dictionary too small:',ndict(2)
	    write(0,*) '**DIADD:  (Trying to add ',
     &                 string(1:ln),')'
	    return
	 endif
	 if (ixfree+ln-1 .gt. MAXDSZ) go to 99
	 ndict(1) = ndict(1) + 1
	 idbeg(ndict(1)) = ixfree
	 idlen(ndict(1)) = ln
	 commst(ixfree:ixfree+ln-1) = string
	 keytab(ndict(1)) = key
	 ixfree = ixfree + ln
      endif
      end

      character*(*) function diget(key,ndict,idbeg,idlen,keytab)
      integer ndict(2)
      integer*2 idbeg(*),idlen(*),keytab(*)
      include 'seed.sta.com'

      do 10 i=1,ndict(1)
	 if (key .eq. keytab(i)) then
	    diget = commst(idbeg(i):idbeg(i)+idlen(i)-1)
	    return
	 endif
10    continue
      diget = '**KEY NOT FOUND**'
      end

      integer function rdadd(key,type,elen)
C     RDADD -- Add an entry to the response dictionary
C
C     Assumes:
C        key - integer key for lookup
C        type - integer type of the response blockette
C        elen - number of words needed to save information (other than the type)
C
C     Returns:
C        function result - index of first word of entry.  First word
C           is set to the type.  Caller sets in the rest of the entries.

      integer type,elen
      include 'seed.sta.com'

      if (nrd(1) .ge. nrd(2)) then
	 write(0,*) '**RDADD:  Response dict. entry table too small.'
	 rdadd = 0
	 return
      endif
      if (ixrdfr+elen+1 .gt. MAXRDS) then
	 write(0,*) '**RDADD:  Response dict. data table too small.'
	 rdadd = 0
	 return
      endif
      nrd(1) = nrd(1) + 1
      rdadd = ixrdfr
      ixrd(nrd(1)) = ixrdfr
      kyrd(nrd(1)) = key
      call rdset(commrd(ixrdfr),type)
      ixrdfr = ixrdfr + 1+elen
      end

      integer function rdget(key)
      include 'seed.sta.com'

      do 10 i=1,nrd(1)
	 if (key .eq. kyrd(i)) then
	    rdget = ixrd(i)
	    return
	 endif
10    continue
      rdget = 0
      end

      subroutine rdset(what,value)
      what = value
      end

      integer function rdint(what)
C     RDINT -- Return an integer value stored in a real variable

      real what, rwhat
      integer iwhat
      equivalence (rwhat,iwhat)
      rwhat = what
      rdint = iwhat
      end

      integer function gtstr(oustr,instr)
C     gtstr -- Get a SEED variable string and return its value and length
C
C     Assumes:
C        instr - SEED input string, terminated with '~'
C
C     Returns:
C        Function result - length of string (including '~')
C        oustr - string value

      character instr*(*), oustr*(*)

      ix = index(instr,'~')
      if (ix .eq. 0) then
	 write(0,*) '**STRGET:  Unterminated string.'
	 gtstr = 0
	 return
      endif
      if (ix .gt. 1) then
	 oustr = instr(1:ix-1)
      else
	 oustr = ' '
      endif
      gtstr = ix
      end
