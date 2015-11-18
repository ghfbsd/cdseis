C     Subroutines to handle position information.
C        G. Helffrich/U. Bristol
      subroutine posset(pos,lrec,loffs)
C     posset  --  Set a position to a given value.
C
C     Assumes:
C        pos - position array
C        lrec - logical record number
C        offset - offset of blockette in logical record
C     Returns:
C        pos - set to values indicating designated position.
      integer pos(2)

      pos(1) = lrec
      pos(2) = loffs
      end

      subroutine poscpy(from,to)
C     poscpy  --  Make a copy of a position
C
C     Assumes:
C        from - position to be copied
C     Returns:
C        to - copy of position
      integer from(2), to(2)

      to(1) = from(1)
      to(2) = from(2)
      end

      subroutine posadd(pos,loffs)
C     posadd  --  Add a value to a position.
C
C     Assumes:
C        pos - position array
C        loffs - number of bytes to increment position by
C     Returns:
C        pos - updated by loffs bytes
      include 'seed.io.com'
      integer pos(2)

      if (loffs .gt. 9999 .or. loffs .lt. 8) 
     &   pause 'POSADD:  Invalid parameter.'
      pos(2) = pos(2) + loffs
      if (pos(2) .ge. lrecl) then
C        Spans logical record.  Increment logical record number and
C        skip the header in the next logical record.
         nleft = pos(2)-lrecl
         nskip = 1 + nleft/(lrecl-8)
	 pos(1) = pos(1) + nskip
	 pos(2) = 8 + mod(nleft,lrecl-8)
      endif
      end

      character*(*) function posstr(pos)
C     posstr -- return a character string definition of a position
C
C     Assumes:
C        pos - position array
C     Returns:
C        character string (at least 11 characters) describing position
C
C     Warning:  Don't call as a function from a write statement!  Fortran
C        I/O isn't recursive, necessarily.
      integer pos(2)
      character result*16

      write(result,'(i6,a1,i4)') pos(1),',',pos(2)
      posstr = result
      end
