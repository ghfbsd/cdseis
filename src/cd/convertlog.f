      program convertlog
c  this program reads a cdrom log file, and outputs a new
c  log file which is identical to the input file except that
c  trailing blanks are removed from all lines and the event lines 
c  are appended with unique event identifiers:  (YYMMDDA) where 
c  YY=year, MM=month, DD=day, A = a if first event of the day, = b
c  if second event, etc.  
c
c  K. Creager 7/26/90

      character*128 infile,outfil,line,cflag*2,cdfile*25
      integer i,k,kk,dim,linlen,lenb
      integer qyr,qmon,qdy,qhr,qmn,nrec
      real    qsc,qlat,qlon,qdep,qmb,qms
      parameter (dim=100000)
      character*6 evdate(0:dim), string*128
      character*1 evid(dim)

      write(*,'(a)') ' enter name of input log file'
      read (*,'(a)') infile
      open (7,file=infile)
      write(*,'(a)') ' enter name of output log file'
      read (*,'(a)') outfil
      open (8,file=outfil)
1     format(a2,i4,2i3,i4,1x,i2,1x,f5.2,f9.3,f10.3,f7.1,2f5.1,2x,a25,i5)
      k=0
      evdate(k)=' '
      do 100 i=1,10000000
         read (7,'(a)',end=110) string
         read (string,1,err=100) cflag,qyr,qmon,qdy,qhr,qmn,
     &                qsc,qlat,qlon,qdep,qmb,qms,cdfile,nrec
         if (cflag .eq. '  ' .and. qyr .ne. 0) then
c          -this line is a new event
            k=k+1
            call chrdate(qyr,qmon,qdy,evdate(k))
            if (evdate(k) .eq. evdate(k-1)) then
               kk=ichar(evid(k-1))
               evid(k)=char(kk+1)
            else
               evid(k)='a'
            endif
         endif
100   continue
110   rewind(7)
      k=0
      do 200 i=1,10000000
         read (7,'(a)',end=210) line
         if (line(1:2).eq.'  ' .and. line(5:6).ne.'  ') then
c          -this line is a new event
            k=k+1
            line(95:100)=evdate(k)(1:6)
            line(101:101)=evid(k)(1:1)
         endif
         linlen=max(1,lenb(line))
C        Write converted line, but omit first (blank) line of raw log file
         if (i.gt.1 .or. line.ne.' ') write(8,'(a)') line(1:linlen)
200   continue
210   close(7)
      close(8)
      end

      subroutine chrdate(qyr,qmon,qday,evdate)
c     input quake year, month and day (integers) and out put 
c     six character string (YYMMDD)
      integer qyr,qmon,qday,qyr1,i
      character*6 evdate

      if (qyr .gt. 1900) then
         qyr1=qyr-1900
      else
         qyr1=qyr
      endif
      write(evdate,'(3i2)') qyr1,qmon,qday
      do 1,i=1,6
         if (evdate(i:i) .eq.' ') evdate(i:i)='0'
1     continue
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

