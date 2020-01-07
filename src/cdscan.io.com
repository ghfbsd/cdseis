C     I/O related parameters and variables.
C     inbuf - input buffer, physical record
C     lrecl - logical record length
C     recno - present physical record number in buffer
      parameter (iucd=9)
      parameter (MAXPRL=32768,MAXLRL=32768,MAXBL=10000)
      character inbuf*(MAXPRL)
      integer lrecl,recno
      common /cdio/ lrecl,recno,inbuf
