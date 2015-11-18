C     Common for temporary storage of data request time window

      integer styr,stmon,stdy,sthr,stmn,stsc,stth,stjdy
      integer enyr,enmon,endy,enhr,enmn,ensc,enth,enjdy
      integer sttim(6),entim(6)
      equivalence (styr,sttim(1)), (enyr,entim(1))
      common /extstm/ styr,stjdy,sthr,stmn,stsc,stth,stmon,stdy
      common /extstm/ enyr,enjdy,enhr,enmn,ensc,enth,enmon,endy
