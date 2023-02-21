      character file*80
      integer readresp, dumpresp
      external readresp, dumpresp

      call getarg(1,file)
      if (file .eq. ' ') stop
      ix = index(file,' ')-1

      is = readresp(file(1:ix))
      print *,'Result is: ',is
      
      if (is .eq. 0) then
        is = dumpresp('/tmp/BOBR.resp','XQ','BOBR ','BHZ','  ',
     &     'BOBR test comment','another comment')
        print *,'BOBR Dump result is: ',is
        is = dumpresp('/tmp/HOYT.resp','XQ','HOYT','BHZ','  ',
     &     'HOYT test comment','another test comment')
        print *,'HOYT Dump result is: ',is
      endif

      call freeresp

      end
