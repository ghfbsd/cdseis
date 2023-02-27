      character file*80, net*2, sta*5, loc*2
      integer readresp, dumpresp
      external readresp, dumpresp

      call getarg(1,file)
      if (file .eq. ' ') call usage
      ix = index(file,' ')-1
      call getarg(2,sta)
      if (sta .eq. ' ') call usage
      call getarg(3,net)
      if (net .eq. ' ') call usage
      call getarg(4,loc)

      is = readresp(file(1:ix))
      print *,'Result of readresp call is: ',is
      
      if (is .eq. 0) then
        ix = index(sta,' ')-1
        file = '/tmp/' // sta(1:ix) // '.resp'
        is = dumpresp(file,net,sta,'BHZ',loc,
     &     sta(1:ix) // ' ' // net // 'test comment','loc ' // loc)
        print *,sta(1:ix),' dump result is: ',is
      endif

      call freeresp

      end

      subroutine usage
      print *,'usage:  resptest <file> <sta> <net> <locid>'
      stop
      end
