c subroutine GET_TT obtains a travel time for a seismic phase
c at a specified range and earthquake depth by interpolating
c from a file containing a table of travel times.
c    Inputs:    phase  =  name of file containing travel time table
c               del    =  range
c               qdep   =  earthquake depth
c    Returns:   tt     =  travel time (minutes)
c               iflag  =  0 for interpolation
c                      =  1 for extrapolation
c                      =  2 for file not found
c
      subroutine GET_TT(phase,del,qdep,tt,iflag)
      character phase*40,phaseold*40,linebuf*80
      real      t(120,16),x(120),d(16)
      integer   id,nd,ix,nx,id1,id2,ix1,ix2,iflag
      real      diag,diagmin,davg,xavg,dfrac,xfrac,tt,qdep,del,t1,t2
      save      t,x,d,phaseold,nx,nd
c
c read file if new phase file is specified
      if (phase.ne.phaseold) then
         open (3,file=phase,status='old',iostat=ier)
         if (ier.ne.0) then
           write(*,'(a,a)') ' WARNING Could not open phasefile :',
     .                        phase
           iflag=2
           return
         endif
c  ignore first line
         read (3,'(a80)') linebuf
         read (3,*) nx,nd
10       format (2i5)
         read (3,*) (d(id),id=1,nd)
15       format (8x,16f8.3)
         do 20 ix=1,nx
         read (3,*) x(ix),(t(ix,id),id=1,nd)
20       continue
30       format (17f8.3)
         close (3)
      end if
      phaseold=phase
c
c find 'box' of travel times which is 'closest' to desired
c depth and range
      diagmin=999.
      do 60 ix=2,nx
      do 50 id=2,nd
         if (t(ix,id).eq.0.) go to 50
         if (t(ix-1,id).eq.0.) go to 50
         if (t(ix,id-1).eq.0.) go to 50
         if (t(ix-1,id-1).eq.0.) go to 50
         xavg=(x(ix)+x(ix-1))/2.
         davg=(d(id)+d(id-1))/2.
         diag=abs(del-xavg)+abs(qdep-davg)/111.
         if (diag.lt.diagmin) then
            diagmin=diag
            ix2=ix
            id2=id
         end if
50    continue
60    continue
      ix1=ix2-1
      id1=id2-1
      iflag=0
      if (qdep.lt.d(id1)) iflag=1
      if (qdep.gt.d(id2)) iflag=1
      if (del.lt.x(ix1)) iflag=1
      if (del.gt.x(ix2)) iflag=1
c
      dfrac=(qdep-d(id1))/(d(id2)-d(id1))
      t1=t(ix1,id1)+dfrac*(t(ix1,id2)-t(ix1,id1))
      t2=t(ix2,id1)+dfrac*(t(ix2,id2)-t(ix2,id1))
      xfrac=(del-x(ix1))/(x(ix2)-x(ix1))
      tt=t1+xfrac*(t2-t1)
c
      go to 999
c
990   print *,'*** phase file not found'
      stop
999   return
      end
