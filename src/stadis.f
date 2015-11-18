      subroutine midpoint(qcolat,qcolon,scolat,scolon,bpcolat,bpcolon)
c input:
c   qcolat = source colatitude     (geographic degrees)
c   qcolon = source colongitude       "          "
c   scolat = station colatitude       "          "
c   scolon = station colongitude      "          "
c output:
c   bpcolat = bounce point colatitude (geographic degrees)
c   bpcolon = bounce point colongitude (geographic degrees)
c  
c  calls subroutine stadis and functions geocen and geogrf
c     
      save rad
      data rad/57.29578/
c get epicentral distance and azimuth
      call stadis(qcolat,qcolon,scolat,scolon,del,azim)
c assume distance to bounce point = .5 * epicentral distance
      del = del/(2.0*rad)
      az  = azim/rad
c convert to geocentric
      t0  = geocen(qcolat/rad)    
      ctheta = sin(del)*sin(t0)*cos(az) + cos(t0)*cos(del)
c compute bounce point colat and colon (t1 and p1)
      t1 = acos(ctheta)
      if (t0.eq.0.0) then
        p1 = azim
      elseif (t1.eq.0.0) then
        p1 = 0.0
      else
        sphi = sin(del)*sin(az)/sin(t1)
        cphi = (cos(del) - cos(t0)*ctheta)/(sin(t0)*sin(t1))
        p1   = qcolon + atan2(sphi,cphi)*rad
      endif
c convert from radians to geographic degrees
c convert colat to geographic corrds.)
      bpcolat = geogrf(t1)*rad      
c assume p1 never > 720
      if (p1.gt.360.0) p1 = p1 - 360.0   
      if (p1.lt.0.0)   p1 = p1 + 360.0
      bpcolon = p1
      return
      end
c
c-----------------------------------------------------------------
c
      subroutine stadis(qcolat,qcolon,scolat,scolon,del,az)
c Computes the epicentral distance and azimuth from source to receiver.
c Colatitudes are converted to geocentric colatitudes prior to performing
c the computations (it is assumed that input colatitudes are geographic).
c  input:
c    qcolat = source colatitude, usually from subroutine source (degrees)
c    qcolon =   "    colongitude,  "      "        "       "       "
c    scolat = station colatitude, from subroutine allsta (degrees)
c    scolon =    "    colongitude, "      "        "        "
c  output:
c    del   = epicentral distance (degrees)
c    az    = azimuth, from source to receiver (degrees, clockwise from N)
c
c  calls function geocen
c
      real*8 co,si,caz,saz
      save rad
      data rad/57.29578/
c  first do eq coords.  use geocentric e.q.colat 
      t0 = geocen(qcolat/rad)    
      p0 = qcolon/rad
      c0 = cos(t0)
      s0 = sin(t0)
c  now do station coords.      use geocentric station colat 
      t1 = geocen(scolat/rad)    
      p1 = scolon/rad
      c1 = cos(t1)
      s1 = sin(t1)
c  now calculate distance
      dp = p1-p0
      co = c0*c1+s0*s1*cos(dp)
      si = dsqrt(1.d0-co*co)
      del = datan2(si,co)*rad
c  now calculate azimuth
      caz = (c1-c0*co)/(si*s0)
      dp2 = -dp
      saz = -s1*sin(dp2)/si
      az  = datan2(saz,caz)*rad
c change az to be between 0 and 360
      if (az.lt.0.0) az = 360.0 + az  
      return
      end
c
c-----------------------------------------------------------------------
c
      real function geocen(arg)
c input:
c   arg    = geographic colatitude (radians)
c output:
c   geocen = geocentric colatitude (radians)
c (n.b. fac=(1-f)**2)
c
      save pi2,fac
      data pi2,fac/1.570796326794895,0.993305621334896/
      geocen=pi2-atan(fac*cos(arg)/amax1(1.e-30,sin(arg)))   
      return
      end
c
c---------------------------------------------------------------------
c
      real function geogrf(arg)
c input:
c   arg    = geocentric colatitude (radians)
c output:
c   geogrf = geographic colatitude (radians
c (n.b. fac=(1-f)**2)
c
      save pi2,fac
      data pi2,fac/1.570796326794895,0.993305621334896/
      geogrf=pi2-atan(cos(arg)/(fac*amax1(1.e-30,sin(arg)))) 
      return                       
      end       
c
c-------------------------------------------------------------------
c
      real function colon(lon,iflag)
c     iflag = 0  convert longitude   -> colongitude
c             1  convert colongitude -> longitude
      real lon
      integer iflag
      colon=lon
      if (iflag .eq. 0) then
         if (colon.lt.0.)   colon=colon+360.
      else
         if (colon.gt.180.) colon=colon-360.
      endif
      return
      end

      real function colat(lat)
c     convert latitude   -> colatitude
c     or      colatitude -> latitude
      real lat
      colat=90.-lat
      return
      end

