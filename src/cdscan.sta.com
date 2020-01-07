C     Common declarations for station-oriented quantities
      parameter (MAXSTA=1500, MAXCHA=25, MAXSCH=3, MAXTIM=10)
