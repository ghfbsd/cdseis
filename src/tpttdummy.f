C     Subroutine TPTT -- Return travel time and phase info from a tau-p
C        based tabulation a la Buland and Kennett.
C
C     Called via:
C        x = tptt(id,delta,depth,dtdd,dtdh)
C        n = mtptt(id,delta,depth,max,idphs,tt,dtdd,dtdh)
C        n = ntptt(id,delta,depth,max,idphs,tt,dtdd,dtdh,d2tdd2)
C
C     Assumes:
C        id - character phase identifier (e.g., S, P, ScS, SKS) or 'all'
C        delta - distance (in degrees)
C        depth - depth (km)
C        max - maximum number of travel time values to be returned
C
C     Returns:
C        Function value (tptt):
C           travel time (in seconds)
C           -1 - Unknown phase id
C           -2 - More than one travel time associated with that branch -- be
C                more specific with your phase name or use mtptt
C
C        Function value (mtptt):
C           number of travel times returned for this phase.  This may be
C           larger than max, meaning that more are returned than space was
C           provided for.
C
C        idphs - character array of matching phase ids (e.g., PKPdf matches
C           PKP as the input ID).  Number of values returned is <= max.
C        tt - array of returned travel time values.  The number of values
C           is always <= max.
C        dtdd - dt/d delta of the phase in question.  If mtptt is used,
C           this is an array of values.
C        dtdh - dt/d depth of the phase in question.  If mtptt is used,
C           this is an array of values.
C        d2tdd2 - d2t/delta2 of the phase in question.  If mtptt is used,
C           this is an array of values.
C
C     Set travel time tables to be use by calling the tpmod routine; see
C     below.
C
C     To strip phase names returned by any of these routines of their branch
C     identifiers (e.g. PKPdf -> PKP), see the phgen routine, below.
C
C     Based on "ttimes" code distributed by Buland and Kennett.  By
C        G. Helffrich Carnegie/DTM Aug 28, 1991.

      function tptt(id,delta,depth,dtdd,dtdh)
      character id*(*)
      real delta, depth, dtdd, dtdh, v(2)
      parameter (max=100)
      character idph(max)*8
      real tr(max),dtddr(max),dtdhr(max),d2tddr(max)

      tptt = -1
      end

      function mtptt(id,delta,depth,max,idph,tt,dtdd,dtdh)
      character id*(*), idph(max)*(*)
      real delta, depth, tt(max), dtdd(max), dtdh(max), v(2)
      parameter (maxph=100)
      character idphr(maxph)*16
      real tr(maxph),dtddr(maxph),dtdhr(maxph),d2tddr(maxph)

      mtptt = 0
      end

      function ntptt(id,delta,depth,max,idph,tt,dtdd,dtdh,d2tdd2)
      character id*(*), idph(max)*(*)
      real delta, depth, tt(max), dtdd(max), dtdh(max), d2tdd2(max)
      parameter (maxph=100)
      character idphr(maxph)*16
      real tr(maxph),dtddr(maxph),dtdhr(maxph),d2tddr(maxph),v(2)

      ntptt = 0
      end
