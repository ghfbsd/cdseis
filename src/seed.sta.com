C     Common declarations for station-oriented quantities
C        G. Helffrich/U. Bristol
      parameter (MAXSTA=5, MAXCHA=25, MAXSCH=3, MAXTIM=10)

C     Station quantities.
      integer sn, snch(MAXSTA), snsch(MAXSTA,MAXCHA),shdr(MAXSTA)
      integer snid(MAXSTA)
      real slat(MAXSTA), slon(MAXSTA), sel(MAXSTA), sdep(MAXSTA)
      real srate(MAXSTA,MAXCHA,MAXSCH), sdrift(MAXSTA,MAXCHA,MAXSCH)
      real schaz(MAXSTA,MAXCHA,MAXSCH), schdip(MAXSTA,MAXCHA,MAXSCH)
      integer sdatl(MAXSTA,MAXCHA,MAXSCH), sform(MAXSTA,MAXCHA,MAXSCH)
      character schid(MAXSTA,MAXCHA)*5
      character sname(MAXSTA)*5, skeys(MAXSTA)*6, snetcd(MAXSTA)*2
      character sst(MAXSTA)*23, sse(MAXSTA)*23

      common /sedsta/ sn, snch, snsch, shdr, snid
      common /sedsta/ slat, slon, sel, sdep, srate, sdrift
      common /sedsta/ schaz, schdip
      common /sedsta/ sdatl, sform
      common /sedsta/ schid, sname, skeys, sst, sse, snetcd

C     Dictionary quantities
      parameter (MAXDSZ=6*4096-2, MAXCD=550, MAXGA=350, MAXUA=100)
      parameter (MAXRD=1*2048, MAXRDS=5*MAXRD)

      integer ncd(2)
      integer*2 ixcd(MAXCD),   lncd(MAXCD),   kycd(MAXCD)
      integer nga(2)
      integer*2 ixga(MAXGA),   lnga(MAXGA),   kyga(MAXGA)
      integer nua(2)
      integer*2 ixua(MAXGA),   lnua(MAXUA),   kyua(MAXUA)
      integer nrd(2)
      integer*2 ixrd(MAXRD),                  kyrd(MAXRD)
      integer*2 ixfree
      integer ixrdfr
      character commst*(MAXDSZ)
      real commrd(MAXRDS)

      common /sedcom/ ncd, ixcd, lncd, kycd
      common /sedcom/ nga, ixga, lnga, kyga
      common /sedcom/ nua, ixua, lnua, kyua
      common /sedcom/ nrd, ixrd,       kyrd
      common /sedcom/ ixfree, commst
      common /sedcom/ ixrdfr, commrd
