FC=gfortran
#        -O1 fixes problem in gfortran 4.9 extseed.f with IEEE_DENORMAL errors
FFLAGS=	-g -m32 -O1 -fno-second-underscore

CFLAGS=	-g -m32

TPTT=	/usr/local/lib/geophy/tpttsub.o
SACLIB= /usr/local/lib/libsacio-32.a

OBJS= cdsubs.o datetime.o datlog.o extract.o extseed.o \
	stadis.o ibits.o gcdist.o \
	seed.dict.o seed.iosubs.o seed.possubs.o seedhdr.o rseed.o

OOBJS= datout.o get_tt.o \
	write_ah.o read_ah.o write_sac.o \
	add_null.o xdr_ioroutin.o

all: cdseis

cdseis: Makefile cdseis.o $(OBJS) $(OOBJS)
	$(FC) $(FFLAGS) cdseis.o $(OBJS) $(OOBJS) $(SACLIB) -o cdseis
	chmod g+rwx,o+rx cdseis

makelog: Makefile makelog.o $(OBJS)
	$(FC) makelog.o $(OBJS) -o makelog
	chmod g+rwx,o+rx makelog

convertlog: Makefile convertlog.o
	$(FC) convertlog.o -o convertlog

ckstainfo: ckstainfo.o cdsubs.o
	$(FC) ckstainfo.o -o ckstainfo cdsubs.o

make_tt: make_tt.o ${TPTT}
	$(FC) make_tt.o -o make_tt ${TPTT}

seedhdr.o seed.dict.o extseed.o: seed.io.com seed.sta.com extseed.com
seed.iosubs.o: seed.io.com
seed.possubs.o: seed.io.com

clean:
	/bin/rm -f *.o core output

distclean: clean
	/bin/rm -f cdseis makelog convertlog .gdb_history
