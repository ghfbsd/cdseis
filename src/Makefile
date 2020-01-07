OBJS= cdsubs.o datetime.o datlog.o extract.o extseed.o \
	stadis.o ibits.o gcdist.o \
	seed.dict.o seed.iosubs.o seed.possubs.o seedhdr.o rseed.o

MLOBJS = iosubs.o possubs.o timesubs.o datasubs.o

OOBJS= datout.o get_tt.o \
	write_ah.o read_ah.o write_sac.o \
	add_null.o xdr_ioroutin.o

all: cdseis

cdseis: cdseis.o $(OBJS) $(OOBJS)
	$(FC) $(FFLAGS) -o cdseis cdseis.o $(OBJS) $(OOBJS) $(SACLIB)
	chmod g+rwx,o+rx cdseis

makelog: makelog.o datspan.o ${MLOBJS}
	$(FC) ${FFLAGS} -o makelog makelog.o datspan.o ${MLOBJS}
	chmod g+rwx,o+rx makelog

make_tt: make_tt.o ${TPTT}
	$(FC) make_tt.o -o make_tt ${TPTT}

seedhdr.o seed.dict.o extseed.o: seed.io.com seed.sta.com extseed.com
seed.iosubs.o: seed.io.com
seed.possubs.o: seed.io.com

clean:
	/bin/rm -f *.o core output fort.9

distclean: clean
	/bin/rm -f cdseis makelog convertlog .gdb_history
