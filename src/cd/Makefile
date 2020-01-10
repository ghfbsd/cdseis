makelog: makelog.o datlog.o datetime.o stadis.o cdsubs.o extract.o
	$(FC) -o makelog \
	   makelog.o datlog.o datetime.o stadis.o cdsubs.o extract.o
	chmod g+rwx,o+rx makelog

convertlog: convertlog.o
	$(FC) convertlog.o -o convertlog

ckstainfo: ckstainfo.o cdsubs.o
	$(FC) ckstainfo.o -o ckstainfo cdsubs.o

clean:
	/bin/rm -f *.o

distclean: clean
	/bin/rm -f makelog convertlog ckstainfo
	
