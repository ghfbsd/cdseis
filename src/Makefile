OBJS= cdsubs.o datetime.o datlog.o extract.o extseed.o \
	stadis.o ibits.o gcdist.o \
	seed.dict.o seed.iosubs.o seed.possubs.o seedhdr.o rseed.o

MLOBJS = iosubs.o possubs.o timesubs.o datasubs.o

OOBJS= datout.o get_tt.o \
	write_ah.o read_ah.o write_sac.o \
	add_null.o xdr_ioroutin.o

RESPSUBS=\
   Resp/seedresp.o \
   Resp/output_resp.o \
   Parsers/get_response.o \
   Parsers/parse_key.o \
   Parsers/parse_10.o \
   Parsers/parse_11.o \
   Parsers/parse_12.o \
   Parsers/parse_30.o \
   Parsers/parse_31.o \
   Parsers/parse_32.o \
   Parsers/parse_33.o \
   Parsers/parse_34.o \
   Parsers/parse_35.o \
   Parsers/parse_41.o \
   Parsers/parse_42.o \
   Parsers/parse_43.o \
   Parsers/parse_44.o \
   Parsers/parse_45.o \
   Parsers/parse_46.o \
   Parsers/parse_47.o \
   Parsers/parse_48.o \
   Parsers/parse_50.o \
   Parsers/parse_51.o \
   Parsers/parse_52.o \
   Parsers/parse_53.o \
   Parsers/parse_54.o \
   Parsers/parse_55.o \
   Parsers/parse_56.o \
   Parsers/parse_57.o \
   Parsers/parse_58.o \
   Parsers/parse_59.o \
   Parsers/parse_60.o \
   Parsers/parse_61.o \
   Parsers/parse_62.o \
   Parsers/parse_70.o \
   Parsers/parse_71.o \
   Parsers/parse_72.o \
   Parsers/parse_73.o \
   Parsers/parse_74.o \
   Parsers/parse_100.o \
   Parsers/parse_2k.o \
   Parsers/free_all.o \
   Parsers/free_response.o \
   Parsers/free_11.o \
   Parsers/free_12.o \
   Parsers/free_30.o \
   Parsers/free_31.o \
   Parsers/free_32.o \
   Parsers/free_33.o \
   Parsers/free_34.o \
   Parsers/free_35.o \
   Parsers/free_41.o \
   Parsers/free_42.o \
   Parsers/free_43.o \
   Parsers/free_44.o \
   Parsers/free_45.o \
   Parsers/free_46.o \
   Parsers/free_47.o \
   Parsers/free_48.o \
   Parsers/free_50.o \
   Parsers/free_51.o \
   Parsers/free_52.o \
   Parsers/free_53.o \
   Parsers/free_54.o \
   Parsers/free_55.o \
   Parsers/free_56.o \
   Parsers/free_57.o \
   Parsers/free_58.o \
   Parsers/free_59.o \
   Parsers/free_60.o \
   Parsers/free_61.o \
   Parsers/free_62.o \
   Parsers/free_70.o \
   Parsers/free_71.o \
   Parsers/free_72.o \
   Parsers/free_73.o \
   Parsers/free_74.o \
   Parsers/free_2k.o \
   Parsers/prc_abrvd.o \
   Parsers/prc_blank.o \
   Parsers/prc_stnh.o \
   Parsers/prc_timeh.o \
   Parsers/prc_volh.o \
   Printers/print_channel.o \
   Printers/print_response.o \
   Printers/print_stnh.o \
   Printers/find_30.o\
   Printers/find_31.o \
   Printers/find_32.o \
   Printers/find_33.o \
   Printers/find_34.o \
   Printers/find_41.o \
   Printers/find_42.o \
   Printers/find_43.o \
   Printers/find_44.o \
   Printers/find_45.o \
   Printers/find_46.o \
   Printers/find_47.o \
   Printers/find_48.o \
   Printers/print_50.o \
   Printers/print_51.o \
   Printers/print_52.o \
   Printers/print_53.o \
   Printers/print_54.o \
   Printers/print_55.o \
   Printers/print_56.o \
   Printers/print_57.o \
   Printers/print_58.o \
   Printers/print_59.o \
   Printers/print_60.o \
   Printers/print_61.o \
   Printers/print_62.o \
   Utilities/alloc_linklist_element.o \
   Utilities/allocation_error.o \
   Utilities/find_wordorder.o \
   Utilities/memncpy.o \
   Utilities/parse_double.o \
   Utilities/parse_int.o \
   Utilities/parse_long.o \
   Utilities/parse_nchar.o \
   Utilities/parse_varlstr.o \
   Utilities/read_blockette.o \
   Utilities/read_logical_record.o \
   Utilities/split.o \
   Utilities/timeadd.o \
   Utilities/timeadd_double.o \
   Utilities/timeaddphase.o \
   Utilities/timedif.o \
   Utilities/timecvt.o \
   Utilities/timeprt.o \
   Utilities/timecmp.o \
   Utilities/timetol.o \
   Utilities/timeqc.o \
   Utilities/timepch.o \
   Utilities/strlst.o\
   Utilities/get_date.o

%.o : %.c
	[ $(@D) = '.' ] && $(CC) $(CFLAGS) -c $< || \
	( cd $(@D) ; make $(@F) CFLAGS="-I../Include -g" )

all: cdseis

cdseis: cdseis.o $(OBJS) $(OOBJS) $(RESPSUBS)
	$(FC) $(FFLAGS) -o cdseis cdseis.o \
	   $(OBJS) $(OOBJS) $(RESPSUBS) $(SACLIB)
	chmod g+rwx,o+rx cdseis

makelog: makelog.o datspan.o ${MLOBJS}
	$(FC) ${FFLAGS} -o makelog makelog.o datspan.o ${MLOBJS}
	chmod g+rwx,o+rx makelog

make_tt: make_tt.o ${TPTT}
	$(FC) make_tt.o -o make_tt ${TPTT}

resptest: ${RESPSUBS}
	$(FC) -g -o resptest resptest.f ${RESPSUBS}

seedhdr.o seed.dict.o extseed.o: seed.io.com seed.sta.com extseed.com
seed.iosubs.o: seed.io.com
seed.possubs.o: seed.io.com

clean:
	/bin/rm -f *.o */*.o core output fort.9 *.dSYM

distclean: clean
	/bin/rm -f cdseis makelog make_tt resptest .gdb_history
