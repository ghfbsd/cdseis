dnl Configure input for CDSEIS - G. Helffrich/ELSI/Titech
AC_INIT(logfiles)
AC_CONFIG_AUX_DIR(utils)
AC_PROG_CC(cc gcc)
AC_PROG_F77(gfortran g77 f77 ifort)
AC_PROG_INSTALL
# AC_CHECK_TYPES(size_t)
# AC_CHECK_TYPES(int32_t)
# AC_CHECK_TYPES(uintptr_t)
AC_F77_LIBRARY_LDFLAGS
dnl Have to undo damage done by F77_LIBRARY_LDFLAGS on MacOS systems
KLUDGE= KEEP=
for L in $ac_cv_flibs $ac_cv_f77_libs ; do
   case $L in
   -l*) ;;
   -L*) KEEP=1 ;;
   *) ;;
   esac
   test -z "${KEEP}" || KLUDGE="${KLUDGE} $L"
done
sym=neither
test -z "$ac_cv_flibs" || sym=ac_cv_flibs
test -z "$ac_cv_f77_libs"  || sym=ac_cv_f77_libs
# : ${sym}="${KLUDGE}" ; FLIBS="${KLUDGE}"
: ${sym}="${KLUDGE}" ; FLIBS="${LDFLAGS}"
dnl Set default CFLAGS, FFLAGS to what we want
KEEP=
for L in $CFLAGS ; do
   case $L in
   -g* | -O*) ;;
   *) KEEP="${KEEP} $L"
   ;;
   esac
done
CFLAGS=`echo "${KEEP}"`
KEEP=
for L in $FFLAGS ; do
   case $L in
   -g* | -O*) ;;
   *) KEEP="${KEEP} $L"
   ;;
   esac
done
FFLAGS=`echo "${KEEP}"`

AC_SUBST(ADD_FFLAGS)
AC_SUBST(ADD_CFLAGS)
AC_SUBST(SACLIB)
AC_SUBST(TPTT)
AC_SUBST(FLIBS)
AC_ARG_VAR(BINDIR,[CDSEIS program and utility executable code])
AC_ARG_VAR(LIBDIR,[SAC library code (sac.a)])
AC_ARG_VAR(SACAUX,[CDSEIS auxiliary files (defaults to LIBDIR/aux)])
SRCDIR=$PWD
BINDIR=${BINDIR:-${exec_prefix}}
LIBDIR=${LIBDIR:-${libdir}}
SACAUX=${SACAUX:-${LIBDIR}/aux}

dnl Check for tau-p tables
AC_CHECK_FILE(
   /usr/local/lib/tpttsub.o,
   [TPTT=/usr/local/lib/tpttsub.o],
   [TPTT=tpttdummy.o ;
      echo '**' ;
      echo '**No tau-p tables; make_tt will not be usable' ;
      echo '**'
   ]
)

dnl SAC I/O library check
AC_LANG_PUSH([Fortran 77])
AC_SEARCH_LIBS(
   [wsac0],[sacio],
   [SACLIB=-lsacio],
   [for d in /usr/local/lib $SACAUX}/.. notfound ; do
       test -f $d/libsacio.a && break
    done
    test $d = notfound && {
      echo '**' ;
      echo "**No SAC library found -- re-configure with" ;
      echo "** ./configure LDFLAGS='-Lpath-to-libsacio.a'" ;
      echo '**' ; } || SACLIB=$d/libsacio.a
   ]
)
AC_LANG_POP([Fortran 77])

dnl Semi-exotic types
AC_CHECK_TYPES(long long,,[ADD_CFLAGS="${ADD_CFLAGS} -DDONTHAVE_LONG_LONG"])

dnl Additional flags based on system type
dnl   -O1 fixes problem in gfortran 4.9 extseed.f with IEEE_DENORMAL errors
if (uname -s > /dev/null) ; then
   case `uname -s` in
      Darwin)
         case `uname -r` in
         7*) LIBS="${LIBS}"
           ;;
#        8*) LIBCRT="-lcrt1.o"
#          ;;
         1[[0-9]].*) ADD_FFLAGS="${ADD_FFLAGS}"
           ;;
         esac
      ;;
   *) ;;
   esac
   case `uname -s` in
      FreeBSD|Linux) ADD_CFLAGS="${ADD_CFLAGS}" ;;
      *) ;;
   esac
   case `uname -r -s` in
      "SunOS 4.1."*) FOPT= ADD_CFLAGS="${ADD_CFLAGS}" ;;
      "Linux "[[34]].*)
         AC_DEFINE([HAVE_FENV_H],[1],[ABI provides IEEE trapping])
      ;;
      *) FOPT=-O1 ADD_CFLAGS="${ADD_CFLAGS} -O1";;
   esac
fi

# AC_MSG_CHECKING(whether libxml present)
dnl AC_SEARCH_LIBS(xmlSAXUserParseFile,xml2, [AC_DEFINE([HAVE_LIBXML],[1]) CFLAGS="${CFLAGS} -I/usr/include/libxml2"])
dnl xml2-config --cflags
# test ${enable_libxml-yes} = no || {
# AC_SEARCH_LIBS(xmlSAXUserParseFile,xml2, [AC_DEFINE([HAVE_LIBXML],[1]) HAVE_LIBXML=" "])
# }
# if test "${HAVE_LIBXML-unset}" = unset ; then
#    AC_DEFINE([HAVE_LIBXML],[0],[whether using libxml])
#    AC_MSG_RESULT(no)
# else
#    AC_MSG_RESULT(yes)
#    AC_MSG_CHECKING(how to get libxml includes)
#    if test -z "`which xml2-config`" ; then
#       AC_MSG_RESULT(probably /usr/include)
#       CFLAGS="${CFLAGS} -I/usr/include/libxml2"
#    else
#       HAVE_LIBXML="`xml2-config --cflags | sed -e 's/-I//'`"
#       AC_MSG_RESULT(seems to be ${HAVE_LIBXML})
#       CFLAGS="${CFLAGS} `xml2-config --cflags`"
#    fi
# fi

LIBS="${LDFLAGS} ${LIBS}"

dnl Fortran/C interoperability
dnl Check whether fortran compiler is 32 or 64 bit mode by default; adjust
dnl CFLAGS if so.
AC_LANG_PUSH([Fortran 77])
AC_LANG_CONFTEST([      end])
ARCHBITS=32
for arg in `$F77 $FFLAGS -v -o conftest conftest.f 2>&1 | $EGREP 'as[[ 	]]+-arch ' | awk '{print $3}'` ; do
   case "${arg}" in
      i386) CFLAGS="${CFLAGS} -m32";;
      x86_64) CFLAGS="${CFLAGS} -m64" ARCHBITS="${ARCHBITS} 64";;
      ppc64) CFLAGS="${CFLAGS} -m64"  ARCHBITS="${ARCHBITS} 64";;
   esac
   EXARCH="${arg}"
done
AC_MSG_CHECKING(Fortran symbol naming)
AC_F77_FUNC(init_sac,SYMBOL)
case "$SYMBOL" in
   init_sac_) 
      AC_MSG_RESULT(they end with _ - fine)
      ;;
   init_sac__) ADD_CFLAGS="${ADD_CFLAGS} -DFSYMSFX=2"
      AC_MSG_RESULT(they end with __ - fine)
      ;;
   init_sac) ADD_CFLAGS="${ADD_CFLAGS} -DIBM"
      AC_MSG_RESULT(they are unchanged - fine)
      ;;
   *) AC_MSG_RESULT(no known method - good luck!)
      ;;
esac
AC_LANG_POP([Fortran 77])

AC_LANG_PUSH([Fortran 77])
SV_FFLAGS="${FFLAGS}" F77BC=
dnl Check for Fortran bounds-check option
for F in fbounds-check C unavailable-option ; do
   FFLAGS="-c -$F"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[break])
done
test "$F" = "unavailable-option" || F77BC="-$F"

dnl Check for Fortran backslash option
FFLAGS="-c -fbackslash"
AC_TRY_COMPILE([],[
      i = index(' \t','t')
],[ADD_FFLAGS="${ADD_FFLAGS} -fbackslash"],[ADD_FFLAGS=])

dnl Defeat loop bounds check optimizations (new feature in gfortran 4.8 that
dnl will cause apparent program failures in do loops involving sacmem()
dnl indexing)
# FFLAGS="-c -fno-aggressive-loop-optimizations"
# AC_TRY_COMPILE([],[
#       i = index(' \t','t')
# ],[ADD_FFLAGS="${ADD_FFLAGS} -fno-aggressive-loop-optimizations"],[])

dnl Check Fortran for brief debug info availability
FFLAGS="-g -feliminate-unused-debug-symbols"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[FDEBUG=" -feliminate-unused-debug-symbols"],[FDEBUG=])

AC_LANG_POP(Fortran 77)
FFLAGS="${SV_FFLAGS} ${F77SIZE}"

dnl Check C for brief debug info availability
AC_LANG_PUSH(C)
SV_CFLAGS="${CFLAGS}" SV_LDFLAGS="${LDFLAGS}"
CFLAGS="-g -feliminate-unused-debug-symbols"
AC_TRY_COMPILE([],[
      int main(){return 0;}
],[CDEBUG=" -feliminate-unused-debug-symbols"],[CDEBUG=])
CFLAGS="${SV_CFLAGS}" LDFLAGS="-nostdlib -lcrt1.o -lSystem"
AC_LINK_IFELSE([
      int main(){return 0;}
],[LIBCRT="${LIBCRT} -lcrt1.o"],[])
AC_LANG_POP(C)
CFLAGS="${SV_CFLAGS}" LDFLAGS="${SV_LDFLAGS}"

test ${enable_debug-no} = yes && {
   ADD_FFLAGS="${ADD_FFLAGS} -g${FDEBUG}" ADD_CFLAGS="${ADD_CFLAGS} -g${CDEBUG}"
}
test ${enable_opt-no} = yes && \
   ADD_FFLAGS="${ADD_FFLAGS} -O2" ADD_CFLAGS="${ADD_CFLAGS} -O2"
test ${enable_opt-no} = no && \
   ADD_FFLAGS="${ADD_FFLAGS} ${FOPT}" ADD_CFLAGS="${ADD_CFLAGS}"
test ${enable_range_check-no} = yes && ADD_FFLAGS="${ADD_FFLAGS} ${F77BC}"

dnl Remove -m32 on unsuitable machines
dnl Does not seem to be needed any more with contemporary compilers
dnl -- GRH 24 May 2012
dnl case "${F77}-`uname -p`-`uname -r`" in
dnl    g77-*-* | *-powerpc-Darwin) 
dnl       ADD_CFLAGS=`echo "${ADD_CFLAGS}" | sed -e "s;-m32;;"`
dnl       FFLAGS=`echo "${FFLAGS}" | sed -e "s;-m32;;"`
dnl       ;;
dnl    *) ;;
dnl esac

dnl Optional compilation defines
AC_ARG_ENABLE(debug,  
   [  --enable-debug          compiles programs with debugging enabled]
)
AC_ARG_ENABLE(range_check,  
   [  --enable-range-check    checks that array references are within bounds]
)
AC_ARG_ENABLE(opt,  
   [  --enable-opt            compiles programs with optimization]
)

AC_OUTPUT(Makefile)
echo "      Configured with C compiler flags" $CFLAGS $ADD_CFLAGS
echo "Configured with Fortran compiler flags" $FFLAGS $ADD_FFLAGS
