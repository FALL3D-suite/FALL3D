#
# This is the autoconf configuration file for Fall3d
# Process this file with autoconf to produce a configure script.
#
# To produce files: configure, Makefiles and Scripts
#    $ automake
#    $ autoconf
#
# Eventually, if you receive errors, you need to regenerate
# file aclocal.m4 with the command:
#    $ autoreconf
#  or
#    $ aclocal
#    $ autoconf
#
#
# Force an error if autoconf version is earlier than 2.63 (leave commented)
# AC_PREREQ([2.63])
#
# Autoconf initialization [Package name], [version], [email for problems]
AC_INIT([fall3d], [8.0.0])
#
# Allows modification of the program file names (keep commented)
#AC_CANONICAL_TARGET
#AC_ARG_PROGRAM
#
AC_CONFIG_AUX_DIR([autoconf])
AC_CONFIG_MACRO_DIR([m4])
#
# Automake initialization
AM_INIT_AUTOMAKE([foreign])
#
# Set language for configuration checks
AC_LANG(Fortran)
#
# Set the build variables: build_cpu, build_vendor and build_os
# It needs files config.guess and config.sub
AC_CANONICAL_BUILD
#
# Make instructions for mkdir -p
# AC_PROG_MKDIR_P
#
# Set default prefix (where directory bin is created)
# This is the default top directory of the installation
AC_PREFIX_DEFAULT(`pwd`)
#
# Scan for possible compilers (used if environment variable FC is not set)
AC_PROG_FC
# Guess default Fortran linker flags
AC_FC_LIBRARY_LDFLAGS
#
# Assume that F90 is compatible with F77
# Note: F77 is used only in SetSrc to compile odepack
AC_MSG_CHECKING([Fortran 77 compiler])
AC_ARG_VAR([F77],[Fortran 77 compiler command])
AS_IF([test "x$F77" = "x"], [F77=$FC])
AC_MSG_RESULT([$F77])
# F77 FLAGS
AC_MSG_CHECKING([Fortran 77 compiler flags])
AC_ARG_VAR([FFLAGS],[Fortran 77 compiler flags])
#AS_IF([test "x$FFLAGS" = "x"], [FFLAGS=$FCFLAGS])
AC_MSG_RESULT([$FFLAGS])
#
# *** Check for NETCDFF
AX_NETCDFF
#
# *** Check for MPI (if flag --enable-parallel is set)
# Check option "--enable-parallel"
MSG_PARALLEL="(serial)"
AC_ARG_ENABLE([parallel],
  [AS_HELP_STRING([--enable-parallel],[turn on parallel execution (with MPI)])],
  [case "${enableval}" in
    yes) parallel=yes ;;
     no) parallel=no ;;
      *) AC_MSG_ERROR([bad value ${enableval} for --enable-parallel]) ;;
   esac], [parallel=yes])
AM_CONDITIONAL([WITH_MPI], [test "x$parallel" = "xyes"])
#
# Check the existence of mpifort
AC_ARG_VAR([MPIFORT],[parallel compiler (used with --enable-parallel)])
AS_IF([test "x$parallel" = "xyes"],
  [AS_IF([test "x$MPIFORT" = "x"], [AC_CHECK_PROGS([MPIFORT], [mpifort mpif90], [mpifort])])]
       [AS_IF([test "x$MPIFORT" = "x"], [AC_MSG_ERROR([cannot find compiler mpifort])])]
[MSG_PARALLEL="(serial and parallel)"])
#
# Check for existence of makedepf90
AC_CHECK_PROG([MAKEDEPF90], [makedepf90], [makedepf90])
#
# Set single precision (default r8)
AC_ARG_WITH([r4],
  [AS_HELP_STRING([--with-r4],[Compile in single precision (R4)])],
  [with_r4=yes],[with_r4=no])
AM_CONDITIONAL([WITH_R4], [test $with_r4 = yes])
#
# Generate instructions for ar
#m4_pattern_allow([AM_PROG_AR])
#AM_PROG_AR
#
# Generate instructions for ranlib
AC_PROG_RANLIB

# Doxygen - look for doxygen (a documentation tool)
# -------------------------------------------------------------
#AC_ARG_VAR(DOXYGEN,[The program doxygen])
#AS_IF([test "x$DOXYGEN" = "x"],[AC_PATH_PROG(DOXYGEN, doxygen)])
#HAVE_DOXYGEN=NO
#if test "x$DOXYGEN" != x ; then
#   HAVE_DOXYGEN=YES
#  AM_CONDITIONAL([HAVE_DOXYGEN],[true])
#fi

# Message inserted in the output scripts
AC_SUBST([DO_NOT_MODIFY_MESSAGE],
["DO NOT MODIFY (it is generated automatically by configure)"])
#

# List of Makefiles to be processed
AC_CONFIG_FILES([Makefile
		Sources/Makefile
		Sources/mod_Config.f90
])

# Conditional output
#AS_IF([test "$HAVE_DOXYGEN" = "YES"],
#   AC_CONFIG_FILES([Manual/Makefile
#]))

#
# Produce all the output
AC_OUTPUT
#
# Write configuration on the screen
AC_MSG_NOTICE([---------------------------------------------------------])
AC_MSG_NOTICE([Configuration complete - $PACKAGE_NAME-$PACKAGE_VERSION $MSG_PARALLEL])
AC_MSG_NOTICE([])
AS_IF([test "x$NC_VERSION" != "x"],AC_MSG_NOTICE([Using $NC_VERSION]))
#
AC_MSG_NOTICE([Single precision:           --with-r4=$with_r4])
AC_MSG_NOTICE([Enable parallel version:    --enable-parallel=$parallel])
AS_IF([test "$parallel" = "yes"],
  [AC_MSG_NOTICE([Fortran 90 PAR compiler:    MPIFORT=$MPIFORT])])
#
AC_MSG_NOTICE([Fortran compiler:           FC=$FC])
AC_MSG_NOTICE([Fortran flags:              FCFLAGS=$FCFLAGS])
AC_MSG_NOTICE([Fortran 77 compiler:        F77=$F77])
AC_MSG_NOTICE([Fortran 77 compiler flags:  FFLAGS=$FFLAGS])
AC_MSG_NOTICE([Root directory of netcdf:   NETCDF=$NETCDF])
AC_MSG_NOTICE([Compiler flags for netcdf:  NC_INC=$NC_INC])
AC_MSG_NOTICE([Linker flags for netcdf:    NC_LIB=$NC_LIB])
AC_MSG_NOTICE([Install prefix:             --prefix=$prefix])
AC_MSG_NOTICE([Executables install prefix: --exec_prefix=$exec_prefix])
AC_MSG_NOTICE([Binary directory:           --bindir=$bindir])
#
AC_MSG_NOTICE([---------------------------------------------------------])
