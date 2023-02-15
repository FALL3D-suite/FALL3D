#
# *** Check for netcdf (Fortran version) ***
#
#  Author G.Macedonio
#
#  Date of this version: 28-JAN-2016
#
#  Search sequence:
#  1. Check if configure NETCDF=<directory> is given
#  2. Look for environment variable NETCDF
#  3. Look for nf-config
#  4. Look for nc-config
#  5. Guess NETCDF from the location of ncdump
#
#  If the environmental variable NETCDF is set, NETCDF_INC is set to
#  $NETCDF/include and NETCDF_LIB to $NETCDF/lib.
#  Moreover, both NETCDF_LIB and NETCDF_INC can be set as environmental
#  variables or passed as arguments fo configure; in this case their
#  value overrides the evaluation of $NETCDF/lib or $NETCDF/include
#
#  The compiler and linker flags NC_INC and NC_LIB are set automatically
#  or obtained from nf-config/nc-config if available, or can be passed
#  to configure.
#
AC_DEFUN([AX_NETCDFF], [
ax_netcdff_ok=no
#
AC_ARG_VAR([NETCDF],[location of the netcdf lib])
AC_ARG_VAR([NC_INC],[compiler flags for netcdf])
AC_ARG_VAR([NC_LIB],[linker flags for netcdf])
if test "X$NETCDF" = "X" ; then
  # NETCDF is not set: look for nf-config or nc-config
  # Check for nf-config
  AC_CHECK_PROG([NC_CONF],[nf-config],[nf-config])
  # If not found, check for nc-config
  AS_IF([test "x$NC_CONF" = "x"],
     [AC_CHECK_PROG([NC_CONF],[nc-config],[nc-config])])
  # If nf-config or nc-config are found
  if test x"$NC_CONF" != "x"; then
    AC_MSG_CHECKING(for netCDF includes)
    AS_IF([test "x$NC_INC" = "x"],[NC_INC=`$NC_CONF --fflags`])
    AC_MSG_RESULT($NC_INC)
    AC_MSG_CHECKING(for netCDF libs)
    AS_IF([test "x$NC_LIB" = "x"],[NC_LIB=`$NC_CONF --flibs`])
    AC_MSG_RESULT($NC_LIB)
    NETCDF=`$NC_CONF --prefix`
    NC_VERSION=`$NC_CONF --version`
    AC_MSG_NOTICE([found $NC_VERSION])
  else
    # Guess location of libnetcdf.a from the directory of ncdump
    AC_PATH_PROG([NETCDF], [ncdump])
    NETCDF=`dirname $NETCDF`   # Strip /ncdump
    NETCDF=`dirname $NETCDF`;  # Strip /bin
    AS_IF([test "x$NETCDF" != "x"],
    [AC_MSG_NOTICE([obtained NETCDF=$NETCDF (from the path of ncdump)])])
  fi
fi
AC_MSG_NOTICE([setting netcdf root directory NETCDF=$NETCDF])
AC_MSG_CHECKING([for the existence of the netcdf root directory])
AS_IF([test -d $NETCDF],[AC_MSG_RESULT([$NETCDF])],
    [AC_MSG_ERROR([directory $NETCDF not found])])
#
# Set the netcdf compiler flags
AC_MSG_CHECKING([for netCDF include directory])
AS_IF([test "x$NETCDF_INC" = "x"],[NETCDF_INC=$NETCDF/include])
AC_MSG_RESULT($NETCDF_INC)
AC_MSG_CHECKING([for the existence of the netcdf include directory])
AS_IF([test -d $NETCDF_INC],[AC_MSG_RESULT([ok])],
    [AC_MSG_ERROR([directory $NETCDF_INC not found])])
AC_MSG_CHECKING([for file netcdf.mod in the include directory])
AS_IF([test -f $NETCDF_INC/netcdf.mod],
   [AC_MSG_RESULT(ok)],
   [AC_MSG_ERROR([netcdf.mod can not be found in $NETCDF_INC])])
AS_IF([ test "x$NC_INC" = "x"],[NC_INC=-I$NETCDF_INC])
#
# Set the netcdf linker flags
AC_MSG_CHECKING([for netCDF library directory])
AS_IF([test "X$NETCDF_LIB" = "X"],[NETCDF_LIB=$NETCDF/lib])
AC_MSG_RESULT([$NETCDF_LIB])
AS_IF([test ! -d $NETCDF_LIB],[AC_MSG_ERROR([No such directory: $NETCDF_LIB])])
AS_IF([test "x$NC_LIB" = "x"],
  [AC_MSG_CHECKING([for -lnetcdff]);
   AS_IF([test -f $NETCDF_LIB/libnetcdff.a],
     [NC_LIB="-L$NETCDF_LIB -lnetcdff -lnetcdf";AC_MSG_RESULT([yes])],
     [NC_LIB="-L$NETCDF_LIB -lnetcdf";AC_MSG_RESULT([no])])])
#
# NC_INC and NC_LIB contain the flags for compilation and linking and are
# exported to Makefiles
AC_SUBST(NC_INC)
AC_SUBST(NC_LIB)
#
ax_netcdff_ok=yes

])dnl end AX_NETCDFF
