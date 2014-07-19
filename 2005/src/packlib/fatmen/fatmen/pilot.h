#if (!defined(CERNLIB_FNAL))&&(!defined(CERNLIB_DESY))
#define CERNLIB_CERN
#endif

#if defined(CERNLIB_WINNT)
# ifdef CERNLIB_MSSTDCALL
#   define CERNLIB_QXCAPT
# endif
#if !defined(CERNLIB_UNIX)
#  define CERNLIB_UNIX
#endif
#endif


#if defined(CERNLIB_UNIX)
#if !defined(CERNLIB_SINGLE)
#define CERNLIB_DOUBLE
#endif

#define CERNLIB_EQUHOLCH
#define CERNLIB_QHOLL
#define CERNLIB_QTRHOLL
#ifndef CERNLIB_QXCAPT
# define CERNLIB_QX_SC
#endif
#endif

#if defined(CERNLIB_VAXVMS)
#define CERNLIB_DOUBLE

#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif

#define CERNLIB_EQUHOLCH
#define CERNLIB_QHOLL
#define CERNLIB_QTRHOLL
#endif
