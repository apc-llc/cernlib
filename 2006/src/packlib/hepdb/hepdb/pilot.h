#if defined(CERNLIB_WINNT)
# if !defined(CERNLIB_UNIX)
#  define CERNLIB_UNIX
# endif
# define CERNLIB_LENDIAN
# ifndef CERNLIB_QFMSOFT
#   define CERNLIB_BSLASH
# endif
# ifdef CERNLIB_MSSTDCALL
#   define CERNLIB_QXCAPT
# else
#   define CERNLIB_QXNO_SC
# endif
#endif

#if defined(CERNLIB_UNIX)
#define CERNLIB__DEBUG
#define CERNLIB__SERVER
#define CERNLIB_EQUHOLCH
#define CERNLIB_QHOLL
#define CERNLIB_QTRHOLL
#ifndef CERNLIB_QXCAPT
# define CERNLIB_QX_SC
#endif
#endif

#if defined(CERNLIB_LINUX)
#if !defined(CERNLIB_PPC)
#define CERNLIB_LENDIAN
#endif
#define CERNLIB_BSLASH
#endif

#if (defined(CERNLIB_DECS))||(defined(CERNLIB_QMVAOS))
#define CERNLIB_LENDIAN
#endif

#if defined(CERNLIB_VAXVMS)
#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#define CERNLIB__DEBUG
#define CERNLIB__SERVER
#define CERNLIB_EQUHOLCH
#define CERNLIB_QHOLL
#define CERNLIB_QTRHOLL
#endif
