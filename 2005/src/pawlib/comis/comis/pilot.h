#ifndef CERNLIB_PAW
#define CERNLIB_PAW
#endif

#if (defined(CERNLIB_CONVEX)) && (!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_LINUX
#define CERNLIB_LINUX
#endif
#endif


#if defined(CERNLIB_HPUX)
#ifndef CERNLIB_ALIGN
#define CERNLIB_ALIGN
#endif
#endif

#if defined(CERNLIB_QMVAOS)
#ifndef CERNLIB_ALPHA_OSF
#define CERNLIB_ALPHA_OSF
#endif
#endif

#if defined(CERNLIB_DECS)   || defined(CERNLIB_IBMRT) || \
    defined(CERNLIB_SUN)    || defined(CERNLIB_SGI)   || \
    defined(CERNLIB_AIX370) || defined(CERNLIB_HPUX)  || \
    defined(CERNLIB_IPSC)   || defined(CERNLIB_ALLIANT)
#ifndef CERNLIB_UNIX
#define CERNLIB_UNIX
#endif
#endif

#if defined(CERNLIB_DECS)  || defined(CERNLIB_IBMRT) || \
    defined(CERNLIB_SUN)   || defined(CERNLIB_SGI)   || \
    defined(CERNLIB_HPUX)  || defined(CERNLIB_MSDOS) || \
    defined(CERNLIB_LINUX) || defined(CERNLIB_WINNT)
#ifndef CERNLIB_STDUNIX
#define CERNLIB_STDUNIX
#endif
#endif

#if defined(CERNLIB_VAXVMS)
#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#ifndef CERNLIB_QXNO_SC
#define CERNLIB_QXNO_SC   
#endif
#endif

#if defined(CERNLIB_CRAY) || defined(CERNLIB_NEXT) || \
    defined(CERNLIB_LINUX)
#ifndef CERNLIB_UNIX
#define CERNLIB_UNIX
#endif
#endif

#if defined(CERNLIB_STDUNIX)
#ifndef CERNLIB_UNIX
#define CERNLIB_UNIX
#endif
#endif

#if defined(CERNLIB_IBMMVS) || defined(CERNLIB_NEWLIB)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#endif

#if defined(CERNLIB_IBM)
#ifndef CERNLIB_ASSEMB
#define CERNLIB_ASSEMB
#endif
#ifndef CERNLIB_QMIBMXA
#define CERNLIB_QMIBMXA
#endif
#endif

#if defined(CERNLIB_UNIX)
#ifndef CERNLIB_DECCC
#define CERNLIB_DECCC
#endif
#endif

#if defined(CERNLIB_CRAY) || defined(CERNLIB_MSSTDCALL)
#ifndef CERNLIB_QXCAPT
#define CERNLIB_QXCAPT
#endif
#endif

#if defined(CERNLIB_HPUX) || defined(CERNLIB_SUN)       || \
    defined(CERNLIB_SGI)  || defined(CERNLIB_ALPHA_OSF) || \
    defined(CERNLIB_IBMRT)|| defined(CERNLIB_LINUX)
#ifndef CERNLIB_SHL
#define CERNLIB_SHL
#endif
#endif

#if defined(CERNLIB_SUN)     || defined(CERNLIB_SGI)    || \
    defined(CERNLIB_DECS)    || defined(CERNLIB_CONVEX) || \
    defined(CERNLIB_IBMRT)   || defined(CERNLIB_HPUX)   || \
    defined(CERNLIB_AIX370)  || defined(CERNLIB_IPSC)   || \
    defined(CERNLIB_ALLIANT) || defined(CERNLIB_NEXT)   || \
    defined(CERNLIB_LINUX)   || defined(CERNLIB_MSDOS)  || \
    defined(CERNLIB_WINNT)
#ifndef CERNLIB_QXCAPT
# ifndef CERNLIB_QX_SC
#   define CERNLIB_QX_SC
# endif
#endif
#endif

#if defined(CERNLIB_APOLLO)
#ifndef CERNLIB_QXNO_SC
#define CERNLIB_QXNO_SC
#endif
#endif

#ifdef CERNLIB_MSSTDCALL
# define type_of_call __stdcall
# ifdef CERNLIB_QX_SC
#  undef CERNLIB_QX_SC
# endif
# ifndef CERNLIB_QXCAPT
#  define CERNLIB_QXCAPT
# endif
#endif

#ifndef type_of_call
#define type_of_call
#endif

