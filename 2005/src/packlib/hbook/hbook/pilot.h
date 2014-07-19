#if defined(CERNLIB_VAX)
#if defined(CERNLIB_QMALPH)
#ifndef CERNLIB_ALPHA
#define CERNLIB_ALPHA
#endif
#endif
#endif

#if defined(CERNLIB_QMVAOS)
#ifndef CERNLIB_ALPHA_OSF
#define CERNLIB_ALPHA_OSF
#ifndef CERNLIB_ALPHA
#define CERNLIB_ALPHA
#endif
#endif
#endif

#if defined(CERNLIB_IBMMVS)||defined(CERNLIB_NEWLIB)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#endif

#if defined(CERNLIB_MSDOS)
#ifndef CERNLIB_QF2C
#define CERNLIB_QF2C
#endif
#endif

#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_NORD)||defined(CERNLIB_UNIVAC)
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif

#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif

#if defined(CERNLIB_UNIX)||defined(CERNLIB_GNU)||defined(CERNLIB_QMALPH)||defined(CERNLIB_WINNT)||defined(CERNLIB_QF2C)
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif

#if (defined(CERNLIB_APOLLO)||defined(CERNLIB_VAX)||defined(CERNLIB_UNIX)||defined(CERNLIB_IBM))&&(!defined(CERNLIB_IPSC))&&(!defined(CERNLIB_MSDOS))
#ifndef CERNLIB_CZ
#define CERNLIB_CZ
#endif
#endif

#if (defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_IPSC))&&(!defined(CERNLIB_MSDOS))
#ifndef CERNLIB_HMMAP
#define CERNLIB_HMMAP
#endif
#endif

#if defined(CERNLIB_UNIX)||defined(CERNLIB_MSDOS)||(defined(CERNLIB_WINNT) && !defined(CERNLIB_MSSTDCALL))
# ifndef CERNLIB_QX_SC
#  define CERNLIB_QX_SC
# endif
#endif

#if defined(CERNLIB_APOLLO)||defined(CERNLIB_CONVEX)
#ifndef CERNLIB_BSD
#define CERNLIB_BSD
#endif
#endif

#if defined(CERNLIB_SUN)
# if defined(CERNLIB_SOLARIS)
#  ifndef CERNLIB_SYS5
#  define CERNLIB_SYS5
#  endif
# else
#  ifndef CERNLIB_BSD
#  define CERNLIB_BSD
#  endif
# endif
#endif

#if defined(CERNLIB_IBMRT)||defined(CERNLIB_DECS)||defined(CERNLIB_HPUX)||defined(CERNLIB_SGI)||defined(CERNLIB_AIX370)||defined(CERNLIB_IPSC)||defined(CERNLIB_LINUX)
#ifndef CERNLIB_SYS5
#define CERNLIB_SYS5
#endif
#endif

#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_AIX370)||defined(CERNLIB_NORD)||defined(CERNLIB_UNIVAC)
#ifndef CERNLIB_NO_IEEE
#define CERNLIB_NO_IEEE
#endif

#endif
#ifdef CERNLIB_MSSTDCALL
# define VISUAL_CPLUSPLUS
# ifndef CERNLIB_QXCAPT
#  define CERNLIB_QXCAPT
# endif
# ifndef type_of_call
#  define type_of_call _stdcall
# endif
#endif

#ifndef type_of_call
# define type_of_call
#endif
