#if defined(CERNLIB_IBMMVS)||defined(CERNLIB_IBMVM)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_VAXULTRIX)
#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#endif
#if defined(CERNLIB_CDC)||defined(CERNLIB_CRAY)
#ifndef CERNLIB_SINGLE
#define CERNLIB_SINGLE
#endif
#endif
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_NORD)
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif
#if defined(CERNLIB_UNIX)
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif
#if defined(CERNLIB_WINNT)||defined(CERNLIB_OS9)
#ifndef CERNLIB_QFMSOFT
# ifndef CERNLIB_BSLASH
#   define CERNLIB_BSLASH
# endif
# endif
#endif

#if defined(CERNLIB_IBM)
#ifndef CERNLIB_QMIBMXA
#define CERNLIB_QMIBMXA
#endif
#endif
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
#ifndef CERNLIB_QSAA
#define CERNLIB_QSAA
#endif
#endif
#if defined(CERNLIB_QMALPH)
#ifndef CERNLIB_FORTRAN
#define CERNLIB_FORTRAN
#endif
#endif
