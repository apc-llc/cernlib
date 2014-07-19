#if defined(CERNLIB_UNIX)
# ifndef CERNLIB_QFMSOFT
#   define CERNLIB_BSLASH
# elif defined(CERNLIB_BSLASH)
#    undef CERNLIB_BSLASH
# endif
#endif

#if (defined(CERNLIB_UNIX))||(!defined(CERNLIB_SINGLE))

#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif

#endif

#if defined(CERNLIB_VAXVMS)

#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif

#endif

#if defined(CERNLIB_QMVAOS)
#ifndef CERNLIB_ALPHAOSF
#define CERNLIB_ALPHAOSF
#endif
#endif

#if defined(CERNLIB_QMALPH)
#ifndef CERNLIB_ALPHAVMS
#define CERNLIB_ALPHAVMS
#define CERNLIB_BSLASH
#endif
#endif

#if defined(CERNLIB_ALPHAVMS) || defined(CERNLIB_ALPHAOSF)
#ifndef CERNLIB_ALPHA
#define CERNLIB_ALPHA
#endif
#endif

#if defined(CERNLIB_ALPHAVMS) || defined(CERNLIB_VAXVMS)
#ifndef CERNLIB_VMS
#define CERNLIB_VMS
#endif
#endif


