#ifndef CERNLIB_EXAMPLES
#define CERNLIB_EXAMPLES
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
#if defined(CERNLIB_UNIX)||(defined(CERNLIB_WINNT) && !defined(CERNLIB_QFMSOFT))
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif
