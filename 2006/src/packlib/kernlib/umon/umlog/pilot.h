#if defined(CERNLIB_IBMMVS)||defined(CERNLIB_IBMVM)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#endif
#if defined(CERNLIB_IBMRT)
#ifndef CERNLIB_IBMRS
#define CERNLIB_IBMRS
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
