#ifndef CERNLIB_QTRHOLL
#define CERNLIB_QTRHOLL
#endif

#ifndef CERNLIB_GCGTOAD
#define CERNLIB_GCGTOAD
#endif
#if defined(CERNLIB_MSDOS)
#ifndef CERNLIB_F2C
#define CERNLIB_F2C
#endif
#endif
#if defined(CERNLIB_UNIX)
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif
#if defined(CERNLIB_WINNT)||defined(CERNLIB_F2C)||defined(CERNLIB_QMALPH)
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif
#if !defined(CERNLIB_QMIBM)
#ifndef CERNLIB_NONEWL
#define CERNLIB_NONEWL
#endif
#endif
