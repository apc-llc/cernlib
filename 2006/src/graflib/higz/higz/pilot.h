#if defined(CERNLIB_GKSGRAL) || defined(CERNLIB_MKGS) \
  || defined(CERNLIB_PLOT10GKS) || defined(CERNLIB_UNIGKS) \
  || defined(CERNLIB_DECGKS) || defined(CERNLIB_GK2000) \
  || defined(CERNLIB_ATCGKS) || defined(CERNLIB_GKSBX)
#ifndef CERNLIB_GKS
#define CERNLIB_GKS
#endif
#endif

#ifndef CERNLIB_GKS

#ifndef CERNLIB_X11
#define CERNLIB_X11
#endif

#ifndef CERNLIB_FALCO
#define CERNLIB_FALCO
#endif

#endif

#ifndef CERNLIB_NTC
#define CERNLIB_NTC
#endif

#ifndef CERNLIB_PSCRIPT
#define CERNLIB_PSCRIPT
#endif

#ifndef CERNLIB_ZEBRA
#define CERNLIB_ZEBRA
#endif

#if defined(CERNLIB_ALPHA)
#ifndef CERNLIB_VAX
#define CERNLIB_VAX
#endif
#endif

#if defined(CERNLIB_WINNT) || defined(CERNLIB_TK_WINDOWS)
#ifndef CERNLIB_X11
#define CERNLIB_X11
#endif
#endif

#if (defined(CERNLIB_MSDOS) || defined(CERNLIB_LINUX) ) && !defined(CERNLIB_WINNT)
#ifndef CERNLIB_F2C
#define CERNLIB_F2C
#endif
#endif

#if defined(CERNLIB_SUN)  || defined(CERNLIB_SGI)    || \
    defined(CERNLIB_DECS) || defined(CERNLIB_IBMRT)  || \
    defined(CERNLIB_HPUX) || \
    defined(CERNLIB_IPSC) || \
    defined(CERNLIB_F2C)  || defined(CERNLIB_QMALPH) || \
    defined(CERNLIB_WINNT)
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif

#ifdef CERNLIB_QFMSOFT
#ifdef CERNLIB_BSLASH
#undef CERNLIB_BSLASH
#endif
#endif

#if defined(CERNLIB_SUNGKS) || defined(CERNLIB_GKSIBM)
#ifndef CERNLIB_GKSGRAL
#define CERNLIB_GKSGRAL
#endif
#endif

#if defined(CERNLIB_GKSGRAL)   || defined(CERNLIB_MGKS)   || \
    defined(CERNLIB_PLOT10GKS) || defined(CERNLIB_UNIGKS) || \
    defined(CERNLIB_DECGKS)    || defined(CERNLIB_GK2000) || \
    defined(CERNLIB_ATCGKS)    || defined(CERNLIB_GKSBX)
#ifndef CERNLIB_GKS
#define CERNLIB_GKS
#endif
#endif

#if defined(CERNLIB_MSDOS)
#ifndef CERNLIB_CIPACK
#define CERNLIB_CIPACK
#endif
#ifndef CERNLIB_GL
#define CERNLIB_GL
#endif
#endif

#if defined(CERNLIB_X11)
#ifndef CERNLIB_ZBUF
#define CERNLIB_ZBUF
#endif

#if !defined(CERNLIB_GIF) && !defined(CERNLIB_WINNT)
#define CERNLIB_GIF
#endif
#endif

#if defined(CERNLIB_GL)     || \
    defined(CERNLIB_X11)    || \
    defined(CERNLIB_MACMPW) || \
    defined(CERNLIB_MSDOS)  || \
    defined(CERNLIB_WINNT)
#ifndef CERNLIB_KERNEL
#define CERNLIB_KERNEL
#endif
#endif

#if defined(CERNLIB_CRAY) || defined(CERNLIB_VAX) || \
    defined(CERNLIB_IBM)
#ifndef CERNLIB_QXNO_SC
#define CERNLIB_QXNO_SC
#endif
#endif

#if defined(CERNLIB_SGI)  || defined(CERNLIB_IBMRT)  || \
    defined(CERNLIB_SUN)  || defined(CERNLIB_CONVEX) || \
    defined(CERNLIB_DECS) || defined(CERNLIB_AIX370) || \
    defined(CERNLIB_HPUX) || \
    defined(CERNLIB_NEXT) || defined(CERNLIB_CONVEX) || \
    defined(CERNLIB_F2C) 
#ifndef CERNLIB_QX_SC
#define CERNLIB_QX_SC
#endif
#endif

#ifdef CERNLIB_MSSTDCALL
# ifndef CERNLIB_QXCAPT
#  define CERNLIB_QXCAPT
# endif
#define type_of_call __stdcall
#endif

#ifndef type_of_call
# define type_of_call
#endif

#if defined(CERNLIB_PHIGS)
#ifndef CERNLIB_NOVERS
#define CERNLIB_NOVERS
#endif
#endif

#if (defined(CERNLIB_CONVEX)) && (!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_CVX
#define CERNLIB_CVX
#endif
#endif

#if (defined(CERNLIB_CONVEX)) && (!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_CVY
#define CERNLIB_CVY
#endif
#endif
