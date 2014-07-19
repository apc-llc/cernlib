#ifndef CERNLIB_COMIS
#define CERNLIB_COMIS
#endif

#if !defined(CERNLIB_FPANELS) && !defined(CERNLIB_WINNT)
#define CERNLIB_FPANELS
#endif

#if !defined(CERNLIB_MOTIF) && !defined(CERNLIB_WINNT)
#define CERNLIB_MOTIF  
#endif

#ifndef CERNLIB_SIGMA
#define CERNLIB_SIGMA
#endif

#ifndef CERNLIB_MLP
#define CERNLIB_MLP
#endif

#if (!defined(CERNLIB_IPSC)) && (!defined(CERNLIB_MSDOS))
#ifndef CERNLIB_CZ
#define CERNLIB_CZ
#endif
#endif

#if defined(CERNLIB_IBMMVS) || defined(CERNLIB_NEWLIB)
#ifndef CERNLIB_IBM
#define CERNLIB_IBM
#endif
#endif

#if defined(CERNLIB_UNIX)||defined(CERNLIB_QMALPH)
#ifndef CERNLIB_BSLASH
#define CERNLIB_BSLASH
#endif
#endif

#if defined(CERNLIB_APOLLO) || defined(CERNLIB_QFMSOFT)
#undef CERNLIB_BSLASH
#endif

#if (defined(CERNLIB_IBM)  || defined(CERNLIB_VAX))    || \
    (defined(CERNLIB_NORD) || defined(CERNLIB_APOLLO))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif

#if (defined(CERNLIB_UNIX) || defined(CERNLIB_WINNT)) && (!defined(CERNLIB_SINGLE))
#ifndef CERNLIB_DOUBLE
#define CERNLIB_DOUBLE
#endif
#endif

#if (defined(CERNLIB_UNIX)  || defined(CERNLIB_APOLLO))  && \
   (!defined(CERNLIB_IPSC)) && (!defined(CERNLIB_MSDOS)) && \
   (!defined(CERNLIB_NEXT)) && (!defined(CERNLIB_CONVEX))
#ifndef CERNLIB_MMAP
#define CERNLIB_MMAP
#endif
#endif

#ifdef CERNLIB_WINNT
#ifndef CERNLIB_MMAP
#define CERNLIB_MMAP
#endif
#endif

#ifdef CERNLIB_MSSTDCALL
#ifndef CERNLIB_QXCAPT
#define CERNLIB_QXCAPT
#endif
#endif

#if defined(CERNLIB_CRAY) || defined(CERNLIB_IBM) || \
    defined(CERNLIB_VAX)
#ifndef CERNLIB_QXNO_SC
#define CERNLIB_QXNO_SC
#endif
#endif

#if (defined(CERNLIB_APOLLO)) && (!defined(CERNLIB_APOF77))
#ifndef CERNLIB_QXNO_SC
#define CERNLIB_QXNO_SC
#endif
#endif

#if defined(CERNLIB_SGI)   || defined(CERNLIB_IBMRT)  || \
    defined(CERNLIB_SUN)   || defined(CERNLIB_CONVEX) || \
    defined(CERNLIB_DECS)  || defined(CERNLIB_AIX370) || \
    defined(CERNLIB_HPUX)  || defined(CERNLIB_APOF77) || \
    defined(CERNLIB_NEXT)  || defined(CERNLIB_LINUX)  || \
    defined(CERNLIB_MSDOS) || defined(CERNLIB_WINNT)
#if !defined(CERNLIB_QX_SC) && !defined(CERNLIB_QXCAPT)
#define CERNLIB_QX_SC
#endif
#endif

#if (defined(CERNLIB_UNIX)    ||   defined(CERNLIB_VAX)     || \
     defined(CERNLIB_IBM))    && (!defined(CERNLIB_IBMMVS)) && \
   (!defined(CERNLIB_NEWLIB))
#ifndef CERNLIB_BUGREPORT
#define CERNLIB_BUGREPORT
#endif
#endif

#ifdef CERNLIB_WINNT
#  ifdef CERNLIB_BUGREPORT
#    undef CERNLIB_BUGREPORT
#  endif
#  ifndef CERNLIB_CZ
#    define CERNLIB_CZ
#  endif
#  ifdef CERNLIB_MSSTDCALL
#   define F77_UPERCASE
#   ifndef CERNLIB_QXCAPT
#     define CERNLIB_QXCAPT 
#   endif
#   ifdef CERNLIB_QX_SC
#     undef CERNLIB_QX_SC
#   endif
#   define type_of_call _stdcall
#   ifndef VISUAL_CPLUSPLUS
#     define VISUAL_CPLUSPLUS
#   endif
#  endif
#endif

#ifndef type_of_call
#define type_of_call
#endif
