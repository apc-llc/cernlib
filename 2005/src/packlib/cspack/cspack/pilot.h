#if !defined(CERNLIB_IBMVM)
#if 0 
*   deactivate CERNLIB_SHIFT if you do not have the SHIFT software
#endif
#define CERNLIB_SHIFT
#endif

#ifndef CERNLIB_SINGLE
#define CERNLIB_DOUBLE
#endif


#if defined(CERNLIB_UNIX)
#define CERNLIB_EQUHOLCH
#define CERNLIB_QHOLL
#define CERNLIB_QTRHOLL
#define CERNLIB_QORTHOLL
#define CERNLIB_QASCII
#define CERNLIB_QX_SC
#define CERNLIB_MMAP
#define CERNLIB_SYS5
#endif

#if defined(CERNLIB_IBMRT)
#define CERNLIB_QMIRT 
#endif

#if defined(CERNLIB_HPUX)
#define CERNLIB_QMHPX
#endif

#if defined(CERNLIB_SUN)
#define CERNLIB_QMSUN 
#define CERNLIB_BUGLRSHF
#undef CERNLIB_QMILSTD
#undef CERNLIB_QISASTD
#endif

#if defined(CERNLIB_SGI)
#define CERNLIB_QMSGI
#undef CERNLIB_RZBYTES
#endif

#if (defined(CERNLIB_DECS))||(defined(CERNLIB_DECOSF))
#define CERNLIB_QMVMI
#define CERNLIB_FQNEEDCV
#undef CERNLIB_Z32
#undef CERNLIB_FQXISN
#undef CERNLIB_RZBYTES
#undef CERNLIB_QMILSTD
#endif

#if defined(CERNLIB_WINNT)
#define CERNLIB_QMDOS
#define CERNLIB_QS_WNT
#ifndef CERNKIB_QFMSOFT
#  ifndef CERNLIB_QFDEC
#    define CERNLIB_QFDEC
#  endif
#endif
#define CERNLIB_FQNEEDCV
#define CERNLIB_F77TRARG
#undef CERNLIB_Z32
#undef CERNLIB_FQXISN
#if defined(_ALPHA_) && defined(CERNLIB_RZBYTES)
#  undef CERNLIB_RZBYTES
#endif
#  ifdef CERNLIB_MSSTDCALL
#     define F77_UPERCASE
#     define CERNLIB_QXCAPT
#     define type_of_call _stdcall
#     ifndef VISUAL_CPLUSPLUS
#       define VISUAL_CPLUSPLUS
#     endif
#  else
#     define F77_LOWERCASE
#     define CERNLIB_QXNO_SC
#  endif
#  ifndef CERNLIB_TCPSOCK
#    define CERNLIB_TCPSOCK
#  endif
#endif

#ifndef type_of_call
#define type_of_call
#endif

#if defined(CERNLIB_VAXVMS)
#define CERNLIB_DECNET
#define CERNLIB_QMVAX
#define CERNLIB_A4
#define CERNLIB_B32
#define CERNLIB_HEX
#define CERNLIB_QHOLL
#define CERNLIB_EQUHOLCH
#define CERNLIB_QTRHOLL
#define CERNLIB_QASCII
#define CERNLIB_CFIO
#define CERNLIB_QMILSTD
#define CERNLIB_QPRINT
#define CERNLIB_QDEBUG
#define CERNLIB_QDEBPRI
#define CERNLIB_FZALFA
#define CERNLIB_FZCHANNE
#define CERNLIB_FZDACC
#define CERNLIB_FZDACCF
#define CERNLIB_FZDACCH
#define CERNLIB_FZDACCL
#define CERNLIB_FZFFNAT
#define CERNLIB_FZFORTRA
#define CERNLIB_FZLIBC
#define CERNLIB_FZMEMORY
#define CERNLIB_JZTIME
#define CERNLIB_RZFRECL
#define CERNLIB_FQIE3FDC
#define CERNLIB_FQIE3FSC
#define CERNLIB_FQIE3TDC
#define CERNLIB_FQIE3TSC
#define CERNLIB_FQNEEDCV
#define CERNLIB_QCDEC

#endif

#ifdef CERNLIB_CRAY
#define CERNLIB_QXCAPT
#endif

