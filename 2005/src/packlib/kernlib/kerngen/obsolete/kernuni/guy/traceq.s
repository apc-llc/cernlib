*
* $Id: traceq.s,v 1.1.1.1 1996/02/15 17:54:01 mclareni Exp $
*
* $Log: traceq.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:01  mclareni
* Kernlib
*
*
#if defined(CERNLIB_GUYFORT)||defined(CERNLIB_GUYFTN)
. **************************************
.
.     SUBROUTINE TRACEQ( LUN, N )
.
. **************************************
.
          AXR$
          FORT$
 
#endif
#if defined(CERNLIB_GUYFORT)
#include "gerr_c.inc"
          RES       1
 
$(1),FIRS01
TRACEQ*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          S,H2      X11,DYN$
LMJSTR    LMJ       X11,STRACE
          L,H2      X11,DYN$
          J         RETUR2
 
          INFO  010  3
$(3),DIAG1 +        0776003,TRACEQ
          +         0,DYN$
          +         'TRACEQ'
DIAG2     +         0776202,LMJSTR+1
          +         LMJSTR-FIRS01,DIAG1
          END
#endif
#if defined(CERNLIB_GUYFTN)
PF        FORM      12,6,18
$(0)
TRAMSG    'NO TRACEQ OR STRACE ROUTINE CU'
          'RRENTLY PROVIDED WITHIN FTN'
LMSG      EQU       $-TRAMSG
TRAPR     PF        1,LMSG,TRAMSG
$(1)
TRACEQ*
STRACE*
          L         A0,TRAPR
          ER        PRINT$
          J         0,X11
          END
#endif
#if defined(CERNLIB_GUYFORT)||defined(CERNLIB_GUYFTN)
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
#endif
