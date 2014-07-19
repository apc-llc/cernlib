*
* $Id: vmodul.s,v 1.1.1.1 1996/02/15 17:53:48 mclareni Exp $
*
* $Log: vmodul.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:48  mclareni
* Kernlib
*
*
        TITLE   VMODUL
        SUBTTL  P. SCHMITZ
#include "reentr.inc"
        SEARCH HELP
       EXTERN  SQRT
        HELLO (VMODUL)
       MOVEM   A01,ACSAV                ;SAVE AC'S
       MOVEM   A02,ACSAV+1
       MOVEI   A02,@0(A16)              ;ADR(A)
       MOVE       ,(A02)
       FMPR                             ;A(1)**2
       MOVE    A01,1(A02)               ;A(1)**2+A(2)**2
       FMPR    A01,A01
       FADR       ,A01
       MOVE    A01,2(A02)               ;A(1)**2+A(2)**2+A(3)**2=...
       FMPR    A01,A01
       FADR       ,A01
       MOVEM      ,ACSAV+2
               ENTER (SQRT)
       JUMP       ,ACSAV+2
       MOVE    A01,ACSAV                ;RESTORE AC'S, EXIT
       MOVE    A02,ACSAV+1
        GOODBY
#include "reent1.inc"
ACSAV: BLOCK   3
       A01= 1
       A02= 2
       A16=16
       PRGEND
#ifdef CERNLIB_TCGENR_VMODUL
#undef CERNLIB_TCGENR_VMODUL
#endif
