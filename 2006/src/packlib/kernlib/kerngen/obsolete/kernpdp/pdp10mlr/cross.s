*
* $Id: cross.s,v 1.1.1.1 1996/02/15 17:53:41 mclareni Exp $
*
* $Log: cross.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:41  mclareni
* Kernlib
*
*
        TITLE   CROSS
;#
; CERN PROGLIB# F117    CROSS           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
#include "reentr.inc"
.COMMON SLATE [50]
        SEARCH HELP
        HELLO (CROSS)
       HRRZI      ,ACSAV                ;SAVE THE AC'S
       BLT        ,ACSAV+5
       MOVEI   A02,SLATE                ;FETCH ADDRESSES
       MOVEI   A03,@0(A16)
       MOVEI   A04,@1(A16)
       MOVEI   A05,@2(A16)
       MOVE       ,1(A03)               ;Z1=X(1)*Y(3)-X(3)*Y(2)
       FMPR       ,2(A04)
       MOVE    A01,2(A03)
       FMPR    A01,1(A04)
       FSBR       ,A01
       MOVEM      ,0(A02)
       MOVEM      ,0(A05)
       MOVE       ,2(A03)               ;Z2=X(3)*Y(1)-X(1)*Y(3)
       FMPR       ,0(A04)
       MOVE    A01,0(A03)
       FMPR    A01,2(A04)
       FSBR       ,A01
       MOVEM      ,1(A02)
       MOVEM      ,1(A05)
       MOVE       ,0(A03)               ;Z(3)=X(1)*Y(2)-X(2)*Y(1)
       FMPR       ,1(A04)
       MOVE    A01,1(A03)
       FMPR    A01,0(A04)
       FSBR       ,A01
       MOVEM      ,2(A05)
       HRLZI      ,ACSAV                ;RESTORE AC'S
       BLT        ,5
        GOODBY
#include "reent1.inc"
ACSAV: BLOCK   6
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_CROSS
#undef CERNLIB_TCGEN_CROSS
#endif
