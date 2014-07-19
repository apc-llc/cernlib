*
* $Id: rot.s,v 1.1.1.1 1996/02/15 17:53:43 mclareni Exp $
*
* $Log: rot.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:43  mclareni
* Kernlib
*
*
        TITLE   ROT
;#
; CERN PROGLIB# F118    ROT             .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
#include "reentr.inc"
        SEARCH HELP
       EXTERN  SIN,COS
.COMMON SLATE [50]
        HELLO (ROT)
       HRRZI      ,ACSAV                ;SAVE AC'S
       BLT        ,ACSAV+7
       MOVE       ,@1(A16)
       MOVEM      ,TEMP
               ENTER (COS)
       JUMP       ,TEMP
       MOVEI   A02,SLATE
       MOVEM      ,2(A02)
               ENTER (SIN)
       JUMP       ,TEMP
       MOVEM      ,3(A02)
       MOVEI   A03,@0(A16)              ;ADR(A)
       MOVEI   A04,@2(A16)              ;ADR(B)
       MOVE    A06,0(A03)               ;B1=A(1)*COSTH-A(2)*SINTH
       FMPR    A06,2(A02)
       MOVE       ,1(A03)
       FMPR       ,3(A02)
       FSBR    A06,
       MOVE    A07,0(A03)               ;B2=A(1)*SINTH+A(2)*COSTH
       FMPR    A07,3(A02)
       MOVE       ,1(A03)
       FMPR       ,2(A02)
       FADR    A07,
       MOVEM   A06,0(A02)               ;B(3)=A(3)
       MOVEM   A07,1(A02)
       MOVEM   A06,0(A04)               ;B(2)=B2
       MOVEM   A07,1(A04)
       MOVE       ,2(A03)               ;B(1)=B1
       MOVEM      ,2(A04)
       HRLZI      ,ACSAV                ;RESTORE AC'S, EXIT
       BLT        ,7
        GOODBY
#include "reent1.inc"
ACSAV: BLOCK   10
TEMP:  0
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A05= 5
       A06= 6
       A07= 7
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_ROT
#undef CERNLIB_TCGEN_ROT
#endif
