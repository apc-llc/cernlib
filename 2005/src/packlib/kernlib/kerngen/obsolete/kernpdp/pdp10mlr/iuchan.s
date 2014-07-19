*
* $Id: iuchan.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: iuchan.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE   IUCHAN
;#
; CERN PROGLIB# Y201    IUCHAN          .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
#include "reentr.inc"
        SEARCH HELP
       EXTERN  IFIX
        HELLO (IUCHAN)
       MOVE       ,@0(A16)              ;AN=(X-XL)/DX+1.
       FSBR       ,@1(A16)
       FDVR       ,@2(A16)
       FADR       ,[EXP ^D1.0]
       SKIPGE                           ;IF (AN.LT.0.) AN=0.
       SETZM
       MOVEM      ,ACSAVE
               ENTER (IFIX)
       JUMP       ,ACSAVE
       CAMG       ,@3(A16)              ;IF (N.GT.NX) NX=NX+1
        GOODBY
       MOVE       ,@3(A16)
       AOS
        GOODBY
#include "reent1.inc"
ACSAVE:0
       A16=16
        PRGEND
#ifdef CERNLIB_TCGEN_IUCHAN
#undef CERNLIB_TCGEN_IUCHAN
#endif
