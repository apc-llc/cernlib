*
* $Id: ucocop.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: ucocop.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE   UCOCOP
;#
; CERN PROGLIB# V302    UCOCOP          .VERSION KERNPDP  1.00  750903
;#
#include "reentr.inc"
        SEARCH HELP
        HELLO (UCOCOP)
       SKIPG      ,@2(A16)
        GOODBY
       SKIPG      ,@3(A16)
        GOODBY
       HRRZI      ,ACSAVE               ;SAVE AC'S
       BLT        ,ACSAVE+4
       HRLI    A01,@0(A16)              ;BLT POINTER [A,B] IN AC 1
       HRRI    A01,@1(A16)
       HRL     A02,@4(A16)              ;INCREMENT OF BLT WORD
       HRR     A02,@5(A16)
       MOVE       ,@2(A16)              ;LOOP COUNT IDO
       MOVEI   A03,@1(A16)              ;FINAL BLT ADDRESS
       SOS        ,A03
       ADD     A03,@3(A16)
       MOVEM   A01,A04                  ;SAVE BLT POINTER IF INTERRUPT
LOOP:  BLT     A01,(A03)                ;CONTRACT VECTOR A INTO B
       ADD     A04,A02
       MOVEM   A04,A01
       ADD     A03,@5(A16)
       SOJG       ,LOOP
       HRLZI      ,ACSAVE               ;RESTORE AC'S, EXIT
       BLT        ,4
        GOODBY
       A01= 1
       A02= 2
       A03= 3
       A04= 4
       A16=16
#include "reent1.inc"
ACSAVE:BLOCK   5
       PRGEND
#ifdef CERNLIB_TCGEN_UCOCOP
#undef CERNLIB_TCGEN_UCOCOP
#endif
