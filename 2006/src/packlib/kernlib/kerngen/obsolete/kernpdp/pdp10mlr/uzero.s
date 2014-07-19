*
* $Id: uzero.s,v 1.1.1.1 1996/02/15 17:53:45 mclareni Exp $
*
* $Log: uzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:45  mclareni
* Kernlib
*
*
        TITLE UZERO   (IV,JL,JR)
;#
; CERN PROGLIB# V300    UZERO           .VERSION KERNPDP  1.00  750903
;#
;ACCUMULATORS:
                JL=1
                JR=2
                B=3
                F=4
        SEARCH HELP
#include "reentr.inc"
        HELLO( UZERO)
        HRRZI   B  ,@(16)       ;LOAD ADDR OF IV
        MOVE    JL,@1(16)       ;LOAD JL, JR
        MOVE    JR,@2(16)
        CAIG    JR,(JL)         ;WORRY ABOUT
        JRST    ONCE            ;ONE PASS THROUGH
        ADDI    JR,-1(B)                ;ADR. OF IV(JR)
        ADDI    B,(JL)          ;ADR. OF IV(JL+1)
        HRLI    B,-1(B)         ;BLT POINTER
        SETZM   -1(B)           ;BLT ZEROS
        BLT     B,(JR)
        GOODBY
ONCE:   ADDI    B,-1(JL)                ;ADR. OF IV(JL)
        SETZM   (B)             ;ZERO ONE WORD
        GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_UZERO
#undef CERNLIB_TCGEN_UZERO
#endif
