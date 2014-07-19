*
* $Id: ublank.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: ublank.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE UBLANK   (IV,JL,JR)
;#
; CERN PROGLIB# V300    UBLANK          .VERSION KERNPDP  1.00  750903
;#
;ACCUMULATORS:
                JL=1
                JR=2
                B=3
                F=4
                T=5
        SEARCH HELP
#include "reentr.inc"
        HELLO( UBLANK)
        MOVE    T,[ASCII /     /]       ;GET 5 BLANKS
        HRRZI   B,@(16)         ;LOAD @ADR. OF IV
        MOVE    JL,@1(16)       ;LOAD JL
        MOVE    JR,@2(16)       ;LOAD JR
        CAIG    JR,(JL)         ;WORRY@ABOUT
        JRST    ONCE            ;ONE PASS?THROUGH
        ADDI    JR,-1(B)                ;ADR. OF IV(JR)
        ADDI    B,(JL)          ;ADR.?OF@IV(JL+1)
        HRLI    B,-1(B)         ; BLT POINTER
        MOVEM   T,-1(B)         ;BLANK?1ST WORD
        BLT     B,(JR)          ;BLT@BLANKS
        GOODBY
ONCE:   ADDI    B,-1(JL)        ;ADR.?OF?IV(JL)
        MOVEM   T,(B)           ;BLANK 1 WORD
        GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_UBLANK
#undef CERNLIB_TCGEN_UBLANK
#endif
