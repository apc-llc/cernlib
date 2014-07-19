*
* $Id: iucomp.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: iucomp.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE         IUCOMP   (IT,IVEC,N)
;#
; CERN PROGLIB# V304    IUCOMP          .VERSION KERNPDP  1.00  750903
;#
        ;ACCUMULATORS:
                        FUNC=0
                        IT=1
                        N=2     ;TO HOLD N
                        COUNT=3
#include "reentr.inc"
        SEARCH HELP
        HELLO( IUCOMP)
        MOVEM   N,SAV1
        MOVEM   COUNT,SAV2
        SETZ    FUNC,           ;CLEAR FUNC IN CASE IUCOMPFAILS
        MOVE    IT,@(16)                ;LOAD IT
        MOVE    N,@2(16)        ;LOAD N
        MOVN    COUNT,N         ;
        HRLZI   COUNT,(COUNT)   ;SET UP COUNTER
        HRRI    COUNT,@1(16)    ;LOAD ADR,OF IVEC(1)
        CAME    IT,(COUNT)      ;COMPARE,SKIP IF SATISFIED
        AOBJN   COUNT,.-1       ;LOOP BACK
        HRLI    COUNT,0
        SUBI    COUNT,@1(16)
        ADDI    COUNT,1
        CAILE   COUNT,(N)
        JRST    EXIT
        MOVEI   FUNC,(COUNT)
EXIT:   MOVE    N,SAV1
        MOVE    COUNT,SAV2
        GOODBY
#include "reent1.inc"
SAV1:   0
SAV2:   0
        PRGEND
#ifdef CERNLIB_TCGEN_IUCOMP
#undef CERNLIB_TCGEN_IUCOMP
#endif
