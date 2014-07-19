*
* $Id: ucopy.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: ucopy.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE UCOPY   (A,B,N)
;#
; CERN PROGLIB# V301    UCOPY           .VERSION KERNPDP  1.00  750903
;#
;ACCUMULATORS:
                N=1
                B=2
                J=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( UCOPY)
        SKIPG   N,@2(16)        ;LOAD N, SKIP IF >0
        JRST    RETURN
        HRLZI   B,@(16)         ;FORM BLT POINER
        HRRI    B,@1(16)
        MOVEI   J,(B)           ;GET LAST WORD ADDRESS
        ADDI    J,-1(N)
        BLT     B,(J)           ;DO TRANSFER
RETURN: GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_UCOPY
#undef CERNLIB_TCGEN_UCOPY
#endif
