*
* $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: ucopy2.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE UCOPY2 (A,B,N)
;#
; CERN PROGLIB# V301    UCOPY2          .VERSION KERNPDP  1.00  750903
;#
        B=1
        N=2
        J=3
        A=4
        TEMP=5
        ;UCOPY ALLOWING OVERLAPS
        SEARCH HELP
#include "reentr.inc"
        HELLO( UCOPY2)
        SKIPG   N,@2(16)        ;LOAD N,RETURN FOR SMALLER 0
        JRST    0,RETURN
        HRRZI   A,@(16)         ;GET ADDR OF A
        HRRZI   B,@1(16)        ;GET ADDR OF B
        CAIL    B,(A)           ;SKIP FOR NO OVERLAP
        JRST    0,BACKW
        HRL     B,A             ;PREPARE POINTER
GO:     MOVEI   J,(B)           ;GET END
        ADDI    J,-1(N)         ;ADDRESS
        BLT     B,(J)           ;TRANSFER
RETURN: GOODBY
BACKW:  CAIN    B,(A)           ;CHECK ADDR OF A= ADDR OF B
        JRST    0,RETURN
        ADDI    B,-1(N)         ;PREPARE
        ADDI    A,-1(N)         ;POINTERS
LOOP:   MOVE    TEMP,(A)        ;AND
        MOVEM   TEMP,(B)        ;DO
        SOJ     A,              ;BACKWARDS
        SOJ     B,              ;LOOP
        SOJN    N,LOOP
        GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_UCOPY2
#undef CERNLIB_TCGEN_UCOPY2
#endif
