*
* $Id: proxim.s,v 1.1.1.1 1996/02/15 17:53:43 mclareni Exp $
*
* $Log: proxim.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:43  mclareni
* Kernlib
*
*
        TITLE   PROXIM
;#
; CERN PROGLIB# B102    PROXIM          .VERSION KERNPDP  1.00  750903
;#
#include "reentr.inc"
        SEARCH HELP
;      WRITTEN  P. SCHMITZ
        HELLO (PROXIM)
       MOVEM   A01,AC01
       MOVEM   A02,AC02
       MOVEM   A03,AC03
       MOVE       ,@0(A16)
P10:   MOVEM      ,A01
       FSBR    A01,@1(A16)
       JUMPG   A01,P30
       JUMPE   A01,P99
       MOVE    A02,PI
       FADR    A02,A01
       JUMPGE  A02,P99
P21:   FADR       ,PI2
       JRST       ,P10
P30:   MOVE    A03,PI
       FSBR    A03,A01
       JUMPGE  A03,P99
P31:   FSBR       ,PI2
       JRST       ,P10
P99:   MOVE    A01,AC01
       MOVE    A02,AC02
       MOVE    A03,AC03
        GOODBY
PI:    EXP     ^D3.14159265
PI2:   EXP     ^D6.28318530
       A01= 1
       A02= 2
       A03= 3
       A16=16
#include "reent1.inc"
AC01:  0
AC02:  0
AC03:  0
      PRGEND
#ifdef CERNLIB_TCGEN_PROXIM
#undef CERNLIB_TCGEN_PROXIM
#endif
