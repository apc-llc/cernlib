*
* $Id: iuend.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: iuend.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE   IUEND
;#
; CERN PROGLIB# M410    IUEND           .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
#include "reentr.inc"
.COMMON SLATE [50]
        SEARCH HELP
        HELLO (IUEND)
       MOVEM   A01,ACSAVE
       MOVEI   A01,SLATE
       MOVE       ,(A01)                ;NDA = ND
       MOVEM      ,@0(A16)
       MOVE       ,1(A01)               ;IUEND = JEND
       MOVE    A01,ACSAVE
        GOODBY
#include "reent1.inc"
ACSAVE:0
       A01= 1
       A16=16
       PRGEND
#ifdef CERNLIB_TCGEN_IUEND
#undef CERNLIB_TCGEN_IUEND
#endif
