*
* $Id: iubin.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: iubin.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE   IUBIN
;#
; CERN PROGLIB# Y202    IUBIN           .VERSION KERNPDP  1.00  750903
;#
#include "reentr.inc"
        SEARCH HELP
;      WRITTEN      P. SCHMITZ
       EXTERN  FLOAT,IFIX
        HELLO (IUBIN)
       MOVEM   A03,AC03
       SETZM      ,A03
       JRST       ,GO
        HELLO (IUHIST)
       MOVEM   A03,AC03
       MOVEI   A03,1
       JRST       ,GO
GO:    MOVEM   A01,AC01
       MOVEM   A02,AC02
       SETZM      ,@2(A16)
       MOVEI   A01,@1(A16)
       MOVE    A02,@0(A16)
       FSBR    A02,2(A01)
       XCT        ,COMAND(A03)
       FADR    A02,CONST
       CAMGE   A02,CONST
       JRST       ,P20
       MOVEM   A03,TEMP2
       MOVE       ,@1(A16)
       AOS
       MOVEM      ,TEMP
       MOVEM   A02,TEMP1
               ENTER (FLOAT)
       JUMP      0,TEMP
       MOVE    A02,TEMP1
       CAML    A02,
       JRST       ,P18
               ENTER (IFIX)
       JUMP      2,TEMP1
       MOVE    A03,TEMP2
       SKIPL
       JRST       ,EXIT
       MOVE       ,@1(A16)
       AOS
       XCT        ,COMAND+2(A03)
       JRST       ,EXIT
P18:   SETOM      ,@2(A16)
       MOVE       ,@1(A16)
       AOS
       JRST       ,EXIT
P20:   SETOM      ,@2(A16)
       SETZM
       JRST       ,EXIT
EXIT:  MOVE    A01,AC01
       MOVE    A02,AC02
       MOVE    A03,AC03
        GOODBY
COMAND:FDVR    A02,1(A01)
       FMPR    A02,1(A01)
       SETZM      ,@2(A16)
       SETOM      ,@2(A16)
CONST: ^D1.
#include "reent1.inc"
AC01:  0
AC02:  0
AC03:  0
TEMP:  0
TEMP1: 0
TEMP2: 0
       A01= 1
       A02= 2
       A03= 3
       A16=16
        PRGEND
#ifdef CERNLIB_TCGEN_IUBIN
#undef CERNLIB_TCGEN_IUBIN
#endif
#ifdef CERNLIB_TCGEN_IUHIST
#undef CERNLIB_TCGEN_IUHIST
#endif
