*
* $Id: erf.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: erf.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE   ERF
;#
; CERN PROGLIB# C300    ERF             .VERSION KERNPDP  1.00  750903
; ORIG.  P. SCHMITZ, AACHEN, 3/9/1975
;#
#include "reentr.inc"
        SEARCH HELP
        HELLO (ERF)
      SETZM ,TF              ;ENTERF = .TRUE.
      MOVE 0,@(16)
      JRST BEIDE
        HELLO (FREQ)
      SETOM ,TF              ;ENTERF = .FALSE.
      MOVE 0,@(16)
      FMPR 0,KONST           ; Y=0.70710678*P
BEIDE:MOVEM 0,Y              ; Y=P
      MOVMM 0,X              ; X=ABS(Y)
      MOVEM 1,AC1            ; SICHERE ACCU 1 UND 2
      MOVEM 2,AC2
      MOVE 0,A               ; S=SIGN(A(1),Y)
      MOVE 1,Y
      CAIGE 1,0
      MOVN 0,0
      MOVEM 0,S
      MOVE 1,X               ; IF    (X.LE.1.E6)     GO TO      2
      CAMLE 1,MILION
      JRST P4                ; ERF=A(1)*S
   P2:FMPR 1,A+6             ; SUM=A(7)*X
      MOVEI 2,5              ; DO        3    I=1,5
   M3:FADR 1,A(2)            ; J=7-I
      FMPR 1,X               ; SUM=SUM+A(J)
      CAILE 2,1              ; SUM=SUM*X
      SOJA 2,M3
      FADR 1,A               ; SUM=SUM+A(1)
      MOVE 0,A
      FDVR 0,1               ; SUM=A(1)/SUM
      FMPR 0,0               ; SUM=SUM**16
      FMPR 0,0
      FMPR 0,0
      FMPR 0,0
      FSBR 0,A               ; ERF=A(1)-SUM
      MOVN 0,0
      FMPR 0,S               ; ERF=ERF*S
   P4:SKIPL ,TF              ;IF (ENTERF) RETURN
      JRST RETURN
      FADR 0,A               ; ERF=0.5*(1.+ERF)
      FMPR 0,HALB
RETURN:MOVE 1,AC1
      MOVE 2,AC2
        GOODBY
    A:DEC 1.
      DEC 0.070523078
      DEC 0.042282012
      DEC 0.0092705272
      DEC 0.0001520143
      DEC 0.0002765672
      DEC 0.0000430638
 HALB:DEC 0.5
MILION:DEC 1000000.
KONST:DEC 0.70710678
#include "reent1.inc"
    TF:0                     ;TRUE-FALSE SWITCH
AC1:    0
AC2:    0
    S:0
    X:0
    Y:0
        PRGEND
#ifdef CERNLIB_TCGEN_ERF
#undef CERNLIB_TCGEN_ERF
#endif
