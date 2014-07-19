*
* $Id: mxequ.s,v 1.1.1.1 1996/02/15 17:53:43 mclareni Exp $
*
* $Log: mxequ.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:43  mclareni
* Kernlib
*
*
        TITLE   MXEQU
;#
; CERN PROGLIB# F109    MXEQU           .VERSION KERNPDP  1.00  750903
;#
#include "reentr.inc"
;      WRITTEN  P. SCHMITZ
        SEARCH HELP
        HELLO (MXEQU)
      RADIX 10
      SETZM 0,SWITCH
 SAVE:MOVEM 13,AC15
      MOVEM 14,AC16
      MOVEM 15,AC17
      MOVEI 15,@(14)         ; HOLE DIE ADRESSEN UND ZAEHLER IN DIE ACCU
      SUBI 15,1              ; LOC(A) -1
      MOVE 13,@2(14)         ; I
      MOVE 12,@3(14)         ; J
      MOVEI 14,@1(14)        ; LOC(B)-1
      SUBI 14,1
      JUMPLE 13,RETURN       ; IF  (I.LE.0)     RETURN
      MOVE 0,SWITCH
      JUMPN 0,P20            ; *****************************************
      MOVEI 11,1
  M15:MOVE 9,15              ; DO       15       L=1,I
      ADD 9,11               ; LOC(A)-1+L
      MOVE 1,11
      SUBI 1,1
      IMUL 1,13
      ADD 1,9
      MOVSI 10,^O576400
      FDVR 10,(1)            ; DIAG=-1./A(L,L)
      MOVEI 8,1              ; DO    12       M=1,I
  M12:CAMN 8,11              ; IF    (L.EQ.M)     GO TO      12
      JRST P12
      MOVE 1,8
      SUBI 1,1
      IMUL 1,13
      ADD 1,9
      FMPRM 10,(1)           ; A(L,M)=-A(L,M)/DIAG
  P12:CAMGE 8,13             ; 12 CONTINUE
      AOJA 8,M12
      MOVE 6,11
      ADDI 6,1               ; LPLUS1=L+1
      CAMLE 6,13             ; IF      (LPLUS1.GT.I)     GO TO     20
      JRST P20
    A:MOVEI 8,1              ; DO         15      M=1,I
    B:CAMN 8,11              ; IF    (L.EQ.M)     GO TO      15
      JRST P15
      MOVE 2,8
      SUBI 2,1
      IMUL 2,13
      MOVE 1,2
      ADD 1,9
      MOVE 0,(1)
      MOVE 1,11
      SUBI 1,1
      IMUL 1,13
      ADD 1,6
      ADD 1,15
      FMPR 0,(1)
      ADD 2,6
      ADD 2,15
      FADRM 0,(2)            ; A(L1,M)=A(L1,M)+A(L1,L)*A(L,M)
  P15:CAMGE 8,13             ; 15 CONTINUE
      AOJA 8,B
      CAMGE 6,13
      AOJA 6,A
      CAMGE 11,13
      AOJA 11,M15
  P20:JUMPLE 12,RETURN       ; IF  (J.LE.0)     RETURN
      MOVEI 11,1             ; DO     25         L=1,I
    C:MOVEI 10,1             ; DO     25         M=1,J
    D:MOVEI 9,1              ; DO     25         N=1,I
    E:CAMN 9,11              ; IF     (L.EQ.N)    GO TO      25
      JRST P25
      MOVE 1,9
      SUBI 1,1
      IMUL 1,13
      ADD 1,11
      ADD 1,15
      MOVE 0,(1)
      MOVE 1,11
      SUBI 1,1
      IMUL 1,12
      ADD 1,10
      ADD 1,14
      FMPR 0,(1)
      MOVE 1,9
      SUBI 1,1
      IMUL 1,12
      ADD 1,10
      ADD 1,14
      FADRM 0,(1)            ; B(M,N)=B(M,N)+B(M,L)*A(L,N)
  P25:CAMGE 9,13             ; 25 CONTINUE
      AOJA 9,E
      CAMGE 10,12
      AOJA 10,D
      CAMGE 11,13
      AOJA 11,C
      MOVEI 10,1             ; DO         35      M=1,J
    F:MOVEI 9,1              ; DO         35      N=1,I
    G:MOVE 1,9
      SUBI 1,1
      IMUL 1,13
      ADD 1,9
      ADD 1,15
      MOVE 2,9
      SUBI 2,1
      IMUL 2,12
      ADD 2,10
      ADD 2,14
      MOVE 0,(2)
      FDVR 0,(1)             ; B(M,N)=B(M,N)/A(N,N)
      MOVEM 0,(2)                                                       ;M2
  P35:CAMGE 9,13             ; 35 CONTINUE
      AOJA 9,G
      CAMGE 10,12
      AOJA 10,F
RETURN:MOVE 13,AC15
       MOVE 14,AC16
       MOVE 15,AC17
       RADIX 8
        GOODBY
        HELLO (MXEQU1)
      SETOM 0,SWITCH
      JRST SAVE
#include "reent1.inc"
AC15: 0
AC16: 0
AC17: 0
SWITCH:0
        PRGEND
#ifdef CERNLIB_TCGEN_MXEQU
#undef CERNLIB_TCGEN_MXEQU
#endif
