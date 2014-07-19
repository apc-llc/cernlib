*
* $Id: trchlu.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trchlu.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRCHLU
*
* CERN PROGLIB# F112    TRCHLU          .VERSION KERNCDC  0.1   761001
*
          ENTRY TRCHLU
          EXT   SQRT.
*     SUBROUTINE TRCHLU(S,A,M)
*     A.AT=S,  A:  TRIANGULAR MXM,  S:  SYMMETRIC MXM
*         WRITTEN BY   ST. MEEN   23-JULY-76
*
*     1.  S IS POSITIV-DEFINIT :OK
*     2.  S IS NOT POSITIV-SEMI-DEFINIT  : ABORT
*     3.  IF ONE DIAGONALELEMENT OF S IS ZERO , RESULT OF  CORRESPONDING
*         ROW AND COLUMN IS ZERO
*     4.  IF ONE DIAGONAL-ELEMENT OF S IS NEGATIV :  NOT OK
*     5.  S IS POSITIV-SEMI-DEFINIT AND HAS NO CORRESPONDING ZERO-
*           ELEMENT IN THE DIAGONAL : MAY ABORT
*
*               B1:  INDEX OF LOOP1
*               B2:  INDEX OF LOOP2
*               B3:  A,S(B3) IS DIAGONAL-ELEMENTS
*               B4:  POINTS TO THE ELEMENTS OF COLUMN B1
*               B5:  INDEX OF SUMUP-LOOP
*               B6:  M
*               B7:  1
*               X0:  ACCUMULATOR IN SUMUP-LOOP
*               X1:  ADR(S)
*               X2:  ADR(A)
*               X3:  S(B3) DIAG-ELEM OF S, 1/A(B3) (A(B3) DIAG-ELEM)
*
          VFD   36/6HTRCHLU,24/TRCHLU
 TRCHLU   BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA3   X3
          SB6   X3             B6= M
*
*                              LOOP1:  B1=1,2,...,M  B3=0,2,5,9,....
          SB3   -B7
          SB1   B0
 LOOP1    SB1   B1+B7          B1= B1+1
          SB3   B3+B1
          SB4   B3
          SA3   X1+B3          X3= DIAG-ELEM NO B1 OF S
*
*                              LOOP2: B2=B1,B1+1,...,M
          SB2   B1
 LOOP2    MX0   0
          EQ    B1,B7,ONE          FIRST TIME IN LOOP2:  NO SUMMING
          ZR    X3,ZERO            HAS FOUND A ZERO-DIAG-ELEM: SEE POINT
          SB5   B3-B1
          SB5   B5+B7          B5= B3-B1+1
          SX5   B4-B1
          SX5   X5+B7
          IX5   X5+X2          X5= B4-B1+1+X2
*
*                              SUMUP-LOOP: B5: B3-B1+1,...,B3-1
*                              MULTIPLIES ROW B1 WITH ROW B2 OF A
*                              RESULT IN X0
 SUMUP    SA4   B5+X2
          SA5   X5
          RX5   X4*X5
          RX0   X0+X5
          NX0   B0,X0
          SB5   B5+B7
          SX5   A5+B7
          LT    B5,B3,SUMUP
*
 ONE      SA4   B4+X1
          RX0   X4-X0
          NX0   B0,X0          X0= S(B4)-X0
 ZERO     EQ    B1,B2,ROOT         EVERY FIRST TIME IN LOOP2 DIAG-ELEM A
          RX6   X0*X3              = SQRT(X0) AND X3=1/A(B4)
          SA6   X2+B4              ELSE A(B4)= X0/A(B3) =X0*X3
 RETROOT  SB4   B4+B2
          SB2   B2+B7
          GE    B6,B2,LOOP2
*
          LT    B1,B6,LOOP1
*
          JP    TRCHLU
*
 ROOT     BX6   X1             B3= B4
          SA6   STORE              FIRST STORE ALL RELEVAT REGISTERS
          BX6   X2
          SA6   A6+B7
          BX6   X3
          SA6   A6+B7
          SX6   B1
          SA6   A6+B7
          SX6   B2
          SA6   A6+B7
          SX6   B3
          SA6   A6+B7
          SX6   B6
          SA6   A6+B7
*
          BX1   X0
+RJSQRT
-         VFD   30/TRCHLU
*
          SB7   1                  LOAD  REGISTERS
          SA1   STORE
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SB1   X4
          SA4   A4+B7
          SB2   X4
          SA4   A4+B7
          SB3   X4
          SB4   B3
          SA4   A4+B7
          SB6   X4
*
          SA6   X2+B4              A(B4)= X6= SQRT(X0)
          NG    X3,Z
          ZR    X3,Z               IF X3 .GT. 0 X3= 1/SQRT(X0)
          SA5   =1.0
          RX3   X5/X6
 Z        JP    RETROOT
 STORE    BSS   7
          END
#ifdef CERNLIB_TCGEN_TRCHLU
#undef CERNLIB_TCGEN_TRCHLU
#endif
