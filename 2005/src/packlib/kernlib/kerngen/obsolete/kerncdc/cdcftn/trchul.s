*
* $Id: trchul.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trchul.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRCHUL
*
* CERN PROGLIB# F112    TRCHUL          .VERSION KERNCDC  0.1   761001
*
          ENTRY TRCHUL
          EXT   SQRT.
*     SUBROUTINE TRCHUL(S,A,M)
*     AT.A=S,  A:TRIANGULARMXM,  S:  SYMMETRIC MXM
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
*               X0:  ACCUMULATOR IN SUMING-LOOP
*               X1:  ADR(S)
*               X2:  ADR(A)
*               X3:   DIAGONAL-ELEMENT NO B1 OF S,
*                     OR: 1/A(B3) (A(B3): DIAGONAL-ELEMENT )
*
*               B1:  INDEX OF LOOP1
*               B2:  INDEX OF SUMUP-LOOP
*               B3:  A(B3), S(B3) IS DIAGONAL-ELEMENTS
*               B4:  FIRST SIGNIFIKANT ELEMENT OF ACOLUMN WE WANT TO
*                   MULTIPLY WITH COLUMN B1
*               B5:  TEST REGISTER FOR LOOP2
*               B6:  M
*               B7:  1
*
          VFD   36/6HTRCHUL,24/TRCHUL
 TRCHUL   BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA3   X3
          SB6   X3             B6= M
*
          DX5   X3*X3
          IX5   X5+X3
          AX5   1
          SB4   X5-1           B4= M*(M+1)/2-1
*
*                              LOOP1:  B1=M,M-1,...,1
          SB1   B6
 LOOP1    SB3   B4                 S,A(B3) IS THE NEXT DIAG.ELEM.
          SA3   X1+B3              PICK UP DIAGONAL-ELEMENT NO B1 OF S
          SB5   B3-B1
*
*                              LOOP2: B4=B3,B3-1,...,B3-B1+1
 LOOP2    MX0   0
          EQ    B1,B6,MM           FIRST TIME IN LOOP2 : NO SUMMING, X0=
          ZR    X3,ZERO            HAS FOUND A ZERO-DIAG-ELE: SEE POINT
*
          SX4   B3+B1
          SX5   B4+B1
          IX4   X4+X2          X4= X2+B3+B1
          IX5   X5+X2          X5= X2+B4+B1
*                              SUMUP-LOOP:B2=B1,B1+1,...,M
*                              MULTIPLIES COL NO B1 WITH COL GIVEN BY B4
*                              RESULT IN X0
          SB2   B1
 SUMUP    SB2   B2+B7
          SA4   X4
          SA5   X5
          RX5   X4*X5
          RX0   X0+X5
          NX0   B0,X0
*
          SX4   A4+B2
          SX5   A5+B2
          LT    B2,B6,SUMUP
*
 MM       SA4   B4+X1
          RX0   X4-X0
          NX0   B0,X0          X0= S(B4)-X0
*
 ZERO     EQ    B4,B3,ROOT         IF S(B4) IS DIAG-ELEMENT A(B4)=SQRT(X
*                              AND X3=1/SQRT(X0)
*                              (THIS  HAPPENS EVERY FIRST RUN IN LOOP2)
*                              ELSE A(B4)=X0/SQRT(-X0-)=X0*X3
          RX6   X0*X3
          SA6   X2+B4
*
 RETROOT  SB4   B4-B7          B4= B4-1
          LT    B5,B4,LOOP2
*
          SB1   B1-B7          B1= B1-1
          LT    B0,B1,LOOP1
*
          JP    TRCHUL             FINISH
*
 ROOT     BX6   X1
*                              B3=B4
*                              FIRST STORE THE REGISTERS
          SA6   STORE
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
-         VFD   30/TRCHUL
*
*                              LOAD REGISTERS
          SB7   1
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
          SB5   B3-B1
          SA4   A4+B7
          SB6   X4
*
          SA6   X2+B4              A(B4)=X6=SQRT(X0)
          NG    X3,Z
          ZR    X3,Z
*                           IF DIAG.ELEM. S(B4) GT 0 THEN X3=1/SQRT(X0)
          SA5   =1.0
          RX3   X5/X6
 Z        JP    RETROOT
 STORE    BSS   7
          END
#ifdef CERNLIB_TCGEN_TRCHUL
#undef CERNLIB_TCGEN_TRCHUL
#endif
