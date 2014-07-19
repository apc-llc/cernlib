*
* $Id: traat.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: traat.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT TRAAT
*
* CERN PROGLIB# F112    TRAAT           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRAAT
*     SUBROUTINE TRAAT(A,S,M,N)
*     S=A*AT       S: SYMETRIC MXM,  A: MXN
*         WRITTEN BY   ST. MEEN   6-JULY-76-
          VFD   30/5HTRAAT,30/TRAAT
 TRAAT    BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA3   X3
          SA4   X4
          SX7   B7             X7= 1
          SB6   X3             B6= M
          SB7   X4             B7= N
          SB3   B0
          SB4   B0
          SX0   B0
*               B1: INDEX OF A
*               B2:  INDEX OF AT
*               B3:  INDEX OF S
*               B4:  COUNTS ROWS
*               B5:  SUMMING INDEX
*               B6:  M
*               B7:  N
*               X0:   A(X0) IS THE FIRST )LEMENT IN ROW B4
*               X3:  COUNTS COLUMNS
*               X7:  1
*
*                              NEWROW-LOOP: B4=0,1,...,M-1
 NEWROW   SB2   B0
          SX3   B4
*                              NEWCOL-LOOP: X3=B4,B4-1,...,0
 NEWCOL   SB1   X0
          IX3   X3-X7
          SB5   B0
          MX6   0
*
*                              SUMUP-LOOP: B5=0,...,M-1
*                              MULTIPLIES ROW B4  BY ROW X3
 SUMUP    SA4   X1+B1
          SA5   X1+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+S(B1+1)*A(B2+1)
*
          SB1   B1+X7
          SB2   B2+X7
          SB5   B5+X7
          LT    B5,B7,SUMUP
          SA6   X2+B3              S(B3)=X6
          SB3   B3+X7
          PL    X3,NEWCOL
          SB4   B4+X7
          SX0   X0+B7          X0= X0+N
          LT    B4,B6,NEWROW
          JP    TRAAT
          END
#ifdef CERNLIB_TCGEN_TRAAT
#undef CERNLIB_TCGEN_TRAAT
#endif
