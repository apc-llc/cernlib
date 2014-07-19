*
* $Id: trasat.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trasat.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRASAT
*
* CERN PROGLIB# F112    TRASAT          .VERSION KERNCDC  1.02  771015
*
          ENTRY TRASAT
*     SUBROUTINE TRASAT(A,S,R,M,N)
*     R:= A.S.AT ,    A:MXN,  S: SYMETRIC NXN,  R: SYMMETRIC MXM.
*         WRITTEN BY   ST. MEEN   12-JULY-76
          VFD   36/6HTRASAT,24/TRASAT
 TRASAT   BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA5   A4+B7
*
          SA4   X4
          SA5   X5
          SB4   X4             B4= M
          SB5   X5             B5= N
*
*         SETS ALL R-ELEMENTS=0
          DX6   X4*X4
          IX6   X6+X4
          AX6   1
          SB1   X6
          MX6   0
 R        SB1   B1-B7
          SA6   X3+B1
          NE    B1,B0,R
*
*               B1:  COUNTS ROWS  IN R AND A
*               B2:  COUNTS COLUMNS IN S
*               B3:  SUMMING INDEX, COUNT COLUMNS IN R
*               B4:  M
*               B5:  N
*               B7:  1
*               X1:  INDEX OF A
*               X2:  INDEX OF S
*               X3:  INDEX OF R
*               A1:  ADR(A)
*               A2:  ADR(S)
*
          SA2   X2
          SA1   X1
          SX1   A1
          SB1   B0
*
*                              NEWROW-LOOP: B1,...,M
 NEWROW   SB1   B1+B7
          SX2   A2
          SB2   B0
*
*                              NEWSUM-LOOP: B2=0,...,N-1
 NEWSUM   SX4   X1
          SX5   X2
          MX0   0
          SB3   B0
*
*                              SUMUP-LOOP:B3=1,2,...,N
*                              MULTIPLIES ROW B1 OF A B! COL B2 OF S
 SUMUP    SB3   B3+B7
          SA4   X4
          SA5   X5
          RX5   X4*X5
          RX0   X0+X5
          NX0   B0,X0
          SX4   A4+B7
          SX5   A5+B7
          GE    B2,B3,SUMUP
          SX5   A5+B3
          LT    B3,B5,SUMUP
*
          SX4   X3
          SB3   B0
          SX5   A1+B2
*                              ADDSUM-LOOP B3= 0,...,B1-1
*                              MULTIPLIES COL B2 OF A B! X0 AND
*                              ADD RESULT TO ROW B1 OF R
 ADDSUM   SA5   X5
          RX6   X0*X5
          SA4   X4
          RX6   X6+X4
          NX6   B0,X6
          SA6   A4
*
          SX4   A4+B7
          SX5   A5+B5
          SB3   B3+B7
          LT    B3,B1,ADDSUM
*
          SB2   B2+B7
          SX2   X2+B2
          LT    B2,B5,NEWSUM
*
          SX1   X1+B5
          SX3   X3+B1
          LT    B1,B4,NEWROW
          JP    TRASAT
          END
#ifdef CERNLIB_TCGEN_TRASAT
#undef CERNLIB_TCGEN_TRASAT
#endif
