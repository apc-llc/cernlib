*
* $Id: trqsq.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trqsq.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRQSQ
*
* CERN PROGLIB# F112    TRQSQ           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRQSQ
*     SUBROUTINE TRQSQ(Q,S,R,M)
*     R:= Q.S.Q ,   Q,S,R: SYMETRIC MXM.
*         WRITTEN BY   ST. MEEN   19-JULY-76
          VFD   30/5HTRQSQ,30/TRQSQ
 TRQSQ    BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
*
          SA4   X4
          SB4   X4             B4= M
*
*         SETS ALL R-ELEMENTS=0
          DX6   X4*X4
          IX6   X6+X4
          AX6   1
          SB1   X6             B1= M*(M+1)/2
          MX6   0
 R        SB1   B1-B7
          SA6   X3+B1
          NE    B1,B0,R
*
*               B1:  COUNT ROWS IN R AND Q
*               B2:  COUNTS COLUMNS IN S AND Q
*               B3:  SUMMING INDEX, COUNT COLUMNS IN R
*               B4:  M
*               B5:  S(B5+X1), R(B5+X3) IS FIRST ELEMENT IN ROW B1
*               B6:  S(B6+X2), Q(B6+X1) FIRST ELEMENT IN COLUMN B2
*               B7:  1
*               X1:  ADR(Q)
*               X2:  ADR(S)
*               X3:  ADR(R)
*
          SB5   B0
          SB1   B0
*
*                              NEWROW-LOOP: B1 =1,...,M
 NEWROW   SB1   B1+B7
          SB6   B0
          SB2   B0
*                              NEWSUM-LOOP: B2=0,...,M-1
*
 NEWSUM   SX4   B5+X1
          SX5   B6+X2
*
          MX0   0
          SB3   B0
*                              SUMUP-LOOP: B3=1,...,M
*                              MULTIPLIES ROW B1 OF Q BY ROW B2 OF S
*                              RESULT IN X0
 SUMUP    SB3   B3+B7
          SA4   X4
          SA5   X5
          RX5   X4*X5
          RX0   X0+X5
          NX0   B0,X0
          SX4   A4+B7
          LT    B3,B1,L1
          SX4   A4+B3
 L1       SX5   A5+B7
          GE    B2,B3,SUMUP
          SX5   A5+B3
          LT    B3,B4,SUMUP
*
          SX4   B5+X3
          SX5   B6+X1
          SB3   B0
*                              ADDSUM-LOOP : B3=1,...,B1-1
*                              MULTIPLIES ROW B2 OF Q BY X0 AND
*                              ADD IT TO ROW B1 OF R
 ADDSUM   SA5   X5
          SA4   X4
          RX6   X0*X5
          RX6   X6+X4
          NX6   B0,X6
          SA6   A4                 R(A4)=R(A4)+X0*Q(A5)
*
          SX4   A4+B7
          SB3   B3+B7
          SX5   A5+B7
          GE    B2,B3,L2
          SX5   A5+B3
 L2       LT    B3,B1,ADDSUM
*
          SB2   B2+B7
          SB6   B6+B2
          LT    B2,B4,NEWSUM
          SB5   B5+B1
          LT    B1,B4,NEWROW
          JP    TRQSQ
          END
#ifdef CERNLIB_TCGEN_TRQSQ
#undef CERNLIB_TCGEN_TRQSQ
#endif
