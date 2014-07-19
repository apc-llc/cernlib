*
* $Id: trsmlu.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trsmlu.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRSMLU
*
* CERN PROGLIB# F112    TRSMLU          .VERSION KERNCDC  0.1   761001
*
          ENTRY TRSMLU
*     SUBROUTINE TRSMLU (A,S,M)
*     S:=A.AT,  A:  LOWER TRIANGULAR MXM,  S:SYMMETRIC MXM
*         WRITTEN BY   ST. MEEN   20-JULY-76
          VFD   36/6HTRSMLU,24/TRSMLU
 TRSMLU   BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA3   X3
*
          DX5   X3*X3
          IX5   X5+X3
          AX5   1
          SB5   X5-1           B5= M*(M+1)/2-1
*
          SB1   X3             B1= M
*                              NEWROW-LOOP: B1=M,M-1,...,1  B6=...,5,2,0
 NEWROW   SB6   B5
*
          SB2   B1
*                              NEWCOL-LOOP: B2=B1,B1-1,...,1
 NEWCOL   SX4   X1+B5
          SX5   X1+B6
          MX6   0
*
*                              SUMUP-LOOP: B3=B2,B2-1,...,1
*                              MULTIPLIES ROW B1 BY ROW B2, RESULT IN X6
          SB3   B2
 SUMUP    SA4   X4
          SA5   X5
          RX5   X5*X4
          RX6   X6+X5
          NX6   B0,X6
*
          SX4   A4-B7
          SX5   A5-B7
          SB3   B3-B7
          LT    B0,B3,SUMUP
*
          SA6   X2+B5              S(B5)=X6
          SB5   B5-B7
          SB6   B6-B2
          SB2   B2-B7
          LT    B0,B2,NEWCOL
*
          SB1   B1-B7
          LT    B0,B1,NEWROW
          JP    TRSMLU
          END
#ifdef CERNLIB_TCGEN_TRSMLU
#undef CERNLIB_TCGEN_TRSMLU
#endif
