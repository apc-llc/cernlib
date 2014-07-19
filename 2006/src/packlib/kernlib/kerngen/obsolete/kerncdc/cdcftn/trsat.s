*
* $Id: trsat.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trsat.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRSAT
*
* CERN PROGLIB# F112    TRSAT           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRSAT
*     SUBROUTINE TRSAT(S,A,B,M,N)
*         WRITTEN BY   ST.MEEN   5-JULY-76
          VFD   30/5HTRSAT,30/TRSAT
 TRSAT    BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SA5   A4-B7
          SA4   X4
          SA5   X5
*
          DX7   X4*X5          X7= M*N
          DX6   X4*X4
          IX6   X6+X4
          AX6   1
          SB6   X6             B6= (M*M+M)/2
                                   Y
          SX0   X4             X0= M
          SB4   X0             B4= M
          SB3   X7+B7          B3= M*N-1
*
*                              NEWROW-LOOP: B4=M-1,...,0
 NEWROW   SB4   B4+B7
          SB2   X7+B7
          SB6   B6+B7
*
*                              NEWCOL-LOOP: B2=M*N-1,M*N-2,...,M*N-N
 NEWCOL   SB1   B6
          SB5   X0+B7
          MX6   0
*
*                              SUMUP-LOOP: B5=M-1,...,0
*                              MULTIPLIES ROW B4 OF S BY ROW M*N-B2 OF A
 SUMUP    SA4   X1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+S(B1+1)*A(B2+1)
*
          SB2   B2+B7
          SB1   B1-B5
          SB5   B5+B7
          GE    B5,B4,SUMUP
          SB1   B1+B5
          GE    B5,B0,SUMUP
*
          SA6   X3+B3
          SB3   B3+B7
          GE    B2,B0,NEWCOL
          NE    B4,B0,NEWROW
          JP    TRSAT
          END
#ifdef CERNLIB_TCGEN_TRSAT
#undef CERNLIB_TCGEN_TRSAT
#endif
