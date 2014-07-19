*
* $Id: trats.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trats.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRATS
*
* CERN PROGLIB# F112    TRATS           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRATS
*     SUBROUTINE TRATS(A,S,B,M,N)
*         WRITTEN BY   ST. MEEN   6-JULY-76
          VFD   30/5HTRATS,30/TRATS
 TRATS    BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SA5   A4-B7
          SA1   X1
          SA4   X4
          SA5   X5
*
          BX1   -X4            X1= -N
          SB6   X5             B6= N
          DX7   X5*X5
          IX7   X7+X5
          AX7   1              X7= N*(N+1)/2
          DX0   X5*X4
          SX0   X0+B7          X0= M*N-1
          SB4   X5             B4= N
          SB3   B0
*
*                              NEWCOL-LOOP: B4=N-1,...,0
*                              B3=M*N-1,M*N-2,...,M*N-N
 NEWCOL   SB4   B4+B7
          SB3   B3+X0          B3= B3+M*N-1
          SB1   B0
          SX7   X7+B7
*
*                              NEWROW-LOOP: WHILE B3 GE0
*                              B3 DECREASES BY N EVERY RUN M TIMES
 NEWROW   SB1   B1+X0
          SB5   B6+B7          B5= N-1
          SB2   X7
          MX6   0
*
*                              SUMUP-LOOP: B5=N-1,...,0
*                              MULTIPLIES ROW M*N-B3 OF A BY ROW B4 OF S
 SUMUP    SA4   A1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+S(B1+1)*A(B2+1)
*
          SB1   B1+X1          B1= B1-M
          SB2   B2-B5
          SB5   B5+B7
          GE    B5,B4,SUMUP
          SB2   B2+B5
          GE    B5,B0,SUMUP
*
          SA6   B3+X3              B(B3+1)=X6
          SB3   B3-B6          B3= B3-N
          GE    B3,B0,NEWROW
          NE    B4,B0,NEWCOL
          JP    TRATS
          END
#ifdef CERNLIB_TCGEN_TRATS
#undef CERNLIB_TCGEN_TRATS
#endif
