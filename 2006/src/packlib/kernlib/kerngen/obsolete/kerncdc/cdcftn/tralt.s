*
* $Id: tralt.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: tralt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRALT
*
* CERN PROGLIB# F112    TRALT           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRALT
*     SUBROUTINE TRALT  (A,U,B,M,N)
*         WRITTEN BY   ST. MEEN 25-JUNE-76
          VFD   30/5HTRALT,30/TRALT
 TRALT    BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SA5   A4-B7
*
          SA4   X4
          SA5   X5
          SB6   X5             B6= N
*
          DX7   X5*X5
          IX7   X7+X5
          AX7   1              X7= N*(N+1)/2
*
          DX5   X5*X4
          SB3   X5+B7          B3= M*N-1
*                              NEWROW-LOOP: WHILE B3GE 0 (M TIMES)
*
 NEWROW   SB2   X7+B7          B2= N*(N+1)/2-1
*
          SB4   -B7            B4= 1
*                              NEWCOL-LOOP: B4=1,2,...,N
 NEWCOL   SB1   B3
          MX6   0
*
          SB5   B4
*                              SUMUP-LOOP: B5=B4,...,N
*                              MULTIPLIES ROW B4 OF B! ROW B3 MOD N OF U
 SUMUP    SA4   X1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+A(B1)*U(B2)
*
          SB1   B1+B7          B1= B1-1
          SB2   B2+B7          B2= B2-1
*
          SB5   B5-B7          B2= B5+1
          GE    B6,B5,SUMUP
*
          SA6   X3+B3              B(B3)=X6
          SB3   B3+B7
*
          SB4   B4-B7          B4= B4+1
          GE    B6,B4,NEWCOL
          GE    B3,B0,NEWROW
          JP    TRALT
          END
#ifdef CERNLIB_TCGEN_TRALT
#undef CERNLIB_TCGEN_TRALT
#endif
