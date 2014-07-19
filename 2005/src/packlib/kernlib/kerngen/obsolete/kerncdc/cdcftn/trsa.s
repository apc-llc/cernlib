*
* $Id: trsa.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trsa.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRSA
*
* CERN PROGLIB# F112    TRSA            .VERSION KERNCDC  0.1   761001
*
          ENTRY TRSA
*     SUBROUTINE TRSA (S,A,B,M,N)
*         WRITTEN BY   ST. MEEN   25-JUNE-76
          VFD   24/4HTRSA,36/TRSA
 TRSA     BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SA5   A4-B7
          SA4   X4
          SA5   X5
*
*               B1 : INDEX OF  S
*               B2 : INDEX OF  A
*               B3 : INDEX OF  B
*               B4 : COUNTS ROWS
*               B5 : SUMMING INDEX
*               B6 : =M-1
*               B7 : =-N
*               X0 : =M*N-1
*               X7 : S(X7) IS THE LAST ELEMENT OF ROW B4
*
          DX7   X4*X4
          IX7   X7+X4
          AX7   1              X7= M*(M+1)/2
          DX0   X4*X5
          SX0   X0+B7          X0= M*N-1
          SB6   X4+B7          B6= M-1
          SB7   X5
          SB7   -B7            B7= -N
          SB3   X0             B3= M*N-1
          SB4   B6             B4= M-1
*                              NEWROW-LOOP: B4=M-U,...,0
 NEWROW   SX7   X7-1
          SB2   B0
*
*                              NEWCOL-LOOP: B2=M*N-1,M*N-2,...,M*N-N
 NEWCOL   SB1   X7
          SB5   B6             B5= M-1
          SB2   B2+X0
          MX6   0
*
*                              SUMUP-LOOP: WHILE B2 GE 0 (M TIMES )
*                              MULTIPLIES ROW B4 OF S B! COL M*N-B2 OF A
 SUMUP    SA4   X1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+S(B1+1)*A(B2+1)
          SB2   B2+B7          B2= B2-N
          SB1   B1-B5
          SB5   B5-1
          GE    B5,B4,SUMUP
          SB1   B1+B5
          GE    B2,B0,SUMUP
*
          SA6   X3+B3              B(B3+1)=X6
          SB3   B3-1
          LT    B7,B2,NEWCOL
*
          SB4   B4-1
          GE    B4,B0,NEWROW
*
          JP    TRSA
          END
#ifdef CERNLIB_TCGEN_TRSA
#undef CERNLIB_TCGEN_TRSA
#endif
