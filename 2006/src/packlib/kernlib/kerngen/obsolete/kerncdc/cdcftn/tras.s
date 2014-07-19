*
* $Id: tras.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: tras.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRAS
*
* CERN PROGLIB# F112    TRAS            .VERSION KERNCDC  0.1   761001
*
          ENTRY TRAS
*     SUBROUTINE TRAS (A,S,B,M,N)
*         WRITTEN BY   ST. MEEN   30-JUNE-76
          VFD   24/4HTRAS,36/TRAS
 TRAS     BSS   1
          SB7   -1
          SA2   A1-B7
          SA3   A2-B7
          SA4   A3-B7
          SA5   A4-B7
          SA4   X4
          SA5   X5
*
*               B1 : INDEX OF A
*              B2 : INDEX OF S
*               B3 : INDEX OF  B
*               B4 : COUNTS COLUMNS
*               B5 : SUMMING INDEX
*              B6 : =N
*               B7 : =-1
*              X0 :  =M*N-1
*              X7 : S(X7) IS THE LAST ELEMENT  OF COLUMN B4
*
          DX7   X5*X5
          IX7   X7+X5
          AX7   1              X7= N*(N+1)/2
          SB6   X5             B6= N
          DX0   X4*X5
          SX0   X0+B7          X0= M*N-1
          SB3   B0
          SB4   B6+B7          B4= N-1
*
*                              NEW COL-LOOP: B4=N-1,...,0
*                              B3= M*N-1,M*N-2,...,M*N-N
 NEWCOL   SX7   X7+B7          X7= X7-1
          SB1   X0             B1= M*N-1
          SB3   B3+X0          B3= B3+M*N-1
*
*                              NEWROWLOOP:  WHILE B3 GE 0 (M TIMES)
*                              B3 DECREASES BY N EVERY RUN
 NEWROW   SB2   X7
          MX6   0
          SB5   B6+B7          B5= N-1
*
*                              SUMUP-LOOP: B5=N-U,...,0
*                              MULTIPLIES ROW $ OF A BY COL B4 OF S
 SUMUP    SA4   X1+B1
          SA5   X2+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+A(B1+1)*S(B2+1)
*
          SB1   B1+B7          B1= B1-1
          SB2   B2-B5
          SB5   B5+B7          B5= B5-1
          GE    B5,B4,SUMUP
          SB2   B2+B5
          GE    B5,B0,SUMUP
*
          SA6   X3+B3              B(B3+1)=X6
          SB3   B3-B6          B3= B3-N
          GE    B3,B0,NEWROW
*
          SB4   B4+B7          B4= B4-1
          GE    B4,B0,NEWCOL
          JP    TRAS
          END
#ifdef CERNLIB_TCGEN_TRAS
#undef CERNLIB_TCGEN_TRAS
#endif
