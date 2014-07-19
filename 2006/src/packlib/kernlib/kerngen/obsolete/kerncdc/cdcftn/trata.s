*
* $Id: trata.s,v 1.1.1.1 1996/02/15 17:51:24 mclareni Exp $
*
* $Log: trata.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:24  mclareni
* Kernlib
*
*
          IDENT TRATA
*
* CERN PROGLIB# F112    TRATA           .VERSION KERNCDC  0.1   761001
*
          ENTRY TRATA
*     SUBROUTINE TRATA(A,S,M,N)
*       S:=AT.A   S:  SYMETRIC NXN  A: NXM
*         WRITTEN BY   ST. MEEN   7-JULY-76
          VFD   30/5HTRATA,30/TRATA
 TRATA    BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA4   A3+B7
          SA3   X3
          SA4   X4
          DX5   X3*X4
          SB6   X5-1           B6= M*N-1
          SB5   X3             B5= M
          SB4   B0
          SB3   B0
*
*               B1:  INDEX OF AT
*               B2:  INDEX OF A
*               B3:  INDEX OF S
*               B4:  COUNTS ROWS
*               B5:  M
*               B6:  M*N-1
*               B7:  1
*
*                              NEWROW-LOOP: B1=0,...,M
 NEWROW   SB2   B0
*                              NEWCOL-LOOP: B2= 0,1,...,B4
 NEWCOL   SB1   B4
          MX6   0
*
*                              SUMUP-LOOP: WHILE B2.LE. M*N-1 (N TIMES)
*                              MULTIPLIES COLUMN B4 B! COOUMN B2
 SUMUP    SA4   X1+B1
          SA5   X1+B2
          RX5   X4*X5
          RX6   X6+X5
          NX6   B0,X6          X6= X6+A(B1)*A(B2)
*
          SB1   B1+B5          B1= B1+M
          SB2   B2+B5          B2= B2+M
          GE    B6,B2,SUMUP        B2.LE.M*N-1
*
          SA6   X2+B3              S(B3)=X6
          SB3   B3+B7          B3= B3+1
          SB2   B2-B6          B2= B2-M*N+1
          GE    B4,B2,NEWCOL       B2.LE.B4
          SB4   B4+B7          B4= B4+1
          LT    B4,B5,NEWROW       B4.LT.M
          JP    TRATA
          END
#ifdef CERNLIB_TCGEN_TRATA
#undef CERNLIB_TCGEN_TRATA
#endif
