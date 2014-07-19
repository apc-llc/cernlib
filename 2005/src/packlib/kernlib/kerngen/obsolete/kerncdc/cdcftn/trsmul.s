*
* $Id: trsmul.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: trsmul.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT TRSMUL
*
* CERN PROGLIB# F112    TRSMUL          .VERSION KERNCDC  0.1   761001
*
          ENTRY TRSMUL
*     SUBROUTINE TRSMUL(A,S,M)
*     S:=AT.A,  A:  LOWER TRIANGULAR MXM, S:  SYMMETRIC MXM
*         WRITTEN BY ST. MEEN    20-JULY-76
          VFD   36/6HTRSMUL,24/TRSMUL
 TRSMUL   BSS   1
          SB7   1
          SA2   A1+B7
          SA3   A2+B7
          SA3   X3
*
          SB4   X3             B4= M
          SB5   -B7
          SB6   B0
*
*               NEWROW-LOOP: B1=1,2,...,M  B5=0,2,5,9,..(DIAG-ELEM)
          SB1   B0
 NEWROW   SB1   B1+B7
          SB5   B5+B1
*
*               NEWCOL-LOOP: B2=1,2,...,B1
*               B6 INCREASES B! 1 EVERY RUN
          SB2   B0
 NEWCOL   SB2   B2+B7
          SX4   X1+B5
          SX5   X1+B6
          MX6   0
*
*               SUMUP-LOOP: B3= B1,...,M
*               MULTIPLIES CL B1 B! COL B2 OF A, RESULT IN X6
          SB3   B1
 SUMUP    SA4   X4
          SA5   X5
          RX5   X5*X4
          RX6   X6+X5
          NX6   B0,X6
          SX4   A4+B3
          SX5   A5+B3
          SB3   B3+B7
          GE    B4,B3,SUMUP
*
          SA6   X2+B6              S(B6)=X6
          SB6   B6+B7
          LT    B2,B1,NEWCOL
*
          LT    B1,B4,NEWROW
          JP    TRSMUL
          END
#ifdef CERNLIB_TCGEN_TRSMUL
#undef CERNLIB_TCGEN_TRSMUL
#endif
