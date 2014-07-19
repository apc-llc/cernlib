*
* $Id: lvmax.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: lvmax.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT LVMAX
*
* CERN PROGLIB# F121    LVMAX           .VERSION KERNCDC  0.1   750820
*
*        I = LVMAX(A,N)
*        SETS I SUCH THAT
*        A(I)  =  MAX(A(J), J=1,N)
*
*
          ENTRY LVMAX
          VFD   30/5HLVMAX,30/LVMAX
 LVMAX    DATA  0
          SB5   -1
          SA3   A1-B5
          SB1   X1
          SA2   X3
          SB3   X2+B5
          SA1   B1+B3
          BX6   X2
          GE    B0,B3,LVMAX
          SB3   B3+B5
          SA4   B1+B3
 LOOP     FX5   X4-X1
          SB3   B3+B5
          NG    X5,ENDLP
          BX1   X4
          SX6   B3+2
 ENDLP    SA4   B1+B3
          GE    B3,B0,LOOP
          EQ    B0,B0,LVMAX
          END
#ifdef CERNLIB_TCGEN_LVMAX
#undef CERNLIB_TCGEN_LVMAX
#endif
