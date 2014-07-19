*
* $Id: lvmaxa.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: lvmaxa.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT LVMAXA
*
* CERN PROGLIB# F121    LVMAXA          .VERSION KERNCDC  0.1   750820
*
*        I = LVMAXA(A,N)
*        SETS I SUCH THAT
*        ABS(A(I))  =  MAX(ABS(AJ)), J=1,N)
*
*
          ENTRY LVMAXA
          VFD   36/6HLVMAXA,24/LVMAXA
 LVMAXA   DATA  0
          SB7   -1
          SA3   A1-B7
          SB1   X1+B7
          SA2   X3
          MX6   0
          SB3   X2
          MX7   0
          GE    B0,B3,LVMAXA
          SA1   B1+B3
 LOOP     BX3   X1
          AX1   60
          SB3   B3-1
          BX4   X1-X3
          SA1   B1+B3
          FX5   X4-X7
          NG    X5,ENDLP
          BX7   X4
          SX6   B3+1
 ENDLP    LT    B0,B3,LOOP
          EQ    B0,B0,LVMAXA
          END
#ifdef CERNLIB_TCGEN_LVMAXA
#undef CERNLIB_TCGEN_LVMAXA
#endif
