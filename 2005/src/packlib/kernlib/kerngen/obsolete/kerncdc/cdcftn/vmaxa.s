*
* $Id: vmaxa.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vmaxa.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VMAXA
*
* CERN PROGLIB# F121    VMAXA           .VERSION KERNCDC  0.1   750820
*
*        Y = VMAXA(A,N)
*        SETS
*        Y = MAX(ABS(A(I)), I=1,N)
*
          ENTRY VMAXA
          VFD   30/5HVMAXA,30/VMAXA
 VMAXA    DATA  0
          SB7   -1
          SA3   A1-B7
          SB1   X1+B7
          SA2   X3
          MX6   0
          SB3   X2
          SA1   B1+B3
          GE    B0,B3,VMAXA
 LOOP     BX3   X1
          AX1   60
          SB3   B3-1
          BX4   X1-X3
          SA1   B1+B3
          FX5   X4-X6
          NG    X5,ENDLP
          BX6   X4
 ENDLP    LT    B0,B3,LOOP
          EQ    B0,B0,VMAXA
          END
#ifdef CERNLIB_TCGEN_VMAXA
#undef CERNLIB_TCGEN_VMAXA
#endif
