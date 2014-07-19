*
* $Id: vdotn2.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vdotn2.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VDOTN2
*
* CERN PROGLIB# F121    VDOTN2          .VERSION KERNCDC  0.1   750820
*
          ENTRY VDOTN2
          VFD   36/6HVDOTN2,24/VDOTN2
 VDOTN2   BSS   1
          SB5   -1
          SA2   A1-B5
          MX6   0                  XX
          SA4   A2-B5
          BX0   X6                 YY
          SA3   X4
          SB6   X3+B5
          LT    B6,B0,VDOTN2
          MX7   0                  XY
 LOOP     SA4   X1+B6
          SA5   X2+B6
          FX3   X4*X4              X * X
          FX6   X6+X3
          SB6   B6+B5
          FX3   X4*X5              X * Y
          FX7   X7+X3
          NX6   X6
          FX4   X5*X5              Y * Y
          NX7   X7
          FX0   X0+X4
          NX0   X0
          GE    B6,B0,LOOP
          FX3   X7/X6              XY / XX
          FX2   X7/X0              XY / YY
          FX6   X3*X2              XY**2 / XX*YY
          EQ    VDOTN2
          END
#ifdef CERNLIB_TCGEN_VDOTN2
#undef CERNLIB_TCGEN_VDOTN2
#endif
