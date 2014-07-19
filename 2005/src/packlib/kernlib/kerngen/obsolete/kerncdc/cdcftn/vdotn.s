*
* $Id: vdotn.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vdotn.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VDOTN
*
* CERN PROGLIB# F121    VDOTN           .VERSION KERNCDC  0.1   751101
*
          ENTRY VDOTN
          EXT   SQRT
 NAME     VFD   30/5HVDOTN,30/VDOTN
 VDOTN    BSS   1
          SB5   -1
          SA2   A1-B5
          MX6   0                  XX
          SA4   A2-B5
          BX0   X6                 YY
          SA3   X4
          SB6   X3+B5
          LT    B6,B0,VDOTN
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
          FX6   X6*X0
          SA6   ARG
          SB1   A6
          SX6   B1
          SA6   ARGA
          MX6   0
          SA6   A6-B5
          SA1   ARGA
          SA7   STORE
 +        RJ    SQRT
 -        LT    B0,B1,NAME
          SA1   STORE
          SB5   59
          FX6   X1/X6
          AX3   B5,X6
          SA2   ONE            X2= 1.
          BX5   X3-X6          X5= ABS (VDOTN)
          FX4   X2-X5          X4= 1. - ABS (VDOTN)
          PL    X4,VDOTN
          BX6   X2-X3
          EQ    VDOTN
 ONE      DATA  17204000000000000000B
 STORE    BSS   1
 ARG      BSS   1
 ARGA     BSS   2
          END
#ifdef CERNLIB_TCGEN_VDOTN
#undef CERNLIB_TCGEN_VDOTN
#endif
