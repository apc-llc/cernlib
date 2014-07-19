*
* $Id: vmod.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vmod.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VMOD
*
* CERN PROGLIB# F121    VMOD            .VERSION KERNCDC  0.1   750820
*
*        Y = VMOD(A,N)
*        SETS
*        Y = MODULUS OF A  (A HAVING DIMENSION N)
*
*
          ENTRY VMOD
          EXT   SQRT
          VFD   24/4HVMOD,36/VMOD
 VMOD     DATA  0
          SB5   -1
          SA3   A1-B5
          MX6   0
          SA2   X3
          SB1   X1
          ZR    X2,VMOD
          SB6   X2+B5
          NG    X2,VMOD
 LOOP     SA1   B1+B6
          FX3   X1*X1
          SB6   B6+B5
          FX7   X6+X3
          NX6   X7
          GE    B6,B0,LOOP
          SA6   LOC
          SB1   A6
          SX7   B1
          SA7   ARG
          MX6   0
          SA6   A7-B5
          SA1   ARG
 +        RJ    SQRT
 -        LT    B0,B1,VMOD-1
          EQ    B0,B0,VMOD
 LOC      DATA  0
 ARG      BSS   2
          END
#ifdef CERNLIB_TCGEN_VMOD
#undef CERNLIB_TCGEN_VMOD
#endif
