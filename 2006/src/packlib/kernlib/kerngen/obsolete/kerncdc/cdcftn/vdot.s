*
* $Id: vdot.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vdot.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VDOT
*
* CERN PROGLIB# F121    VDOT            .VERSION KERNCDC  0.1   750820
*
*        Y = VDOT(A,B,N)
*        SETS
*        Y = A.B = SCALAR PRODUCT OF A AND B (EACH OF DIMENSION N)
*
*
          ENTRY VDOT
          VFD   24/4HVDOT,36/VDOT
 VDOT     DATA  0
          SB5   -1
          SA2   A1-B5
          MX6   0
          SA4   A2-B5
          SA3   X4
          SB6   X3+B5
          LT    B6,B0,VDOT
 LOOP     SA4   X1+B6
          SA5   X2+B6
          FX3   X4*X5
          SB6   B6+B5
          FX7   X6+X3
          NX6   B7,X7
          GE    B6,B0,LOOP
          EQ    B0,B0,VDOT
          END
#ifdef CERNLIB_TCGEN_VDOT
#undef CERNLIB_TCGEN_VDOT
#endif
