*
* $Id: vsum.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vsum.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VSUM
*
* CERN PROGLIB# F121    VSUM            .VERSION KERNCDC  0.1   750820
*
*        Y = VSUM(A,N)
*        SETS
*        Y = A(1) + A(2) + ...... + A(N)
*
*
          ENTRY VSUM
          VFD   24/4HVSUM,36/VSUM
 VSUM     DATA  0
          SB5   -1
          SA3   A1-B5
          MX6   0
          SA2   X3
          SB6   X2+B5
          ZR    X2,VSUM
          NG    X2,VSUM
 LOOP     SA4   X1+B6
          FX3   X6+X4
          SB6   B6+B5
          NX6   X3
          GE    B6,B0,LOOP
          EQ    B0,B0,VSUM
          END
#ifdef CERNLIB_TCGEN_VSUM
#undef CERNLIB_TCGEN_VSUM
#endif
