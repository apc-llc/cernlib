*
* $Id: vscale.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vscale.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VSCALE
*
* CERN PROGLIB# F121    VSCALE          .VERSION KERNCDC  0.1   750820
*
*        CALL VSCALE(A,ALPHA,X,N)
*        SETS
*        X(I) = A(I) * ALPHA   FOR   I = 1,N
*
*
          ENTRY VSCALE
          VFD   36/6HVSCALE,24/VSCALE
 VSCALE   DATA  0
          SB5   -1
          SA4   A1-B5
          SA3   A4-B5
          SA2   X4
          SA5   A3-B5
          SA4   X5
          SB6   X4+B5
          ZR    X4,VSCALE
          NG    X4,VSCALE
 LOOP     SA4   X1+B6
          FX6   X4*X2
          SA6   X3+B6
          SB6   B6+B5
          GE    B6,B0,LOOP
          EQ    B0,B0,VSCALE
          END
#ifdef CERNLIB_TCGEN_VSCALE
#undef CERNLIB_TCGEN_VSCALE
#endif
