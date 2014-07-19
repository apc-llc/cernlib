*
* $Id: vbias.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vbias.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VBIAS
*
* CERN PROGLIB# F121    VBIAS           .VERSION KERNCDC  0.1   750820
*
*        CALL VBIAS(A,ALPHA,X,N)
*        SETS
*        X(I) = A(I) + ALPHA   FOR   I = 1,N
*
*
          ENTRY VBIAS
          VFD   30/5HVBIAS,30/VBIAS
 VBIAS    DATA  0
          SB5   -1
          SA5   A1-B5
          SA2   X5
          SA3   A5-B5
          SA5   A3-B5
          SA4   X5
          SB6   X4+B5
          ZR    X4,VBIAS
          NG    X4,VBIAS
 LOOP     SA5   X1+B6
          FX7   X5+X2
          NX6   X7
          SA6   X3+B6
          SB6   B6+B5
          GE    B6,B0,LOOP
          EQ    B0,B0,VBIAS
          END
#ifdef CERNLIB_TCGEN_VBIAS
#undef CERNLIB_TCGEN_VBIAS
#endif
