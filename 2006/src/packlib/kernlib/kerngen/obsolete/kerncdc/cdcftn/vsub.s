*
* $Id: vsub.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vsub.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VSUB
*
* CERN PROGLIB# F121    VSUB            .VERSION KERNCDC  0.1   750820
*
*        CALL VSUB(A,B,X,N)
*        SETS
*        X(I) = A(I) - B(I)   FOR   I = 1,N
*
*
          ENTRY VSUB
          VFD   24/4HVSUB,36/VSUB
 VSUB     DATA  0
          SB5   -1
          SA2   A1-B5
          SA4   A2-B5
          SB3   X4+1
          SA5   A4-B5
          SA4   X5
          SB6   X4+B5
          SA1   X1+B6
          SA2   X2+B6
          LT    B6,B0,VSUB
          EQ    B6,B0,FIN
 LOOP     FX6   X1-X2
          SA1   A1+B5
          SA2   A2+B5
          NX7   X6
          SB6   B6+B5
          SA7   B3+B6
          GT    B6,B0,LOOP
 FIN      FX6   X1-X2
          NX7   X6
          SA7   B3+B5
          EQ    B0,B0,VSUB
          END
#ifdef CERNLIB_TCGEN_VSUB
#undef CERNLIB_TCGEN_VSUB
#endif
