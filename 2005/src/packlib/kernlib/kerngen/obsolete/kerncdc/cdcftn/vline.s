*
* $Id: vline.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vline.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VLINE
*
* CERN PROGLIB# F121    VLINE           .VERSION KERNCDC  0.1   750820
*
*        CALL VLINE(A,ALPHA,B,BETA,X,N)
*        SETS
*        X(I) = A(I)*ALPHA + B(I)*BETA   FOR   I = 1,N
*
*
          ENTRY VLINE
          VFD   30/5HVLINE,30/VLINE
 VLINE    DATA  0
          SB7   -1
          SA2   A1-B7
          SB1   X1+B7
          SA3   X2
          SA4   A2-B7
          SB3   X4+B7
          SA2   A4-B7
          SA1   X2
          SA5   A2-B7
          SA2   A5-B7
          SA4   X2
          SB6   X4
          GE    B0,B6,VLINE
 LOOP     SA2   B1+B6
          FX6   X2*X3
          SA4   B3+B6
          FX7   X4*X1
          FX0   X6+X7
          SB6   B6-1
          NX7   X0
          SA7   X5+B6
          NE    B0,B6,LOOP
          EQ    B0,B0,VLINE
          END
#ifdef CERNLIB_TCGEN_VLINE
#undef CERNLIB_TCGEN_VLINE
#endif
