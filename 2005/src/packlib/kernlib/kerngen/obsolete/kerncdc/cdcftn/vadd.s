*
* $Id: vadd.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vadd.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VADD
*
* CERN PROGLIB# F121    VADD            .VERSION KERNCDC  0.1   750820
*
*        CALL VADD(A,B,X,N)
*        SETS
*        X(I) = A(I) + B(I)   FOR   I = 1,N
*
*
          ENTRY VADD
          VFD   24/4HVADD,36/VADD
 VADD     DATA  0
          SB5   -1
          SA4   A1-B5
          SA5   A4-B5
          SA2   A5-B5
          SA3   X2
          SB6   X3+B5
          ZR    X3,VADD
          NG    X3,VADD
 LOOP     SA3   X1+B6
          SA2   X4+B6
          FX6   X3+X2
          NX7   X6
          SA7   X5+B6
          SB6   B6+B5
          GE    B6,B0,LOOP
          EQ    B0,B0,VADD
          END
#ifdef CERNLIB_TCGEN_VADD
#undef CERNLIB_TCGEN_VADD
#endif
