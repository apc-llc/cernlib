*
* $Id: vmul.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vmul.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VMUL
*
* CERN PROGLIB# F121    VMUL            .VERSION KERNCDC  0.1   750820
*
*        CALL VMUL(A,B,X,N)
*        SETS
*        X(I) = A(I) * B(I)   FOR   I = 1,N
*
*
          ENTRY VMUL
          VFD   24/4HVMUL,36/VMUL
 VMUL     DATA  0
          SB5   -1
          SA2   A1-B5
          SA3   A2-B5
          SA5   A3-B5
          SA4   X5
          ZR    X4,VMUL
          SB6   X4+B5
          NG    X4,VMUL
 LOOP     SA4   X1+B6
          SA5   X2+B6
          FX7   X4*X5
          SA7   X3+B6
          SB6   B6+B5
          GE    B6,B0,LOOP
          EQ    B0,B0,VMUL
          END
#ifdef CERNLIB_TCGEN_VMUL
#undef CERNLIB_TCGEN_VMUL
#endif
