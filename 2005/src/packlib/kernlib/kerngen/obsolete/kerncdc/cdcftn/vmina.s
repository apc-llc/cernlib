*
* $Id: vmina.s,v 1.1.1.1 1996/02/15 17:51:28 mclareni Exp $
*
* $Log: vmina.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:28  mclareni
* Kernlib
*
*
          IDENT VMINA
*
* CERN PROGLIB# F121    VMINA           .VERSION KERNCDC  0.1   750820
*
*        Y = VMINA(A,N)
*        SETS
*        Y = MIN(ABS(A(I)), I=1,N)
*
          ENTRY VMINA
          VFD   30/5HVMINA,30/VMINA
 VMINA    DATA  0
          SB7   -1
          SA3   A1-B7
          SB1   X1+B7
          SA2   X3
          MX6   0
          SB3   X2
          GE    B0,B3,VMINA
          SA1   B1+B3
          BX5   X1
          AX5   60
          BX6   X5-X1
 LOOP     BX3   X1
          AX1   60
          SB3   B3-1
          BX4   X1-X3
          SA1   B1+B3
          FX5   X6-X4
          NG    X5,ENDLP
          BX6   X4
 ENDLP    LT    B0,B3,LOOP
          EQ    B0,B0,VMINA
          END
#ifdef CERNLIB_TCGEN_VMINA
#undef CERNLIB_TCGEN_VMINA
#endif
