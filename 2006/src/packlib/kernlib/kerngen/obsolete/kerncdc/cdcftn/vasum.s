*
* $Id: vasum.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vasum.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VASUM
*
* CERN PROGLIB# F121    VASUM           .VERSION KERNCDC  0.1   750820
*
*        Y = VASUM(A,N)
*        SETS
*        Y = ABS(A(1)) + ABS(A(2)) + ...... + ABS(A(N))
*
*
          ENTRY VASUM
          VFD   30/5HVASUM,30/VASUM
 VASUM    DATA  0
          SA3   A1+1
          MX6   0
          SA2   X3
          ZR    X2,VASUM
          SB6   X2-1
          NG    X2,VASUM
 LOOP     SA3   X1+B6
          BX4   X3
          AX3   60
          BX5   X3-X4
          FX2   X6+X5
          SB6   B6-1
          NX6   X2
          GE    B6,B0,LOOP
          EQ    B0,B0,VASUM
          END
#ifdef CERNLIB_TCGEN_VASUM
#undef CERNLIB_TCGEN_VASUM
#endif
