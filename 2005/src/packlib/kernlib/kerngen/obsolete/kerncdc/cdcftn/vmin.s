*
* $Id: vmin.s,v 1.1.1.1 1996/02/15 17:51:27 mclareni Exp $
*
* $Log: vmin.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:27  mclareni
* Kernlib
*
*
          IDENT VMIN
*
* CERN PROGLIB# F121    VMIN            .VERSION KERNCDC  0.1   750820
*
*        Y = VMIN(A,N)
*        SETS
*        Y = MIN(A(I), I=1,N)
*
          ENTRY VMIN
          VFD   24/4HVMIN,36/VMIN
 VMIN     DATA  0
          SB5   -1
          SA3   A1-B5
          MX6   0
          SA2   X3
          SB3   X2+B5
          SA5   X1+B3
          GT    B0,B3,VMIN
          BX6   X5
          EQ    B0,B3,VMIN
          SB3   B3+B5
          SA4   X1+B3
 LOOP     FX5   X6-X4
          SB3   B3+B5
          NG    X5,ENDLP
          BX6   X4
 ENDLP    SA4   X1+B3
          GE    B3,B0,LOOP
          EQ    B0,B0,VMIN
          END
#ifdef CERNLIB_TCGEN_VMIN
#undef CERNLIB_TCGEN_VMIN
#endif
