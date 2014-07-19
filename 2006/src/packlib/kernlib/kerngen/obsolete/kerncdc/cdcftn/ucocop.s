*
* $Id: ucocop.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: ucocop.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UCOCOP
*
* CERN PROGLIB# V302    UCOCOP          .VERSION KERNCDC  0.1   750820
*
          ENTRY UCOCOP
*     SUBROUTINE UCOCOP (A,B,IDO,IW,NA,NB)
          VFD   36/6HUCOCOP,24/UCOCOP
 UCOCOP   BSS   1
          SB7   -1
          SA2   A1-B7
          SB1   X1
          SA3   A2-B7
          SB2   X2+B7
          SX6   -B7
          SA2   X3             X2= IDO
          SA5   A3-B7
          BX3   -X2            X3= -IDO
          ZR    X2,UCOCOP
          SA4   X5             X4= IW
          SA1   A5-B7
          ZR    X4,UCOCOP
          SA5   X1             X5= NA
          SA2   A1-B7
          SB5   X5             B5= NA
          SA1   X2             X1= NB
          SB6   X1             B6= NB
 LOOPDO   SA1   B1
          SB3   B0
          BX0   -X4            X0= -IW
 LOOPIW   LX7   B0,X1
          SB3   X6+B3
          IX0   X0+X6
          SA7   B2+B3
          SA1   B1+B3
          NG    X0,LOOPIW
          SB1   B1+B5
          IX3   X3+X6
          SB2   B2+B6
          NG    X3,LOOPDO
          EQ    UCOCOP
          END
#ifdef CERNLIB_TCGEN_UCOCOP
#undef CERNLIB_TCGEN_UCOCOP
#endif
