*
* $Id: udicop.s,v 1.1.1.1 1996/02/15 17:51:25 mclareni Exp $
*
* $Log: udicop.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:25  mclareni
* Kernlib
*
*
          IDENT UDICOP
*
* CERN PROGLIB# V302    UDICOP          .VERSION KERNCDC  0.1   750820
*
          ENTRY UDICOP
*     SUBROUTINE UDICOP (A,B,IDO,IW,NA,NB)
          VFD   36/6HUDICOP,24/UDICOP
 UDICOP   BSS   1
          SB7   1
          SA5   A1+B7
          SX6   B7             X6= 1
          SA4   A5+B7
          SA2   X4             X2= IDO
          SA3   A4+B7
          IX7   X2-X6          X7= IDO-1
          ZR    X2,UDICOP
          SA4   X3             X4= IW
          BX3   -X2            X3= -IDO
          ZR    X4,UDICOP
          SB1   X1
          SA1   A3+B7
          SB2   X5
          SA5   X1             X5= NA
          SA2   A1+B7
          PX0   B0,X5              PACKED NA
          SB5   X5             B5= NA
          SA1   X2             X1= NB
          PX5   B0,X7          X5= PACKED (IDO-1)
          SB2   B2+X4
          PX2   B0,X1              PACKED NB
          SB6   X1             B6= NB
          DX7   X0*X5
          IX1   X4-X6
          DX0   X2*X5
          UX7   B0,X7          X7= NA*(IDO-1)
          IX2   X1+X7
          SB1   B1+X2              POINTER FOR A-ADDRESS
          UX0   B0,X0          X0= NB*(IDO-1)
          SB2   B2+X0              POINTER FOR B-ADDRESS
 LOOPDO   SA1   B1
          SB3   B0
          BX0   -X4            X0= -IW
 LOOPIW   LX7   B0,X1
          SB3   X6+B3
          IX0   X0+X6
          SA7   B2-B3
          SA1   B1-B3
          NG    X0,LOOPIW
          SB1   B1-B5
          IX3   X3+X6
          SB2   B2-B6
          NG    X3,LOOPDO
          EQ    UDICOP
          END
#ifdef CERNLIB_TCGEN_UDICOP
#undef CERNLIB_TCGEN_UDICOP
#endif
