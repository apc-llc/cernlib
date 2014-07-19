*
* $Id: loren4.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: loren4.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT LOREN4             10-JUNE-68
*
* CERN PROGLIB# U101    LOREN4          .VERSION KERNCDC  0.1   750820
*
          ENTRY LOREN4
*     SUBROUTINE LOREN4  (DIR,P4IN,P4OUT)
 NAME     VFD   36/6HLOREN4,24/LOREN4
 LOREN4   BSS   1
          EXT   SQRT
*
          SB7   1              B7= 1
          SA2   A1+B7
          SB1   X1
          SA3   A2+B7
          SA1   B1
          BX7   X2
          RX0   X1*X1          X0= DIR(1)*DIR(1)
          SB2   X7
          BX6   X3
          SA2   A1+B7
          SB3   X6
          LX6   20
          RX1   X2*X2          X1= DIR(2)*DIR(2)
          SA3   A2+B7
          BX6   X6+X7
          SX7   B1
          RX2   X3*X3          X2= DIR(3)*DIR(3)
          LX6   20
          SA4   A3+B7
          RX5   X1+X0
          BX6   X6+X7
          NX5   B0,X5
          RX0   X4*X4          X0= DIR(4)*DIR(4)
          RX4   X5+X2
          SA6   B1B2B3         X6= B3-B2-B1
          NX7   B0,X4
          RX1   X0-X7
          SA7   PCM2           X7= PCM2
          NX6   B0,X1
          NO
          SA6   ARG
          SX7   A6
          SA7   ARGA
          MX6   0
          SA6   A7+B7
          SA1   ARGA
 +        RJ    SQRT
 -        LT    B0,B1,NAME
          SA2   B1B2B3
          SA1   =1.
          SA3   X2
          AX2   20
          RX0   X1/X6          X0= ONMCM
          SA4   X2
          SB7   1              B7= 1
          SA5   A3+B7
          RX7   X3*X4          X7= DIR(1)*P4IN(1)
          SA4   A4+B7
          AX2   20
          RX6   X5*X4          X6= DIR(2)*P4IN(2)
          SA3   A5+B7
          SB3   X2
          SA4   A4+B7
          RX7   X7+X6
          RX2   X3*X4          X2= DIR(3)*P4IN(3)
          SA5   PCM2
          SB4   3
          NX7   B0,X7
          SA3   A3+B7
          RX6   X2+X7
          SA4   A4+B7
          NX7   B0,X6          X7= EPBETA
          RX6   X3*X4          X6= DIR(4)*P4IN(4)
          RX3   X3*X0          X3= DIR(4)*ONMCM
          RX5   X7/X5          X5= EPBETA/PCM2
          RX7   X6-X7
          NX2   B0,X7
          RX4   X4*X0          X4= P4IN(4)*ONMCM
          RX6   X2*X0          X6= P4OUT(4)
          RX1   X3-X1
          NX0   B0,X1
          SA3   A3-B7
          RX2   X0*X5
          SA6   B3+B4              STORES P4OUT(4)
          RX1   X2-X4
          NX0   B0,X1          X0= PROD
*                                     LOOP IS TAKEN IN THE REVERSE ORDER
 LOOP     RX5   X0*X3
          SA4   A4-B7
          RX7   X4+X5
          SA3   A3-B7
          NX6   B0,X7
          SB4   B4-B7
          NO
          SA6   A6-B7
          NE    B4,B0,LOOP
          EQ    LOREN4
*
 ARG      BSS   1
 PCM2     BSS   1
 B1B2B3   BSS   1
 ARGA     BSS   2
          END
#ifdef CERNLIB_TCGEN_LOREN4
#undef CERNLIB_TCGEN_LOREN4
#endif
