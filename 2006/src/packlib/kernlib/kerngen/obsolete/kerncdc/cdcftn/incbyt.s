*
* $Id: incbyt.s,v 1.1.1.1 1996/02/15 17:51:20 mclareni Exp $
*
* $Log: incbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:20  mclareni
* Kernlib
*
*
          IDENT INCBYT
*
* CERN PROGLIB# M423    INCBYT          .VERSION KERNCDC  0.1   730801
*
          ENTRY INCBYT
*
*         FUNCTION INCBYT(INC,A,J,NBITS)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          VFD   36/6HINCBYT,24/INCBYT
 INCBYT   DATA  0
          SB7   -1
          SA2   A1-B7          X2= ADR(A)
          SA3   A2-B7          X3= ADR(J)
          SA4   A3-B7          X4= ADR(NBITS)
          SB6   B0-B7          B6= 1
          SA3   X3             X3= J
          SA5   X4+B6          X5= INWORD
          SX3   X3+B7          X3= J-1
          PX5   X5                 PACK INWORD INTO X5
          PX7   X3                 PACK J-1 INTO X7
          NX6   X5                 NORMALISE INWORD
          FX6   X7/X6          X6= JW-1
          SA4   X4                 XJ= NBITS
          MX0   1                  MASK STARTED
          SB4   X4+B7          B4= NBITS-1
          PX4   X4                 PACK NBITS INTO X4
          AX0   B4,X0              MASK TO LEFT
          SB2   X2             B2= ADR(A)
          SB4   B4-B7          B4= NBITS
          UX6   B5,X6              UNPACK JW-1
          LX6   B5,X6              UNNORMALISE
          PX3   X6                 PACK  JW-1
          DX3   X3*X5          X3= J-JW
          SA1   X1             X1= INC
          SB2   X6+B2          B2= ADR(JWORD)
          FX3   X7-X3          X3= JB-1
          DX6   X3*X4          X6= L-1
          SA5   A5-B7          X5= MAX
          SA2   B2             X2= JWORD
          LX0   B4,X0              MASK TO RIGHT
          SB3   X6-60          B3= -(61-L)
          SB4   X6             B4= L-1
          AX2   B3,X2              SHIFT JWORD TO RIGHT
          BX6   X2*X0              MASK OUT NON BYTE IN JWORD
          IX1   X1+X6          X1= BYTE+INC
          IX6   X1-X5          X6= NEW BYTE - MAX
          BX3   X6             X3= X6
          AX3   59             X3= +0 IF X3.GT.MAX
          BX6   -X3*X6         X6= OVERFLOW
          BX7   -X0*X2             MASK BYTE IN JWORD
          IX1   X1-X6          X1= SUM-OVERFLOW
          BX3   X7+X1          X3= RESULT (SHIFTED)
          LX7   B4,X3              SHIFT TO L
          SA7   B2                 RESTORE RESULT
          EQ    INCBYT
          END
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
