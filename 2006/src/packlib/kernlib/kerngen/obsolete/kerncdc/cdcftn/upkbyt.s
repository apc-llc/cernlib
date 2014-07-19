*
* $Id: upkbyt.s,v 1.1.1.1 1996/02/15 17:51:26 mclareni Exp $
*
* $Log: upkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:26  mclareni
* Kernlib
*
*
          IDENT UPKBYT
*
* CERN PROGLIB# M422    UPKBYT          .VERSION KERNCDC  0.1   750820
*
          ENTRY UPKBYT
*     SUBROUTINE UPKBYT (A,JTH,M,N,NBITS)
          VFD   36/6HUPKBYT,24/UPKBYT
 UPKBYT   BSS   1
          SB7   1              B7= 1
          SA2   A1+B7
          SB1   X1
          SA3   X2             X3= JTH
          SA4   A2+B7
          SB3   X4
          SA2   A4+B7
          SA4   X2             X4= N
          SA5   A2+B7
          ZR    X4,UPKBYT
          SX6   A0
          SA1   X5+B7          X1= INWORD
          PX2   B0,X1          X2= PACKED INWORD
          SA6   SAVEA0
          SA5   A1-B7          X5= NBITS
          NX7   B0,X2          X7= NORMALISED INWORD
          NZ    X5,L12
          SA2   SIXTY              INWORD=60
          SX5   B7                 NBITS=1
          NX7   B0,X2
 L12      SX1   X3-1           X1= JTH-1
          PX3   B0,X1          X1= PACKED (JTH-1)
          FX7   X3/X7
          SB5   X5             B5= NBITS
          SB2   X3                 23=JTH-1
          MX0   1
          SB6   B5-B7          B6= NBITS-1
          SB4   B0                 CURRENT BYTE
          AX0   B6,X0              NBITS BITS ON LEFT
          SB6   B0             B6= LOC
          SA0   X2             A0= INWORD
          LT    B2,B7,START
          UX6   B2,X7
          LX6   B2,X6          X6= NW A-WORDS UNTOUCHED
          PX6   B0,X6
          SB1   B1+X6          B1= JA
          DX1   X6*X2
          FX3   X3-X1
          PX5   B0,X5          X5= PACKED NBITS
          DX7   X3*X5
          SB4   X3             B4= JPOS
          SB6   X7             B6= LOC
 START    LX0   B5,X0              NBITS BITS ON THE RIGHT
          SA1   B1             X1= A(JA)
          SB2   B3+X4              END OF M
          AX2   B6,X1
 L21      SB4   A0-B4              REMAINING NUMBER OF BYTES
 L24      BX6   X2*X0              PICK THE BYTE
          SB3   B3+B7
          SB4   B4-B7
          AX2   B5,X2
          NO
          SA6   B3-B7
          GE    B3,B2,UPKBYE
          NE    B4,B0,L24
          SA2   B1+B7
          SB1   B1+B7
          EQ    L21
 UPKBYE   SA1   SAVEA0
          SA0   X1
          EQ    UPKBYT
 SIXTY    DATA  20000000000000000074B
 SAVEA0   BSS   1
          END
#ifdef CERNLIB_TCGEN_UPKBYT
#undef CERNLIB_TCGEN_UPKBYT
#endif
