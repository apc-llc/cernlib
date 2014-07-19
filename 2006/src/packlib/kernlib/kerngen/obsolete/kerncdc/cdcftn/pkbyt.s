*
* $Id: pkbyt.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: pkbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT PKBYT
*
* CERN PROGLIB# M422    PKBYT           .VERSION KERNCDC  0.1   750820
*
          ENTRY PKBYT
*     SUBROUTINE PKBYT (M,A,JTH,N,NBITS)
          VFD   30/5HPKBYT,30/PKBYT
 PKBYT    BSS   1
          SB7   1
          SA2   A1+B7
          SB1   X1
          SA4   A2+B7
          SB2   X2
          SA5   A4+B7
          SA3   X4             X3= JTH
          SA2   A5+B7
          SA4   X5             X4= N
          ZR    X4,PKBYT
          SA1   X2+B7          X1= INWORD
          SX6   A0
          SA5   X2             X5= NBITS
          PX2   B0,X1          X2= PACKED INWORD
          SA6   SAVEA0
          NX7   B0,X2          X7= NORMALISED INWORD
          NZ    X5,L12
          SA2   SIXTY              INWORD=60
          SX5   B7                 NBITS=1
          NX7   B0,X2
 L12      SX1   X3-1           X1= JTH-1
          PX3   B0,X1          X1= PACKED (JTH-1)
          FX7   X3/X7
          SB5   X5             B5= NBITS
          SB3   X3             B3= JTH-1
          MX0   1
          SB6   B5-B7          B6= NBITS-1
          SB4   B0                 CURRENT BYTE
          AX0   B6,X0              NBITS BITS ON LEFT
          SB6   B0             B6= LOC
          SA0   X2             A0= INWORD
          LT    B3,B7,START
          UX6   B3,X7
          LX6   B3,X6          X6= NW A-WORDS UNTOUCHED
          PX6   B0,X6
          SB2   B2+X6          B2= NEW JA
          DX1   X6*X2
          FX3   X3-X1
          PX5   B0,X5          X5= PACKED NBITS
          DX7   X3*X5
          SB4   X3
          SB6   X7             B6= LOC
 START    SA2   B2                 A(JA)
          LX0   B5,X0              NBITS BITS ON THE RIGHT
          SB2   B1+X4              END OF M
          SA1   B1                 M(JM)
 L21      BX6   X2
          SB4   A0-B4              REMAINING NUMBER OF BYTES
          LX7   B6,X0              SHIFT MASK
 L24      BX6   -X7*X6             ZERO OUT BYTE IN A(JA)
          LX1   B6,X1              SHIFT M(JM)
          SB4   B4-B7
          SB6   B6+B5              LOC
          BX5   X1*X7
          SB1   B1+B7
          LX7   B5,X7
          BX6   X6+X5              STORE M(JM) IN A(JA)
          GE    B1,B2,END
          SA1   B1                 NEXT M(JM)
          NE    B4,B0,L24
          SA6   A2                 STORE A(JA)
          SA2   A2+B7              NEXT A(JA)
          SB6   B0             B6= LOC
          EQ    L21
 END      SA6   A2
          SA1   SAVEA0
          SA0   X1
          EQ    PKBYT
 SIXTY    DATA  20000000000000000074B
 SAVEA0   BSS   1
          END
#ifdef CERNLIB_TCGEN_PKBYT
#undef CERNLIB_TCGEN_PKBYT
#endif
