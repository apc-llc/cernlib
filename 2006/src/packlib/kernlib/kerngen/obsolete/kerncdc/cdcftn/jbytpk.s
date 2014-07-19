*
* $Id: jbytpk.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: jbytpk.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT JBYTPK
*
* CERN PROGLIB# M422    JBYTPK          .VERSION KERNCDC  0.1   730801
*
*         FUNCTION JBYTPK (A,J,NBITS)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          ENTRY JBYTPK
          VFD   36/6HJBYTPK,24/JBYTPK
 JBYTPK   DATA  0
          SB7   -1             B7= -1
          SA2   A1-B7
          SA3   A2-B7
          SB6   -60            B6= -60
          SA4   X3+1           X4= INWORD
          SA3   X3             X3= NBITS
          SA2   X2             X2= J
          SB4   X3             B4= NBITS
          SX2   X2+B7          X2= J-1
          NE    B0,B4,NODFLT       JUMP IF NBITS NONZERO
          SX4   -B6                INWORD= 60
          SX3   -B7            X3= NBITS= 1
 NODFLT   PX2   X2                 PACK J-1 INTO X2
          PX4   X4                 PACK INWORD INTO X4
          NX5   X4                 NORMALISE INWORD INTO X5
          FX5   X2/X5          X5= (J-1)/INWORD= JW-1
          MX0   1                  START MASK
          SB4   X3+B7          B4= NBITS-1
          PX3   X3                 PACK NBITS INTO X3
          SB1   X1             B1= ADR(A)
          AX0   B4,X0              MASK ON LEFT
          UX5   B5,X5              UNPACK X5 INTO B5 AND X5
          LX5   B5,X5              UNNORMALISE X5 FOR JW-1
          PX6   X5                 PACK JW-1 INTO X6
          DX6   X6*X4          X6= (JW-1)*INWORD
          SB4   B4-B7          B4= NBITS
          SB1   X5+B1          B1= ADR(JWORD)
          FX6   X2-X6          X6= JB-1
          DX6   X6*X3          X6= L-1
          SA1   B1             X1= JWORD
          LX0   B4,X0              MASK ON RIGHT
          SB6   X6+B6          B6= (L-1)-60
          AX1   B6,X1              SHIFT BYTE TO RIGHT
          BX6   X0*X1              MASK OUT NON BYTE
          EQ    JBYTPK
          END
#ifdef CERNLIB_TCGEN_JBYTPK
#undef CERNLIB_TCGEN_JBYTPK
#endif
