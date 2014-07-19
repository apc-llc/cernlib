*
* $Id: sbytpk.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sbytpk.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT SBYTPK
*
* CERN PROGLIB# M422    SBYTPK          .VERSION KERNCDC  0.1   730801
*
*         SUBROUTINE SBYTPK(X,A,J,NBITS)
*
*         ROUTINE WRITTEN BY P. CROZIER FOR FTN
*
          ENTRY SBYTPK
          VFD   36/6HSBYTPK,       24/SBYTPK
 SBYTPK   DATA  0
          SB7   -1             B7= -1
          SA2   A1+1
          SA3   A2-B7
          SB6   60
          SA4   A3-B7
          SA3   X3             X3= J
          SA5   X4+1           X5= INWORD
          SA4   X4             X4= NBITS
          SX3   X3+B7          X3= J-1
          NO
          SB4   X4             B4= NBITS
          PX7   X3                 PACK J-1 INTO X7
          NE    B0,B4,NODFLT       JUMP IF NBITS NONZERO
          SX5   B6                 INWORD= 60
          SX4   -B7            X4= NBITS= 1
 NODFLT   PX5   X5                 PACK INWORD INTO X5
          NX6   X5                 NORMALISE INWORD INTO X6
          FX6   X7/X6          X6= (J-1)/INWORD= JW-1
          MX0   1                  START MASK
          SB4   X4+B7          B4= NBITS-1
          PX4   X4                 PACK NBITS INTO X4
          SA1   X1             X1= X
          SB2   X2             B2= ADR(A)
          AX0   B4,X0              MASK ON LEFT
          UX6   B5,X6              UNPACK X6 INTO B5 AND X6
          LX6   B5,X6              UNNORMALISE X6 FOR JW-1
          PX3   X6                 PACK JW-1 INTO X3
          DX3   X3*X5          X3= (JW-1)*INWORD
          SB4   B4-B7          B4= NBITS
          SB2   X6+B2          B2= ADR(JWORD)
          FX3   X7-X3          X3= JB-1
          DX3   X3*X4          X3= L-1
          SA2   B2             X2= JWORD
          LX0   B4,X0              MASK ON RIGHT
          BX7   X0*X1              MASK OUT NON BYTE IN X
          SB6   X3             B6= L-1
          LX0   B6,X0              SHIFT MASK TO BIT L
          BX6   -X0*X2             MASK OUT BYTE IN JWORD
          LX7   B6,X7              SHIFT BYTE IN X TO L
          BX6   X7+X6          X6= SUM OF BYTES
          SA6   B2                 RESTORE RESULT
          EQ    SBYTPK
          END
#ifdef CERNLIB_TCGEN_SBYTPK
#undef CERNLIB_TCGEN_SBYTPK
#endif
