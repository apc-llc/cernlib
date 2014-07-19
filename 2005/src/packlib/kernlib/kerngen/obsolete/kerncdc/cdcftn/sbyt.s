*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT SBYT
*
* CERN PROGLIB# M421    SBYT            .VERSION KERNCDC  0.1   750820
*
          ENTRY SBYT
*         SUBROUTINE SBYT (VALUE,WORD,LOC,NBITS)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   24/4HSBYT,36/SBYT
 SBYT     BSS   1
          SA5   A1+3           X5= ADDRESS OF NBITS
          SB5   -1
          SA4   X5             X4= NBITS
          MX6   1
          SA5   A5+B5          X5= ADDRESS OF LOC
          SA1   X1             X1= VALUE
          SB6   X4+B5          B6= NBITS-1
          SB4   X4
          AX6   B6,X6
          SA3   X5             X3= LOC
          SA5   A5+B5          X5= ADDRESS OF WORD
          LX6   X6,B4              NBITS BITS ON THE RIGHT
          SB7   X3+B5          B7= LOC-1
          BX7   X1*X6              MASK NBITS BITS OF VALUE
          SA2   X5             X2= WORD
          LX6   X6,B7              SHIFT MASK
          LX1   X7,B7              SHIFT VALUE
          BX7   -X6*X2             ZERO OUT BYTE IN WORD
          BX6   X7+X1              ADD VALUE
          SA6   A2                 STORE RESULT
          EQ    SBYT
          END
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
