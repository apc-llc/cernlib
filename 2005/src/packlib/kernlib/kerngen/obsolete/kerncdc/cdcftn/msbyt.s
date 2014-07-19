*
* $Id: msbyt.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: msbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT MSBYT
*
* CERN PROGLIB# M421    MSBYT           .VERSION KERNCDC  2.15  850813
* ORIG.  JZ, 12/8/85
*
*         FUNCTION MSBYT (VALUE,WORD,LOC,NBITS)
*
          ENTRY MSBYT
          VFD   30/5HMSBYT,30/MSBYT
 MSBYT    BSS   1
          SA5   A1+3           X5= ADDRESS OF NBITS
          SB5   -1
          SA4   X5             X4= NBITS
          MX6   1
          SA5   A5+B5          X5= ADDRESS OF LOC
          SA1   X1             X1= VALUE
          SB6   X4+B5          B6= NBITS-1
          SB4   X4             B4 = NBITS
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
          EQ    MSBYT
          END
#ifdef CERNLIB_TCGEN_MSBYT
#undef CERNLIB_TCGEN_MSBYT
#endif
