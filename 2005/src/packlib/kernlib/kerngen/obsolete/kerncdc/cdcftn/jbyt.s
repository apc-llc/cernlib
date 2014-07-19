*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT JBYT
*
* CERN PROGLIB# M421    JBYT            .VERSION KERNCDC  0.1   750820
*
          ENTRY JBYT
*         FUNCTION JBYT (WORD,J,NBITS)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   24/4HJBYT,36/JBYT
 JBYT     BSS   1
          SA5   A1+2           X5= ADD OF NBITS
          SA2   A1+1           X2= ADD OF J
          SA3   X5             X3= NBITS
          MX6   1
          SX4   61
          SB6   X3-1
          SA2   X2             X2= J
          AX6   B6,X6
          SB3   X3             B3= NBITS
          SA1   X1             X1= WORD
          IX4   X4-X2
          LX6   B3,X6          X6= NBITS BITS ON THE RIGHT
          SB4   X4             B4= X4=61-J
          LX5   B4,X1
          BX6   X5*X6
          EQ    JBYT
          END
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
