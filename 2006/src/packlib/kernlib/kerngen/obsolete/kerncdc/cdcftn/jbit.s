*
* $Id: jbit.s,v 1.1.1.1 1996/02/15 17:51:21 mclareni Exp $
*
* $Log: jbit.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:21  mclareni
* Kernlib
*
*
          IDENT JBIT
*
* CERN PROGLIB# M421    JBIT            .VERSION KERNCDC  0.1   750820
*
          ENTRY JBIT
*         FUNCTION JBIT (WORD,J)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   24/4HJBIT,36/JBIT
 JBIT     BSS   1
          SA4   A1+1
          SX3   61
          SA2   X4             X2= J
          IX3   X3-X2
          SA1   X1             X1= WORD
          MX6   1
          LX6   1                  SET MASK FOR LAST BIT
          SB3   X3             B3= X3= 61-J
          LX4   B3,X1
          BX6   X4*X6
          EQ    JBIT
          END
#ifdef CERNLIB_TCGEN_JBIT
#undef CERNLIB_TCGEN_JBIT
#endif
