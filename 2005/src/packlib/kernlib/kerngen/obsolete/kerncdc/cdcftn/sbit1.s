*
* $Id: sbit1.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT SBIT1
*
* CERN PROGLIB# M421    SBIT1           .VERSION KERNCDC  0.1   750820
*
          ENTRY SBIT1
*         SUBROUTINE SBIT1(WORD,LOC)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   30/5HSBIT1,30/SBIT1
 SBIT1    BSS   1
          SA4   A1+1
          MX6   1
          SA1   X1             X1= IWORD
          SA2   X4             X2= LOC
          SB2   X2
          LX3   X6,B2
          BX6   X1+X3
          SA6   A1
          EQ    SBIT1
          END
