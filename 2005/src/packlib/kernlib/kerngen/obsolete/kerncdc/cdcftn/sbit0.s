*
* $Id: sbit0.s,v 1.1.1.1 1996/02/15 17:51:23 mclareni Exp $
*
* $Log: sbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:23  mclareni
* Kernlib
*
*
          IDENT SBIT0
*
* CERN PROGLIB# M421    SBIT0           .VERSION KERNCDC  0.1   750820
*
          ENTRY SBIT0
*         SUBROUTINE SBIT0(WORD,LOC)
*        (WRITTEN FOR SCOPE 3.3 - FTN)
          VFD   30/5HSBIT0,30/SBIT0
 SBIT0    BSS   1
          SA4   A1+1
          MX6   1
          SA1   X1             X1= IWORD
          SA2   X4             X2= LOC
          SB2   X2
          LX3   X6,B2
          BX6   -X3*X1             CLEAR BIT IN WORD
          SA6   A1
          EQ    SBIT0
          END
