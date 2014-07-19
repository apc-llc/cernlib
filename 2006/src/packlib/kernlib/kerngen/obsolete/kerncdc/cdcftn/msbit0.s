*
* $Id: msbit0.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: msbit0.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT MSBIT0
*
* CERN PROGLIB# M421    MSBIT0          .VERSION KERNCDC  2.15  850813
* ORIG.  JZ, 12/8/85
*
*         FUNCTION MSBIT0 (WORD,LOC)
*
          ENTRY MSBIT0
          VFD   36/6HMSBIT0,24/MSBIT0
 MSBIT0   BSS   1
          SA4   A1+1
          MX6   1
          SA1   X1             X1= IWORD
          SA2   X4             X2= LOC
          SB2   X2
          LX3   X6,B2
          BX6   -X3*X1             CLEAR BIT IN WORD
          EQ    MSBIT0
          END
