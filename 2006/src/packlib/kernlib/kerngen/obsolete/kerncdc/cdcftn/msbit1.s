*
* $Id: msbit1.s,v 1.1.1.1 1996/02/15 17:51:22 mclareni Exp $
*
* $Log: msbit1.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:22  mclareni
* Kernlib
*
*
          IDENT MSBIT1
*
* CERN PROGLIB# M421    MSBIT1          .VERSION KERNCDC  2.15  850813
* ORIG.  JZ, 12/8/85
*
*         FUNCTION MSBIT1 (WORD,LOC)
*
          ENTRY MSBIT1
          VFD   36/6HMSBIT1,24/MSBIT1
 MSBIT1   BSS   1
          SA4   A1+1
          MX6   1
          SA1   X1             X1= IWORD
          SA2   X4             X2= LOC
          SB2   X2
          LX3   X6,B2
          BX6   X1+X3
          EQ    MSBIT1
          END
