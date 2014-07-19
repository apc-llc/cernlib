*
* $Id: weor.s,v 1.1.1.1 1996/02/15 17:51:38 mclareni Exp $
*
* $Log: weor.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:38  mclareni
* Kernlib
*
*
          IDENT WEOR
*
* CERN PROGLIB# K402    WEOR            .VERSION KERNCDC  1.16  820129
*
          SST
          ENTRY WEOR
          EXT   GETFIT.
*
*     CALL WEOR (LUN)
*
 TRACE    VFD   24/4HWEOR,36/WEOR
 SVA0     BSSZ  1
 WEOR     BSSZ  1
          SX6   A0
          SA6   SVA0
          MX6   1
          BX1   X1+X6
+RJGETFIT
-         VFD   30/TRACE
          WEOR  X1
          SA5   SVA0
          SA0   X5
          EQ    WEOR
          END
