*
* $Id: lxbits.s,v 1.1.1.1 1996/02/15 17:51:17 mclareni Exp $
*
* $Log: lxbits.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:17  mclareni
* Kernlib
*
*
          IDENT LXBITS
*
* CERN PROGLIB# Z200    LXBITS          .VERSION KERNCDC  1.16  820129
*
*     FUNCTION  LXBITS (IUNIT)  VERSION CDC SCOPE 3
*     RETURNS IN X6 THE LENGTH IN BITS OF RECORD
*
          ENTRY LXBITS
 TRACE    VFD   42/7HLXBITS ,18/LXBITS
 STOREA0  BSS   1
 LXBITS   DATA  0
          SX6   A0               SAVE A0
          SA6   STOREA0
          SA0   A1               IUNIT IN X1
          SA1   A0
          MX6   1
          BX1   X1+X6
+RJ=XGETFIT
-         VFD   30/TRACE
          FETCH X1,RL,X6
          SB1   1
          LX5   X6,B1
          SB1   2
          LX7   X6,B1
          IX6   X5+X7
          SA5   STOREA0          RESTORE A0
          SA0   X5
          EQ    LXBITS
          END
