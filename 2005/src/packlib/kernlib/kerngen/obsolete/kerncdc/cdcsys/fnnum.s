*
* $Id: fnnum.s,v 1.1.1.1 1996/02/15 17:51:34 mclareni Exp $
*
* $Log: fnnum.s,v $
* Revision 1.1.1.1  1996/02/15 17:51:34  mclareni
* Kernlib
*
*
          IDENT FNNUM
*
* CERN PROGLIB# Z204    FNNUM           .VERSION KERNCDC  1.08  790126
*
          ENTRY FNNUM
*
*     NAME = FNNUM (NAME)
*
          VFD   30/5HFNNUM,30/FNNUM
 FNNUM    DATA  0
          SA1   X1
          MX0   53
          BX6   X0*X1
          NZ    X6,FNNUM
          SA4   =5LTAPE0
          SB2   -10
          SX5   1R0
          SX6   X1+B2
          LX1   30
          NG    X6,JOIN
 TWO      SX5   X5+100B
          BX7   X6
          SX6   X6+B2
          PL    X6,TWO
          IX1   X5+X7
          LX1   24
 JOIN     IX6   X4+X1
          SA6   A1
          JP    FNNUM
          END
