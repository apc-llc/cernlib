*
* $Id: ranset.s,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: ranset.s,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#include "kernnum/linkg900.inc"
          GBLC      &NAME
&NAME     SETC      'RANSET'
&NAME     START
&NAME     AMODE     ANY
&NAME     RMODE     ANY
REGLIM    EQU       3
          PROLOG    REGLIM
          ST        13,R13SAVE          SAVE R13
          LR        3,15                SAVE BASE REG 15 IN 3
          LA        13,SAVEAREA
          L         15,ASUB             R15 = (G900ST)
          BALR      14,15               F0 = G900ST(SEED)
          LR        15,3                RESTORE BASE REG 15
          L         13,R13SAVE          RESTORE R13
          EXIT      REGLIM
R13SAVE   DS        1F
SAVEAREA  DS        18F
ASUB      DC        V(G900ST)
          END
