*
* $Id: ranget.s,v 1.1.1.1 1996/02/15 17:48:31 mclareni Exp $
*
* $Log: ranget.s,v $
* Revision 1.1.1.1  1996/02/15 17:48:31  mclareni
* Kernlib
*
*
#include "kernnum/linkg900.inc"
          GBLC      &NAME
&NAME     SETC      'RANGET'
&NAME     START
&NAME     AMODE     ANY
&NAME     RMODE     ANY
REGLIM    EQU       3
          PROLOG    REGLIM
*                                       R1 = ((SEED))
          ST        13,R13SAVE          SAVE R13
          LR        3,15                SAVE BASE REG 15 IN 3
          LA        13,SAVEAREA         R13 = (SAVE AREA FOR G900GT)
          L         15,ASUB             R15 = (G900GT)
          BALR      14,15               F0 = G900GT(SEED)[ARG NOT USED]
          LR        15,3                RESTORE BASE REG 15
          L         2,0(1)              R2 = (SEED)
          N         2,=X'7FFFFFFF'      CLEAR TOP BIT OF ADDRESS WORD
          STD       0,0(2)              SEED = F0
          L         13,R13SAVE          RESTORE R13 FOR EXIT
          EXIT      REGLIM
R13SAVE   DS        1F                  HOLDS R13
SAVEAREA  DS        18F                 FOR G900GT
ASUB      DC        V(G900GT)           EXTERNAL ADDRESS
          END
