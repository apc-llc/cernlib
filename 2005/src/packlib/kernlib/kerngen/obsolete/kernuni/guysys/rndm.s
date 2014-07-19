*
* $Id: rndm.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: rndm.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     PSEUDO RANDOM NUMBERS                      *** V 104 ***
.     FUNCTION RNDM(X)
.     SUBROUTINE RDMOUT(OCT)
.     SUBROUTINE RDMIN(OCT)
.     SUBROUTINE INRNDM
. $$$*****$$$*****$$$*****$$$*****
.
 
          AXR$
          FORT$
$(0)
ININUM    +12113                       . AMORCE
W        + 12113               . FACTEUR VARIABLE
T        + 1220703125          . FACTEUR CONSTANT, 5**PREMIER
$(1)
.     SUBROUTINE INRNDM RESETS THE VALUE OF THE GENERATOR TO THE INITIAL VALUE
INRNDM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A0,ININUM
          S         A0,W
          J         RETUR0
RNDM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A0,W
         MSI     A0,T          . FACTEUR CONSTANT
         AND     A0,(0377777777777)    . ENLEVER LE SIGNE
         SA      A1,W
         LA,U    A2,0170       . EXPOSANT POUR < 1
         LCF     A2,A1
         SA      A3,A0          . C'EST UNE FUNCTION
          J         RETUR1
.     SUBROUTINE RDMIN( OCT ) REPLACES THE CURRENT VALUE OF THE GENERATOR BY
.     THE VALUE FOUND IN OCT
RDMIN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A0,0,XARG          . LOC(OCT)
          L         A0,0,A0            . OCT = NEW VALUE OF GENERATOR
          S         A0,W
          J         RETUR1
.     SUBROUTINE RDMOUT( OCT ) STORES IN OCT THE CURRENT VALUE OF THE GENERATOR
RDMOUT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A0,0,XARG          . LOC(OCT)
          L         A2,W
          S         A2,0,A0            . CURRENT VALUE OF GENERATOR
          J         RETUR1
 
#if (defined(CERNLIB_WBACK))&&(defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
          INFO      010  3
$(3)      +         0776003,RNDM
          +         0,SAVX11
          +         'RNDM  '
#include "gerr_c.inc"
SAVX11    RES       1
#endif
        END
