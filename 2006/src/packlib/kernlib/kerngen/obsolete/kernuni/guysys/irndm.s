*
* $Id: irndm.s,v 1.1.1.1 1996/02/15 17:54:08 mclareni Exp $
*
* $Log: irndm.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:08  mclareni
* Kernlib
*
*
. $$$*****$$$*****$$$*****$$$*****
.
.     FUNCTION IRNDM(I)                          *** V 104 ***
.     SUBROUTINE IRDMIN(OCT)
.     SUBROUTINE IRDOUT(OCT)
.     SUBROUTINE INIRDM(N)
.     GENERATION OF PSEUDO RANDOM INTEGER NUMBERS WITH UNIFORM DISTRIBUTION
.     IN THE RANGE SPECIFIED BY ROUTINE INIRDM
.     DEFAULT RANGE IS (1,2**34-1)
.
. $$$*****$$$*****$$$*****$$$*****
 
          AXR$
          FORT$
$(0)
ININUM    +12113
T        + 1220703125          . FACTEUR CONSTANT, 5**PREMIER
WI        +12113
N         +0200000,0
$(1)
.     SUBROUTINE INIRDM( N ) SETS THE RANGE TO (0,N-1)
INIRDM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A1,0,XARG          . LOC(N)
          L         A4,ININUM
          L         A5,0,A1            . N
          DS        A4,WI              . W AND N
          J         RETUR1
IRNDM*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A0,WI
         MSI     A0,T          . FACTEUR CONSTANT
         AND     A0,(0377777777777)    . ENLEVER LE SIGNE
          S         A1,WI
         MF      A1,N
         LA      A0,A1         . C'EST UNE FUNCTION
          J         RETUR1
.     SUBROUTINE IRDMIN( OCT ) REPLACES THE CURRENT VALUE OF THE GENERATOR BY
.     THE VALUE FOUND IN OCT
IRDMIN*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A1,0,XARG          . LOC(OCT)
          L         A0,0,A1            . OCT
          S         A0,WI
          J         RETUR1
.     SUBROUTINE IRDOUT( OCT ) STORES IN OCT THE CURRENT VALUE OF THE GENERATOR
IRDOUT*
#include "ftnors1.inc"
#include "spyuse.inc"
* Ignoring t=pass
          L         A4,WI
          L         A1,0,XARG          . LOC(OCT)
          S         A4,0,A1
          J         RETUR1
 
#if (defined(CERNLIB_WBACK))&&(defined(CERNLIB_GUYDIAGP))&&(defined(CERNLIB_GUYFORT))
          INFO      010  3
$(3)      +         0776003,IRNDM
          +         0,SAVX11
          +         'IRNDM '
#include "gerr_c.inc"
SAVX11    RES       1
#endif
         END
