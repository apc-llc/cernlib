*
* $Id: fnzero.F,v 1.1.1.1 1996/02/15 17:52:35 mclareni Exp $
*
* $Log: fnzero.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:35  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      INTEGER FUNCTION FNZERO (NAMEP)
C
C CERN PROGLIB# Z204    FNZERO          .VERSION KERNCRY  1.00  830612
C ORIG. 12/06/83 JZ
C

      NAME = NAMEP

      DO 17  JR=1,7
      IF ((NAME.AND.255).NE.32)  GO TO 21
      NAME = SHIFTR (NAME,8)
   17 CONTINUE
      JR = 8

   21 NAME  = SHIFT (NAME,8*(JR-1))
      NAMEP = NAME
      FNZERO= NAME
      RETURN
      END
