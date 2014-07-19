*
* $Id: ipack.F,v 1.1.1.1 1996/02/15 17:53:26 mclareni Exp $
*
* $Log: ipack.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION IPACK (N,I)
C
C
      DIMENSION N(2)
C
      IPACK=0
         DO 5 J=1,I
    5 CALL STORE6 (IPACK,J,N(J))
      RETURN
      END
