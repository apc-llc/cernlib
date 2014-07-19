*
* $Id: random.F,v 1.10 2001/10/08 14:03:18 mclareni Exp $
*
* $Log: random.F,v $
* Revision 1.10  2001/10/08 14:03:18  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION RANDOM()
C------------------------------------------------------------------
C-
C- Generate a random number
C-
C------------------------------------------------------------------
C
      RANDOM=RANF()
C
      RETURN
      END
