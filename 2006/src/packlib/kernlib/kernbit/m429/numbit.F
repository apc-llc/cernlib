*
* $Id: numbit.F,v 1.1.1.1 1996/02/15 17:47:48 mclareni Exp $
*
* $Log: numbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:48  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_FORTRAN))&&(!defined(CERNLIB_F4))
      INTEGER FUNCTION NUMBIT(I)
      LOGICAL BTEST
#include "m429btpw.inc"
      IBCOUN=0
      DO 10 JB=0,NBITPW-1
        IF(BTEST(I,JB)) IBCOUN=IBCOUN+1
  10  CONTINUE
      NUMBIT=IBCOUN
*
      END
#endif
