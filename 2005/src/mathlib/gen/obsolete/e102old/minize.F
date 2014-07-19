*
* $Id: minize.F,v 1.1.1.1 1996/04/01 15:03:06 mclareni Exp $
*
* $Log: minize.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:06  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if defined(CERNLIB_FORTRAN)||!defined(CERNLIB_CDC)
      FUNCTION MINIZE(IA, N)
          DIMENSION IA(1)
          MINIZE    =  1
          I         =  IA(1)
          IF(N .LE. 1)  RETURN
          DO  10    K  =  2, N
                    IF(IA(K) .GE. I)    GO TO 10
                    MINIZE    =  K
                    I         =  IA(K)
   10               CONTINUE
          RETURN
      END
#endif
