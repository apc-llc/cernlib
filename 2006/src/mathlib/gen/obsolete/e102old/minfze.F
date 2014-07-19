*
* $Id: minfze.F,v 1.1.1.1 1996/04/01 15:03:06 mclareni Exp $
*
* $Log: minfze.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:06  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
#if defined(CERNLIB_FORTRAN)||!defined(CERNLIB_CDC)
      FUNCTION MINFZE(FA, N)
          DIMENSION FA(1)
          MINFZE    =  1
          F         =  FA(1)
          IF(N .LE. 1)  RETURN
          DO  10    K  =  2, N
                    IF(FA(K) .GE. F)    GO TO 10
                    MINFZE    =  K
                    F         =  FA(K)
   10               CONTINUE
          RETURN
      END
#endif
