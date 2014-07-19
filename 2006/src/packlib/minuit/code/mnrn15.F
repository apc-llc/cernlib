*
* $Id: mnrn15.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnrn15.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNRN15(VAL,INSEED)
#include "minuit/d506dp.inc"
C         This is a super-portable random number generator.
C         It should not overflow on any 32-bit machine.
C         The cycle is only ~10**9, so use with care!
C         Note especially that VAL must not be undefined on input.
C                    Set Default Starting Seed
      PARAMETER (THREE=3.0)
      DATA ISEED/12345/
      IF (VAL .EQ. THREE)  GO TO 100
C
      INSEED = ISEED
      K = ISEED/53668
      ISEED = 40014*(ISEED-K*53668) - K*12211
      IF (ISEED .LT. 0) ISEED = ISEED + 2147483563
      VAL = REAL(ISEED) * 4.656613E-10
      RETURN
C               "entry" to set seed, flag is VAL=3.
  100 ISEED = INSEED
      RETURN
      END
