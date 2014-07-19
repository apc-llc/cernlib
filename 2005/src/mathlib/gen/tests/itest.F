*
* $Id: itest.F,v 1.1.1.1 1996/04/01 15:01:12 mclareni Exp $
*
* $Log: itest.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:12  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      FUNCTION ITEST(CODE,TEST)
#include "iorc.inc"
      CHARACTER*(*) CODE
      LOGICAL TEST
      IF(TEST) THEN
        PRINT 1000,NTEST,CODE
        IF (LOUT .NE. 6) WRITE(LOUT,1000) NTEST,CODE
        ITEST=0
      ELSE
        PRINT 1001,NTEST,CODE
        IF (LOUT .NE. 6) WRITE(LOUT,1001) NTEST,CODE
        ITEST=1
      ENDIF
      NFAIL=NFAIL+ITEST
1000  FORMAT(' Test#',I3,' ( ',A,' ):     completed successfully')
1001  FORMAT(' Test#',I3,' ( ',A,' ): *** failed ***')
      RETURN
      END
