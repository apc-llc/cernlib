*
* $Id: c302err.F,v 1.1.1.1 1996/04/01 15:03:05 mclareni Exp $
*
* $Log: c302err.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:05  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
      SUBROUTINE C302ERR(IRC,IERN,X)
C
      CHARACTER*80 ERRTXT
C---  Set standard corrective action
      IRC=0
      IF(IERN.EQ.290) THEN
        WRITE(ERRTXT,101) X
        CALL MTLPRT('GAMMA' ,'C302.1',ERRTXT)
      ELSEIF(IERN.EQ.300) THEN
        WRITE(ERRTXT,101) X
        CALL MTLPRT('DGAMMA','C302.1',ERRTXT)
      ENDIF
      RETURN
  101 FORMAT('ARGUMENT IS NEGATIVE = ',1P,E15.1)
      END
#endif
