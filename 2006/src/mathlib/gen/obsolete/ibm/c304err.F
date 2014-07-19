*
* $Id: c304err.F,v 1.1.1.1 1996/04/01 15:03:05 mclareni Exp $
*
* $Log: c304err.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:05  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_IBMMVS)
      SUBROUTINE C304ERR(IRC,IERN,X)
C
      CHARACTER*80 ERRTXT
C---  Set standard corrective action
      IRC=0
      IF(IERN.EQ.291) THEN
        WRITE(ERRTXT,101) X
        CALL MTLPRT('ALGAMA','C304.1',ERRTXT)
      ELSEIF(IERN.EQ.301) THEN
        WRITE(ERRTXT,101) X
        CALL MTLPRT('DLGAMA','C304.1',ERRTXT)
      ENDIF
      RETURN
  101 FORMAT('NON-POSITIVE ARGUMENT  X = ',1P,E15.6)
      END
#endif
