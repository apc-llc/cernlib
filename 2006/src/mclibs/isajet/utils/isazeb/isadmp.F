*
* $Id: isadmp.F,v 1.10 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isadmp.F,v $
* Revision 1.10  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ISADMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       dump ISAJET event banks
C-
C-   Created   6-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      LOGICAL ISADMP
      CHARACTER*4 PATH
      INTEGER PRUNIT,DMPUNI
      EXTERNAL PRCAEP
C----------------------------------------------------------------------
C
      PRUNIT=DMPUNI()
      CALL PRTEVZ(PRUNIT)
      CALL PATHGT(PATH)
      IF(PATH.EQ.'FAKE') THEN
        PRUNIT=DMPUNI()
        WRITE(PRUNIT,100) PATH
        CALL DMPANY('CAEP',PRCAEP)
      ENDIF
      ISADMP=.TRUE.
  999 RETURN
  100 FORMAT(//,'***********  CAEP dumped for path= ',A)
      END
