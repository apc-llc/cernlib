*
* $Id: zepcls.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepcls.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPCLS(IZ,LUN,IERR)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       IF LUN IS INPUT UNIT  DO NOTHING                         *
C.    *       IF LUN IS OUTPUT      CALL EPCLOS                        *
C.    *       IF LUN=0  CLOSE ALL OPEN FILES                           *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
      DIMENSION IZ(1)
C.
C.    ------------------------------------------------------------------
C.
      IERR   = 0
      JZ     = IZ(1)
      JD     = IZ(JZ - 3)
      IF (JD.LE.0)                               GO TO 99
      NL     = IZ(JD) - 2
C
      DO 10 NLUN = 1,NL
      IF (LUN.NE.0.AND.LUN.NE.IZ(JD + NLUN))     GO TO 10
C
      JBUF   = IZ(JD - NLUN)
      IF (JBUF.LE.0)                             GO TO 10
      LU=IZ(JD+NLUN)
C
      IF(IZ(JBUF+1).GT.3) CALL EPCLOS(LU,IZ(JBUF+3),IERR)
  10  CONTINUE
C
  99  RETURN
      END
