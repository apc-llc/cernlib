*
* $Id: zmove.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zmove.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZMOVE(IZ,N)
C
C     ******************************************************************
C     *                                                                *
C     *       MOVE END OF DYNAMIC AREA IZ BY N WORDS                   *
C     *       N POSITIVE OR NEGATIVE                                   *
C     *       IF N.GT.0 POSSIBILITY OF MEMORY EXTENSION                *
C     *                                                                *
C     ******************************************************************
C
      COMMON/HFLAG/IH(31)
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      NW     = IZ(JZ + 14)
      NEW    = NW + N
      IF (NW + N.GT.IZ(JZ + 11))RETURN
C
      IF (N)10,99,20
  10  CALL ZGARB(IZ)
      IF (IZ(JZ + 1).GT. - N)                    GO TO 30
      CALL ZERROR(IZ,500,'ZMOVE ',0)
      GO TO 99
C
  20  CALL HISTGO(NEW)
      IF (IH(9).NE.NEW + 1)RETURN
C
  30  IZ(JZ + 14) = NEW
      IZ(JZ + 1) = IZ(JZ + 1) + N
      IF(N.LT.0) CALL HISTGO(NEW)
C
  99  RETURN
      END
