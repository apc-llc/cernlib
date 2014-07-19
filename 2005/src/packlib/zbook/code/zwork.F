*
* $Id: zwork.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zwork.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZWORK(IZ,IWORK)
C
C     ******************************************************************
C     *                                                                *
C     *       RESERVES IWORK WORDS OF WORKING SPACE                    *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      N      = IWORK - IZ(JZ + 2)
      IF (N.EQ.0)                                GO TO 99
      IF (N.LE.IZ(JZ + 1))                       GO TO 10
      CALL ZGARB(IZ)
      IF (N.LE.IZ(JZ + 1))                       GO TO 10
      NMOVE  = N - IZ(JZ + 1) + 1
      CALL ZMOVE(IZ,NMOVE)
      IF (N.LE.IZ(JZ + 1))                       GO TO 10
      CALL ZERROR(IZ,100,'ZWORK ',0)
      RETURN
C
  10  IOLD   = IZ(JZ + 3) + IZ(JZ + 2) + 2
      IZ(JZ + 2) = IWORK
      IZLAST = IZ(JZ + 15)
      IZ(1)  = JZ + N
C
      CALL ZSHIFT(IZ,IOLD,IZLAST,N,'ZWORK ')
C
  99  RETURN
      END
