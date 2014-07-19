*
* $Id: zgarb.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zgarb.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZGARB(IZ)
C
C     ******************************************************************
C     *                                                                *
C     *       PERFORMS A GARBAGE COLLECTION                            *
C     *       ALL DROPPED BANKS  ARE DELETED FROM MEMORY               *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IF (IZ(JZ + 5).LE.0)                       GO TO 99
      IFOLD  = JZ + IZ(JZ) + 3
      IFIRST = IFOLD
C
  10  NLONG  = IZ(IFIRST)
      IF (NLONG.EQ.0)                            GO TO 90
      IF (NLONG.GT.0)                            GO TO 20
C
C             DROPPED BANKS
C
      IFIRST = IFIRST - NLONG
      GO TO 10
C
C             LIFE BANKS
C
  20  IF (IFIRST.NE.IFOLD)                       GO TO 30
      IFIRST = IFIRST + NLONG
      IFOLD  = IFIRST
      GO TO 10
C
  30  ILAST  = IFIRST + NLONG - 1
      IF (ILAST.GT.IZ(JZ + 15))                  GO TO 90
      IF (IZ(ILAST + 1).LE.0)                    GO TO 40
      NLONG  = NLONG + IZ(ILAST + 1)
      GO TO 30
C
  40  NPUSH  = IFOLD - IFIRST
      IF (IZ(JZ + 18).GT.IFIRST.AND.IZ(JZ + 18).LT.ILAST)
     +         IZ(JZ+18)=IZ(JZ+18)+NPUSH
      NID    = IZ(ILAST)
      CALL ZSHIFT(IZ,IFIRST,ILAST,NPUSH,'ZGARB ')
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      IFOLD  = IFOLD + NLONG
      IFIRST = ILAST + 1
      GO TO 10
C
  90  IZ(JZ + 15) = IFOLD
      IZ(IFOLD) = 0
      IZ(JZ + 1) = IZ(JZ + 14) - IZ(JZ + 15)
      IZ(JZ + 5) = 0
      IZ(JZ + 9) = IZ(JZ + 9) + 1
C
  99  RETURN
      END
