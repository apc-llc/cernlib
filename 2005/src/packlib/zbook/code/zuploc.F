*
* $Id: zuploc.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zuploc.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZUPLOC(IZ,IFIRST,ILAST,NP)
C
C     ******************************************************************
C     *                                                                *
C     *       UPDATE USER LOCAL POINTERS POINTING IN                   *
C     *       THE AREA  IZ(IFIRST)---> IZ(ILAST)                       *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      JLOC   = IZ(JZ - 1)
      NLOC   = IZ(JLOC) - 2
      IF (NLOC.LE.0)                             GO TO 99
C
      NPUSH  = NP
      DO 20 I = 1,NLOC
      JL     = IZ(JLOC + I)
      IF (JL.EQ.0)                               GO TO 20
      NL     = IZ(JL)
      IF (NL.LE.0)                               GO TO 20
C
      DO 10 J = 1,NL
      IF (IZ(JL + J).LT.IFIRST)                  GO TO 10
      IF (IZ(JL + J).GT.ILAST)                   GO TO 10
      IF (NPUSH.NE.0)IZ(JL + J) = IZ(JL + J) + NPUSH
      IF (NPUSH.EQ.0)IZ(JL + J) = 0
  10  CONTINUE
  20  CONTINUE
C
  99  RETURN
      END
