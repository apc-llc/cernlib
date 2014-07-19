*
* $Id: zlocal.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zlocal.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZLOCAL(IZ,IAD)
C
C     ******************************************************************
C     *                                                                *
C     *       STORE ADDRESS OF LOCAL USER POINTERS TO BE UPDATED       *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),IAD(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      LOCAD  = LOCF(IAD(1)) + IZ(JZ + 16) + 1
      IF (IZ(JZ - 1).NE.0)                       GO TO 10
C
C             CREATE BANK TO STORE FIRST ADDRESS OF LOCAL POINTERS
C
      CALL ZBOOKN(IZ,IZ(JZ-1),1,0,'*LOC',1)
      JLOC   = IZ(JZ - 1)
      IF (JLOC.EQ.0)                             GO TO 99
      IZ(JLOC + 1) = LOCAD
      GO TO 99
C
C             CHECK IF ADDRESS ALREADY STORED
C
  10  JLOC   = IZ(JZ - 1)
      NLOC   = IZ(JLOC) - 2
      DO 20 I = 1,NLOC
      IF (IZ(JLOC + I).EQ.LOCAD)                 GO TO 99
  20  CONTINUE
C
      CALL ZPUSHS(IZ,JLOC,1,0)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      IZ(JLOC + NLOC + 1) = LOCAD
C
  99  RETURN
      END
