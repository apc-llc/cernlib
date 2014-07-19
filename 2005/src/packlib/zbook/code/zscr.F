*
* $Id: zscr.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zscr.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSCR(IZ,LUN,KEY)
C
C     ******************************************************************
C     *                                                                *
C     *       SCRATCHES  BANK  WITH PASSWORD KEY ON MASS STORAGE       *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 95
      NL     = IZ(JD) - 2
      NLUN   = 1
   5  IF (IZ(JD + NLUN).EQ.LUN)                  GO TO 7
      NLUN   = NLUN + 1
      IF (NLUN.LE.NL)                            GO TO 5
      GO TO 95
C
   7  CALL ZSEARC(IZ,NLUN,KEY,LOC)
      IF (LOC.LE.0)                              GO TO 91
C
C             RESET RECORD POINTERS
C
      JDIR   = IZ(JD - NLUN)
      IZ(JDIR + 10) = 1
      IF     = IZ(LOC + 1)
      NW     = IABS(IZ(LOC + 2))
      NR     = (NW + IZ(JDIR + 3) - 1) / IZ(JDIR + 3)
      DO 10 I = 1,NR
  10  IZ(JDIR + 9 + IF + I) = 0
      IZ(JDIR + 7) = IZ(JDIR + 7) + NR
      IF (IF.LT.IZ(JDIR + 8))IZ(JDIR + 8) = IF
C
C             DELETES ID FROM ADDRESS TABLE
C
      N      = IZ(JDIR + 5) + JDIR - LOC - 3
      IF (N.EQ.0)                                GO TO 30
      DO 20 I = 1,N
  20  IZ(LOC + I - 1) = IZ(LOC + I + 2)
C
  30  IZ(JDIR + 6) = IZ(JDIR + 6) + 1
      IZ(JDIR + 5) = IZ(JDIR + 5) - 3
      GO TO 99
C
C             ID DOES NOT EXIST ON MASS STORAGE
C
  91  CALL ZERROR(IZ,700,'ZSCR  ',0)
      GO TO 99
C
C             LUN IS UNDEFINED
C
  95  CALL ZERROR(IZ,900,'ZSCR  ',0)
C
  99  RETURN
      END
