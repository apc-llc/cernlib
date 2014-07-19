*
* $Id: zsearc.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zsearc.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSEARC(IZ,NLUN,KEY,LOC)
C
C     ******************************************************************
C     *                                                                *
C     *       LOCATES KEYWORD=KEY IN THE ADDRESS TABLE FOR             *
C     *       MASS STORAGE ACCESS   (BINARY SEARCH)                    *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      JD     = IZ(JZ - 2)
      JDIR   = IZ(JD - NLUN)
C
      LOCID  = IZ(JDIR + 4) + JDIR
      LUP    = (IZ(JDIR + 5) - IZ(JDIR + 4)) / 3 + 1
      LDOWN  = 0
C
  10  IF (LUP - LDOWN.LE.1)                      GO TO 40
      LMEAN  = (LUP + LDOWN) / 2
      LOCID  = IZ(JDIR + 4) + 3 * (LMEAN - 1) + JDIR
      IF (KEY - IZ(LOCID))20,99,30
  20  LUP    = LMEAN
      GO TO 10
C
  30  LDOWN  = LMEAN
      LOCID  = LOCID + 3
      GO TO 10
C
  40  LOCID  = - LOCID
C
  99  LOC    = LOCID
      RETURN
      END
