*
* $Id: zopen.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zopen.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZOPEN(IZ,LUN)
C
C     ******************************************************************
C     *                                                                *
C     *       DIRECTORY FROM MASS STORAGE FILE LUN IS READ             *
C     *       IN MASS STORAGE DESCRIPTION BANK                         *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),IDIR(3)
C
C     ------------------------------------------------------------------
C
      CALL ZIODO(LUN,0,3,IDIR,3,1)
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      IF(IZ(JZ-2).EQ.0) CALL ZBOOKN(IZ,IZ(JZ-2),1,1,'*ZDI',1)
      JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 99
      NL     = IZ(JD) - 2
      NLUN   = 1
C
   5  IF (IZ(JD + NLUN).EQ.0.AND.IZ(JD - NLUN).EQ.0) GO TO 7
      NLUN   = NLUN + 1
      IF(NLUN.GT.NL) CALL ZPUSHS(IZ,JD,1,1)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      GO TO 5
   7  CALL ZBOOKN(IZ,IZ(JD-NLUN),IDIR(3)-2,0,'*LUN',LUN)
      JD     = IZ(JZ - 2)
      JDIR   = IZ(JD - NLUN)
      IF (JDIR.EQ.0)                             GO TO 99
      CALL ZIODO(LUN,IDIR(2),IDIR(1),IZ(JDIR+1),IDIR(3),1)
      IZ(JDIR + 1) = LUN
      IZ(JDIR+10)=0
      IZ(JD + NLUN) = LUN
  99  RETURN
      END
