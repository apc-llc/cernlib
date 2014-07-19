*
* $Id: zmigra.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zmigra.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZMIGRA(IZ,ID,LUN)
C
C     ******************************************************************
C     *                                                                *
C     *       SAME AS ZWRITE + ZDROP                                   *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 900
      NL     = IZ(JD) - 2
      NLUN   = 1
 5    IF (IZ(JD + NLUN).EQ.LUN)                  GO TO 7
      NLUN   = NLUN + 1
      IF (NLUN.LE.NL)                            GO TO 5
      GO TO 900
 7    JDIR   = IZ(JD - NLUN)
      IF (.NOT.ZIDOK(IZ,ID))                     GO TO 300
      JKEY   = IZ(JDIR + 4)
      LBLG   = IZ(JDIR + 5)
      KEY    = 900000
      NKEY   = (LBLG - JKEY) / 3
      IF (NKEY.LE.0)                             GO TO 9
      KEYBLG = IZ(JDIR + LBLG - 3)
      IF (KEYBLG.GE.900000) KEY = KEYBLG + 1
C
 9    CALL ZWRITE(IZ,ID,LUN,KEY)
      IF (IZ(JZ + 6).NE.0)  RETURN
C
      IDATA=ID(1)
      ILAST=IDATA+IZ(IDATA)+2
      NID=IZ(ILAST)
      CALL ZDROP(IZ,ID)
      IF (IZ(JZ + 6).NE.0)  RETURN
C
      ID(1)  = - KEY
      IZ(NID) = - KEY
C
 99   RETURN
 300  CALL ZERROR(IZ,300,'ZMIGRA',ID)
      GO TO 99
 900  CALL ZERROR(IZ,900,'ZMIGRA',ID)
      GO TO 99
      END
