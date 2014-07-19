*
* $Id: zdrop.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zdrop.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZDROP(IZ,ID)
C
C     ******************************************************************
C     *                                                                *
C     *       DROPS BANK  ID                                           *
C     *       IF ID IS A BANK OF POINTERS,ZDROP DROPS                  *
C     *       LOWER BANKS AS WELL (UP TO 10 LEVELS)                    *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      DIMENSION JD(51),NL(51)
      LOGICAL ZIDOK
C
      DATA MAXLEV/50/
C
C     ------------------------------------------------------------------
C
      IF (ZIDOK(IZ,ID))                          GO TO 10
      CALL ZERROR(IZ,300,'ZDROP ',ID)
      RETURN
 10   JZ     = IZ(1)
      IZ(JZ + 6) = 0
      I      = 1
      JD(I)  = ID(1)
      ID(1)  = 0
      JDI    = JD(I)
      ND     = IZ(JDI)
      IL     = JDI + ND + 1
      NL(I)  = IZ(IL)
      IFIRST = JDI - NL(I) - 1
      IF(IZ(JZ-1).NE.0) CALL ZUPLOC(IZ,IFIRST,IL,0)
      IF (IZ(IL).EQ.0.AND.IZ(IL + 2).EQ.0)       GO TO 60
      IZ(JZ + 5) = IZ(JZ + 5) + IZ(IFIRST)
      IZ(IFIRST) = - IZ(IFIRST)
      NID    = IZ(IL + 1)
      IZ(NID) = 0
      IF (NL(I).LE.0)                            GO TO 999
C
 21   CONTINUE
      JDI    = JD(I)
      NLI    = NL(I)
      JD(I + 1) = IZ(JDI - NLI)
      IF (JD(I + 1).NE.0)                        GO TO 50
 45   CONTINUE
      NL(I)  = NL(I) - 1
      IF (NL(I).NE.0)                            GO TO 21
      I      = I - 1
      IF (I.LE.0)  GO TO 999
      GO TO 45
 50   CONTINUE
      JDI    = JD(I + 1)
      ND     = IZ(JDI)
      IL     = JDI + ND + 1
      NL(I + 1) = IZ(IL)
      IFIRST = JDI - NL(I + 1) - 1
      IF(IZ(JZ-1).NE.0) CALL ZUPLOC(IZ,IFIRST,IL,0)
      IZ(JZ + 5) = IZ(JZ + 5) + IZ(IFIRST)
      IZ(IFIRST) = - IZ(IFIRST)
      NID    = IZ(IL + 1)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 940
      IF (NID.GT.IZ(JZ + 13))                    GO TO 940
      IZ(NID) = 0
      IF (NL(I + 1).LE.0)                        GO TO 45
      I      = I + 1
      IF (I.GT.MAXLEV)                           GO TO 980
      GO TO 21
C
C             LAST DATA BANK IN MEMORY ===> DELETE IT
C
  60  IZ(JZ + 15) = IFIRST
      IZ(JZ + 1) = IZ(JZ + 14) - IFIRST
      IZ(IFIRST) = 0
      NID    = IZ(IL + 1)
      IZ(NID) = 0
      GO TO 999
C
C             ERROR IN STRUCTURE
C
 940  CALL ZERROR(IZ,400,'ZDROP ',ID)
      GO TO 999
C
C             LEVEL TOO BIG
C
 980  CALL ZERROR(IZ,800,'ZDROP ',ID)
C
 999  RETURN
      END
