*
* $Id: zptree.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zptree.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZPTREE(IZ,ID,LABEL,LFRMT)
C
C     ******************************************************************
C     *                                                                *
C     *      PRINTS THE STRUCTURE DEPENDING ON BANK  ID                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      DIMENSION LABEL(1)
      DIMENSION JD(51),NL(51),NM(51)
C
      DATA MAXLEV/50/
C
C     ------------------------------------------------------------------
C
      IF (ID(1).EQ.0)  RETURN
      JZ     = IZ(1)
      LOUT   = IZ(JZ + 4)
      I      = 1
      JD(I)  = ID(1)
      JDI    = JD(I)
      LEVEL  = I - 1
      WRITE(LOUT,6001) LEVEL
      LLEVL  = LEVEL
      CALL ZPRINT(IZ,JDI,LABEL,LFRMT,0,0)
      ND     = IZ(JDI)
      IL     = JDI + ND + 1
      NID    = IZ(IL + 1)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 940
      IF (NID.GT.IZ(JZ + 13))                    GO TO 940
      NL(I)  = IZ(IL)
      NM(I)  = NL(I)
      IF (NL(I).LE.0)                            GO TO 999
C
 21   CONTINUE
      JDI    = JD(I)
      NLI    = NL(I)
      NMI    = NM(I)
      NLII   = NMI - NLI + 1
      JD(I + 1) = IZ(JDI - NLII)
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
      NID    = IZ(IL + 1)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 940
      IF (NID.GT.IZ(JZ + 13))                    GO TO 940
      NL(I + 1) = IZ(IL)
      NM(I + 1) = NL(I + 1)
      LEVEL  = I
      IF (LLEVL.NE.LEVEL) WRITE(LOUT,6001) LEVEL
      LLEVL  = LEVEL
      CALL ZPRINT(IZ,JDI,LABEL,LFRMT,0,0)
      IF (NL(I + 1).LE.0)                        GO TO 45
      I      = I + 1
      IF (I.GT.MAXLEV)                           GO TO 980
      GO TO 21
C
C             ERROR IN STRUCTURE
C
 940  CALL ZERROR(IZ,400,'ZPTREE',ID)
      GO TO 999
C
C             LEVEL TOO BIG
C
 980  CALL ZERROR(IZ,800,'ZPTREE',ID)
C
 999  RETURN
 6001 FORMAT(//,10X,' +++ LEVEL ',I3)
      END
