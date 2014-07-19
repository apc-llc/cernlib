*
* $Id: zagain.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zagain.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZAGAIN(IZ,ID)
C
C     ******************************************************************
C     *                                                                *
C     *       DELETES BANK  ID AND ALL BANKS  CREATED AFTERWARDS       *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      IF (ZIDOK(IZ,ID))                          GO TO 10
      CALL ZERROR(IZ,300,'ZAGAIN',ID)
      RETURN
C
  10  JZ     = IZ(1)
      IZ(JZ + 6) = 0
      IDATA=ID(1)
      ILINK=IDATA+IZ(IDATA)+1
      IFIRST=IDATA-IZ(ILINK)-1
      IF (IFIRST.GT.IZ(JZ + 14))                 GO TO 30
      IF (IFIRST.LT.1)                           GO TO 30
C
C             UPDATE LOCAL POINTERS
C
      IF(IZ(JZ-1).NE.0) CALL ZUPLOC(IZ,IFIRST,IZ(JZ+15),0)
C
      IZ(JZ + 15) = IFIRST
      IZ(JZ + 1) = IZ(JZ + 14) - IFIRST
      IAGAIN = IFIRST
  20  NLONG  = IZ(IFIRST)
      IF (NLONG.EQ.0)                            GO TO 99
      N      = IABS(NLONG)
      IFIRST = IFIRST + N
      IF (IFIRST.GT.IZ(JZ + 14))                 GO TO 30
      IF (NLONG.GT.0)                            GO TO 25
      IZ(JZ + 5) = IZ(JZ + 5) - N
      GO TO 20
C
  25  NID    = IZ(IFIRST - 1)
      IF (NID.LT.IZ(JZ + 12))                    GO TO 30
      IF (NID.GT.IZ(JZ + 13))                    GO TO 30
      IZ(NID) = 0
      GO TO 20
C
  30  NBANK  = IZ(JZ + 3)
      IF (NBANK.LE.0)                            GO TO 60
      IF (NBANK.GT.IZ(JZ + 14))                  GO TO 60
      DO 50 I = 1,NBANK
      IF(IZ(I+1).GE.IDATA)IZ(I+1)=0
  50  CONTINUE
C
  60  CALL ZERROR(IZ,400,'ZAGAIN',ID)
      RETURN
C
  99  IZ(IAGAIN) = 0
      ID(1)=0
      RETURN
      END
