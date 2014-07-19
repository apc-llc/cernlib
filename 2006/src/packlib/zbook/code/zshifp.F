*
* $Id: zshifp.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zshifp.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSHIFP(IZ,IFIRST,IDATA,ILAST,NLP)
C
C     ******************************************************************
C     *                                                                *
C     *       SHIFT POINTERS AREA IN BANK POINTED BY IDATA             *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      NLINK  = IZ(ILAST - 1)
      IZ(ILAST - 1) = NLINK + NLP
      IF (NLINK.EQ.0)                            GO TO 50
C
      IF (NLP.GE.0)                              GO TO 20
      N      = - NLP
      DO 10 I = 1,N
      IF (IZ(IFIRST + I).EQ.0)                   GO TO 10
      CALL ZDROP(IZ,IZ(IFIRST+I))
  10  CONTINUE
  20  CONTINUE
C
      DO 30 I = 1,NLINK
      JP     = IZ(IFIRST + I)
      IF (JP.EQ.0)                               GO TO 30
      ND     = IZ(JP)
      IL     = JP + ND + 2
      IF (IL.LT.0)                               GO TO 90
      IF (IL.GT.IZ(JZ + 14))                     GO TO 90
      NID    = IZ(IL) + NLP
      IF (NID.LT.IZ(JZ + 12))IZ(JZ + 12) = NID
      IF (NID.GT.IZ(JZ + 13))IZ(JZ + 13) = NID
      IZ(IL) = NID
  30  CONTINUE
C
      IF (NLP.GT.0)                              GO TO 40
      IF(IZ(JZ-1).NE.0)CALL ZUPLOC(IZ,IFIRST-NLP+1,IDATA-1,NLP)
      N      = NLINK + NLP
      IF (N.LE.0)                                GO TO 99
      CALL UCOPY2(IZ(IFIRST-NLP+1),IZ(IFIRST+1),N)
      GO TO 99
C
  40  IF(IZ(JZ-1).NE.0)CALL ZUPLOC(IZ,IFIRST+1,IDATA-1,NLP)
      CALL UCOPY2(IZ(IFIRST+1),IZ(IFIRST+NLP+1),NLINK)
C
  50  IF (NLP.LE.0)                              GO TO 99
#if !defined(CERNLIB_BESM6)
      CALL VZERO(IZ(IFIRST+1),NLP)
#endif
#if defined(CERNLIB_BESM6)
      CALL UZERO(IZ(IFIRST+1),1,NLP)
#endif
      GO TO 99
C
C             STRUCTURE HAS BEEN OVERWRITTEN
C
  90  CALL ZERROR(IZ,400,'ZSHIFP',0)
C
  99  RETURN
      END
