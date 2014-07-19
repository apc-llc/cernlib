*
* $Id: zswap.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zswap.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSWAP(IZ,IDA,IDB)
C
C     ******************************************************************
C     *                                                                *
C     *       INTERCHANGE BANKS  IDA  AND  IDB                         *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),IDA(1),IDB(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      ID1    = IDA(1)
      ID2    = IDB(1)
      JZ     = IZ(1)
      JZ16   = IZ(JZ + 16)
C
C             CHECK IF ID1 IS A VALID POINTER
C
      IF (ZIDOK(IZ,ID1))                         GO TO 21
      ID1    = 0
C
C             CHECK IF ID2 IS A VALID POINTER
C
 21   IF (ZIDOK(IZ,ID2))                         GO TO 31
      ID2    = 0
C
C             DO THE SWAPING OF THE BANKS
C
 31   CONTINUE
      IF (ID1.EQ.0 .AND. ID2.EQ.0)               GO TO 999
      IF (ID1.EQ.0)                              GO TO 101
      L1     = IZ(ID1)
      ILST1  = ID1 + L1 + 2
      NID1   = IZ(ILST1)
 101  CONTINUE
      IF (ID2.EQ.0)                              GO TO 102
      L2     = IZ(ID2)
      ILST2  = ID2 + L2 + 2
      NID2   = IZ(ILST2)
 102  CONTINUE
      IDA(1) = ID2
      IDB(1) = ID1
      IF (ID1.EQ.0.OR.ID2.EQ.0)                  GO TO 103
      IZ(ILST1) = NID2
      IZ(ILST2) = NID1
      IZ(NID1) = ID2
      IZ(NID2) = ID1
 103  IF (ID1.NE.0)                              GO TO 104
      IZ(ILST2) = LOCF(IDA(1)) + JZ16 + 1
      IZ(JZ + 12) = MIN0(IZ(ILST2),IZ(JZ + 12))
      IZ(JZ + 13) = MAX0(IZ(ILST2),IZ(JZ + 13))
      IZ(NID2) = 0
 104  IF (ID2.NE.0)                              GO TO 105
      IZ(ILST1) = LOCF(IDB(1)) + JZ16 + 1
      IZ(JZ + 12) = MIN0(IZ(ILST1),IZ(JZ + 12))
      IZ(JZ + 13) = MAX0(IZ(ILST1),IZ(JZ + 13))
      IZ(NID1) = 0
C
C             UPDATE LOCAL POINTERS
C
 105  JLOC   = IZ(JZ - 1)
      IF (JLOC.EQ.0)                             GO TO 999
      NLOC   = IZ(JLOC) - 2
      IF (NLOC.LE.0)                             GO TO 999
C
      DO 211 I = 1,NLOC
      JL     = IZ(JLOC + I)
      IF (JL.EQ.0)                               GO TO 211
      NL     = IZ(JL)
      IF (NL.LE.0)                               GO TO 211
C
      DO 212 J = 1,NL
      IF (IZ(JL + J).EQ.0)                       GO TO 212
      IF (IZ(JL + J).EQ.ID1) IZ(JL + J) = - ID2
      IF (IZ(JL + J).EQ.ID2) IZ(JL + J) = - ID1
 212  CONTINUE
C
 211  CONTINUE
C
      DO 221 I = 1,NLOC
      JL     = IZ(JLOC + I)
      IF (JL.EQ.0)                               GO TO 221
      NL     = IZ(JL)
      IF (NL.LE.0)                               GO TO 221
C
      DO 222 J = 1,NL
      IF (IZ(JL + J).LT.0) IZ(JL + J) = - IZ(JL + J)
 222  CONTINUE
C
 221  CONTINUE
C
 999  RETURN
C
      END
