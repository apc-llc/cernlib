*
* $Id: zdgarb.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zdgarb.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZDGARB(IZ,NLUN)
C
C     ******************************************************************
C     *                                                                *
C     *       PERFORMS A GARBAGE COLLECTION ON MASS STORAGE            *
C     *       ALL USED RECORDS BECOME CONSECUTIVE                      *
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
      LUN    = IZ(JD + NLUN)
      LREC   = IZ(JDIR + 3)
      KEYOLD = 0
C
C             CREATES TEMPORARY BANK TO HANDLE RECORD BUFFER
C
      CALL ZBOOKN(IZ,IZ(JZ-3),LREC,0,'*GAR',1)
      JGARB  = IZ(JZ - 3)
      IF (JGARB.EQ.0)                            GO TO 99
C
C             SEARCH NEXT ZERO RECORD
C
      JD     = IZ(JZ - 2)
      JDIR   = IZ(JD - NLUN)
      NZERO  = IZ(JDIR + 8)
      NONE   = NZERO + 1
  10  IF (IZ(JDIR + 10 + NZERO).EQ.0)            GO TO 20
      NZERO  = NZERO + 1
      GO TO 10
C
C             SEARCH NEXT NON ZERO RECORD
C
  20  IF (NONE.LE.NZERO)NONE = NZERO + 1
  30  IF (NONE.GT.IZ(JDIR + 2))                  GO TO 60
      IF (IZ(JDIR + 10 + NONE).NE.0)             GO TO 40
      NONE   = NONE + 1
      GO TO 30
C
C             COPY NONE TO NZERO
C
  40  CALL ZIODO(LUN,NONE ,LREC,IZ(JGARB+1),LREC,1)
      CALL ZIODO(LUN,NZERO,LREC,IZ(JGARB+1),LREC,2)
C
C             RESET POINTERS IN IDS BANK
C
      IF (IZ(JDIR + 10 + NONE).EQ.KEYOLD)        GO TO 50
      KEYOLD = IZ(JDIR + 10 + NONE)
      CALL ZSEARC(IZ,NLUN,KEYOLD,LOC)
      IF (LOC.LE.0)                              GO TO 91
      IZ(LOC + 1) = NZERO
  50  IZ(JDIR + 10 + NZERO) = IZ(JDIR + 10 + NONE)
      IZ(JDIR + 10 + NONE) = 0
      NONE   = NONE + 1
      NZERO  = NZERO + 1
      GO TO 30
C
C             RESET FIRST NON ZERO RECORD
C
  60  IZ(JDIR + 8) = NZERO
      CALL ZDELET(IZ,IZ(JZ-3))
      GO TO 99
C
C             STRUCTURE HAS BEEN OVERWRITTEN
C
  91  CALL ZERROR(IZ,400,'ZDGARB',KEYOLD)
C
  99  RETURN
      END
