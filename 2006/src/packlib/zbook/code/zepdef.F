*
* $Id: zepdef.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepdef.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPDEF(IZ,LUN,LREC,IERR)
C
C     ******************************************************************
C     *                                                                *
C     *       INITIALISE EP PACKAGE FOR LOGICAL UNIT LUN               *
C     *       LREC IS THE SIZE IN 16 BITS WORDS                        *
C     *       OF THE PHYSICAL RECORD                                   *
C     *       IF(LREC=0)LREC IS SET TO THE DEFAULT EPREAD VALUE        *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
#include "zbook/wlength.inc"
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      IERR   = 0
      IF (IZ(JZ - 3).GT.0)                       GO TO 10
C
C             INITIALISE EP PACKAGE
C
      CALL EPINIT
C
C             CREATE SYSTEM BANK AND ASSOCIATED BUFFER BANK
C
      CALL ZBOOKN(IZ,IZ(JZ-3),1,1,'*ZEP',1)
C
  10  JD     = IZ(JZ - 3)
      IF (JD.LE.0)                               GO TO 99
      NL     = IZ(JD) - 2
      NLUN   = 1
C
  20  IF (IZ(JD - NLUN).LE.0)                    GO TO 30
      NLUN   = NLUN + 1
      IF(NLUN.GT.NL) CALL ZPUSHS(IZ,JD,1,1)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      GO TO 20
C
C             COMPUTE BUFFER LENGTH AS A FUNCTION OF LREC
C
  30  LR     = LREC
      IF (LR.LE.0)LR = 1800
      NW     = (16 * LR - 1) / NBIT + 3
C
      IF(LREC.GT.0) CALL EPSETW(LUN,1,LR,IERR)
      IF (IERR.NE.0)                             GO TO 99
      CALL EPSETW(LUN,5,-1,IERR)
      IF (IERR.NE.0)                             GO TO 99
      CALL EPSETW(LUN,6,-1,IERR)
      IF (IERR.NE.0)                             GO TO 99
      IZ(JD + NLUN) = LUN
C
      CALL ZBOOKN(IZ,IZ(JD-NLUN),NW,0,'*LUN',LUN)
C
  99  IERR   = IERR + IZ(JZ + 6)
      RETURN
      END
