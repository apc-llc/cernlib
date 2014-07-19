*
* $Id: zclose.F,v 1.1.1.1 1996/03/08 12:01:11 mclareni Exp $
*
* $Log: zclose.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:11  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZCLOSE(IZ,LUN)
C
C     ******************************************************************
C     *                                                                *
C     *       CLOSE MASS STORAGE FILE                                  *
C     *       BANK DESCRIPTOR IS DELETED FROM MEMORY                   *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      IZ(JZ + 6) = 0
      JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 95
      NL     = IZ(JD) - 2
      NLUN   = 1
   5  IF (IZ(JD + NLUN).EQ.LUN)                  GO TO 7
      NLUN   = NLUN + 1
      IF (NLUN.LE.NL)                            GO TO 5
      GO TO 95
C
   7  JDIR   = IZ(JD - NLUN)
      IF(IZ(JDIR+10).EQ.0)GO TO 20
      IZ(JDIR+10)=0
      IF (IZ(JDIR + 6).NE.0)                     GO TO 10
      CALL ZPUSHS(IZ,JDIR,3,0)
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
      IZ(JDIR + 6) = 1
C
C             WRITE DIRECTORY AS A NORMAL BANK WITH KEY='ZDIR'
C
  10  CALL ZWRITE(IZ,JDIR,LUN,'ZDIR')
      IF (IZ(JZ + 6).NE.0)                       GO TO 99
C
      CALL ZSEARC(IZ,NLUN,'ZDIR',LOC)
      IF (LOC.LT.0)                              GO TO 99
      IZ(JDIR + 4) = IZ(JDIR + 3)
      IZ(JDIR + 5) = IZ(LOC + 1)
      IZ(JDIR + 6) = JBYT(IZ(LOC + 2),1,18)
C
C             WRITES RECORD 1  GENERAL DIRECTORY
C
      CALL ZIODO(LUN,0,IZ(JDIR+3),IZ(JDIR+4),3,2)
C
C             DELETE DIRECTORY BANK FROM MEMORY
C
#if defined(CERNLIB_CDC)
      IF(LUN.LE.0)GO TO 20
      CALL CLOSMS(LUN)
#endif
#if defined(CERNLIB_NORD)
      CLOSE(LUN)
#endif
  20  CALL ZDELET(IZ,JDIR)
      JD     = IZ(JZ - 2)
      IZ(JD + NLUN) = 0
      GO TO 99
C
C     LUN IS UNDEFINED
C
   95 CALL ZERROR(IZ,900,'ZCLOSE',0)
C
  99  RETURN
      END
