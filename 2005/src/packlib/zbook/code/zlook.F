*
* $Id: zlook.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zlook.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZLOOK(IZ,LUN,KEY,LOC)
C
C     ******************************************************************
C     *                                                                *
C     *       RETURN ADDRESS OF BANK WITH KEYWORD KEY                  *
C     *       IN THE DIRECTORY BANK                                    *
C     *       IF(LOC.LE.0)KEY DOES NOT EXIST                           *
C     *       IF(LOC.GT.0)THEN                                         *
C     *             IZ(LOC)=KEY                                        *
C     *             IZ(LOC+1)=NUMBER OF THE FIRST RECORD ONTO DISK     *
C     *             IZ(LOC+2)=NUMBER OF WORDS IN THE BANK              *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1)
C
C     ------------------------------------------------------------------
C
      JZ     = IZ(1)
      LOC    = - 1
      JD     = IZ(JZ - 2)
      IF (JD.EQ.0)                               GO TO 10
      NL     = IZ(JD) - 2
      NLUN   = 1
   5  IF (IZ(JD + NLUN).EQ.LUN)                  GO TO 7
      NLUN   = NLUN + 1
      IF (NLUN.LE.NL)                            GO TO 5
      GO TO 10
C
   7  CALL ZSEARC(IZ,NLUN,KEY,LOC)
      GO TO 99
C
   10 CALL ZERROR(IZ,900,'ZLOOK ',0)
C
  99  RETURN
      END
