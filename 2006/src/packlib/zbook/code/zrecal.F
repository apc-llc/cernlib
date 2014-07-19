*
* $Id: zrecal.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zrecal.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZRECAL(IZ,ID,LUN)
C
C     ******************************************************************
C     *                                                                *
C     *       SAME AS ZREAD + ZSCR                                     *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),ID(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      IDD    = ID(1)
      IF (ZIDOK(IZ,ID))  RETURN
      KEY    = - IDD
      ID(1)  = 0
C
      CALL ZREAD(IZ,ID,LUN,KEY)
      IF (ID(1).EQ.0)  RETURN
C
      CALL ZSCR(IZ,LUN,KEY)
C
      RETURN
      END
