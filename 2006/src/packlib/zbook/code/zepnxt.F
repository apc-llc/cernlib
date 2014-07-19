*
* $Id: zepnxt.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepnxt.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPNXT (IZ,LUN,IDCAT,IERR)
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       READS NEXT CATALOG AND RETURN CATALOG IDENTIFIER         *
C     *       IN IDCAT                                                 *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1), IH(4)
C
C     ------------------------------------------------------------------
C
      IERR   = 0
      IDS    = IZ(1)
      IZ(IDS+6)=0
C
C     CHECK IF LUN IS DEFINED
C
      JBUF   = IZ(IDS - 3)
      IF (JBUF.LE.0)                             GO TO 10
C
      NUNIT  = IZ(JBUF) - 2
      DO 5 I=1,NUNIT
      IF(IZ(JBUF+I).NE.LUN)GO TO 5
      LBUF=I
      GO TO 20
   5  CONTINUE
C
C     LUN IS UNDEFINED
C
   10 CALL ZERROR (IZ,900,'ZEPNXT',0)
      GO TO 40
C
C     LUN IS DEFINED
C
   20 JBUF   = IZ(JBUF - LBUF) + 3
C
   30 CALL EPFHDR (LUN,4,IH,IZ(JBUF),IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)               GO TO 40
      IF (IH(2).NE.1)                            GO TO 30
C
      IDCAT  = IH(4)
      JF     = JBUF - 2
      IZ(JF) = 1
      IZ(JF + 1) = IDCAT
C
   40 IERR   = IERR + IZ(IDS + 6)
C
      RETURN
      END
