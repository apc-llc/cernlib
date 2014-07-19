*
* $Id: zepcat.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zepcat.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEPCAT (IZ,LUN,JCAT,IDCAT,IERR)
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       READS NEXT CATALOG AND RETURN LIST OF KEYS               *
C     *       IN BANK POINTED BY JCAT                                  *
C     *                                                                *
C     *       IF JCAT DOES NOT EXIST IT IS CREATED                     *
C     *       IF IT EXISTS ITS SIZE IS ADJUSTED TO THE CURRENT         *
C     *       SIZE OF THE CATALOG                                      *
C     *                                                                *
C     *       THE CATALOG IDENTIFIER IS RETURNED IN IDCAT              *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1), JCAT(1)
      LOGICAL ZIDOK
C
C     ------------------------------------------------------------------
C
      IERR   = 0
      IDS    = IZ(1)
      IZ(IDS+6)=0
C
C     CHECK IF HEADER ALREADY READ
C
      JBUF   = IZ(IDS - 3)
      IF (JBUF.LE.0)                             GO TO 10
C
      NUNIT  = IZ(JBUF) - 2
      LBUF   = IUCOMP (LUN,IZ(JBUF + 1),NUNIT)
      IF (LBUF.EQ.0)                             GO TO 10
C
      JBUF   = IZ(JBUF - LBUF) + 1
      IF (IZ(JBUF).NE.1)                         GO TO 10
C
C     ALREADY READ
C
      IDCAT  = IZ(JBUF + 1)
      GO TO 20
C
C     LOOK FOR NEXT CATALOG
C
   10 CALL ZEPNXT (IZ,LUN,IDCAT,IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)               GO TO 50
C
C     COMPUTE 'NKEY'
C
   20 CALL EPGETW (LUN,20,IW20,IERR)
      IF (IERR.NE.0)                             GO TO 50
C
      CALL EPGETW (LUN,21,IW21,IERR)
      IF (IERR.NE.0)                             GO TO 50
C
      NKEY   = (IW20 - IW21) / 2
C
C     CHECK IF JCAT IS A VALID POINTER
C
      IF (.NOT.ZIDOK(IZ,JCAT))                   GO TO 30
C
C     JCAT IS A VALID POINTER
C
      KCAT   = JCAT(1)
      NPUSH  = NKEY - IZ(KCAT) + 2
      CALL ZPUSHS(IZ,JCAT(1),NPUSH,0)
      GO TO 40
C
C     BANK 'JCAT' DOES NOT EXIST
C
   30 CALL ZBOOKN (IZ,JCAT(1),NKEY,0,'*CAT',1)
C
   40 IF (IZ(IDS + 6).NE.0)                      GO TO 50
C
      JBUF   = IZ(IDS - 3)
      NUNIT  = IZ(JBUF) - 2
      LBUF   = IUCOMP (LUN,IZ(JBUF + 1),NUNIT)
      JBUF   = IZ(JBUF - LBUF) + 3
      KCAT   = JCAT(1)
C
C     READ IN CATALOG
C
      CALL EPFRD (LUN,13,NW,IZ(KCAT+1),IZ(JBUF),IERR)
      IF (IERR.NE.0)                             GO TO 50
C
      JF     = JBUF - 2
      IZ(JF) = 2
C
      CALL ZFRIBM (IZ(KCAT+1),NKEY,2)
C
   50 IERR   = IERR + IZ(IDS + 6)
C
      RETURN
      END
