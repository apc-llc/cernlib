*
* $Id: zerror.F,v 1.1.1.1 1996/03/08 12:01:12 mclareni Exp $
*
* $Log: zerror.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:12  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZERROR(IZ,IERR,KEY,ID)
C
C     ******************************************************************
C     *                                                                *
C     *       PRINTS ERROR MESSAGES ON UNIT LOUT                       *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),KEY(1),LAB(6),ID(1)
C
C     ------------------------------------------------------------------
C
C             CALL USER ROUTINE
C
      NEWERR = IERR
      JZ     = IZ(1)
      IF(IZ(JZ+19).EQ.0) CALL ZUSER(IZ,NEWERR,KEY,ID)
#if (defined(CERNLIB_SINGLE)||defined(CERNLIB_DOUBLE)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_PDP10))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_BESM6))
      IF(IZ(JZ+19).NE.0) CALL ZJUMP(IZ(JZ+19),IZ,NEWERR,KEY,ID)
#endif
#if defined(CERNLIB_VAX)
      IF(IZ(JZ+19).NE.0) CALL ZJUMP(%VAL(IZ(JZ+19)),IZ,NEWERR,KEY,ID)
#endif
#if defined(CERNLIB_BESM6)
      IF(IZ(JZ+19).NE.0) CALL ZJUMP(IZ,NEWERR,KEY,ID,IZ(JZ+19))
#endif
C
      IF (NEWERR.LE.0)RETURN
C
      IZ(JZ + 6) = NEWERR
      IZ(JZ + 10) = IZ(JZ + 10) - 1
      IF (IZ(JZ + 10).LT.0)RETURN
      LOUT   = IZ(JZ + 4)
      NID    = LOCF(ID(1)) + IZ(JZ + 16) + 1
      CALL UCTOH1(KEY,LAB,6)
      WRITE(LOUT,1000)NEWERR,LAB,NID
      IF (IZ(JZ + 10).EQ.0)WRITE(LOUT,2000)
C
 1000 FORMAT(11X,'***** ERROR NO =',I4,2X,'IN CALLING ROUTINE ',
     + 6A1,5X,'NID =',I6,/)
 2000 FORMAT(/11X,'***** MAXIMUM NUMBER OF PRINTED MESSAGES REACHED IN'
     +,' ZBOOK'/)
C
      RETURN
      END
