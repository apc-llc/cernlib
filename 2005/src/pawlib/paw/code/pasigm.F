*
* $Id: pasigm.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: pasigm.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.00/01 15/12/92  14.57.39  by  O.Couet
*-- Author :    P.Zanarini   03/01/89
      SUBROUTINE PASIGM
#include "paw/pcchar.inc"
      CHARACTER*32 CHEXIT
*
      CALL KUAPPL(LUNAPP,MACFLG,CHEXIT)
      IF (LUNAPP.EQ.5) THEN
        CALL KUGETE(CHTITL,L)
        IF (L.GT.0) CALL SIGMA(CHTITL)
        RETURN
      ENDIF
*
10    CONTINUE
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      CHTITL=' '
      READ (LUNAPP,NUM=NBYT,ERR=100,END=200) CHTITL
#endif
#if !defined(CERNLIB_IBM)||defined(CERNLIB_NEWLIB)||defined(CERNLIB_IBMMVS)
      READ (LUNAPP,'(A)',ERR=100,END=200) CHTITL
#endif
      CHTEMP=CHTITL
      CALL CLTOU(CHTEMP)
      IF (CHTEMP.EQ.CHEXIT) RETURN
      CALL SIGMA(CHTITL)
      GO TO 10
100   PRINT *,'*** PASIGM: Error in reading'
      RETURN
200   PRINT *,'*** PASIGM: End of file'
      END
