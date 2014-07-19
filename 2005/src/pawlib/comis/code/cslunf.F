*
* $Id: cslunf.F,v 1.1.1.1 1996/02/26 17:16:32 mclareni Exp $
*
* $Log: cslunf.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:32  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 18/10/94  11.01.53  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   22/09/94
      SUBROUTINE CSLUNF(LUN)
      COMMON/CSLUNCS/LUNS(10)
      DATA LFIRST/0/
      SAVE LFIRST
#if defined(CERNLIB_PAW)
      IF(LFIRST.EQ.0)THEN
        DO 1 I=1,10
          LUNS(I)=0
  1     CONTINUE
        LFIRST=60
      ENDIF
      DO 3 I=0,39
        CALL PALUNF(LFIRST+I,3,LUN)
        IF(LUN.EQ.0)GO TO 5
        III=0
        DO 2 J=1,10
           IF(III.EQ.0 .AND. LUNS(J).EQ.0)III=J
           IF(LUNS(J).EQ.LUN)GO TO 3
  2     CONTINUE
        IF(III.EQ.0)GO TO 4
        LUNS(III)=LUN
***        print *,' debug info from cslunf: lun=',lun
        RETURN
  3   CONTINUE
#endif
#if !defined(CERNLIB_PAW)
      IF(LFIRST.EQ.0)THEN
        DO 1 I=1,10
          LUNS(I)=0
  1     CONTINUE
        LFIRST=60
      ENDIF
      DO I=1,10
        IF(LUNS(I).EQ.0)THEN
          LUN=LFIRST+I-1
          LUNS(I)=LUN
          RETURN
        ENDIF
      ENDDO
#endif
  4   LUN=0
  5   PRINT *,' no more free I/O chanels'
      END

