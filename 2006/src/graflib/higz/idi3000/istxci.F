*
* $Id: istxci.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: istxci.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/07 04/04/89  10.38.12  by  O.Couet
*-- Author :
      SUBROUTINE ISTXCI(ICOLI)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the text color index
*.
*.   DI-3000 access:
*.   ---------------
*.     DI-3000 sets current/color primitive attribute.
*.     Use JCOLOR via HIGZ routine ISCR.
*.     CVALUE must be in the range 0...32767
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CVALUE, RANGE(2)
      INTEGER MAP(0:7)
      DATA MAP / 9, 0, 1, 2, 4, 5, 6, 3 /
      DATA RANGE(1), RANGE(2) /0, 32767/
*.______________________________________
*
      ITXCI=ICOLI
      IF (GFLAG) THEN
C
C        Perform GKS-->DI3000 mapping for color indices 0-7.
C
         IF((ICOLI.GE.0).AND.(ICOLI.LE.7))THEN
            CVALUE=MAP(ICOLI)
         ELSE
            CVALUE=ICOLI
         ENDIF
         IF((CVALUE.LT.RANGE(1)).OR.(CVALUE.GT.RANGE(2))) CVALUE=0
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         CALL JCOLOR (CVALUE)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(ITXCCO)
#endif
*
      END
#endif
