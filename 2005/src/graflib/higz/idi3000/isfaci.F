*
* $Id: isfaci.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: isfaci.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/00 29/05/89  12.16.42  by  O.Couet
*-- Author :
      SUBROUTINE ISFACI(ICOLI)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the polygon interior color and intensity attributes.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JPIDEX (CCOLOR, CINTEN)
*.
*.   Parameters:
*.   -----------
*.     CCOLOR                     [ INTEGER; Input ]
*.
*.      - The new value for the polygon interior color.
*.
*.        NOTE : CCOLOR must be in the range 0...32767.
*.
*.     CINTEN                     [ INTEGER; Input ]
*.
*.      - The new value for the polygon interior intensity.
*.
*.        NOTE : CINTEN must be in the range 0...32767.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CCOLOR, RANGEC(2)
      INTEGER STYLE
      INTEGER MAP(0:7)
      DATA MAP / 9, 0, 1, 2, 4, 5, 6, 3 /
      DATA RANGEC /0, 32767/
*.______________________________________
*
      IFACI=ICOLI
      IF (GFLAG) THEN
C
C        Perform GKS-->DI3000 mapping for color indices 0-7.
C
         IF((ICOLI.GE.0).AND.(ICOLI.LE.7))THEN
            CCOLOR=MAP(ICOLI)
         ELSE
            CCOLOR=ICOLI
         ENDIF
         IF((CCOLOR.LT.RANGEC(1)).OR.(CCOLOR.GT.RANGEC(2))) CCOLOR=0
         CALL J1IGET (11, STYLE)
         CALL J1IGET (13, CINTEN)
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         IF (STYLE .NE. 1) CALL JPINTR (1)
         CALL JPIDEX (CCOLOR, CINTEN)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFACCO)
#endif
*
      END
#endif
