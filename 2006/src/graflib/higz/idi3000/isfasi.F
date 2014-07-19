*
* $Id: isfasi.F,v 1.1.1.1 1996/02/14 13:10:29 mclareni Exp $
*
* $Log: isfasi.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:29  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/01 20/07/89  10.12.07  by  O.Couet
*-- Author :
      SUBROUTINE ISFASI(ISTYL)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the polygon interior style attribute, color
*.     and intensity.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JPINTR (CVALUE)
*.
*.   Parameters:
*.   -----------
*.     CVALUE                     [ INTEGER; Input ]
*.
*.      - The new value for the polygon interior style.
*.        style is reset to CVALUE.
*.
*.        NOTE : CVALUE must be in the range 0...32767.
*.
*.        The styles are:
*.
*.          0 - empty (the polygon is not filled)
*.          1 - the polygon is solid filled using the current
*.              polygon interior index values (JPIDEX).
*.   2..32767 - the index of a device dependent pattern to be
*.              used for filling the polygon.
*.
*.         NOTE : The polygon edge style is set by JPEDGE.
*.
*.         NOTE : JBEGIN sets the deafult interior style to 0.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      INTEGER CVALUE, CCOLOR, STYLE
      INTEGER MAP(20)
      DATA MAP / 44, 43, 45, 46,
     +           41, 35, 42, 36,
     +           32, 31, 33, 34,
     +           29, 23, 30, 24,
     +           20, 19, 21, 22/
*.______________________________________
*
      IFASI=ISTYL
      IF (GFLAG) THEN
         IF (.NOT. SEGOPN) THEN
            CALL JOPEN
            SEGOPN=.TRUE.
         ENDIF
         IF ((ISTYL.LE.-101).AND.(ISTYL.GE.-120)) THEN
            CVALUE = MAP(ABS(ISTYL)-100)
         ELSE
            CVALUE = ABS(MOD(ISTYL,48))
         ENDIF
         CALL J1IGET(11, STYLE)
         IF (STYLE.NE.1) CALL JPINTR(1)
         CALL J1IGET(12, CCOLOR)
         CALL JPIDEX (CCOLOR, CVALUE)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZATT(IFASCO)
#endif
*
      END
#endif
