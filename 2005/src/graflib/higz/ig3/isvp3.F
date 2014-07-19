*
* $Id: isvp3.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: isvp3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/03 06/01/95  13.24.56  by  O.Couet
*-- Author :    O.Couet   20/12/93
      SUBROUTINE ISVP3(NT,X1,X2,Y1,Y2)
*.===========>
*.
*. Define the 3D viewport (in 2D world coordinates)
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalisation transformation number.
*.  REAL X1,X2,Y1,Y2 : 3D viewport.
*.
*..==========> (O.Couet)
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
*
      IF ( X2.LE.X1 .OR. Y2.LE.Y1 ) THEN
         CALL IGERR ('Invalid 3D viewport definition','ISVP3')
         RETURN
      ENDIF
*
      IF ( IGNSCU(NT) .EQ. 0 ) CALL IGNCRE (NT)
      CALL IGNS3V (X1,X2,Y1,Y2)
#endif
      END
