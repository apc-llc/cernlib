*
* $Id: iswn3.F,v 1.1.1.1 1996/02/14 13:10:43 mclareni Exp $
*
* $Log: iswn3.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:43  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/03 06/01/95  13.29.57  by  O.Couet
*-- Author :    O.Couet   20/12/93
      SUBROUTINE ISWN3(NT,X1,X2,Y1,Y2,Z1,Z2,PHI,THETA,PSI,CHOPT)
*.===========>
*.
*. Define the 3D window
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalisation transformation number.
*.  REAL X1,X2,Y1,Y2,Z1,Z2 : 3D volume.
*.  REAL PHI,THETA,PSI : 3D vue angles.
*.  CHARACTER CHOPT : Options.
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHOPT
*.______________________________________
*
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
*
      IF ( X2.LE.X1. OR. Y2.LE.Y1. OR. Z2.LE.Z1 ) THEN
         CALL IGERR ('Invalid 3D window definition','ISWN3')
         RETURN
      ENDIF
*
      IF ( IGNSCU(NT) .EQ. 0 ) CALL IGNCRE(NT)
      CALL IGNS3W (X1,X2,Y1,Y2,Z1,Z2,PHI,THETA,PSI)
#endif
      END
