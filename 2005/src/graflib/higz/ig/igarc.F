*
* $Id: igarc.F,v 1.1.1.1 1996/02/14 13:10:31 mclareni Exp $
*
* $Log: igarc.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/09 06/12/91  17.15.23  by  O.Couet
*-- Author :
      SUBROUTINE IGARC(X,Y,R1,R2,PHI1,PHI2)
*.===========>
*.
*.   This routine draws an arc . If R1 not equal R2 the area between the two
*. arcs of radius R1 and R2 is filled according to the fill area interior style
*. index and the fill area style index .
*.
*. _Input parameters:
*.
*.  REAL X,Y : Center of the arc .
*.  REAL R1 : First radius .
*.  REAL R2 : Second radius .
*.  REAL PHI1 : First angle in degrees.
*.  REAL PHI2 : Second angle in degrees.
*.
*..==========> (O.Couet R.Nierhaus)
#include "higz/hiflag.inc"
*.______________________________________
*
      PHI1R=(3.14159*PHI1)/180.
      PHI2R=(3.14159*PHI2)/180.
      IF(GFLAG.OR.PFLAG)THEN
         PHI3=PHI2R
         IF(PHI1R.EQ.0..AND.PHI3.EQ.0.)PHI3=2.*3.14159
         IF(R1.EQ.R2)THEN
            CALL IGARC1(X,Y,R1,PHI1R,PHI3)
         ELSEIF(R1.EQ.0.)THEN
            IF(PHI1R.EQ.0..AND.PHI2R.EQ.0.)THEN
               CALL IGARC2(X,Y,R2)
            ELSE
               CALL IGARC3(X,Y,R2,PHI1R,PHI3)
            ENDIF
         ELSEIF(R1.GT.R2)THEN
            CALL IGARC4(X,Y,R2,R1,PHI1R,PHI3)
         ELSE
            CALL IGARC4(X,Y,R1,R2,PHI1R,PHI3)
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZARC(X,Y,R1,R2,PHI1,PHI2)
#endif
*
      END
