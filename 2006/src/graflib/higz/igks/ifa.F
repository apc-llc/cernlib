*
* $Id: ifa.F,v 1.2 1996/06/05 10:20:32 cernlib Exp $
*
* $Log: ifa.F,v $
* Revision 1.2  1996/06/05 10:20:32  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.12/11 12/04/91  16.32.20  by  O.Couet
*-- Author :
      SUBROUTINE IFA(N,X,Y)
*.===========>
*.
*.   This routine draws a fill area .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points .
*.  REAL X(N) : X coordinates .
*.  REAL Y(N) : Y coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      DIMENSION X(*),Y(*)
*.______________________________________
*
      IF(N.LT.3)THEN
         CALL IGERR('Invalid number of points','IFA')
         RETURN
      ENDIF
*
      IF(IFAIS.EQ.3.AND.IFASI.GE.100)THEN
         CALL IGFA(N,X,Y)
      ELSE
         IF(GFLAG)THEN
            CALL IGSG(0)
            CALL GFA(N,X,Y)
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZFA(N,X,Y)
#endif
*
      END
#endif
