*
* $Id: ipm.F,v 1.2 1996/06/05 10:20:37 cernlib Exp $
*
* $Log: ipm.F,v $
* Revision 1.2  1996/06/05 10:20:37  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.18/10 21/06/93  11.10.53  by  O.Couet
*-- Author :
      SUBROUTINE IPM(N,X,Y)
*.===========>
*.
*.   This routines draws markers .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of markers .
*.  REAL X(N) : Markers X coordinates .
*.  REAL Y(N) : Markers Y coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*)
*.______________________________________
*
      IF(N.LT.1)THEN
         CALL IGERR('Invalid number of points','IPM')
         RETURN
      ENDIF
*
      IF(GFLAG.OR.PFLAG)THEN
         CALL IGSG(0)
         IF(IMK.LT.20)THEN
            IF(.NOT.GFLAG)GOTO 20
            CALL GPM(N,X,Y)
         ELSE
            CALL IGPM(N,X,Y,IMK)
         ENDIF
      ENDIF
*
   20 CONTINUE
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZPM(N,X,Y)
#endif
*
      END
#endif
