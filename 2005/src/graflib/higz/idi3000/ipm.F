*
* $Id: ipm.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: ipm.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.18/10 21/06/93  11.11.11  by  O.Couet
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
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/di3seg.inc"
      REAL X(N),Y(N)
*.______________________________________
*
      IF((N.LE.0).OR.(IMK.GT.31))GOTO 999
*
      IF(GFLAG.OR.PFLAG)THEN
         CALL IGSG(0)
         IF(IMK.LT.20)THEN
            IF(.NOT.GFLAG)GOTO 20
            IF (.NOT. SEGOPN) THEN
               CALL JOPEN
               SEGOPN=.TRUE.
            ENDIF
            CALL JPMARK(X,Y,N)
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
  999 END
#endif
