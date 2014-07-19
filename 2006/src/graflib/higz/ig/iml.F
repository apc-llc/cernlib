*
* $Id: iml.F,v 1.1.1.1 1996/02/14 13:10:41 mclareni Exp $
*
* $Log: iml.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:41  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/06 10/03/89  08.13.26  by  O.Couet
*-- Author :
      SUBROUTINE IML(N,X,Y)
*.===========>
*.
*.   This routine draws a multiline .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points in the polyline .
*.  REAL X(N) : X coordinates .
*.  REAL Y(N) : Y coordinates .
*.
*..==========> (O.Couet)
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*)
      LOGICAL ZSAV
*.______________________________________
*
      IF(N.LT.2)THEN
         CALL IGERR('Invalid number of points','IML')
         RETURN
      ENDIF
*
      NI=N
      IF(MOD(NI,2).NE.0)THEN
         NI=NI-1
      ENDIF
      NI=NI/2
      IF(GFLAG)THEN
         CALL IGSG(0)
         ZSAV=ZFLAG
         ZFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         DO 10 I=1,NI
            CALL IPL(2,X(2*I-1),Y(2*I-1))
   10    CONTINUE
         ZFLAG=ZSAV
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZML(N,X,Y)
#endif
*
      END
