*
* $Id: ifa.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: ifa.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.12/11 12/04/91  16.33.18  by  O.Couet
*-- Author :
      SUBROUTINE IFA(N,X,Y)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Output a polygon.
*.
*.    ***** Define closed area polygon of N points; move to the first point,
*.    ***** contiguous draws through the following points,  and an implicit
*.    ***** draw from the Nth point back to the first point.
*.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JPOLGN (X, Y, N)
*.
*.   Parameters:
*.   -----------
*.
*.     X, Y                       [ REAL; Array; Input ]
*.
*.      - The arrays of world coordinates defining the polygon
*.
*.     N                          [ INTEGER; Input ]
*.
*.      - The number of points in the polygon.  The above parameters
*.        should be DIMENSIONed to at least N in the calling routine
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      REAL X(1),Y(1)
#include "higz/di3seg.inc"
*.______________________________________
*
      IF(IFAIS.EQ.3.AND.IFASI.GE.100)THEN
         CALL IGFA(N,X,Y)
      ELSE
         IF(GFLAG)THEN
            CALL IGSG(0)
            IF (.NOT. SEGOPN) THEN
               CALL JOPEN
               SEGOPN=.TRUE.
            ENDIF
            CALL JPOLGN (X, Y, N)
         ENDIF
      ENDIF
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZFA(N,X,Y)
#endif
*
      END
#endif
