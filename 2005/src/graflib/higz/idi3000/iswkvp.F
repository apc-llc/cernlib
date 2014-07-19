*
* $Id: iswkvp.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: iswkvp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/03 21/12/88  18.27.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISWKVP(WKID,X1,X2,Y1,Y2)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the workstation transformation window.
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JVPORT (VXMIN, VXMAX, VYMIN, VXMAX)
*.
*.        VXMIN, VXMAX     [ REAL; Input ]
*.         - The minimum and maximum virtual coordinate system
*.           vieport boundaries in the X-direction
*.
*.        VYMIN, VYMAX     [ REAL; Input ]
*.         - The minimum and maximum virtual coordinate system
*.           vieport boundaries in the Y-direction
*.
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      INTEGER WKID
#include "higz/di3seg.inc"
*.______________________________________
*
      RDVXMI=X1
      RDVXMA=X2
      RDVYMI=Y1
      RDVYMA=Y2
      IF(IGIWTY(WKID).LT.0)RETURN
*
      IF(GFLAG)THEN
         IF (SEGOPN) THEN
            CALL JCLOSE
            SEGOPN = .FALSE.
         ENDIF
         CALL JDEVVP (WKID, X1, X2, Y1, Y2)
      ENDIF
*
      END
#endif
