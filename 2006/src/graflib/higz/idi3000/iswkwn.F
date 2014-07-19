*
* $Id: iswkwn.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: iswkwn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.06/03 21/12/88  18.27.21  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISWKWN(WKID,X1,X2,Y1,Y2)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the workstation transformation window
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JDEVWN (DSPDEV, VXMIN, VXMAX, VYMIN, VYMAX)
*.
*.        DSPDEV
*.         - The identifer of the display device for
*.           which the device window is being defined
*.
*.        VXMIN, VXMAX
*.         - The minimum and maximum devcie window boundaries
*.           in the X-direction in virtual coordinates.
*.           VXMIN and VXMAX must be in the range:
*.           (-1.0 .le. XMIN .le. XMAX .le 1.0)
*.
*.        VYMIN, VYMAX
*.         - The minimum and maximum devcie window boundaries
*.           in the Y-direction in virtual coordinates.
*.           VYMIN and VYMAX must be in the range:
*.           (-1.0 .le. YMIN .le. YMAY .le 1.0)
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      INTEGER WKID
#include "higz/di3seg.inc"
*.______________________________________
*
      IF((X1.LT.0.).OR.(X1.GT.1.)
     +            .OR.
     +   (X2.LT.0.).OR.(X2.GT.1.)
     +            .OR.
     +   (Y1.LT.0.).OR.(Y1.GT.1.)
     +            .OR.
     +   (Y2.LT.0.).OR.(Y2.GT.1.))THEN
         CALL IGERR('Bad window or viewport size','ISWKWN')
         RETURN
      ENDIF
*
      RDWXMI=X1
      RDWXMA=X2
      RDWYMI=Y1
      RDWYMA=Y2
      IF(IGIWTY(WKID).LT.0)RETURN
*
      IF(GFLAG)THEN
         IF (SEGOPN) THEN
            CALL JCLOSE
            SEGOPN = .FALSE.
         ENDIF
         CALL JDEVWN (WKID, X1, X2, Y1, Y2)
      ENDIF
*
      END
#endif
