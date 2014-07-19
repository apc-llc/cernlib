*
* $Id: isvp.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: isvp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.22/03 06/01/95  11.28.36  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISVP(NT,XX1,XX2,YY1,YY2)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Define the boundaries of the virtual coordinate system viewport
*.
*.   DI-3000 access:
*.   ---------------
*.     CALL JVPORT (VXMIN, VMAX, VYMIN, VYMAX)
*.
*.   Parameters:
*.   -----------
*.      VXMIN, VXMAX                  [ REAL; Input ]
*.
*.       - The minimum and maximum virtual coordinate system viewport
*.         boundaries in the X-direction
*.
*.      VYMIN, VYMAX                  [ REAL; Input ]
*.
*.       - The minimum and maximum virtual coordinate system viewport
*.         boundaries in the Y-direction
*.
*.         NOTE: One of the two boundary pairs must be equal (-1.0, 1.0)
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hint.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#include "higz/di3seg.inc"
*.______________________________________
*
*              Verify the viewport parameters
*
      X1 = XX1
      X2 = XX2
      Y1 = YY1
      Y2 = YY2
      IF((X1.LT.0).OR.(X1.GT.1)
     +            .OR.
     +   (X2.LT.0).OR.(X2.GT.1)
     +            .OR.
     +   (Y1.LT.0).OR.(Y1.GT.1)
     +            .OR.
     +   (Y2.LT.0).OR.(Y2.GT.1)
     +            .OR.
     +   (Y1.GE.Y2).OR.(X1.GE.X2))THEN
         CALL IGERR('Invalid viewport definition','ISVP')
         RETURN
      ENDIF
*
      IF(NT.EQ.0)RETURN
*
      SCALE = 1.
      IF(REDIT.NE.0.)SCALE = REDIT
#if !defined(CERNLIB_NTC)
*
      CALL IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
*
*              NT exits in the main transformation storage
*
      IF(IFIND.NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            IF(GFLAG)THEN
               IF(SEGOPN) THEN
                  CALL JCLOSE
                  SEGOPN = .FALSE.
               ENDIF
               CALL JVPORT (X1,X2,Y1,Y2)
            ENDIF
            RVXMIN = X1
            RVYMIN = Y1
            RVXMAX = X2
            RVYMAX = Y2
         ENDIF
         WNVPST(IAWV+4) = X1
         WNVPST(IAWV+5) = X2
         WNVPST(IAWV+6) = Y1
         WNVPST(IAWV+7) = Y2
*
*              NT does not exit in the main transformation storage
*
      ELSE
         NBNT = NTSTOR(1)
         IF(NBNT+1.GT.NBNTMX)THEN
            CALL IGERR('Too many normalisation transformation','ISVP')
            RETURN
         ENDIF
         NTSTOR(2*NBNT+2) = NT
         WNVPST(8*NBNT+1) = 0.
         WNVPST(8*NBNT+2) = 1.
         WNVPST(8*NBNT+3) = 0.
         WNVPST(8*NBNT+4) = 1.
         WNVPST(8*NBNT+5) = X1
         WNVPST(8*NBNT+6) = X2
         WNVPST(8*NBNT+7) = Y1
         WNVPST(8*NBNT+8) = Y2
         NTSTOR(1)        = NTSTOR(1)+1
      ENDIF
#endif
#if defined(CERNLIB_NTC)
      IF(IGNSCU(NT).NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            IF(GFLAG)THEN
               IF(SEGOPN) THEN
                  CALL JCLOSE
                  SEGOPN = .FALSE.
               ENDIF
               CALL JVPORT (X1,X2,Y1,Y2)
            ENDIF
            RVXMIN = X1
            RVYMIN = Y1
            RVXMAX = X2
            RVYMAX = Y2
         ENDIF
      ELSE
         CALL IGNCRE(NT)
      ENDIF
      CALL IGNSVP(X1,X2,Y1,Y2)
#endif
*
      END
#endif
