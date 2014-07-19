*
* $Id: iswn.F,v 1.1.1.1 1996/02/14 13:10:30 mclareni Exp $
*
* $Log: iswn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.21/03 01/06/94  13.31.40  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISWN(NT,XX1,XX2,YY1,YY2)
*.===========>
*.
*.   Purpose:
*.   --------
*.     Set the window coordinates of the transformation NT
*.
*.   DI-3000 access:
*.   ---------------
*.     ***** ***** *****
*.
*..==========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hint.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#include "higz/di3seg.inc"
*.______________________________________
*
      IF(NT.EQ.0)RETURN
*
      X1 = XX1
      X2 = XX2
      Y1 = YY1
      Y2 = YY2
*
#if !defined(CERNLIB_NTC)
      CALL IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
*
*              NT exits in the main transformation storage
*
      IF(IFIND.NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            IF (GFLAG) THEN
               IF(SEGOPN) THEN
                  CALL JCLOSE
                  SEGOPN = .FALSE.
               ENDIF
               CALL JWINDO (X1, X2, Y1, Y2)
            ENDIF
            RWXMIN = X1
            RWYMIN = Y1
            RWXMAX = X2
            RWYMAX = Y2
         ENDIF
         WNVPST(IAWV)   = X1
         WNVPST(IAWV+1) = X2
         WNVPST(IAWV+2) = Y1
         WNVPST(IAWV+3) = Y2
*
*              NT does not exit in the main transformation storage
*
      ELSE
         NBNT=NTSTOR(1)
         IF(NBNT+1.GT.NBNTMX)THEN
            CALL IGERR('Too many normalisation transformation','ISWN')
            RETURN
         ENDIF
         NTSTOR(2*NBNT+2) = NT
         WNVPST(8*NBNT+1) = X1
         WNVPST(8*NBNT+2) = X2
         WNVPST(8*NBNT+3) = Y1
         WNVPST(8*NBNT+4) = Y2
         WNVPST(8*NBNT+5) = 0.
         WNVPST(8*NBNT+6) = 1.
         WNVPST(8*NBNT+7) = 0.
         WNVPST(8*NBNT+8) = 1.
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
               CALL JWINDO (X1, X2, Y1, Y2)
            ENDIF
            RWXMIN = X1
            RWYMIN = Y1
            RWXMAX = X2
            RWYMAX = Y2
         ENDIF
      ELSE
         CALL IGNCRE(NT)
      ENDIF
      CALL IGNSWN(X1,X2,Y1,Y2)
#endif
*
      END
#endif
