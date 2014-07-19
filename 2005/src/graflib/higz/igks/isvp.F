*
* $Id: isvp.F,v 1.2 1996/06/05 10:21:01 cernlib Exp $
*
* $Log: isvp.F,v $
* Revision 1.2  1996/06/05 10:21:01  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.22/03 06/01/95  11.28.12  by  O.Couet
*-- Author :
      SUBROUTINE ISVP(NT,XX1,XX2,YY1,YY2)
*.===========>
*.
*.   This routine sets the transformation normalisation viewport NT .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalization transformation number (0<=NT<NBNTMX).
*.  REAL X1,Y1 : X,Y down left corner viewport coordinates .
*.  REAL X2,Y2 : X,Y right up corner viewport coordinates .
*.
*..==========> (O.Couet)
#if !defined(CERNLIB_NTC)
#include "higz/hint.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
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
               CALL GSVP(1
     +,                  X1*SCALE-(RDWXMI*REDIT)
     +,                  X2*SCALE-(RDWXMI*REDIT)
     +,                  Y1*SCALE-(RDWYMI*REDIT)
     +,                  Y2*SCALE-(RDWYMI*REDIT))
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
         NBNT=NTSTOR(1)
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
               CALL GSVP(1
     +,                  X1*SCALE-(RDWXMI*REDIT)
     +,                  X2*SCALE-(RDWXMI*REDIT)
     +,                  Y1*SCALE-(RDWYMI*REDIT)
     +,                  Y2*SCALE-(RDWYMI*REDIT))
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
      END
#endif
