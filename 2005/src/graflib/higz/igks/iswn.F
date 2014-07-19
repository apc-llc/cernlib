*
* $Id: iswn.F,v 1.2 1996/06/05 10:21:04 cernlib Exp $
*
* $Log: iswn.F,v $
* Revision 1.2  1996/06/05 10:21:04  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:46  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.21/03 01/06/94  10.40.05  by  O.Couet
*-- Author :
      SUBROUTINE ISWN(NT,XX1,XX2,YY1,YY2)
*.===========>
*.
*.   This routine sets the window coordinates of the transformation NT .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalization transformation number (0<=NT<NBNTMX).
*.  REAL X1,Y1 : X,Y down left corner window coordinates in WC space .
*.  REAL X2,Y2 : X,Y right up corner window coordinates in WC space .
*.
*..==========> (O.Couet)
#if !defined(CERNLIB_NTC)
#include "higz/hint.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
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
            IF(GFLAG)CALL GSWN(1,X1,X2,Y1,Y2)
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
         NBNT = NTSTOR(1)
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
            IF(GFLAG)CALL GSWN(1,X1,X2,Y1,Y2)
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
