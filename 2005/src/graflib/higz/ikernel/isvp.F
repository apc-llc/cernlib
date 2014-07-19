*
* $Id: isvp.F,v 1.1.1.1 1996/02/14 13:10:58 mclareni Exp $
*
* $Log: isvp.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:58  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.22/03 06/01/95  11.27.19  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISVP(NT,X1,X2,Y1,Y2)
      ENTRY      GSVP(NT,X1,X2,Y1,Y2)
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
#include "higz/hint.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
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
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
      CALL IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
*
      IF(IFIND.NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            RVXMIN=X1
            RVYMIN=Y1
            RVXMAX=X2
            RVYMAX=Y2
         ENDIF
         WNVPST(IAWV+4)=X1
         WNVPST(IAWV+5)=X2
         WNVPST(IAWV+6)=Y1
         WNVPST(IAWV+7)=Y2
*
      ELSE
         NBNT=NTSTOR(1)
         IF(NBNT+1.GT.NBNTMX)THEN
            CALL IGERR('Too many normalisation transformation','ISVP')
            RETURN
         ENDIF
         NTSTOR(2*NBNT+2)=NT
         WNVPST(8*NBNT+1)=0.
         WNVPST(8*NBNT+2)=1.
         WNVPST(8*NBNT+3)=0.
         WNVPST(8*NBNT+4)=1.
         WNVPST(8*NBNT+5)=X1
         WNVPST(8*NBNT+6)=X2
         WNVPST(8*NBNT+7)=Y1
         WNVPST(8*NBNT+8)=Y2
         NTSTOR(1)=NTSTOR(1)+1
      ENDIF
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      IF(IGNSCU(NT).NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            RVXMIN=X1
            RVYMIN=Y1
            RVXMAX=X2
            RVYMAX=Y2
         ENDIF
      ELSE
         CALL IGNCRE(NT)
      ENDIF
      CALL IGNSVP(X1,X2,Y1,Y2)
#endif
      END
