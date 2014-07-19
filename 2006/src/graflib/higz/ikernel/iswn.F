*
* $Id: iswn.F,v 1.1.1.1 1996/02/14 13:10:59 mclareni Exp $
*
* $Log: iswn.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:59  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/00 28/04/93  10.53.48  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ISWN(NT,X1,X2,Y1,Y2)
      ENTRY      GSWN(NT,X1,X2,Y1,Y2)
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
#include "higz/hint.inc"
#endif
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      IF(NT.EQ.0)RETURN
*
      IF(X1.GE.X2.OR.Y1.GE.Y2)THEN
         CALL IGERR('Bad window range','ISWN')
         RETURN
      ENDIF
*
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
      CALL IZGNT(NT,IFIND,IAWV,IPRIO,IHPRIO)
*
      IF(IFIND.NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            RWXMIN=X1
            RWYMIN=Y1
            RWXMAX=X2
            RWYMAX=Y2
         ENDIF
         WNVPST(IAWV)  =X1
         WNVPST(IAWV+1)=X2
         WNVPST(IAWV+2)=Y1
         WNVPST(IAWV+3)=Y2
*
      ELSE
         NBNT=NTSTOR(1)
         IF(NBNT+1.GT.NBNTMX)THEN
            CALL IGERR('Too many normalisation transformation','ISWN')
            RETURN
         ENDIF
         NTSTOR(2*NBNT+2)=NT
         WNVPST(8*NBNT+1)=X1
         WNVPST(8*NBNT+2)=X2
         WNVPST(8*NBNT+3)=Y1
         WNVPST(8*NBNT+4)=Y2
         WNVPST(8*NBNT+5)=0.
         WNVPST(8*NBNT+6)=1.
         WNVPST(8*NBNT+7)=0.
         WNVPST(8*NBNT+8)=1.
         NTSTOR(1)=NTSTOR(1)+1
      ENDIF
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      IF(IGNSCU(NT).NE.0)THEN
         IF((INTR.EQ.NT).AND.GFLAG)THEN
            RWXMIN=X1
            RWYMIN=Y1
            RWXMAX=X2
            RWYMAX=Y2
         ENDIF
      ELSE
         CALL IGNCRE(NT)
      ENDIF
      CALL IGNSWN(X1,X2,Y1,Y2)
#endif
      END
