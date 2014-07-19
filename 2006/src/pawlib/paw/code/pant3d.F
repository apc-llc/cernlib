*
* $Id: pant3d.F,v 1.1.1.1 1996/03/01 11:38:41 mclareni Exp $
*
* $Log: pant3d.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/17 10/11/95  17.25.19  by  O.Couet
*-- Author : G.Richards
      SUBROUTINE PANT3D(XLOW,XUP,YLOW,YUP,ZLOW,ZUP,CLOW,CUP
     +,                 XAX,YAX,ZAX,CHOPT)
******************************************************************
*       Initalises the PHIGS and set up stuff
*       IDF - id assigned to scatt plot
*       XLOW,XUP,YLOW,YUP,ZLOW,ZUP - max and min of the axis
*       CLOW,CUP - max and min of colour
*       XAX,YAX,ZAX - titles of the axies
******************************************************************
*
#include "paw/pawnpu.inc"
#include "paw/gplotst.inc"
#include "paw/pcchar.inc"
#include "paw/quest.inc"
      CHARACTER*(*) ZAX,YAX,XAX,CHOPT
*.______________________________________
*
      IF(INDEX(CHOPT,'S').NE.0)THEN
         NEW = .FALSE.
      ELSE
         ISTCNT   = 102
         RMAXP(1) = XUP
         RMAXP(2) = YUP
         RMAXP(3) = ZUP
         RMAXP(4) = CUP
         RMINP(1) = XLOW
         RMINP(2) = YLOW
         RMINP(3) = ZLOW
         RMINP(4) = CLOW
         NEW      = .TRUE.
      ENDIF
*
      CALL IGQ('DIME',RDIM)
      IF (RDIM.NE.3.) THEN
         IF (NEW) THEN
            CALL HPLFR3(RMINP(1),RMAXP(1),RMINP(2),RMAXP(2)
     +,                 RMINP(3),RMAXP(3),ANGLE1,ANGLE2,'BWF')
            CALL PAUTIT(CHTITL)
         ENDIF
      ELSE
         CALL ISWN(1,-.5,1.5,-.5,1.5)
         CALL ISELNT(1)
      ENDIF
*
      CALL IGSTR(0,'CD')
*
      CALL IGPHST(RMINP,RMAXP,ANGLE1,ANGLE2,0.0,CHOPT)
      IF (NEW) THEN
         CALL PANTAX(XAX,YAX,ZAX)
      ENDIF
*
      END
