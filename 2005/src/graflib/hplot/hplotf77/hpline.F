*
* $Id: hpline.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hpline.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.19/00 24/10/94  10.56.39  by  O.Couet
*-- Author :
      SUBROUTINE HPLINE(XU,YU,N,CHOPT)
*.==========>
*.            Draws a line or a series of staight lines
*.            on the picture
*.            (Transformation NTHIST or 1 is selected
*.            according to CHOPT)
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl11.inc"
      CHARACTER*(*) CHOPT
      DIMENSION XU(N),YU(N)
*._____________________________
*
*----
*     cm are required ?
*----
      IF (INDEX(CHOPT,'C').NE.0) THEN
         LOCM=.TRUE.
      ELSE
         LOCM=.FALSE.
      ENDIF
*
      IF(LOCM)THEN
         IF(NTWIN.NE.1)CALL ISELNT(1)
         NTWIN=1
         CALL IPL(N,XU,YU)
         RETURN
      ELSE
         IF(NTWIN.NE.NTHIST)CALL ISELNT(NTHIST)
         NTWIN=NTHIST
      ENDIF
*
      IF(N.GE.NMAX)THEN
         CALL HBUG('Too many points','HPLINE',0)
         RETURN
      ENDIF
      DO 10 I=1,N
         IF(LOGXFL)THEN
            IF(XU(I).LE.0.)GOTO 20
            XLINE(I)=LOG10(XU(I))
         ELSE
            XLINE(I)=XU(I)
         ENDIF
         YY=FLOARG(YU(I))*FACTOR
         IF(LOGYFL)THEN
            IF(YY.LE.0.)GOTO 20
            YLINE(I)=LOG10(YY)
         ELSE
            YLINE(I)=YY
         ENDIF
   10 CONTINUE
      CALL IPL(N,XLINE,YLINE)
      RETURN
*
   20 CALL HBUG('Routine called with zero or negative argument with log
     +scale','HPLINE',0)
*
      END

