*
* $Id: hplive.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplive.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/16 25/08/94  15.26.44  by  O.Couet
*-- Author :
      SUBROUTINE HPLIVE
*.==========>
*.       define picture coordinates , set window and draw histogram
*.       surrounding frame in case of option "Update" if scale is
*.       no more correct.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl13.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      LOGICAL LOGYSC
      DIMENSION XAX(3),YAX(3)
*._____________________________
*
*     Find transformation number and
*     check if we need to change scale.
*     If we do not have to change scale
*     we only select transformation NTZEB.
*----
      NTZEB  = IQ(LHPLIP+2)
      YWMAXZ = Q(LHPLIP+19)
      IF(YMAXI.GT.YWMAXZ)LOSCAL = .TRUE.
*---
*     If scale must be changed whe have to change the window
*     of transformation NTZEB (YWMAXI must be greater).
*     Final adjustment of YWMAXI is made if HMAXIM not called.
*     We change of picture (HPLI) and we have to redraw all
*     the histogram created with option "Live", in their respective
*     zone.
*---
      IF(LOSCAL)THEN
*----
*       New picture
*----
         CALL HPLI
*----
*       First we redraw histogram to update
*----
*       Contour of zone
*----
         XLOWZ  = Q(LHPLIP+12)
         XHIGHZ = Q(LHPLIP+13)
         YLOWZ  = Q(LHPLIP+14)
         YHIGHZ = Q(LHPLIP+15)
         XLOW   = XLOWZ
         XHIGH  = XHIGHZ
         YLOW   = YLOWZ
         YHIGH  = YHIGHZ
         CALL ISLN(1)
         CALL HPLBOX(XLOW,YLOW,XHIGH,YHIGH,'CM  ')
         CALL ISLN(ILTYP)
*----
*       New window
*----
         XWMINI = Q(LHPLIP+16)
         XWMAXI = Q(LHPLIP+17)
         YWMINI = Q(LHPLIP+18)
         LOGYSC = .FALSE.
         IF(IQ(LHPLIP+3).NE.0)LOGYSC = .TRUE.
         IF(.NOT.LOGYSC)THEN
            IF(.NOT.LOMAX)THEN
               HSCAL = SCMAX/3.
            ELSE
               HSCAL = SCMAX
            ENDIF
            IF(I20.EQ.0.)THEN
               IF(YMAXI.GT.0.)THEN
                 YMAXI = YMAXI*(1./HSCAL)
               ELSE
                 YMAXI = YMAXI*HSCAL
               ENDIF
            ENDIF
         ELSE
           IF(.NOT.LOMAX)THEN
              HSCAL = SCMAX*3.
           ELSE
              HSCAL = SCMAX
           ENDIF
           IF(I20.EQ.0)YMAXI = YMAXI+LOG10(2.*(HSCAL/0.9))
         ENDIF
         YWMAXI = YMAXI
         Q(LHPLIP+19) = YWMAXI
         CALL ISWN(NTZEB,XWMINI,XWMAXI,YWMINI,YWMAXI)
         XMINI = XWMINI
         XMAXI = XWMAXI
         YMINI = YWMINI
         YMAXI = YWMAXI
*----
*       Redraw histogram, axis and title.
*----
         LIY = LHPLIP+19
*
         DO 10 J=1,IQ(LHPLIP+6)
            C1 = HCX(J,1)*FACTOR
            IF(ABS(YMAXI-YMINI).GE.0.000001)THEN
               IF(.NOT.LOGYSC)THEN
                  YB = C1
               ELSE
                  C1 = MAX(C1,10**YMINI)
                  YB = LOG10(C1)
               ENDIF
            ENDIF
            YB       = MAX(YB,YMINI)
            YB       = MIN(YB,YMAXI)
            Q(LIY+J) = YB
  10     CONTINUE
*
         XAX(1) = XMINI
         XAX(2) = XMINI
         XAX(3) = XMAXI
         YAX(1) = YMINI
         YAX(2) = YMAXI
         YAX(3) = YMINI
         CALL HPLRDW(NTZEB,LHPLIP,XAX,YAX)
         IF(.NOT.LOUTIT) CALL HPLTIT(' ')
         CALL HPLGTL
*----
*       Then we have a loop for all other histograms
*       which are "Live"
*----
         LPTLIV = LQ(LHPLOT-1)
  20     IF(LPTLIV.EQ.0)GOTO 30
         IF((LPTLIV.NE.LHPLIP).AND.(JBIT(IQ(LPTLIV),1).NE.0))THEN
            NTLIV = IQ(LPTLIV+2)
*---
*       Contour of zone
*----
            XLOW  = Q(LPTLIV+12)
            XHIGH = Q(LPTLIV+13)
            YLOW  = Q(LPTLIV+14)
            YHIGH = Q(LPTLIV+15)
            CALL ISLN(1)
            CALL HPLBOX(XLOW,YLOW,XHIGH,YHIGH,'CM  ')
            CALL ISLN(ILTYP)
*----
*       Redraw histogram and axis
*----
            XMINI  = Q(LPTLIV+16)
            XMAXI  = Q(LPTLIV+17)
            YMINI  = Q(LPTLIV+18)
            YMAXI  = Q(LPTLIV+19)
            XAX(1) = XMINI
            XAX(2) = XMINI
            XAX(3) = XMAXI
            YAX(1) = YMINI
            YAX(2) = YMAXI
            YAX(3) = YMINI
            CALL HPLRDW(NTLIV,LPTLIV,XAX,YAX)
            LCOLD = LCID
            IDPOS = LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),IQ(LPTLIV+1))
            IF(IDPOS.GT.0)THEN
               LCID = LQ(LTAB-IDPOS)
               IF(.NOT.LOUTIT) CALL HPLTIT(' ')
            ENDIF
            LCID = LCOLD
         ENDIF
         LPTLIV = LQ(LPTLIV)
         GOTO 20
*
  30     CONTINUE
*----
*       Reset variables XLOW,YLOW,XHIGH,YHIGH,
*       XMINI,XMAXI,YMINI,YMAXI which may have changed
*       when redrawing every "live" histogram.
*----
         XLOW  = XLOWZ
         XHIGH = XHIGHZ
         YLOW  = YLOWZ
         YHIGH = YHIGHZ
         XMINI = XWMINI
         XMAXI = XWMAXI
         YMINI = YWMINI
         YMAXI = YWMAXI
*
      ENDIF
*----
*       Select transformation NTZEB
*----
      CALL ISELNT(NTZEB)
      NTWIN  = NTZEB
      NTHIST = NTZEB
*
      END
