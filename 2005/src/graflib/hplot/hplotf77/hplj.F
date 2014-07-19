*
* $Id: hplj.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplj.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.09/00 03/05/90  16.22.13  by  O.Couet
*-- Author :
      SUBROUTINE HPLJ
*.==========>
*.       Defines picture coordinates, set window and viewport,
*.       then draws histogram title and global title if request
*.       and histogram surrounding frame.
*.       Sorts the NTs in the current HIGZ picture if (LOZFL is
*.       .TRUE.) via a call to IZPICT with option 'O'.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl12.inc"
*._____________________________
*----
*     Define transformation of normalization
*     and new frame limits
*----
*     Frame limits of histogram
*----
      IF (.NOT.LOWIND) THEN
         XLOW=XMLEFT
         XHIGH=XSIZ-XMRIT
         YLOW=YMGLOW
         YHIGH=YSIZ-YMGUP
      ELSE
*
         IX=MOD(IWIN,IXWIN)-1
         IY=IYWIN-(IWIN/IXWIN)-1
         IF (IX.LT.0) THEN
            IX=IXWIN-1
            IY=IY+1
         ENDIF
*
         XLOW=XMLEFT+IX*(XWIND+XWDIST)
         XHIGH=XLOW+XWDIST
         YLOW=YMGLOW+IY*(YWIND+YWDIST)
         YHIGH=YLOW+YWDIST
      ENDIF
*
      IF(.NOT.LOSQR) GOTO 20
*----
*     Redefine XLOW,XHIGH etc if 'SQR' option in effect
*----
      XY=((XHIGH-XLOW)-(YHIGH-YLOW))
      IF(XY.EQ.0.0) GOTO 20
      XY=0.5*XY
      IF(XY.GT.0.) GOTO 10
*----
*     X length less than Y,compensate Y
*----
      YHIGH=YHIGH+XY
      YLOW=YLOW-XY
      GOTO 20
*----
*     Y length less than X, compensate X
*----
   10 XHIGH=XHIGH-XY
      XLOW=XLOW+XY
*
   20 CONTINUE
*
      IF((XHIGH.EQ.XLOW).OR.(YHIGH.EQ.YLOW))GOTO 40
*----
*     Determine transformation of normalisation histogram
*----
      IF (IXWIN.GE.100) GOTO 40
      IF (IYWIN.GE.100) GOTO 50
      NTHIST=NCHIST*10
*----
*     Test if the histogram is empty
*----
      IF((XMINI.EQ.XMAXI).OR.(YMINI.EQ.YMAXI))GOTO 30
*
      XVP1=XLOW*XYTWN+DXWN
      XVP2=XHIGH*XYTWN+DXWN
      YVP1=YLOW*XYTWN+DYWN
      YVP2=YHIGH*XYTWN+DYWN
*
      CALL ISWN(NTHIST,XMINI,XMAXI,YMINI,YMAXI)
      CALL ISVP(NTHIST,XVP1,XVP2,YVP1,YVP2)
*----
*     XMINI, XMAXI, YMINI, YMAXI are the histogram coordinates
*     calculated by HPLC after calling HPLOT
*     XWMINI, XWMAXI, YWMINI, YWMAXI are the window coordinates.
*     It is not necessary the same especially when several histograms
*     are plotted on the same picture (the window does not change in
*     that case.
*----
      XWMINI=XMINI
      XWMAXI=XMAXI
      YWMINI=YMINI
      YWMAXI=YMAXI
*
   30 CONTINUE
      IF(LOHBOX)THEN
         CALL ISLN(1)
         IF(LOGXFL.OR.LOGYFL.OR.ISBCOL.NE.0)THEN
            CALL HPLBOX(XLOW,YLOW,XHIGH,YHIGH,'CMS ')
         ELSE
            FACOLD=FACTOR
            FACTOR=1.
            CALL HPLBOX(XMINI,YMINI,XMAXI,YMAXI,'WC  ')
            FACTOR=FACOLD
         ENDIF
         CALL ISLN(ILTYP)
      ENDIF
*----
*     Histogram title
*----
      IF(.NOT.LOUTIT) CALL HPLTIT(' ')
*----
*     Draw global title
*----
      CALL HPLGTL
*----
*     Select the normalisation transformation for histogram,
*     and sort the NTs in the current picture if LOZFL is .TRUE.
*----
      CALL ISELNT(NTHIST)
      NTWIN=NTHIST
      IF(LOZFL)CALL IZPICT(' ','O')
*
      RETURN
   40 CALL HBUG('Too many windows requested in X : increase plot size or
     + decrease number of windows','HPLZON',0)
      RETURN
   50 CALL HBUG('Too many windows requested in Y : increase plot size or
     + decrease number of windows','HPLZON',0)
*
      END
