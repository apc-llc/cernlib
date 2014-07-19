*
* $Id: hplzon.F,v 1.1.1.1 1996/01/19 10:50:12 mclareni Exp $
*
* $Log: hplzon.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:12  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/01 13/07/93  15.43.22  by  O.Couet
*-- Author :
      SUBROUTINE HPLZON(IX,IY,IWI,KIC)
*.==========>
*.       sets up window options
*..=========>
#include "hbook/hcbook.inc"
#include "hplot/quest.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
      CHARACTER*(*) KIC
*._____________________________
*
      IQUEST(1)=0
      IW = MAX(1,IWI)
*
      NARG=4
      CALL NOARG(NARG)
      IF(NARG.LT.2) GOTO 996
      LWSAME=.FALSE.
*
      IF(NARG.GE.4)THEN
         IF(KIC(1:1).EQ.'S') LWSAME=.TRUE.
      ENDIF
*----
*     check if space for IX and IY windows
*----
      IXWIN=IABS(IX)
      IF(IXWIN.EQ.0) GOTO 996
      XWDIST=XMLEFT+((IXWIN-1)*XWIND)+XMRIT
      IF (XWDIST.GE.XSIZ) GOTO 997
      XWDIST=(XSIZ-XWDIST)/IXWIN
   20 IYWIN=IABS(IY)
      IF(IYWIN.EQ.0) GOTO 996
      YWDIST=YMGLOW+((IYWIN-1)*YWIND)+YMGUP
      IF (YWDIST.GE.YSIZ) GOTO 998
      YWDIST=(YSIZ-YWDIST)/IYWIN
*
      LOWIND=.FALSE.
      IF(NARG.LT.3) THEN
         IWIN=0
         NCHIST=0
         IWIN0=1
      ELSE
         IWIN=MAX(IW-1,0)
         IF(.NOT.LWSAME)NCHIST=IWIN
         IWIN0=IW
      ENDIF
*
      IF(IXWIN*IYWIN.GT.1) LOWIND=.TRUE.
      IF(LOWIND) GOTO 999
*
  996 CONTINUE
      NCHIST=1
      IWIN=1
      IWIN0=1
      IXWIN=1
      IYWIN=1
      LOWIND=.FALSE.
      GOTO 999
*
  997 CALL HBUG('Too many windows requested in X : increase plot size or
     + decrease number of windows','HPLZON',0)
      IQUEST(1)=1
      IXWIN=1
      XWDIST=XMLEFT+XMRIT
      XWDIST=XSIZ-XWDIST
      GOTO 20
*
  998 CALL HBUG('Too many windows requested in Y : increase plot size or
     + decrease number of windows','HPLZON',0)
      IQUEST(1)=1
      IYWIN=1
      YWDIST=YMGLOW+YMGUP
      YWDIST=YSIZ-YWDIST
  999 CONTINUE
      END


