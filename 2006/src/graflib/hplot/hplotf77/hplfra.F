*
* $Id: hplfra.F,v 1.1.1.1 1996/01/19 10:50:08 mclareni Exp $
*
* $Log: hplfra.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:08  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.19/03 08/02/95  11.58.38  by  O.Couet
*-- Author :    O.Couet   09/11/89
      SUBROUTINE HPLFRA(X1I,X2I,Y1I,Y2I,CHOPT)
*.==========>
*.
*.   Define a FRAme.
*.   If X1, X2, etc. are given, draw a frame box with the window
*. coordinates set to X1, X2, Y1, Y2. Axis labels and tick marks
*. are drawn by default.
*.   If option 'S' is also specified, this routine is a convenient
*. way to redefine the FRAme for the current zone.
*.   If the option 'A' is given then axis labels and tick marks
*. are not drawn.
*.   If the option 'B' is given the box around the histo is not
*. not drawn.
*.
*. _Input parameters:
*.
*.    REAL X1I : Low range in X
*.    REAL X2I : High range in X
*.    REAL Y1I : Low range in Y
*.    REAL Y2I : High range in Y
*.    CHARACTER CHOPT : Options 'S,A,B'
*.
*..=========>
#include "hplot/quest.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl3.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl12.inc"
#include "hbook/hcprin.inc"
      CHARACTER*(*) CHOPT
      DIMENSION IOPT(3)
      EQUIVALENCE (IOPTS,IOPT(1)),(IOPTA,IOPT(2))
      EQUIVALENCE (IOPTB,IOPT(3))
      DIMENSION XAX(3),YAX(3)
*.______________________________________
*
      CALL UOPTC(CHOPT,'SAB',IOPT)
*
      X1=X1I
      X2=X2I
      Y1=Y1I
      Y2=Y2I
*
      IF(LOGX.NE.0)THEN
         LOGXFL=.TRUE.
      ELSE
         LOGXFL=.FALSE.
      ENDIF
      IF(LOGY.NE.0)THEN
         LOGYFL=.TRUE.
      ELSE
         LOGYFL=.FALSE.
      ENDIF
      FACTOR=1.
*
      IF((X2.LT.X1).OR.(Y2.LT.Y1))THEN
         CALL HBUG('Frame coordinates must be in increasing order'
     +             ,'HPLFRA',0)
         GOTO 999
      ENDIF
      IF(X2.EQ.X1)THEN
         X1=X1-1.
         X2=X2+1.
      ENDIF
      IF(Y2.EQ.Y1)THEN
         Y1=Y1-1.
         Y2=Y2+1.
      ENDIF
      IF(LOGX.EQ.0)THEN
         XWMINI=X1
         XWMAXI=X2
      ELSE
         IF(X2.LE.0)THEN
            CALL HBUG('X2 must be >0 in LOG scale','HPLFRA',0)
            GOTO 999
         ENDIF
         IF(X1.LE.0.)X1=MIN(X2/1000.,1.)
         XWMINI=ALOG10(X1)
         XWMAXI=ALOG10(X2)
      ENDIF
      IF(LOGY.EQ.0)THEN
         YWMINI=Y1
         YWMAXI=Y2
      ELSE
         IF(Y2.LE.0)THEN
            CALL HBUG('Y2 must be >0 in LOG scale','HPLFRA',0)
            GOTO 999
         ENDIF
         IF(Y1.LE.0.)Y1=MIN(Y2/1000.,1.)
         YWMINI=ALOG10(Y1)
         YWMAXI=ALOG10(Y2)
      ENDIF
*
      IF(IOPTB.NE.0)LOHBOX=.FALSE.
      CALL IZSAVA
*
      IF(IOPTS.EQ.0)THEN
         XMINI=XWMINI
         XMAXI=XWMAXI
         YMINI=YWMINI
         YMAXI=YWMAXI
         CALL HPLNUL
      ELSE
         NTWIN=NTWIN+1
         NTHIST=NTWIN
      ENDIF
*
      IQUEST(1)=0
      CALL ISVP(NTWIN,XVP1,XVP2,YVP1,YVP2)
      IF(IQUEST(1).NE.0)GOTO 999
      CALL ISWN(NTWIN,XWMINI,XWMAXI,YWMINI,YWMAXI)
      CALL ISELNT(NTWIN)
*
      IF(IOPTS.NE.0.AND.IOPTA.EQ.0)CALL HPLWIR('XYAR',X2,Y2,'TICK')
      IF(IOPTS.EQ.0.AND.IOPTA.EQ.0)THEN
         XAX(1)=XWMINI
         XAX(2)=XWMINI
         XAX(3)=XWMAXI
         YAX(1)=YWMINI
         YAX(2)=YWMAXI
         YAX(3)=YWMINI
         CALL HPLAXI(XAX,YAX,0)
      ENDIF
      IF((LOTIC).AND.(.NOT.LOSAME).AND.IOPTS.EQ.0)THEN
         CALL HPLWIR(' ',BIGP,BIGP,'TICK')
      ENDIF
*
  999 CONTINUE
      LOHBOX=.TRUE.
      CALL IZSETA
      END
