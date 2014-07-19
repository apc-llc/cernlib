*
* $Id: iewin.F,v 1.1.1.1 1996/02/14 13:10:23 mclareni Exp $
*
* $Log: iewin.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:23  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.19/10 15/09/93  10.06.31  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEWIN(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      CHARACTER*12 CINTR
      CHARACTER*32 CHVAL(5)
      CHARACTER*20 CCEWX1,CCEWX2,CCEWY1,CCEWY2,CCENT
*.______________________________________
*
      ICENT=100
 21   ICENT=ICENT+1
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
      CALL IZGNT(ICENT,IFIND,IAWV,IPRIO,IHPRIO)
      IF(IFIND.NE.0)GOTO 21
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      IF(IGNSCU(ICENT).NE.0)GOTO 21
#endif
      RCEWX1=0.
      RCEWX2=1.
      RCEWY1=0.
      RCEWY2=1.
      CALL IZITOC(ICENT,CCENT)
      CALL IZRTOC(RCEWX1,CCEWX1)
      CALL IZRTOC(RCEWX2,CCEWX2)
      CALL IZRTOC(RCEWY1,CCEWY1)
      CALL IZRTOC(RCEWY2,CCEWY2)
      TITLE(3)='CURRENT WINDOW PARAMETERS'
      CHITEM(1,3)='NT'
      CHITEM(2,3)='XMIN'
      CHITEM(3,3)='XMAX'
      CHITEM(4,3)='YMIN'
      CHITEM(5,3)='YMAX'
      CHDEF(1)=CCENT
      CHDEF(2)=CCEWX1
      CHDEF(3)=CCEWX2
      CHDEF(4)=CCEWY1
      CHDEF(5)=CCEWY2
      IPLACE=3
      CALL IGREQ(310,0,5,IPLACE,ICHOIC,CHVAL)
*
      IF(ICHOIC.EQ.-1000)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      IF(IPLACE.EQ.2)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      CALL IZCTOI(CHVAL(1),ICENT)
      CALL IZCTOR(CHVAL(2),RCEWX1)
      CALL IZCTOR(CHVAL(3),RCEWX2)
      CALL IZCTOR(CHVAL(4),RCEWY1)
      CALL IZCTOR(CHVAL(5),RCEWY2)
      X(1)=RQUEST(11)/REDIT
      Y(1)=RQUEST(12)/REDIT
      CALL IGPLOT(X(1),Y(1))
      CALL IGLOC(51,NTLOC,IBN,X(2),Y(2),XWC,YWC)
      IF(IBN.EQ.0)RETURN
*
      X(2)=X(2)/REDIT
      Y(2)=Y(2)/REDIT
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
      CALL ISWN(ICENT,RCEWX1,RCEWX2,RCEWY1,RCEWY2)
      CALL IGEX(X(1),X(2))
      CALL IGEX(Y(1),Y(2))
      CALL ISVP(ICENT,X(1),X(2),Y(1),Y(2))
      CALL ISELNT(ICENT)
      CALL IZITOC(ICENT,CINTR)
      CHUSKE(1,2)=CINTR
      IPLACE=2
      CALL IGMENU(0,TITLE(IPLACE)
     +,  XPM(1,IPLACE),XPM(2,IPLACE)
     +,  YPM(1,IPLACE),YPM(2,IPLACE)
     +,  NBU(IPLACE),CHUSKE(1,IPLACE)
     +,  NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,  CHDEF,CHVAL,1,'UTW')
*
      END
