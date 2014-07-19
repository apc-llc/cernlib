*
* $Id: igreq.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: igreq.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/03 07/06/90  14.38.29  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGREQ(MN,NBUSE,NBI,IPLAC,ICHOIC,CHVAL)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
      PARAMETER (CHSIZ=0.025)
      CHARACTER*(*) CHVAL(NBI)
      CHARACTER*8 CHOPT
      LOGICAL ZFS,GFS
*.______________________________________
*
      ZFS=ZFLAG
      GFS=GFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
*
      IPLACE=IPLAC
      NBITEM(IPLACE)=NBI
      NBU(IPLACE)=NBUSE
      CHOPT=' '
*
      IF(NBU(IPLACE).NE.0)THEN
         INC=1
      ELSE
         INC=0
      ENDIF
*
      IF(IPLACE.EQ.1)THEN
         XPM(1,1)=REDIT+0.005
         XPM(2,1)=1.-0.015
         YPM(2,1)=1.-0.015
         YPM(1,1)=YPM(2,1)-CHSIZ*(NBI+INC)
      ENDIF
      IF(IPLACE.EQ.2)THEN
         XPM(1,2)=XPM(1,1)
         XPM(2,2)=XPM(2,1)
         YPM(2,2)=YPM(1,1)-0.03
         YPM(1,2)=YPM(2,2)-CHSIZ*(NBI+INC)
      ENDIF
      IF(IPLACE.EQ.3)THEN
         XPM(1,3)=0.001
         XPM(2,3)=REDIT-0.015
         YPM(2,3)=YPM(2,1)
         YPM(1,3)=YPM(2,1)-CHSIZ*(NBI+INC)
         IF(YPM(1,3).LT.REDIT+0.005)YPM(1,3)=REDIT+0.005
      ENDIF
*
      DO 10 I=IPLACE+1,3
         IF(ICUMEN(I).NE.0)THEN
            CALL IGMENU(0,TITLE(I)
     +,                 XPM(1,I),XPM(2,I)
     +,                 YPM(1,I),YPM(2,I)
     +,                 NBU(I),CHUSKE(1,I)
     +,                 NBITEM(I),CHITEM(1,I)
     +,                 CHDEF,CHVAL,ICHOI,'EW')
            ICUMEN(I)=0
            ICUCHO(I)=0
         ENDIF
   10 CONTINUE
*
      IF((ICUMEN(IPLACE).EQ.MN))THEN
         CHOPT(1:3)='WT '
      ELSE
         CHOPT(1:3)='DWT'
         ICUMEN(IPLACE)=MN
      ENDIF
      IF(IPLACE.EQ.3)CHOPT(4:4)='P'
      IF(JBIT(ISOF,1).NE.0)CHOPT(5:5)='S'
      IF(JBIT(ISOF,2).NE.0)CHOPT(6:6)='A'
*
      CALL IGMENU(0,TITLE(IPLACE)
     +,XPM(1,IPLACE),XPM(2,IPLACE)
     +,YPM(1,IPLACE),YPM(2,IPLACE)
     +,NBU(IPLACE),CHUSKE(1,IPLACE)
     +,NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,CHDEF,CHVAL,ICHOI,CHOPT)
*
      IF(IPLACE.EQ.3)THEN
         CALL IGMENU(0,TITLE(IPLACE)
     +,              XPM(1,IPLACE),XPM(2,IPLACE)
     +,              YPM(1,IPLACE),YPM(2,IPLACE)
     +,              NBU(IPLACE),CHUSKE(1,IPLACE)
     +,              NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,              CHDEF,CHVAL,ICHOI,'CPRT')
      ELSE
         ICHOI=ICUCHO(IPLACE)
         CALL IGMENU(0,TITLE(IPLACE)
     +,              XPM(1,IPLACE),XPM(2,IPLACE)
     +,              YPM(1,IPLACE),YPM(2,IPLACE)
     +,              NBU(IPLACE),CHUSKE(1,IPLACE)
     +,              NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,              CHDEF,CHVAL,ICHOI,'CHRT')
      ENDIF
*
*              Picked outside the menu
*
      IF(ICHOI.EQ.0)THEN
         ILOOP=IPLACE-1
         DO 30 I=ILOOP,1,-1
            CALL IGMENU(0,TITLE(I)
     + ,                XPM(1,I),XPM(2,I)
     +,                 YPM(1,I),YPM(2,I)
     +,                 NBU(I),CHUSKE(1,I)
     +,                 NBITEM(I),CHITEM(1,I)
     +,                 CHDEF,CHVAL,ICHOI,'CNT')
            IF(ICHOI.NE.0)THEN
               IPLACE=I
               DO 20 J=3,IPLACE+1,-1
                  IF(ICUMEN(J).NE.0)THEN
                     CALL IGMENU(0,TITLE(J)
     +,                          XPM(1,J),XPM(2,J)
     +,                          YPM(1,J),YPM(2,J)
     +,                          NBU(J),CHUSKE(1,J)
     +,                          NBITEM(J),CHITEM(1,J)
     +,                          CHDEF,CHVAL,ICHOI,'EW')
                     ICUMEN(J)=0
                     ICUCHO(J)=0
                  ENDIF
   20          CONTINUE
               ICHOI=ICUCHO(IPLACE)
               CALL IGMENU(0,TITLE(I)
     + ,                   XPM(1,I),XPM(2,I)
     +,                    YPM(1,I),YPM(2,I)
     +,                    NBU(I),CHUSKE(1,I)
     +,                    NBITEM(I),CHITEM(1,I)
     +,                    CHDEF,CHVAL,ICHOI,'TCNH')
               IPLAC=IPLACE
               GOTO 40
            ENDIF
   30    CONTINUE
         IF(ICHOI.EQ.0)THEN
            IPLAC=0
         ENDIF
         GOTO 50
      ENDIF
*
*              Picked into the title bar
*
      IF(ICHOI.EQ.-100)THEN
         ICHOI=-1000
      ENDIF
*
   40 IF(ICHOI.GT.0)ICUCHO(IPLACE)=ICHOI
   50 ICHOIC=ICHOI
*
      IF(ICHOI.LT.0.AND.ICHOI.GT.-100)THEN
         CALL IGMENU(0,TITLE(IPLACE)
     +,              XPM(1,IPLACE),XPM(2,IPLACE)
     +,              YPM(1,IPLACE),YPM(2,IPLACE)
     +,              NBU(IPLACE),CHUSKE(1,IPLACE)
     +,              NBITEM(IPLACE),CHITEM(1,IPLACE)
     +,              CHDEF,CHVAL,ABS(ICHOI),'UT')
      ENDIF
*
      ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
*
      END
