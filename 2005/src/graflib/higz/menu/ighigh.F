*
* $Id: ighigh.F,v 1.2 1998/01/30 15:22:48 couet Exp $
*
* $Log: ighigh.F,v $
* Revision 1.2  1998/01/30 15:22:48  couet
* - APOLLO version removed
*
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.13/06 17/10/91  16.19.14  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGHIGH(ICHOIC,CHITEM,CHUSER,IOPT)
*.===========>
*.
*.   This routine highlith the choice
*.
*..==========> (O.Couet)
#include "higz/himenu.inc"
#include "higz/hiflag.inc"
      DIMENSION X(2),Y(2)
      DIMENSION IOPT(*)
      CHARACTER*(*) CHITEM(*)
      CHARACTER*(*) CHUSER(*)
      LOGICAL ZFS,GFS
*.______________________________________
*
      ZFS=ZFLAG
      GFS=GFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
*
      IF(ICHOIC.LE.-100.OR.ICHOIC.GT.NBCHOI)GOTO 10
      CALL ISTXAL(0,0)
      CALL IGSET('TANG',0.)
      IF(ISOFT.NE.0)THEN
         CALL ISTXFP(0,2)
      ELSE
#if defined(CERNLIB_X11)
         CALL ISTXFP(-8,0)
#endif
#if !defined(CERNLIB_X11)
         CALL ISTXFP(1,0)
#endif
      ENDIF
      CALL ISCHH(OTH)
      XOPT=XPOS(1)+RINT
*
*              Highlith user key
*
      IF(ICHOIC.LT.0)THEN
         CALL ISFAIS(1)
         CALL ISFACI(1)
         CALL ISPLCI(1)
         CALL ISTXCI(0)
         Y(1)=YPOS(1)
         Y(2)=Y4
         X(1)=XPOS(1)-(ICHOIC+1)*USIZ-IOPT(19)*(ICHOIC+1)*10.*DEC
         X(2)=X(1)+USIZ
         CALL IGBOX(X(1),X(2),Y(1),Y(2))
         YU=YPOS(1)+RINT/2.-(UTH/2.)
         XU=X(1)+(USIZ/2.)
         CALL ISCHH(UTH)
         CALL ISTXAL(2,0)
         CALL ITX(XU,YU,CHUSER(-ICHOIC))
         GOTO 10
      ENDIF
*
*              Invert the last highlithed choice ICUCH
*
      IF(ICUCH.GE.0)THEN
         IF(ICUCH.EQ.0)ICUCH=ICHOIC
         CALL ISFAIS(1)
         CALL ISFACI(0)
         CALL ISPLCI(1)
         CALL ISTXCI(1)
         CALL IGSET('BORD',1.)
         X(1)=XPOS(1)+DEC
         X(2)=XPOS(1)+XSIZ-DEC
         Y(1)=Y3-ICUCH*YSIZ
         Y(2)=Y3-(ICUCH-1)*YSIZ
         CALL IGBOX(X(1),X(2),Y(1),Y(2))
         IF(CHITEM(ICUCH)(1:1).EQ.'-')THEN
            CALL ISFACI(1)
            CALL ISFAIS(3)
#if defined(CERNLIB_GL)||defined(CERNLIB_X11)
            CALL ISFASI(2)
#endif
#if (defined(CERNLIB_GKSGRAL))
            CALL ISFASI(-104)
#endif
#if (!defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))
            CALL ISFASI(1)
#endif
            CALL IGBOX(X(1),X(2),Y(1),Y(2))
         ENDIF
         IF(CHITEM(ICUCH)(1:1).EQ.'|')THEN
            CALL ITX(XOPT,Y3-YSIZ*(ICUCH-1)-YSTEP,CHITEM(ICUCH)(2:))
         ELSEIF(CHITEM(ICUCH)(1:1).EQ.'-')THEN
            CALL ITX(XOPT,Y3-YSIZ*(ICUCH-1)-YSTEP,CHITEM(ICUCH)(2:))
         ELSE
            CALL ITX(XOPT,Y3-YSIZ*(ICUCH-1)-YSTEP,CHITEM(ICUCH))
         ENDIF
      ENDIF
*
*              Highlith the choice ICHOIC
*
      CALL ISFAIS(1)
      CALL ISFACI(1)
      CALL ISPLCI(1)
      CALL ISTXCI(0)
      X(1)=XPOS(1)+DEC
      X(2)=XPOS(1)+XSIZ-DEC
      Y(1)=Y3-ICHOIC*YSIZ
      Y(2)=Y3-(ICHOIC-1)*YSIZ
      CALL IGBOX(X(1),X(2),Y(1),Y(2))
      IF(CHITEM(ICHOIC)(1:1).EQ.'|')THEN
         CALL ITX(XOPT,Y3-YSIZ*(ICHOIC-1)-YSTEP,CHITEM(ICHOIC)(2:))
      ELSEIF(CHITEM(ICHOIC)(1:1).EQ.'-')THEN
         CALL ITX(XOPT,Y3-YSIZ*(ICHOIC-1)-YSTEP,CHITEM(ICHOIC)(2:))
      ELSE
         CALL ITX(XOPT,Y3-YSIZ*(ICHOIC-1)-YSTEP,CHITEM(ICHOIC))
      ENDIF
      ICUCH=ICHOIC
*
   10 ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
      END

