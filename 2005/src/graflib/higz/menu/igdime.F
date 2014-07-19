*
* $Id: igdime.F,v 1.2 1998/01/30 15:22:46 couet Exp $
*
* $Log: igdime.F,v $
* Revision 1.2  1998/01/30 15:22:46  couet
* - APOLLO version removed
*
* Revision 1.1.1.1  1996/02/14 13:11:15  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/00 01/11/93  10.51.24  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGDIME(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT)
*.===========>
*.
*.   This routine display the menu.
*.
*..==========> (O.Couet)
#include "higz/himenu.inc"
#if defined(CERNLIB_GKS)||defined(CERNLIB_DI3000)
#include "higz/hiflag.inc"
#endif
      DIMENSION X(6),Y(6)
      DIMENSION IOPT(*)
      CHARACTER*4 CHOPT
      CHARACTER*(*) CHITEM(*),TITLE,CHDEF(*),CHVAL(*)
      CHARACTER*(*) CHUSER(*)
*.______________________________________
*
      CALL IGSMP(IOPT)
*
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_MGKS))
      IF(GFLAG)THEN
         IF(MENNUM.NE.0)THEN
            IF(ISEGM(MENNUM).NE.0)THEN
               CALL GSVIS(MENNUM,1)
               ICUCH=0
               RETURN
            ELSE
               ISEGM(MENNUM)=1
               CALL GCRSG(MENNUM)
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_DI3000)
      IF(MENNUM.NE.0)THEN
         IF(ISEGM(MENNUM).NE.0)THEN
            CALL JVISBL(MENNUM,1)
            ICUCH=0
            RETURN
         ELSE
            ISEGM(MENNUM)=1
            CALL JROPEN(MENNUM)
         ENDIF
      ENDIF
#endif
*
*              Set the correct font and precision
*
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
      CALL ISPLCI(1)
      CALL ISLN(1)
      CALL IGSET('BORD',1.)
*
      CALL ISCLIP(0)
*
*              Draw the Frame
*
      IF(IOPT(19).NE.0)GOTO 10
      CHOPT=' '
      DZ=0.
      ISFRAM=0
      IF(IOPT(14).NE.0)CHOPT(1:2)='TR'
      IF(IOPT(15).NE.0)CHOPT(1:2)=' R'
      IF(IOPT(16).NE.0)CHOPT(3:3)='P'
      IF(IOPT(18).NE.0)CHOPT(4:4)='S'
      IF(CHOPT.NE.' ')THEN
         DZ=5.*DEC
         IF(IOPT(18).EQ.0)THEN
#if (defined(CERNLIB_GL)||defined(CERNLIB_X11))&&(!defined(CERNLIB_MSDOS))
            ISFRAM=2
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_MSDOS))
            ISFRAM=-104
#endif
#if (!defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))
            ISFRAM=1
#endif
         ELSE
            ISFRAM=1001
         ENDIF
      ENDIF
      CALL IGPAVE(XPOS(1),XPOS(2),YPOS(1),YPOS(2)
     +,           DZ,0,ISFRAM,CHOPT)
      IF(NBCHOI.GT.0)THEN
         CALL ISFACI(0)
         CALL IGSET('BORD',1.)
         CALL IGBOX(XPOS(1)+DEC,XPOS(2)-DEC,Y4,Y3)
      ENDIF
      X(1)=XPOS(1)+RINT
      X(2)=X(1)
      Y(1)=Y3
      Y(2)=YPOS(2)
      CALL IPL(2,X,Y)
      X(1)=XPOS(2)-RINT
      X(2)=X(1)
      CALL IPL(2,X,Y)
      Y(1)=Y4
      Y(2)=Y4
      CALL IPL(2,XPOS,Y)
*
*              Draw the Title bar
*
   10 IF(IOPT(13).EQ.0)THEN
         CALL ISCHH(TH)
         CALL IGSET('TANG',0.)
         CALL ISTXAL(2,0)
         CALL ITX(XM,YT,TITLE)
         CALL ISFACI(0)
         X(1)=XPOS(1)+DEC
         X(2)=XPOS(1)+RINT-DEC
         Y(1)=Y3+DEC
         Y(2)=YPOS(2)-DEC
         CALL IGBOX(X(1),X(2),Y(1),Y(2))
         X(1)=XPOS(2)-RINT+DEC
         X(2)=XPOS(2)-DEC
         CALL IGBOX(X(1),X(2),Y(1),Y(2))
      ENDIF
*
*              Draw the middle bar in case of panel menu type
*
      IF(IOPT(5).NE.0)THEN
         X(1)=XM
         X(2)=XM
         Y(1)=Y3
         Y(2)=Y4
         CALL IPL(2,X,Y)
         X(1)=XPOS(2)-RINT
         X(2)=X(1)
         CALL IPL(2,X,Y)
      ENDIF
*
*              Draw the Options
*
      CALL ISCHH(OTH)
      YOPT=0.
      XOPT=XPOS(1)+RINT
      DO 20 I=1,NBCHOI
         Y(1)=Y3-YSIZ*(I-1)
         Y(2)=Y(1)
         IF(I.NE.1)THEN
            X(1)=XPOS(1)+DEC
            X(2)=XPOS(2)-DEC
            CALL IPL(2,X,Y)
         ELSE
            X(1)=XPOS(1)
            X(2)=XPOS(2)
            CALL IPL(2,X,Y)
         ENDIF
         YOPT=YOPT+YSTEP
         CALL ISTXAL(0,0)
         IF(CHITEM(I)(1:1).EQ.'|')THEN
            CALL ISFAIS(1)
            CALL ISFACI(1)
            CALL IGBOX(XPOS(1),XPOS(1)+DEC,Y(1)-YSIZ,Y(1))
            CALL ITX(XOPT,Y3-YOPT,CHITEM(I)(2:))
         ELSEIF(CHITEM(I)(1:1).EQ.'-')THEN
            CALL ISFAIS(3)
#if (defined(CERNLIB_GL)||defined(CERNLIB_X11))&&(!defined(CERNLIB_MSDOS))
            CALL ISFASI(2)
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_MSDOS))
            CALL ISFASI(-104)
#endif
#if (!defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))
            CALL ISFASI(1)
#endif
            CALL ISFACI(1)
            CALL IGBOX(XPOS(1)+DEC,XPOS(2)-DEC,Y(1)-YSIZ,Y(1))
            CALL ITX(XOPT,Y3-YOPT,CHITEM(I)(2:))
         ELSE
            CALL ITX(XOPT,Y3-YOPT,CHITEM(I))
         ENDIF
         CALL ISTXAL(2,0)
         IF(IOPT(5).NE.0)THEN
            CALL ISTXAL(0,0)
            CALL ITX(XM+RINT,Y3-YOPT,CHVAL(I))
            IF(INDEX(CHDEF(I),',').NE.0)THEN
               CALL ISFAIS(1)
               CALL ISFACI(1)
               CALL IGBOX(XPOS(2)-RINT+0.3*DEC,XPOS(2)-1.3*DEC ,
     +         Y(1)-YSIZ+0.3*DEC,Y(1)-0.3*DEC)
            ENDIF
            CALL ISTXAL(2,0)
         ENDIF
         YOPT=YOPT+YSIZ-YSTEP
   20 CONTINUE
*
*              Draw the user keys
*
      IF(NBUSER.NE.0)THEN
         CHOPT=' '
         DZ=0.
         ISFRAM=0
         IF(IOPT(19).NE.0)THEN
            DZ=5.*DEC
            CHOPT='TRK'
#if (defined(CERNLIB_GL)||defined(CERNLIB_X11))&&(!defined(CERNLIB_MSDOS))
            ISFRAM=2
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_MSDOS))
            ISFRAM=-104
#endif
#if (!defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))
            ISFRAM=1
#endif
         ENDIF
         Y(1)=YPOS(1)
         Y(2)=Y4
         X(1)=XPOS(1)
         X(2)=XPOS(1)+USIZ
         DO 30 I=1,NBUSER
            CALL IGPAVE(X(1),X(2),Y(1),Y(2)
     +,                 DZ,1000,ISFRAM,CHOPT)
            X(1)=X(1)+USIZ+IOPT(19)*DZ*2
            X(2)=X(1)+USIZ
   30    CONTINUE
         YU=YPOS(1)+RINT/2.-(UTH/2.)
         XU=XPOS(1)+USIZ/2.
         CALL ISCHH(UTH)
         CALL ISTXAL(2,0)
         DO 40 I=1,NBUSER
            CALL ITX(XU,YU,CHUSER(I))
            XU=XU+USIZ+IOPT(19)*DZ*2
   40    CONTINUE
      ENDIF
*
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_MGKS))
      IF(GFLAG)THEN
         IF(MENNUM.NE.0)THEN
            CALL GCLSG
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_DI3000)
      IF (GFLAG) THEN
         IF(MENNUM.NE.0)THEN
            CALL JRCLOS
         ENDIF
      ENDIF
#endif
*
      ICUCH=0
*
      END
