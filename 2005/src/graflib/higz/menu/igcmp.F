*
* $Id: igcmp.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: igcmp.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/00 01/11/93  10.49.48  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCMP(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT,IR)
*.===========>
*.
*.   This routine change the place of the current displayed menu.
*.
*..==========> (O.Couet)
#include "higz/himenu.inc"
#include "higz/hiloc.inc"
#if defined(CERNLIB_GKS)||defined(CERNLIB_DI3000)
#include "higz/hiatt.inc"
#endif
#if defined(CERNLIB_GKS)||defined(CERNLIB_DI3000)
#include "higz/hiflag.inc"
#endif
      DIMENSION IOPT(*)
      CHARACTER*(*) CHITEM(*),TITLE,CHDEF(*),CHVAL
      CHARACTER*(*) CHUSER(*)
*.______________________________________
*
      CALL IGDELM(IOPT)
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_MGKS))
      IF(GFLAG)THEN
         IF(MENNUM.NE.0)THEN
            IF(ISEGM(MENNUM).NE.0)THEN
               CALL GDSGWK(IDID,MENNUM)
               ISEGM(MENNUM)=0
               RETURN
            ENDIF
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_DI3000)
      IF(MENNUM.NE.0)THEN
         IF(ISEGM(MENNUM).NE.0)THEN
            CALL JPURGE(MENNUM)
            ISEGM(MENNUM)=0
            RETURN
         ENDIF
      ENDIF
#endif
*
      IF(IR.NE.0)THEN
   10    CALL IGLOC(1,NT,IBN,X1,Y1,XWC,YWC)
         IF(IBN.EQ.0)GOTO 10
         XPOS(1)=X1-(XPOS(2)-XPOS(1))
         YPOS(1)=Y1-(YPOS(2)-YPOS(1))
         XPOS(2)=X1
         YPOS(2)=Y1
         GOTO 30
      ELSE
   20    CALL IGLOC(1,NT,IBN,X1,Y1,XWC,YWC)
         IF(IBN.EQ.0)GOTO 20
         CALL IGLOC(51,NT,IBN,X2,Y2,XWC,YWC)
         IF(IBN.EQ.0)GOTO 20
         IF((ABS(X1-X2).LT.0.01).OR.
     +   (ABS(Y1-Y2).LT.0.01))GOTO 30
         XPOS(1)=X1
         XPOS(2)=X2
         YPOS(1)=Y1
         YPOS(2)=Y2
         IF(XPOS(2).LT.XPOS(1))THEN
            R=XPOS(2)
            XPOS(2)=XPOS(1)
            XPOS(1)=R
         ENDIF
         IF(YPOS(2).LT.YPOS(1))THEN
            R=YPOS(2)
            YPOS(2)=YPOS(1)
            YPOS(1)=R
         ENDIF
      ENDIF
*
   30 ICUCHS=ICUCH
      CALL ISTXCI(1)
      CALL IGDIME(TITLE,CHUSER,CHITEM,CHDEF,CHVAL,IOPT)
      IF(IOPT(1).NE.0.AND.IOPT(5).EQ.0)THEN
         CALL IGHIGH(ICUCHS,CHITEM,CHUSER,IOPT)
      ENDIF
      OLDXP=XPOS(1)+(XPOS(2)-XPOS(1))/2
      OLDYP=Y3-YSTEP
*
      END
