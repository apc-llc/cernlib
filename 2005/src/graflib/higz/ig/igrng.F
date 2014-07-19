*
* $Id: igrng.F,v 1.1.1.1 1996/02/14 13:10:38 mclareni Exp $
*
* $Log: igrng.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if !defined(CERNLIB_DI3000)
*CMZ :  1.21/03 31/05/94  14.59.57  by  O.Couet
*-- Author :
      SUBROUTINE IGRNG(XSIZI,YSIZI)
*.==========>
*.
*.   This routines returns the screen range and map the workstation
*. window on the whole screen. If the screen is not a square a part
*. of the NDC space is not visible .
*.
*. _Input parameters:
*.
*.  REAL XSIZ YSIZ : Screen range .
*.
*..=========>
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_DECGKS)
      REAL*4 RDUMMY(4),VIEW(4)
#endif
      DIMENSION RVAL(2)
*.______________________________________
*
      XSIZ=XSIZI
      YSIZ=YSIZI
#if defined(CERNLIB_MAIL)
*
*              Mail option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,2E16.7)') 201,XSIZ,YSIZ
         CALL IMWRIT(1)
         READ (5,'(5E15.6)') XYTWN,DXWN,DYWN,XNCM,YNCM
         CALL IMWRIT(5)
         MFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL ISWN(1,0.,XSIZ,0.,YSIZ)
         CALL ISVP(1,DXWN,XNCM+DXWN,DYWN,YNCM+DYWN)
         CALL ISELNT(1)
         RQUEST(11)=XYTWN
         RQUEST(12)=DXWN
         RQUEST(13)=DYWN
         RQUEST(14)=XNCM
         RQUEST(15)=YNCM
         MFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
#endif
*
      IF(XSIZ.LT.0..OR.YSIZ.LT.0.)THEN
         CALL IGERR('X size and Y size must be positive','IGRNG')
         RETURN
      ENDIF
*
*              Initialise parameters
*
      RVAL(1)=1.
      RVAL(2)=1.
      XWKWN=1.
      YWKWN=1.
      IWKID=1
*
*              If output on metafile
*
      IF (METACT.OR.PFLAG) THEN
         IERR=0
#if defined(CERNLIB_MGKS)||defined(CERNLIB_PLOT10GKS)||defined(CERNLIB_UNIGKS)
         CALL GQMDS(IWTYPE,IERR,IDC,RVAL(1),RVAL(2),ILX,ILY)
#endif
#if defined(CERNLIB_GKSGRAL)||defined(CERNLIB_DECGKS)||defined(CERNLIB_ATCGKS)||defined(CERNLIB_GKSBX)
         CALL GQDSP(IWTYPE,IERR,IDC,RVAL(1),RVAL(2),ILX,ILY)
#endif
#if defined(CERNLIB_ATCGKS)
         IF(IERR.EQ.22)THEN
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_DECGKS))&&(!defined(CERNLIB_ATCGKS))
         IF(IERR.EQ.31)THEN
#endif
#if defined(CERNLIB_GKSGRAL)||defined(CERNLIB_DECGKS)||defined(CERNLIB_ATCGKS)
            RVAL(1)=1.
            RVAL(2)=1.
         ENDIF
#endif
         XWKWN=MIN(1.,RVAL(1)/RVAL(2))
         YWKWN=MIN(1.,RVAL(2)/RVAL(1))
#if defined(CERNLIB_PSCRIPT)
         IF(PFLAG)CALL IPRNG(XSIZ,YSIZ)
#endif
      ENDIF
*
*              If output on screen
*
      IF(TEKACT)THEN
         IERR=0
#if defined(CERNLIB_MGKS)||defined(CERNLIB_PLOT10GKS)||defined(CERNLIB_UNIGKS)
         CALL GQMDS(IWTYPE,IERR,IDC,RVAL(1),RVAL(2),ILX,ILY)
#endif
#if defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS)||defined(CERNLIB_GKSBX)
         CALL GQDSP(IWTYPE,IERR,IDC,RVAL(1),RVAL(2),ILX,ILY)
#endif
#if defined(CERNLIB_ATCGKS)
         IF(IERR.EQ.22)THEN
#endif
#if (defined(CERNLIB_GKSGRAL))&&(!defined(CERNLIB_ATCGKS))
         IF(IERR.EQ.31)THEN
#endif
#if defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS)
            RVAL(1)=1.
            RVAL(2)=1.
         ENDIF
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_DECGKS)||defined(CERNLIB_PHIGS)||defined(CERNLIB_FALCO)
         CALL IGQWK(IWKID,'MXDS',RVAL)
#endif
*
         XWKWN=MIN(1.,RVAL(1)/RVAL(2))
         YWKWN=MIN(1.,RVAL(2)/RVAL(1))
         CALL ISWKWN(IWKID,0.,XWKWN,0.,YWKWN)
#if !defined(CERNLIB_DECGKS)
         CALL ISWKVP(IWKID,0.,RVAL(1),0.,RVAL(2))
#endif
#if defined(CERNLIB_X11)
         DO 10 I=1,INOPWK
            IWKL = ILOPWK(I)
            CALL IGQWK(IWKL,'MXDS',RVAL)
            CALL ISWKWN(IWKL,0.,XWKWN,0.,YWKWN)
            CALL ISWKVP(IWKL,0.,RVAL(1),0.,RVAL(2))
            CALL IKUWK(IWKL)
            XMARG=0.
            YMARG=0.
            IF(RVAL(1).GT.XWKV2(I))XMARG = (RVAL(1)-XWKV2(I))/2.
            IF(RVAL(2).GT.YWKV2(I))YMARG = (RVAL(2)-YWKV2(I))/2.
            IF(XMARG.NE.0..OR.YMARG.NE.0.)THEN
               CALL ISWKVP(IWKL,XMARG,XWKV2(I)+XMARG
     +,                         YMARG,YWKV2(I)+YMARG)
               CALL IKUWK(IWKL)
            ENDIF
   10    CONTINUE
#endif
      ENDIF
*
*              Transformation no 1 : cm <---> espace ecran
*
      IF (XSIZ.LT.YSIZ) THEN
         XNCM=YWKWN*XSIZ/YSIZ
         YNCM=YWKWN
         DXWN=(XWKWN-XNCM)/2.
         IF(DXWN.LT.0.)THEN
            XNCM=XWKWN
            DXWN=0.
            YNCM=XWKWN*YSIZ/XSIZ
            DYWN=(YWKWN-YNCM)/2.
            XYTWN=XWKWN/XSIZ
         ELSE
            DYWN=0.
            XYTWN=YWKWN/YSIZ
         ENDIF
      ELSE
         XNCM=XWKWN
         YNCM=XWKWN*YSIZ/XSIZ
         DYWN=(YWKWN-YNCM)/2.
         IF(DYWN.LT.0.)THEN
            YNCM=YWKWN
            DYWN=0.
            XNCM=YWKWN*XSIZ/YSIZ
            DXWN=(XWKWN-XNCM)/2.
            XYTWN=YWKWN/YSIZ
         ELSE
            DXWN=0.
            XYTWN=XWKWN/XSIZ
         ENDIF
      ENDIF
#if defined(CERNLIB_PSCRIPT)
*
      IF(PFLAG)THEN
         XVS1=DXWN
         XVS2=XNCM+DXWN
         YVS1=DYWN
         YVS2=YNCM+DYWN
      ENDIF
#endif
*
      CALL ISWN(1,0.,XSIZ,0.,YSIZ)
      CALL ISVP(1,DXWN,XNCM+DXWN,DYWN,YNCM+DYWN)
      CALL ISELNT(1)
#if (!defined(CERNLIB_SUNGKS))&&(!defined(CERNLIB_ATCGKS))
      IF (METACT) THEN
         CALL ISWKWN (IDMETA,DXWN,XNCM+DXWN,DYWN,YNCM+DYWN)
         CALL ISWKVP (IDMETA,0., XSIZ*XMFACT,0.,YSIZ*YMFACT)
         CALL IUWK   (IDMETA,1)
      ENDIF
#endif
*
      RQUEST(11)=XYTWN
      RQUEST(12)=DXWN
      RQUEST(13)=DYWN
      RQUEST(14)=XNCM
      RQUEST(15)=YNCM
*
      END
#endif
