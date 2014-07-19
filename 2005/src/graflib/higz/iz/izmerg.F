*
* $Id: izmerg.F,v 1.1.1.1 1996/02/14 13:11:11 mclareni Exp $
*
* $Log: izmerg.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:11  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.19/10 15/09/93  10.06.31  by  O.Couet
*-- Author :
      SUBROUTINE IZMERG(PNAMEI,X0I,Y0I,RI,CHOPT)
*.===========>
*.
*.   This routine merges the picture PNAME in the current picture .
*. the viewport defined in PNAME are transformed according to the
*. parameters X0,Y0,R . These parameters are defined in the NDC space .
*.
*. _Input parameters:
*.
*.  REAL X0,Y0 : Down left corner (in NDC space) of the PNAME picture in
*.               the current picture .
*.  REAL R : Lenght in NDC of the square onto is mapped the transformation 0 .
*.  CHARACTER PNAME : Picture name .
*.  CHARACTER CHOPT : Option .
*.
*.                    CHOPT='D' : If the displayed picture is not the current
*.                                picture the displayed picture is cleared and
*.                                the current picture is displayed .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
      CHARACTER*(*) PNAMEI,CHOPT
      CHARACTER*20 PNAME
      LOGICAL GFSAV,ZFSAV
      DIMENSION IOPT(1)
      EQUIVALENCE (IOPT(1),IOPTD)
*.______________________________________
*
      CALL UOPTC (CHOPT,'D',IOPT)
      PNAME=PNAMEI
*
*              Adressing pictures by number
*
      INUM=IGASCI(PNAME(1:1))
      IF(49.LE.INUM.AND.INUM.LE.57)THEN
         READ (PNAME,'(I15)',ERR=20 ) INUM
         IF(INUM.GT.NBPICT)THEN
            CALL IGERR('Picture not in memory','IZMERG')
            RETURN
         ENDIF
         LP=LQ(LHIGZ)
         DO 10 I=1,INUM-1
            LP=LQ(LP)
   10    CONTINUE
         LCH=LQ(LP-4)
         CALL UHTOC(IQ(LCH+1),4,PNAME,IQ(LP+1))
         GOTO 30
   20    CALL IGERR('PNAME must begin with a letter','IZMERG')
         RETURN
      ENDIF
   30 CONTINUE
*
      X0=X0I
      Y0=Y0I
      R=RI
      LPIC2=IZRPIP(PNAME)
      IF(LPIC2.LE.0)RETURN
      IF(LPICT.LE.0)THEN
         CALL IGERR('No current picture','IZMERG')
         RETURN
      ENDIF
*
      IF((X0.LT.0.).OR.(X0.GT.1.))RETURN
      IF((Y0.LT.0.).OR.(Y0.GT.1.))RETURN
      IF((R.LT.0.).OR.(R.GT.1.))RETURN
      RX=-1.
      RY=-1.
      IF(X0+R.GT.1.)RX=1.-X0
      IF(Y0+R.GT.1.)RY=1.-Y0
      IF((RX.GE.0).OR.(RY.GE.0.))R=MAX(RX,RY)
      IF((RX.GE.0).AND.(RY.GE.0.))R=MIN(RX,RY)
*
      L2NT0=LQ(LPIC2-1)
      L2I=LQ(LPIC2-2)
      L2F=LQ(LPIC2-3)
      L2C=LQ(LPIC2-4)
*
      GFSAV=GFLAG
      ZFSAV=ZFLAG
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      IF((IOPTD.NE.0).AND.(.NOT.MFLAG))THEN
         GFLAG=.TRUE.
         IF(LPICD.NE.LPICT)THEN
            ZFLAG=.FALSE.
            GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
            CALL ICLRWK(0,0)
            CALL IZDIPI(LPICT)
         ENDIF
      ENDIF
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      NTNEW=99
   40 NTNEW=NTNEW+1
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
      CALL IZGNT(NTNEW,IFIND,IAWV,IPRIO,IHPRIO)
      IF(IFIND.NE.0)GOTO 40
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
      IF(IGNSCU(NTNEW).NE.0)GOTO 40
#endif
      CALL ISVP(NTNEW,X0,X0+R,Y0,Y0+R)
      CALL ISELNT(NTNEW)
      CALL IZDNB(L2NT0,L2I,L2F,L2C,0,0)
      IF(IQ(LPIC2+8).EQ.1)GOTO 90
*
*              Merge the NT banks
*
      NGT=0
   50 L2NT=IZGNGT(LPIC2,NGT)
      NGT=IQ(L2NT+3)
      IF(IQ(L2NT+1).EQ.0)GOTO 50
      N=IQ(LPIC2+8)-1
      DO 80 I=1,N
*
* Set the normalisation transformation
*
   60    NTNEW=NTNEW+1
#if (!defined(CERNLIB_NTC))&&(!defined(CERNLIB_X11))
         CALL IZGNT(NTNEW,IFIND,IAWV,IPRIO,IHPRIO)
         IF(IFIND.NE.0)GOTO 60
#endif
#if defined(CERNLIB_NTC)||defined(CERNLIB_X11)
         IF(IGNSCU(NTNEW).NE.0)GOTO 60
#endif
         IWVA=IQ(L2NT+2)
         CALL ISWN(NTNEW
     +,            Q(L2F+IWVA)
     +,            Q(L2F+IWVA+1)
     +,            Q(L2F+IWVA+2)
     +,            Q(L2F+IWVA+3))
         CALL ISVP(NTNEW
     +,           (Q(L2F+IWVA+4)*R+X0)
     +,           (Q(L2F+IWVA+5)*R+X0)
     +,           (Q(L2F+IWVA+6)*R+Y0)
     +,           (Q(L2F+IWVA+7)*R+Y0))
         CALL ISELNT(NTNEW)
*
* Decode the NT bank linked to LN
*
         CALL IZDNB(L2NT,L2I,L2F,L2C,0,0)
*
* Give the link of the next highest priority NT bank
*
   70    LN=IZGNGT(LPIC2,NGT)
         IF(LN.EQ.0)RETURN
         L2NT=LN
         NGT=IQ(L2NT+3)
         IF(IQ(L2NT+1).EQ.0)GOTO 70
   80 CONTINUE
*
   90 GFLAG=GFSAV
      ZFLAG=ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      END
#endif
