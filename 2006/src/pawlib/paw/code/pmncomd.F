*
* $Id: pmncomd.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pmncomd.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/19 19/09/94  11.52.08  by  Rene Brun
*-- Author :    Rene Brun   27/07/93
      SUBROUTINE PMNCOMD(FCN,UFCN)
*.
*.        Routine called by HMINUT to read MINUIT data cards
*.        Minuit command MNC is intercepted if it has 3 arguments
*.           In this case a graphical contour is drawn
*.           and the points in the contour are saved in vectors XFIT,YFIT
*.
*.        Normal PAW commands can be executed from MINUIT if they
*.        are prefixed with a +
*.
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcminpu.inc"
#include "hbook/hcunit.inc"
#include "paw/quest.inc"
      DIMENSION ITOK(2,20)
      CHARACTER*80 CHLINE
      CHARACTER*32 CHOPT
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION XPTU(50),YPTU(50)
      DOUBLE PRECISION PAR,AL,BL,WE
#endif
#if !defined(CERNLIB_DOUBLE)
      DIMENSION XPTU(50),YPTU(50)
#endif
      DIMENSION LENG(3)
      EXTERNAL FCN,UFCN
*_________________________________________________
*
      NINP=NCMINP
  10  IF(NINP.EQ.0)THEN
         CHINPU(1)=' '
         CALL KUPROS('Minuit >',CHINPU(1),NCH)
         NCMINP=1
      ENDIF
      DO 20 IC=1,NCMINP
         CHLINE=CHINPU(IC)
         CALL KILEXP(CHLINE,' ','''','''',10,ITOK,NTOK,' ')
         IF(NTOK.LE.0)GO TO 20
         IF(CHLINE(1:1).EQ.'+')THEN
            CALL KUEXEL(CHLINE(2:))
            GO TO 20
         ENDIF
         CALL CLTOU(CHLINE)
*
*--   HELP command
         IF(CHLINE(1:3).EQ.'HEL')THEN
            NCH=LENOCC(CHLINE)
            ITEM=INDEX(CHLINE(1:NCH),' ')
            IF(ITEM.EQ.0)THEN
               CALL MNHELP('*',LOUT)
               WRITE(LOUT,11100)
11100          FORMAT(' NAMe      Set name of one parameter')
            ELSE
               IF(CHLINE(ITEM+1:ITEM+3).EQ.'NAM')THEN
                  WRITE(LOUT,11000)
11000             FORMAT(' ***>NAME <parnum> <parname>',/,
     +          ' Set name of parameter number <parnum>')
                  GO TO 20
               ENDIF
               DO 12 I=ITEM+1,NCH
                  IF(CHLINE(I:I).NE.' ')THEN
                     CALL MNHELP(CHLINE(I:NCH),LOUT)
                     GO TO 20
                  ENDIF
  12           CONTINUE
            ENDIF
            GO TO 20
         ENDIF
*
*--  NAMe
         IF(NTOK.GT.2.AND.CHLINE(1:3).EQ.'NAM')THEN
            K11=ITOK(1,2)
            K12=ITOK(2,2)
            CALL KICTOI(CHLINE(K11:K12),I)
            CALL MNPOUT(I,FITNAM(I),PAR,WE,AL,BL,IERFLP)
            IF(IERFLP.LT.0)GO TO 20
            K21=ITOK(1,3)
            FITNAM(I)=CHINPU(IC)(K21:)
            CALL MNPARM(I,FITNAM(I),PAR,WE,AL,BL,IERFLP)
            GO TO 20
         ENDIF
*
*--  MNCONtour
         IF(NTOK.GT.3.AND.CHLINE(1:3).EQ.'MNC')THEN
            K11=ITOK(1,2)
            K12=ITOK(2,2)
            K21=ITOK(1,3)
            K22=ITOK(2,3)
            K31=ITOK(1,4)
            K32=ITOK(2,4)
            CHOPT=CHLINE(K31:K32)
            CALL KICTOI(CHLINE(K11:K12),KE1)
            CALL KICTOI(CHLINE(K21:K22),KE2)
            NPTU=50
            CALL MNCONT(FCN,KE1,KE2,NPTU,XPTU,YPTU,IERR,UFCN)
            IF(IERR.LT.0)GO TO 20
            IF(IERR.GT.0)NPTU=IERR
            CALL KUVECT('XFIT',LX1,LX2)
            IF(LX1.NE.0)THEN
               CALL KUVDEL('XFIT')
               CALL KUVDEL('YFIT')
            ENDIF
            NCHA=NPTU+1
            LENG(1)=NCHA
            LENG(2)=0
            LENG(3)=0
            CALL KUVCRE('XFIT','R',LENG,LX1,LX2)
            CALL KUVCRE('YFIT','R',LENG,LY1,LY2)
            DO 15 I=1,NPTU
               Q(LX1+I-1)=XPTU(I)
               Q(LY1+I-1)=YPTU(I)
  15        CONTINUE
            Q(LX1+NPTU)=Q(LX1)
            Q(LY1+NPTU)=Q(LY1)
            CALL PAHLOG(LOGX,LOGY,LOGZ)
            XMIN=VMIN(Q(LX1),NCHA)
            XMAX=VMAX(Q(LX1),NCHA)
            DX=0.05*(XMAX-XMIN)
            IF(DX.EQ.0.)DX=0.05*XMAX
            IF(DX.EQ.0.)DX=1.
            YMIN=VMIN(Q(LY1),NCHA)
            YMAX=VMAX(Q(LY1),NCHA)
            DY=0.05*(YMAX-YMIN)
            IF(DY.EQ.0.)DY=0.05*YMAX
            IF(DY.EQ.0.)DY=1.
            X1=XMIN-DX
            X2=XMAX+DX
            Y1=YMIN-DY
            Y2=YMAX+DY
            IF(X1.LT.0..AND.XMIN.GE.0.)X1=0.
            IF(X2.GT.0..AND.XMAX.LE.0.)X2=0.
            IF(Y1.LT.0..AND.YMIN.GE.0.)Y1=0.
            IF(Y2.GT.0..AND.YMAX.LE.0.)Y2=0.
            IF(LOGX.NE.0)THEN
               X1=0.5*XMIN
               X2=2.*XMAX
            ENDIF
            IF(LOGY.NE.0)THEN
               Y1=0.5*YMIN
               Y2=2.*YMAX
            ENDIF
            CALL HPLFRA(X1,X2,Y1,Y2,' ')
            CALL HUOPTC(CHOPT,'LC*PBF',IQUEST(81))
            IOPDR = 0
            DO 80 I=1,6
               IOPDR = IOPDR+IQUEST(80+I)
   80       CONTINUE
            IF(IOPDR.NE.0)THEN
               IF(LOGX.NE.0.OR.LOGY.NE.0)CHOPT(32:32) = 'G'
               IF(LOGX.NE.0)CHOPT(31:31)              = 'X'
               IF(LOGY.NE.0)CHOPT(30:30)              = 'Y'
               IQUEST(81)   = LX1
               IQUEST(82)   = LY1
               CHOPT(29:29) = 'Z'
               CALL IGRAPH(NCHA,Q(LX1),Q(LY1),CHOPT)
               CALL HPLAX(FITNAM(KE1),FITNAM(KE2))
            ENDIF
            GO TO 20
         ENDIF
         CALL MNCOMD(FCN,CHLINE,IERR,UFCN)
         IF(IERR.GE.10.AND.IERR.LE.12)GO TO 90
  20  CONTINUE
      IF(NINP.EQ.0)GO TO 10
  90  CONTINUE
*
      RETURN
      END
