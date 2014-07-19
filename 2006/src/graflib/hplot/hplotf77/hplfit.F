*
* $Id: hplfit.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplfit.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLFIT
*.==========>
*.
*.   Print fit informations on picture.
*.
*..==========> (O.Couet)
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl13.inc"
      CHARACTER*4 NAME
      CHARACTER*32 CTEXT
      DIMENSION RTXFP(2), RTXAL(2)
      DIMENSION VPNT(4),VP1(4),IBIT(3)
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=1)
      REAL SS
#endif
*.______________________________________
*
      IF((IOPTU.NE.0).OR.(.NOT.LOFIT))RETURN
*
*              Copy Fit parameters from Zebra structure
*
      IF(LCID.NE.0)THEN
         LFUNC=LQ(LCONT-1)
         IF(LFUNC.EQ.0)RETURN
         IF(IQ(LFUNC-2).EQ.0)RETURN
         LHFIT =LQ(LFUNC-1)
         IF(LHFIT.EQ.0)RETURN
         IF(JBIT(IQ(LHFIT),5).EQ.0)THEN
*           Old format
            NFPAR =Q(LHFIT+1)
            NFLINE=NFPAR
            IF(NFPAR.EQ.0)RETURN
            NPFITS=Q(LHFIT+2)
            FITCHI=Q(LHFIT+3)
            DO 10 I=1,NFPAR
               FITPAR(I)=Q(LHFIT+ 4+I)
               FITSIG(I)=Q(LHFIT+24+I)
               CALL UHTOC(Q(LHFIT+43+2*I),4,FITNAM(I),8)
   10       CONTINUE
            IF(FITNAM(1).EQ.'Constant')IFITTY=NFPAR
            IF(FITNAM(1)(1:2).EQ.'A0')IFITTY=1
         ELSE
*           New format (29/07/92).
            IFITTY=IQ(LHFIT+1)
            IF(IFITTY.EQ.0)RETURN
            NFPAR=IQ(LHFIT+2)
            NFLINE=NFPAR
            IF(NFPAR.EQ.0)RETURN
            NPFITS=IQ(LHFIT+3)
            NOTHER=IQ(LHFIT+4)
            FITCHI=Q(LHFIT+6)
            IF(IFITTY.EQ.4)THEN
               CALL HQGETF(LHFIT)
            ELSE
               NP=MIN(NFPAR,35)
               II=11
               DO 20 I=1,NP
                  CALL UCOPY(Q(LHFIT+II),SS,NWW)
                  FITPAD(I)=SS
                  FITPAR(I)=SS
*           Note: FITPAR is only single precision.
                  II=II+NWW
   20          CONTINUE
               NWERR=IQ(LHFIT-1)-NWW*(NFPAR+NOTHER)
               IF(NWERR.GT.0)THEN
                  II=IQ(LHFIT-1)-NWERR+11
                  DO 30 I=1,NP
                     CALL UCOPY(Q(LHFIT+II),SS,NWW)
                     FITSIG(I)=SS
*              Note: FITSIG is only single precision.
                     II=II+NWW
   30             CONTINUE
               ELSE
                  CALL VZERO(FITSIG,NP)
               ENDIF
* Get names if available, otherwise generate from IFITTY.
               DO 40 I=1,NP
                  FITNAM(I)=' '
   40          CONTINUE
               IF(IFITTY.EQ.1)THEN
* Polynomial.
                  N1=MAX(NP,10)
                  DO 50 I=1,N1
                     WRITE(FITNAM(I),'(''A'',I1,6X)')I-1
   50             CONTINUE
                  IF(NP.GT.10)THEN
                     DO 60 I=11,NP
                        WRITE(FITNAM(I),'(''A'',I2,5X)')I-1
   60                CONTINUE
                  END IF
               ELSE IF(IFITTY.EQ.2)THEN
* Exponential.
                  FITNAM(1)='Constant'
                  FITNAM(2)='Slope'
               ELSE IF(IFITTY.EQ.3)THEN
* Gaussian.
                  FITNAM(1)='Constant'
                  FITNAM(2)='Mean'
                  FITNAM(3)='Sigma'
               ELSE IF(IFITTY.NE.4)THEN
                  L=LQ(LHFIT)
   70             CONTINUE
                  IF(L.NE.0)THEN
                     CALL UHTOC(IQ(L-4),4,NAME,4)
                     IF(NAME.EQ.'HFNA')THEN
                        NFLINE=0
                        DO 80 I=1,NP
                           CALL UHTOC(Q(L+2*I-1),4,FITNAM(I),8)
                           IF(FITNAM(I)(1:1).NE.'-')NFLINE=NFLINE+1
   80                   CONTINUE
                     ELSE
                        GO TO 70
                     END IF
                  END IF
               END IF
            END IF
*        Get covariances if required and when available.
         ENDIF
      ELSE
         NFLINE=NFPAR
      ENDIF
*
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('TXCI',RTXCI)
      CALL IGQ('TXFP',RTXFP)
      CALL IGQ('CHHE',RCHHE)
      CALL IGQ('TANG',RTANG)
      CALL IGQ('TXAL',RTXAL)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('BORD',RBORD)
      CALL IGQ('LTYP',RLTYP)
      CALL IGQ('LWID',RLWID)
*
*              Set transformation no 1 (cm)
*
      NTWOLD=NTWIN
      CALL ISELNT(NTHIST)
      CALL IGQWK(1,'NTVP',VPNT)
      CALL ISELNT(1)
      CALL IGQWK(1,'NTVP',VP1)
      NTWIN=1
      CALL IGPID(1,'fit-box',1,' ')
*
*              Decode IFIT
*
      IFIT2=IFIT
      DO 90 I=3,1,-1
         IBIT(I)=IFIT2/(10**(I-1))
         IFIT2=IFIT2-(IBIT(I)*(10**(I-1)))
   90 CONTINUE
      IF(IFITTY.EQ.4)IBIT(1)=0
      NLINES=0
      IF(IBIT(1).NE.0)NLINES=NFLINE
      IF(IBIT(3).NE.0)NLINES=NLINES+1
      IF(IBIT(2).NE.0)THEN
         NCHARD=30
      ELSE
         NCHARD=20
      ENDIF
      ISTAT2=ISTAT
*
*              Compute the offset in case of option STAT
*
      OFFSET=0.
      IF(LOSTAT)THEN
         ISAT2=ISTAT
         DO 100 I=7,1,-1
            IB=ISTAT2/(10**(I-1))
            IF(IB.NE.0)OFFSET=OFFSET+2.
            ISTAT2=ISTAT2-(IB*(10**(I-1)))
  100    CONTINUE
         IF(I230.NE.0)OFFSET=OFFSET+2.
      ENDIF
*
*              Initialize the drawing parameters
*
      CBSIZ=TVSIZ(5)/IXWIN
      BOXHEI=CBSIZ*FLOAT(NLINES)*2.
      BOXLEN=CBSIZ*FLOAT(NCHARD)
*
      XL    = XSIZ*((VPNT(1)-VP1(1))/(VP1(2)-VP1(1)))
      YL    = YSIZ*((VPNT(3)-VP1(3))/(VP1(4)-VP1(3)))
      XH    = XSIZ*((VPNT(2)-VP1(1))/(VP1(2)-VP1(1)))
      YH    = YSIZ*((VPNT(4)-VP1(3))/(VP1(4)-VP1(3)))
      XBOXL = XH-BOXLEN
      YBOXL = YH-BOXHEI-OFFSET*CBSIZ
      XBOXH = XH
      YBOXH = YH-OFFSET*CBSIZ
      XOFF  = (XH-XL)*SMRIT
      YOFF  = (YH-YL)*SMGUP
*
*             Define starting position for text
*
      X1 = XBOXL+CBSIZ-XOFF
      XC = (XBOXH-XBOXL)/2.+XBOXL-XOFF
      X2 = XBOXH-CBSIZ-XOFF
      YT = YBOXH+0.5*CBSIZ-YOFF
*
*             Draw the FIT box
*
      CALL ISLN(ILTYP)
      CALL ISLN(1)
      CALL ISPLCI(1)
      CALL ISTXCI(1)
      CALL ISLWSC(1.)
      CTEXT='TRS'
      IF(LOSTAT)CTEXT='RS'
      SSIZ=CBSIZ
      IF(ISSCOL.EQ.0)SSIZ=0.
      CALL IGPAVE(XBOXL-SSIZ-XOFF,XBOXH-SSIZ-XOFF
     +,           YBOXL-SSIZ-YOFF,YBOXH-SSIZ-YOFF
     +,           SSIZ,0,1000+ISSCOL,CTEXT)
      X1=X1-SSIZ
      X2=X2-SSIZ
      YT=YT-SSIZ
*
*             Set text size, font and precision
*
      KFONT=IABS(IHFONT(5))
      ISTFON=KFONT/10
      ISTPRE=MOD(KFONT,10)
      IF(IHFONT(5).LT.0)ISTFON=-ISTFON
      CALL ISTXFP(ISTFON,ISTPRE)
      CALL ISCHH(CBSIZ)
      CALL IGSET('TANG',0.)
*
*              Draws the CHISQUARE
*
      IF(IBIT(3).NE.0)THEN
         YT=YT-2.0*CBSIZ
         CALL IGTEXT(X1,YT,'[h]^2!/ndf',CBSIZ,0.,'L')
         CTEXT=' '
         WRITE(CTEXT,'(G11.4,''/'',I6)')FITCHI*(NPFITS-NFPAR),
     +   NPFITS-NFPAR
         CALL ISTXAL(3,0)
         IF(IBIT(2).NE.0)THEN
            CALL ITX(XC+2*CBSIZ,YT,CTEXT)
         ELSE
            CALL ITX(X2,YT,CTEXT)
         ENDIF
      ENDIF
*
*              Draws the fit parameters
*
      IF(IBIT(1).NE.0)THEN
         DO 110 I=1,NFPAR
            IF(FITNAM(I)(1:1).EQ.'-')GOTO 110
            YT=YT-2.0*CBSIZ
            CALL ISTXAL(0,0)
            CALL ITX(X1,YT,FITNAM(I))
            CTEXT=' '
            WRITE(CTEXT,'(G11.4)')FITPAR(I)
            CALL ISTXAL(3,0)
            IF(IBIT(2).NE.0)THEN
               CALL ITX(XC+2*CBSIZ,YT,CTEXT)
               CALL IGTEXT(XC+3*CBSIZ,YT,'"A',CBSIZ,0.,'C')
               CTEXT=' '
               WRITE(CTEXT,'(G11.4)')FITSIG(I)
            ENDIF
            CALL ITX(X2,YT,CTEXT)
  110    CONTINUE
      ENDIF
*
*              Reset NT
*
      IF(NTWIN.NE.NTWOLD)THEN
         NTWIN=NTWOLD
         CALL ISELNT(NTWOLD)
      ENDIF
      IF(LOZFL)CALL IZPICT(' ','O')
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('TXCI',RTXCI)
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      CALL IGSET('CHHE',RCHHE)
      CALL IGSET('TANG',RTANG)
      CALL ISTXAL(INT(RTXAL(1)),INT(RTXAL(2)))
      CALL IGSET('FACI',RFACI)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('BORD',RBORD)
      CALL IGSET('LTYP',RLTYP)
      CALL IGSET('LWID',RLWID)
*
      END
