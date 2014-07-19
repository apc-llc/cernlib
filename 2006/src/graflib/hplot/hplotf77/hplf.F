*
* $Id: hplf.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplf.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/02 11/07/95  10.21.04  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE HPLF(IFUN)
*.==========>
*.            draws a function
*..=========>
#include "hplot/quest.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl11.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
      CHARACTER*4 NAME
      CHARACTER*8 CHOPT
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=1)
      REAL SS
#endif
*._____________________________
*
      CALL IGQ('LWID',RLWID)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('BORD',RBORD)
      CALL IGQ('MSCF',RMSCF)
*
      IF (LOASTK) THEN
         IF(YMAXI.EQ.YMINI)GOTO 999
         RTY = (YHIGH-YLOW)/(YMAXI-YMINI)
         SC  = RTY*TVSIZ(8)
         SCN = (SC*XYTWN)/0.009
         CALL ISMKSC(SCN)
      ENDIF
*----
*             Retrieve the first (IFIRST) and last (ILAST)
*             channels, compute the numbers of bins (NBINS),
*             take care of the option ZOOM (HPLZOM) and compute
*             the X value corresponding to IFIRST an ILAST.
*----
      IF (IFUN.EQ.3) THEN
         LFUNC  = LQ(LCONT-1)
         IFIRST = IQ(LFUNC+1)
         ILAST  = IQ(LFUNC+2)
      ELSE
         IFIRST = 1
         ILAST  = ICMAX
      ENDIF
      IF (LOZOOM) THEN
         IF (IFIRST.GT.NCMAX) GOTO 999
         IF (ILAST .LT.NCMIN) GOTO 999
         IFIRST = MAX(NCMIN,IFIRST)
         ILAST  = MIN(NCMAX,ILAST)
      ENDIF
      NBINS     = ILAST-IFIRST+1
      XFIRST    = XX0+XXSIZE*(IFIRST-1)
      XLAST     = XX0+XXSIZE*ILAST
*----
*             Retrieve the fit parameters
*----
      IFITTY = 0
      IF (LCID.NE.0) THEN
         LFUNC = LQ(LCONT-1)
         IF (LFUNC.EQ.0) GOTO 90
         IF (IQ(LFUNC-2).EQ.0) GO TO 90
         LHFIT = LQ(LFUNC-1)
         IF (LHFIT.EQ.0) GO TO 90
         IF (JBIT(IQ(LHFIT),5).EQ.0) THEN
*           Old format
            NFPAR = Q(LHFIT+1)
            IF (NFPAR.EQ.0) GO TO 90
            NPFITS = Q(LHFIT+2)
            FITCHI = Q(LHFIT+3)
            DO 10 I=1,NFPAR
               FITPAR(I) = Q(LHFIT+ 4+I)
               FITSIG(I) = Q(LHFIT+24+I)
               CALL UHTOC(Q(LHFIT+43+2*I),4,FITNAM(I),8)
   10       CONTINUE
            IF (FITNAM(1).EQ.'Constant') IFITTY = NFPAR
            IF (FITNAM(1)(1:2).EQ.'A0')  IFITTY = 1
         ELSE
* New format (29/07/92).
            IFITTY = IQ(LHFIT+1)
            IF (IFITTY.EQ.0) GO TO 90
            NFPAR = IQ(LHFIT+2)
            IF (NFPAR.EQ.0) GO TO 90
            NPFITS = IQ(LHFIT+3)
            NOTHER = IQ(LHFIT+4)
            FITCHI = Q(LHFIT+6)
            IF (IFITTY.EQ.4) THEN
               CALL HQGETF(LHFIT)
            ELSE
               NP = MIN(NFPAR,35)
               II = 11
               DO 20 I=1,NP
                  CALL UCOPY(Q(LHFIT+II),SS,NWW)
                  FITPAD(I) = SS
                  FITPAR(I) = SS
*              Note: FITPAR is only single precision.
                  II = II+NWW
   20          CONTINUE
               NWERR = IQ(LHFIT-1)-NWW*(NFPAR+NOTHER)
               IF (NWERR.GT.0) THEN
                  II = IQ(LHFIT-1)-NWERR+11
                  DO 30 I=1,NP
                     CALL UCOPY(Q(LHFIT+II),SS,NWW)
                     FITSIG(I) = SS
*                 Note: FITSIG is only single precision.
                     II = II+NWW
   30             CONTINUE
               ELSE
                  CALL VZERO(FITSIG,NP)
               ENDIF
* Get names if available, otherwise generate from IFITTY.
               DO 40 I=1,NP
                  FITNAM(I) = ' '
   40          CONTINUE
               IF (IFITTY.EQ.1) THEN
* Polynomial.
                  N1 = MAX(NP,10)
                  DO 50 I=1,N1
                     WRITE(FITNAM(I),'(''A'',I1,6X)')I-1
   50             CONTINUE
                  IF (NP.GT.10) THEN
                     DO 60 I=11,NP
                        WRITE(FITNAM(I),'(''A'',I2,5X)')I-1
   60                CONTINUE
                  END IF
               ELSE IF (IFITTY.EQ.2) THEN
* Exponential.
                  FITNAM(1) = 'Constant'
                  FITNAM(2) = 'Slope'
               ELSE IF (IFITTY.EQ.3) THEN
* Gaussian.
                  FITNAM(1) = 'Constant'
                  FITNAM(2) = 'Mean'
                  FITNAM(3) = 'Sigma'
               ELSE IF (IFITTY.NE.4) THEN
                  L = LQ(LHFIT)
   70             CONTINUE
                  IF (L.NE.0) THEN
                     CALL UHTOC(IQ(L-4),4,NAME,4)
                     IF (NAME.EQ.'HFNA') THEN
                        DO 80 I=1,NP
                           CALL UHTOC(Q(L+2*I-1),4,FITNAM(I),8)
   80                   CONTINUE
                     ELSE
                        GO TO 70
                     END IF
                  END IF
               END IF
            END IF
*        Get covariances if required and when available.
         END IF
         IF (IFITTY.GT.0.AND.IFITTY.LT.100) GOTO 110
      ENDIF
*
   90 IF (LQ(LHPLOT-3).NE.0) CALL MZDROP(IHDIV,LQ(LHPLOT-3),' ')
      IF (LQ(LHPLOT-4).NE.0) CALL MZDROP(IHDIV,LQ(LHPLOT-4),' ')
      IF (I6.EQ.0) THEN
*----
*             Equidistant bins
*----
         CALL MZNEED(IHDIV,2+(NBINS+1)+50,'G')
         IF (IQUEST(11).LT.0) THEN
            CALL HBUG('Not enough space in memory','HPLOT',IDD)
            GOTO 999
         ENDIF
         CALL MZBOOK(IHDIV,LX,LHPLOT,-3,'TMPX',0,0,2,3,0)
      ELSE
*----
*             Non_equidistant bins
*----
         CALL MZNEED(IHDIV,2*(NBINS+1)+50,'G')
         IF (IQUEST(11).LT.0) THEN
            CALL HBUG('Not enough space in memory','HPLOT',IDD)
            GOTO 999
         ENDIF
         CALL MZBOOK(IHDIV,LX,LHPLOT,-3,'TMPX',0,0,NBINS+1,3,0)
      ENDIF
      CALL MZBOOK(IHDIV,LY,LHPLOT,-4,'TMPY',0,0,NBINS+1,3,0)
*
      IF (I6.EQ.0) THEN
*----
*             Equidistant bins
*----
         Q(LX+1) = XFIRST
         Q(LX+2) = XLAST
      ELSE
*----
*             Non_equidistant bins
*----
         LBINS   = LQ(LCID-2)
         Q(LX+1) = Q(LBINS+IFIRST)
      ENDIF
*
      DO 100 I=1,NBINS
         IF (I6.NE.0) THEN
            Q(LX+I+1) = Q(LBINS+I+IFIRST)
         ENDIF
         F1 = HCX(I+IFIRST-1,IFUN)
         IF (.NOT.LOGYFL) THEN
            Q(LY+I) = F1
         ELSE
            F1      = AMAX1(F1,10**YMINI)
            Q(LY+I) = ALOG10(F1)
         ENDIF
  100 CONTINUE
*----
*             Attributes setting
*----
  110 CALL HPLATT(2)
*----
*             CHOPT encoding
*----
      CHOPT = ' '
      IF (LOASTK) THEN
         CHOPT(1:1) = '*'
      ELSE
         IF (IFITTY.GT.0.AND.IFITTY.LT.100) THEN
            CHOPT(2:2) = 'L'
         ELSE
            CHOPT(2:2) = 'C'
         ENDIF
      ENDIF
      IF (I6.NE.0)CHOPT(3:3) = 'N'
      IF (IFTYP.NE.1000.AND.IFTYP.NE.0) THEN
         CHOPT(4:4) = 'F'
         IF (LOGYFL) CHOPT(5:5) = '1'
      ENDIF
      IF (LOGXFL) CHOPT(6:7) = 'GX'
*----
*             Draws the special functions according to IFITTY:
*
*             IFITTY = 1 --> Polynomial
*             IFITTY = 2 --> Exponential
*             IFITTY = 3 --> Gaussian
*             IFITTY = 4 --> Multiquadric
*
*----
      IF (IFITTY.GT.0.AND.IFITTY.LT.100) THEN
         NP = 199
         IF (LOGXFL) THEN
            XSTP = (10**XMAXI-10**XMINI)/FLOAT(NP-1)
         ELSE
            XSTP = (XMAXI-XMINI)/FLOAT(NP-1)
         ENDIF
*
         IS     = 1
         IE     = NP
         XDIFF1 = ABS(XLAST-XFIRST)
         XDIFF2 = XDIFF1
*
         DO 120 I=1,NP
            IF (LOGXFL) THEN
               XLINE(I) = 10**XMINI+(I-1)*XSTP
            ELSE
               XLINE(I) = XMINI+(I-1)*XSTP
            ENDIF
*
            IF (ABS(XLINE(I)-XFIRST).LT.XDIFF1) THEN
               XDIFF1 = ABS(XLINE(I)-XFIRST)
               IS     = I
            ENDIF
            IF (ABS(XLINE(I)-XLAST).LT.XDIFF2) THEN
               XDIFF2 = ABS(XLINE(I)-XLAST)
               IE     = I
            ENDIF
*
            IF (IFITTY.EQ.1) YLINE(I) = HPOLYN(XLINE(I))
            IF (IFITTY.EQ.2) YLINE(I) = HDEXPO(XLINE(I))
            IF (IFITTY.EQ.3) YLINE(I) = HGAUS(XLINE(I))
            IF (IFITTY.EQ.4) YLINE(I) = HQF(XLINE(I))
            IF (LOGYFL) THEN
               YLINE(I) = MIN(10**YMAXI,YLINE(I))
               YLINE(I) = AMAX1(YLINE(I),10**YMINI)
               YLINE(I) = ALOG10(YLINE(I))
            ELSE
               YLINE(I) = MIN(YMAXI,YLINE(I))
            ENDIF
  120    CONTINUE
*
         NP = IE-IS+1
*
         IF (CHOPT(4:4).EQ.'F') THEN
            IF (LOGXFL) THEN
               XLINE(200) = 10**XMAXI
               XLINE(201) = 10**XMINI
            ELSE
               XLINE(200) = XMAXI
               XLINE(201) = XMINI
            ENDIF
            YLINE(200)  = YMINI
            YLINE(201)  = YMINI
            NP          = NP+2
            YLINE(IE+1) = YMINI
            YLINE(IE+2) = YMINI
            XLINE(IE+1) = XLINE(IE)
            XLINE(IE+2) = XLINE(IS)
         ENDIF
         CALL IGRAPH(NP,XLINE(IS),YLINE(IS),CHOPT)
      ELSE
*----
*             Draws the function
*----
         IQUEST(81) = LX+1
         IQUEST(82) = LY+1
         CHOPT(8:8) = 'Z'
         CALL IGHIST(NBINS,Q(LX),Q(LY),CHOPT)
         CALL MZDROP(IHDIV,LQ(LHPLOT-3),' ')
         CALL MZDROP(IHDIV,LQ(LHPLOT-4),' ')
         LQ(LHPLOT-3) = 0
         LQ(LHPLOT-4) = 0
      ENDIF
*
  999 CALL IGSET('LWID',RLWID)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('BORD',RBORD)
      CALL IGSET('MSCF',RMSCF)
*
      END
