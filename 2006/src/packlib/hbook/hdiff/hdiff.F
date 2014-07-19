*
* $Id: hdiff.F,v 1.2 1997/02/21 12:11:51 couet Exp $
*
* $Log: hdiff.F,v $
* Revision 1.2  1997/02/21 12:11:51  couet
* In the comparison of two scatter plots, if OPT=F1 or OPT=F2
* the Kolmogoroff probability can become anamalously large.
* Bug fixed by Garry Levman levman@physics.utoronto.ca
*
* Revision 1.1.1.1  1996/01/16 17:07:56  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  10.13.44  by  Julian Bunn
*-- Author :  Fred James, Inc.
      SUBROUTINE HDIFF(ID1,ID2,PRB,CHOPT)
*.==========>
*.       Statistical test of compatibility in shape between
*.          histograms ID1 and ID2, using Kolmogorov test.
*.          Default: Ignore under- and overflow bins in comparison
*.
*.              CHOPT is a character string to specify options
*.                 'U' include Underflows in test  (also for 2-dim)
*.                 'O' include Overflows     (also valid for 2-dim)
*.                 'N' include comparison of normalizations
*.                 'D' Put out a line of 'Debug' printout
*.                 'F1' Histogram 1 has no error (is a function)
*.                 'F2' Histogram 2 has no error (is a function)
*.              and for 2-dim scattergrams only:
*.                 'L'=Left: include x-underflows
*.                 'R'=Right: include x-overflows
*.                 'T'=Top: include y-overflows
*.                 'B'=Bottom: include y-underflows
*. for example: 'OB' means x- and y-overflows and y-underflows !!
*.
*.              PRB output parameter : probability of test
*.               (PRB much less than one means NOT compatible)
*.
*..=========> ( F.James, March, 1987 )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
      LOGICAL  AFUNC1, AFUNC2
      CHARACTER*(*) CHOPT
      DIMENSION IOPT(11)
*.___________________________________________
      PRB = 0.
*           Find addresses of ID1 and ID2
      CALL HFIND(ID1,'HDIFF ')
      IF(LCID.EQ.0)GO TO 99
      LCID1=LCID
      CALL HFIND(ID2,'HDIFF ')
      IF(LCID.EQ.0)GO TO 99
      LCID2=LCID
      NCX1=IQ(LCID1+KNCX)
      NCX2=IQ(LCID2+KNCX)
*
*           Decode options
      CALL HUOPTC(CHOPT,'UONDLRTBF12',IOPT)
*
*                 Check consistency in number of channels
      IF(NCX1.NE.NCX2)THEN
         CALL HBUG('Number of channels is different','HDIFF',ID1)
         GO TO 99
      ENDIF
*              Check consistency in channel edges
      DIFPREC=1.E-5
      DIFF1=ABS(Q(LCID1+KXMIN)-Q(LCID2+KXMIN))
      DIFF2=ABS(Q(LCID1+KXMAX)-Q(LCID2+KXMAX))
      IF    (DIFF1.GT.DIFPREC .OR. DIFF2.GT.DIFPREC)  THEN
           CALL HBUG('Different binning','HDIFF',ID1)
         GO TO 99
      ENDIF
*               Check if histograms are in fact functions
      AFUNC1 = .FALSE.
      AFUNC2 = .FALSE.
      IF (IOPT(9) .NE. 0)  THEN
         IF (IOPT(10) .NE. 0) AFUNC1 = .TRUE.
         IF (IOPT(11) .NE. 0) AFUNC2 = .TRUE.
      ENDIF
      IF (AFUNC1 .AND. AFUNC2) THEN
         CALL HBUG('Cannot compare two functions','HDIFF',ID1)
         GO TO 99
      ENDIF
      IF (JBIT(IQ(LCID1+KBITS),1).NE. JBIT(IQ(LCID2+KBITS),1)) THEN
         CALL HBUG('Cannot compare 1-dim and 2-dim','HDIFF',ID1)
         GO TO 99
      ENDIF
      IF(JBIT(IQ(LCID1+KBITS),1).EQ.0)GO TO 100
*
*             1-DIM HIST    ...........................................
      IBEGIN = 1
      IEND = NCX1
      IF (IOPT(1) .NE. 0) IBEGIN = 0
      IF (IOPT(2) .NE. 0) IEND = IEND + 1
*
      SUM2=0.
      DO 10 I=IBEGIN,IEND
         SUM2=SUM2+HCX(I,1)
  10  CONTINUE
      TSUM2 = SUM2
      IF (IOPT(1) .EQ. 0)  TSUM2 = TSUM2 + HCX(0,1)
      IF (IOPT(2) .EQ. 0)  TSUM2 = TSUM2 + HCX(IEND+1,1)
*          Sums for second histogram
      LCONT=LQ(LCID1-1)
      SUM1=0.
      DO 20 I=IBEGIN,IEND
         SUM1=SUM1+HCX(I,1)
  20  CONTINUE
      TSUM1 = SUM1
      IF (IOPT(1) .EQ. 0)  TSUM1 = TSUM1 + HCX(0,1)
      IF (IOPT(2) .EQ. 0)  TSUM1 = TSUM1 + HCX(IEND+1,1)
*                Error if one or both histograms is empty
      IF (SUM1.EQ.0.)  CALL HBUG('Integral is zero','HDIFF',ID1)
      IF (SUM2.EQ.0.)  CALL HBUG('Integral is zero','HDIFF',ID2)
      IF (SUM1.EQ.0. .OR. SUM2.EQ.0.) GO TO 99
      ESUM1 = SUM1
      IF (.NOT. AFUNC1)  THEN
*                Check if histogram 1 is weighted.
*                 If (number of entries=number of channels) then
*                    we assume HPAK has been called
         CALL HNOENT(ID1,NDD)
         DIFSUM1=(REAL(NDD)-TSUM1)/TSUM1
         IF ((DIFSUM1.GT.DIFPREC).AND.(NDD.NE.NCX1))  THEN
            IF (IOPT(1)+IOPT(2) .NE. 0)  THEN
               CALL HBUG('U/O option with weighted events',
     +                   'HDIFF',ID1)
               GO TO 99
            ENDIF
            IF (LQ(LQ(LCID1-1)) .EQ. 0)  THEN
               CALL HBUG('Weighted events and no HBARX','HDIFF',ID1)
               GO TO 99
            ELSE
               ESUM1 = HSTATI(ID1,3,'HIST',1)
            ENDIF
         ENDIF
      ENDIF
*                  Look at second histogram
      ESUM2 = SUM2
      IF (.NOT. AFUNC2)  THEN
*                Check if histogram 2 is weighted.
         CALL HNOENT(ID2,NDD)
         DIFSUM2=(REAL(NDD)-TSUM2)/TSUM2
         IF ((DIFSUM2.GT.DIFPREC).AND.(NDD.NE.NCX1))  THEN
            IF (IOPT(1)+IOPT(2) .NE. 0)  THEN
               CALL HBUG('U/O option with weighted events',
     +                   'HDIFF',ID2)
               GO TO 99
            ENDIF
            IF (LQ(LQ(LCID2-1)) .EQ. 0)  THEN
               CALL HBUG('Weighted events and no HBARX','HDIFF',ID2)
               GO TO 99
            ELSE
               ESUM2 = HSTATI(ID2,3,'HIST',1)
            ENDIF
         ENDIF
      ENDIF
*
      S1 = 1.0/SUM1
      S2 = 1.0/SUM2
*
*          Find largest difference for Kolmogorov Test
      DFMAX = 0.
      RSUM1 = 0.
      RSUM2 = 0.
*
      DO 30 IBIN= IBEGIN,IEND
         LCONT = LQ(LCID1-1)
         RSUM1 = RSUM1 + S1*HCX(IBIN,1)
         LCONT = LQ(LCID2-1)
         RSUM2 = RSUM2 + S2*HCX(IBIN,1)
         DFMAX = MAX(DFMAX,ABS(RSUM1-RSUM2))
   30 CONTINUE
*         Get Kolmogorov probability
      IF (AFUNC1) THEN
         Z = DFMAX*SQRT(ESUM2)
      ELSE IF (AFUNC2) THEN
         Z = DFMAX*SQRT(ESUM1)
      ELSE
         Z = DFMAX*SQRT(ESUM1*ESUM2/(ESUM1+ESUM2))
      ENDIF
      PRB = HDIPKL(Z)
      IF (IOPT(3) .NE. 0)  THEN
*                Combine probabilities for shape and normalization,
         PRB1 = PRB
         RESUM1 = ESUM1
         IF (AFUNC1) RESUM1 = 0.
         RESUM2 = ESUM2
         IF (AFUNC2) RESUM2 = 0.
         CHISQ = (ESUM1-ESUM2)**2 / (RESUM1+RESUM2)
         PRB2 = PROB(CHISQ,1)
*                see Eadie et al., section 11.6.2
         IF (PRB.GT.0. .AND. PRB2.GT.0.) THEN
            PRB = PRB*PRB2*(1.0-LOG(PRB*PRB2))
         ELSE
            PRB = 0.
         ENDIF
      ENDIF
*        debug printout
      IF (IOPT(4) .NE. 0) THEN
      WRITE (LOUT,2001) ID1,SUM1,CHOPT,PRB,ID2,SUM2,DFMAX
        IF (IOPT(3) .NE. 0)  WRITE (LOUT,2002)  PRB1,PRB2
      ENDIF
*
 2001 FORMAT (/' HDIFF OPTIONS   PROB   ID1=',I10,'  SUM1=',E14.7,
     +     '  MAX DIST'/3X,A10,F8.5,'   ID2=',I10,'  SUM2=',E14.7,
     +     2X,2F8.5)
 2002     FORMAT ('        PROB=',F8.5,' FOR SHAPE ALONE,  ',
     +             F8.5,' FOR NORMALIZATION ALONE.')
*
*            This numerical error condition should never occur:
      IF (ABS(RSUM1-1.0) .GT. 0.002)
     +  CALL HBUG('Numerical problems','HDIFF',ID1)
      IF (ABS(RSUM2-1.0) .GT. 0.002)
     +  CALL HBUG('Numerical problems','HDIFF',ID2)
      GO TO 99
*
*             2-DIM HIST        .......................................
*
  100 CONTINUE
*               Check if number of Y-bins is the same
      NCY1=IQ(LCID1+KNCY)
      NCY2=IQ(LCID2+KNCY)
      IF(NCY1.NE.NCY2)THEN
         CALL HBUG('Number of Y channels is different','HDIFF',ID1)
         GO TO 99
      ENDIF
*              Check consistency in Y-channel edges
      DIFF1=ABS(Q(LCID1+KYMIN)-Q(LCID2+KYMIN))
      DIFF2=ABS(Q(LCID1+KYMAX)-Q(LCID2+KYMAX))
      IF    (DIFF1.GT.DIFPREC .OR. DIFF2.GT.DIFPREc)  THEN
           CALL HBUG('Different binning in Y','HDIFF',ID1)
           GO TO 99
      ENDIF
*                Should we include Uflows, Oflows?
      IBEG = 1
      IF (IOPT(1).NE.0 .OR. IOPT(5).NE.0)  IBEG = 0
      IEND = NCX1
      IF (IOPT(2).NE.0 .OR. IOPT(6).NE.0)  IEND = NCX1+1
      JBEG = 1
      IF (IOPT(1).NE.0 .OR. IOPT(8).NE.0)  JBEG = 0
      JEND = NCY1
      IF (IOPT(2).NE.0 .OR. IOPT(7).NE.0)  JEND = NCY1+1
*
      LSCAT=LQ(LCID2-1)
      SUM2=0.
      TSUM2 = 0.
      DO 120 I=0,NCX1+1
         DO 110 J=0,NCY1+1
            HSAV = HCXY(I,J,1)
            TSUM2 = TSUM2 + HSAV
            IF (I.GE.IBEG.AND.I.LE.IEND.AND.J.GE.JBEG.AND.
     +         J.LE.JEND)  SUM2=SUM2+HSAV
 110     CONTINUE
 120  CONTINUE
*
      LSCAT=LQ(LCID1-1)
      SUM1 = 0.
      TSUM1 = 0.
      DO 140 I=0,NCX1+1
         DO 130 J=0,NCY1+1
            HSAV = HCXY(I,J,1)
            TSUM1 = TSUM1 + HSAV
            IF (I.GE.IBEG.AND.I.LE.IEND.AND.J.GE.JBEG.AND.
     +         J.LE.JEND)  SUM1=SUM1+HSAV
 130     CONTINUE
 140  CONTINUE
*           Check that both scatterplots contain events
      IF (SUM1 .EQ. 0.) THEN
         CALL HBUG('Integral is zero','HDIFF',ID1)
         GO TO 99
      ENDIF
      IF (SUM2 .EQ. 0.) THEN
         CALL HBUG('Integral is zero','HDIFF',ID2)
         GO TO 99
      ENDIF
*         Check that scatterplots are not weighted or saturated
      CALL HNOENT(ID1,NUM1)
      IF (REAL(NUM1) .NE. TSUM1)  THEN
         CALL HBUG('Saturation or weighted events','HDIFF',ID1)
         GO TO 99
      ENDIF
      CALL HNOENT(ID2,NUM2)
      IF (REAL(NUM2) .NE. TSUM2)  THEN
         CALL HBUG('Saturation or weighted events','HDIFF',ID2)
         GO TO 99
      ENDIF
*            Find first Kolmogorov distance for scatterplots
      S1 = 1.0/SUM1
      S2 = 1.0/SUM2
      DFMAX = 0.
      RSUM1 = 0.
      RSUM2 = 0.
      DO 160 I=IBEG,IEND
         DO 150 J= JBEG,JEND
         LSCAT = LQ(LCID1-1)
         RSUM1 = RSUM1 + S1*HCXY(I,J,1)
         LSCAT = LQ(LCID2-1)
         RSUM2 = RSUM2 + S2*HCXY(I,J,1)
         DFMAX = MAX(DFMAX,ABS(RSUM1-RSUM2))
  150    CONTINUE
  160 CONTINUE
*            Find second Kolmogorov distance for scatterplots
      DFMAX2 = DFMAX
      DFMAX = 0.
      RSUM1 = 0.
      RSUM2 = 0.
      DO 180 J=JBEG,JEND
         DO 170 I= IBEG,IEND
         LSCAT = LQ(LCID1-1)
         RSUM1 = RSUM1 + S1*HCXY(I,J,1)
         LSCAT = LQ(LCID2-1)
         RSUM2 = RSUM2 + S2*HCXY(I,J,1)
         DFMAX = MAX(DFMAX,ABS(RSUM1-RSUM2))
  170    CONTINUE
  180 CONTINUE
*          Get Kolmogorov probability for scatterplot
      IF (AFUNC1) THEN
         FACTNM = SQRT(SUM2)
      ELSE IF (AFUNC2) THEN
         FACTNM = SQRT(SUM1)
      ELSE
         FACTNM = SQRT(SUM1*SUM2/(SUM1+SUM2))
      ENDIF
      Z = DFMAX * FACTNM
      Z2= DFMAX2* FACTNM
      PRB = HDIPKL(0.5*(Z+Z2))
      IF (IOPT(3) .NE. 0)  THEN
*                 Combine probabilities for shape and normalization,
         PRB1 = PRB
         RESUM1 = SUM1
         IF (AFUNC1) RESUM1 = 0.
         RESUM2 = SUM2
         IF (AFUNC2) RESUM2 = 0.
         CHISQ = (SUM1-SUM2)**2 / (RESUM1+RESUM2)
         PRB2 = PROB(CHISQ,1)
*                 see Eadie et al., section 11.6.2
         IF (PRB.GT.0. .AND. PRB2.GT.0.) THEN
            PRB = PRB*PRB2*(1.0-LOG(PRB*PRB2))
         ELSE
            PRB = 0.
         ENDIF
      ENDIF
*        debug printout
      IF (IOPT(4) .NE. 0) THEN
        WRITE (LOUT,2001) ID1,SUM1,CHOPT,PRB,ID2,SUM2,DFMAX,DFMAX2
        IF (IOPT(3) .NE. 0)  WRITE (LOUT,2002)  PRB1,PRB2
      ENDIF
      IF (ABS(RSUM1-1.0) .GT. 0.002)
     +  CALL HBUG('Numerical problems','HDIFF',ID1)
      IF (ABS(RSUM2-1.0) .GT. 0.002)
     +  CALL HBUG('Numerical problems','HDIFF',ID2)
  99  CONTINUE
      IDLAST = 0
      END
