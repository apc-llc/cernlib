*
* $Id: hgfit.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgfit.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 15/12/92  21.48.30  by  Rene Brun
*-- Author :
      SUBROUTINE HGFIT(IDD,NFPAR,NPFITS,FITCHI,FITPAR,FITSIG,FITNAM)
*.==========>
*.
*.   Returns fit parameters
*.
*..==========> (R.Brun)
#include "hbook/hcbook.inc"
      DIMENSION FITPAR(1),FITSIG(1)
      CHARACTER*(*) FITNAM(3)
*.______________________________________
*
      CHARACTER*4 NAME
#if defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=2)
      DOUBLE PRECISION SS
#endif
#if !defined(CERNLIB_DOUBLE)
      PARAMETER (NWW=1)
      REAL SS
#endif
*
      CALL HFIND(IDD,'HGFIT ')
*
      NFPAR=0
      IF(LCID.LE.0)GO TO 999
*
      LFUNC=LQ(LCONT-1)
      IF(LFUNC.EQ.0)GO TO 999
      IF(IQ(LFUNC-2).EQ.0)GO TO 999
      LHFIT =LQ(LFUNC-1)
      IF(LHFIT.EQ.0)GO TO 999
*
      IF(JBIT(IQ(LHFIT),5).EQ.0)THEN
*        Old format
         NFPAR =Q(LHFIT+1)
         IF(NFPAR.EQ.0)GO TO 999
         NPFITS=Q(LHFIT+2)
         FITCHI=Q(LHFIT+3)
         NCH=LEN(FITNAM(1))
         IF(NCH.GT.8)NCH=8
         DO 10 I=1,NFPAR
            FITPAR(I)=Q(LHFIT+ 4+I)
            FITSIG(I)=Q(LHFIT+24+I)
            FITNAM(I)=' '
            CALL UHTOC(Q(LHFIT+43+2*I),4,FITNAM(I),NCH)
   10    CONTINUE
      ELSE
*        New format (29/07/92).
         IFITTY=IQ(LHFIT+1)
         IF(IFITTY.EQ.0)GO TO 999
         NFPAR=IQ(LHFIT+2)
         IF(NFPAR.EQ.0)GO TO 999
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
               FITPAR(I)=SS
*           Note: FITPAR is only single precision.
               II=II+NWW
   20       CONTINUE
            NWERR=IQ(LHFIT-1)-NWW*(NFPAR+NOTHER)
            IF(NWERR.GT.0)THEN
               II=IQ(LHFIT-1)-NWERR+11
               DO 30 I=1,NP
                  CALL UCOPY(Q(LHFIT+II),SS,NWW)
                  FITSIG(I)=SS
*              Note: FITSIG is only single precision.
                  II=II+NWW
   30          CONTINUE
            ENDIF
* Get names if available, otherwise generate from IFITTY.
            DO 40 I=1,NP
               FITNAM(I)=' '
   40       CONTINUE
            IF(IFITTY.EQ.1)THEN
* Polynomial.
               N1=MAX(NP,10)
               DO 50 I=1,N1
                  WRITE(FITNAM(I),'(''A'',I1,6X)')I-1
   50          CONTINUE
               IF(NP.GT.10)THEN
                  DO 60 I=11,NP
                     WRITE(FITNAM(I),'(''A'',I2,5X)')I-1
   60             CONTINUE
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
   70          CONTINUE
               IF(L.NE.0)THEN
                  CALL UHTOC(IQ(L-4),4,NAME,4)
                  IF(NAME.EQ.'HFNA')THEN
                     DO 80 I=1,NP
                        CALL UHTOC(Q(L+2*I-1),4,FITNAM(I),8)
   80                CONTINUE
                  ELSE
                     GO TO 70
                  END IF
               END IF
            END IF
         END IF
*        Get covariances if required and when available.
      END IF
*
  999 END
