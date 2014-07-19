*
* $Id: hfcnh.F,v 1.3 1997/09/02 13:09:02 mclareni Exp $
*
* $Log: hfcnh.F,v $
* Revision 1.3  1997/09/02 13:09:02  mclareni
* WINNT correction
*
* Revision 1.2  1997/03/14 17:04:23  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:27:59  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFCNH (NPAR,GRAD,F,UD,IFLAG,UFCN)
*.==========>
*.       Computes Minuit function (histogram case)
*..=========> ( R.Brun, E.Lessner )
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION F,GRAD,UD
      DOUBLE PRECISION CUSUM,EUSUM,FUSUM,DERSUM
      DOUBLE PRECISION CU,EU,FU,FSUM,FSUB,FOBS,DSUB
#endif
      DIMENSION DERSUM(35),EXDA(5),XV(2),GRAD(*),UD(*)
#include "hbook/hcfit2.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcfitd.inc"
      EXTERNAL UFCN
*.___________________________________________
*
      NPFITS=0
      F=0.
      DO 10 I=1,NFPAR
         FITPAR(IOFSET+I)=UD(I)
         FITPAD(I)=UD(I)
   10 CONTINUE
      IF (IFLAG.EQ.2) THEN
         DO 20 I=1,NFPAR
            GRAD(I)=0.
            DERSUM(I)=0.
   20    CONTINUE
      ENDIF
*
*=======>     Chisquare fit
*
      IF(LINEAR.NE.0)GO TO 100
      CUSUM=0.
      EUSUM=0.
      FUSUM=0.
      DO 60 L1=1,NUMEP
         CALL HFITH1 (EXDA,L1)
         DO 30 I=1,IDIMPN-2
            XV(I)=EXDA(I+2)
   30    CONTINUE
         CU=EXDA(1)
         EU=EXDA(2)
         IF(EU.LE.0.)GO TO 60
         IF(IDIMPN.EQ.3)THEN
#if defined(CERNLIB_MACMPW) || defined(CERNLIB_MSSTDCALL)
            FU=UFCN(XV,0.)
#else
            FU=UFCN(XV)
#endif
         ELSE
            FU=UFCN(XV(1),XV(2))
         ENDIF
         IF(IFLFUN.NE.0)THEN
            FU=FITFUN
         ENDIF
         IF (IFLAG.EQ.2) THEN
            DO 40 K=1,NFPAR
               DERSUM(K)=DERSUM(K)+FITDER(K)
   40       CONTINUE
         ENDIF
         EU=EU*EU
         FUSUM=FUSUM+FU
         CUSUM=CUSUM+CU
         EUSUM=EUSUM+EU
         TEST=CUSUM*CUSUM/EUSUM
         IF (L1.EQ.NUMEP.OR.IWEIGH.NE.0.OR.TEST.GE.0.) THEN
            NPFITS=NPFITS+1
            IF (IFLAG.EQ.2) THEN
               DO 50 K=1,NFPAR
                  GRAD(K)=GRAD(K)+2*DERSUM(K)*(FUSUM-CUSUM)/EUSUM
                  DERSUM(K)=0.
   50          CONTINUE
            ENDIF
            FSUM=(CUSUM-FUSUM)**2/EUSUM
            F=F+FSUM
            CUSUM=0.
            EUSUM=0.
            FUSUM=0.
         ENDIF
   60 CONTINUE
      RETURN
*
*=======>      Log-Likelihood fit
*     Basically, it forms the likelihood by determining the Poisson
*     probability that given a number of entries in a particualar bin,
*     the fit would predict it's value.  This is then done for each bin,
*     and the sum of the logs is taken as the likelihood.
*
  100 CONTINUE
      DO 160 L1=1,NUMEP
         CALL HFITH1 (EXDA,L1)
         DO 130 I=1,IDIMPN-2
            XV(I)=EXDA(I+2)
  130    CONTINUE
         ICU=EXDA(1)
         EU=EXDA(2)
         IF(IWEIGH.NE.0)THEN
            IF(EU.LE.0.)GO TO 160
         ENDIF
         IF(IDIMPN.EQ.3)THEN
#if defined(CERNLIB_MACMPW) || defined(CERNLIB_MSSTDCALL)
            FU=UFCN(XV,0.)
#else
            FU=UFCN(XV)
#endif
         ELSE
            FU=UFCN(XV(1),XV(2))
         ENDIF
         IF(IFLFUN.NE.0)THEN
            FU=FITFUN
         ENDIF
         IF (FU.LT.1.E-9) FU=1.E-9
         FSUB=-FU+ICU*LOG(FU)
         FOBS= 0.0
         IF (ICU.GT.1) THEN
            DO 135 J=1,ICU
               FOBS=FOBS+LOG(FLOAT(J))
  135       CONTINUE
         ENDIF
         FSUB=FSUB-FOBS
         F=F-FSUB
         NPFITS=NPFITS+1
         IF (IFLAG.EQ.2) THEN
            DO 150 K=1,NFPAR
               DSUB=ICU*FITDER(K)/FU - FITDER(K)
               GRAD(K)=GRAD(K)-DSUB
  150       CONTINUE
         ENDIF
  160 CONTINUE
      F=2.*F
      IF (IFLAG.EQ.2) THEN
         DO 170 I=1,NFPAR
            GRAD(I)=2.*GRAD(I)
  170    CONTINUE
      ENDIF
*
      END
