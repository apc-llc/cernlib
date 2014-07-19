*
* $Id: hfcnv.F,v 1.1.1.1 1996/01/16 17:07:36 mclareni Exp $
*
* $Log: hfcnv.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:36  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFCNV (NPAR,GRAD,F,UD,IFLAG,UFCN)
*.==========>
*.       Computes Minuit function (vector case)
*..=========> ( R.Brun, E.Lessner )
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION F,GRAD,UD
      DOUBLE PRECISION CUSUM,EUSUM,FUSUM,DERSUM
      DOUBLE PRECISION CU,EU,FU,FSUM
#endif
      DIMENSION GRAD(*),UD(*)
#include "hbook/hcfit2.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcfitd.inc"
#include "hbook/hcbook.inc"
      DIMENSION XV(50),DERSUM(35)
      EXTERNAL UFCN
*.___________________________________________
*
      NPFITS=0
      F=0.
*
*     INITIALIZE THE SUMS:
      CUSUM=0.
      EUSUM=0.
      FUSUM=0.
*
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
*====>  User own fitting model (option 'FCN' given)
*
      IF(LINEAR.NE.0)THEN
         CALL HFCNV1(NUMEP,Q(ILXE),Q(ILYE),Q(ILEY),NPAR,IFLAG,UFCN)
         F=FITFUN
         RETURN
      ENDIF
*
*====>      Chisquare method
      DO 60 L1=1,NUMEP
         CU=Q(ILYE+L1-1)
         EU=Q(ILEY+L1-1)
         IF(EU.LE.0.)GO TO 60
         DO 30 I=1,NX
            XV(I)=Q(ILXE+L1-1+NUMEP*(I-1))
   30    CONTINUE
#if defined(CERNLIB_MACMPW)
         FU=UFCN(XV,0.)
#endif
#if !defined(CERNLIB_MACMPW)
         FU=UFCN(XV)
#endif
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
      END
