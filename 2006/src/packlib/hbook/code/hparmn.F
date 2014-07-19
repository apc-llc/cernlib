*
* $Id: hparmn.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hparmn.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/02 08/12/92  08.41.57  by  Rene Brun
*-- Author :
      SUBROUTINE HPARMN (X,Y,EY,NPMX,NDIM,ICONTR,R2MINI,MAXPOW,
     +                   COEFFI,ITERM,NCOEF)
*.==========>
*.      SEE HPARAM - CALLING SEQUENCE FOR N-DIM DISTRIBUTIONS
*..=========> ( R.Brun D.Lienart )
      DIMENSION X(NPMX,NDIM),Y(1),EY(1),MAXPOW(1),COEFFI(1),ITERM(1,1)
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcpar0.inc"
#include "hbook/hcpar1.inc"
#include "hbook/hcpar2.inc"
#include "hbook/hcpout.inc"
      DIMENSION DQ(2)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DQ,COEFF,COEFFI
#endif
      EQUIVALENCE (Q(1),DQ(1))
*
*  INITIALIZATIONS
*
      IFLAG=-1
      ND=NDIM
      NPMAX=NPMX
      NP=NPMAX
      NBF=0
      NBFMAX=500
      NEF=0
      NCOMAX=50
      ITAP=0
      ICONT=ICONTR
      DO 5 I=1,8
         IOPT(I)=ICONT-(ICONT/10)*10
         ICONT=ICONT/10
    5 CONTINUE
      SELLIM=1.
      FLEVEL=1.
      R2MIN=R2MINI
      IF (IOPT(7).EQ.2) THEN
         R2MIN=1.5
      ELSE IF (R2MIN.GE.1.) THEN
         R2MIN=2.
      ENDIF
      IF (IOPT(5).GE.1) NEF=PNEF
      IF (IOPT(5).EQ.2.OR.IOPT(6).EQ.2) NBF=PNBF
      IF (PSEL.GT.0..AND.PSEL.LE.ND) SELLIM=PSEL
      IF (PFLV.GT.0..AND.PFLV.LT.1000.) FLEVEL=PFLV
      IF (PLUN.GT.0..AND.PLUN.LT.100.) ITAP=PLUN
      IF (PNBX.GT.0..AND.PNBX.LE.2000.) NBFMAX=PNBX
      IF (PNCX.GT.0..AND.PNCX.LE.50.) NCOMAX=PNCX
*
*  RESERVE WORKING SPACE IN /PAWC/: SET START ADDRESSES
*
#if !defined(CERNLIB_DOUBLE)
      LAHPAR=NPMAX+(ND+1)*NBFMAX+(3*NCOMAX+NPMAX+6)*NCOMAX
#endif
#if defined(CERNLIB_DOUBLE)
      LAHPAR=2*NPMAX+(ND+1)*NBFMAX+(6*NCOMAX+2*NPMAX+11)*NCOMAX
#endif
      CALL HWORK (LAHPAR,ICO,'HPARMN')
      IF (ICO.EQ.0) THEN
         IFLAG=6
         RETURN
      ENDIF
#if defined(CERNLIB_DOUBLE)
      IF ((ICO/2)*2.EQ.ICO) ICO=ICO+1
      IBF=ICO+(10+2*NPMAX+6*NCOMAX)*NCOMAX+2*NPMAX
      ICO=(ICO+1)/2
#endif
      ICT=ICO+NCOMAX
      IW=ICT+NCOMAX
      IWT=IW+NPMAX*NCOMAX
      IWY=IWT+NPMAX
      IV=IWY+NCOMAX
      IVT=IV+NCOMAX*NCOMAX
      IVTT=IVT+NCOMAX*NCOMAX
      IDD=IVTT+NCOMAX*NCOMAX
      IFF=IDD+NCOMAX
#if !defined(CERNLIB_DOUBLE)
      IBF=IFF+NCOMAX
#endif
      IMB=IBF+ND*NBFMAX
      IBM=IMB+NBFMAX
*
*  PRINT OUT FIT OPTIONS AND CHARACTERISTICS
*
      WRITE (LOUT,100)
      IF (IOPT(2).GE.1) THEN
         WRITE (LOUT,110) ND,LAHPAR,NBF,NEF,NCOMAX,(MAXPOW(I),
     +   I=1,ND)
         WRITE (LOUT,120) (IOPT(I),I=2,8),SELLIM,FLEVEL
         IF (ITAP.NE.0) WRITE (LOUT,160) ITAP
         IF (R2MIN.EQ.1.5) THEN
            WRITE (LOUT,130)
         ELSE
            IF (R2MIN.EQ.2.) THEN
               WRITE (LOUT,140)
            ELSE
               WRITE (LOUT,150) R2MIN
            ENDIF
         ENDIF
      ENDIF
      IF (IOPT(2).EQ.2) CALL HCORRL (X,Y)
*
      IF (IOPT(3).EQ.0) THEN
         DO 10 I=1,NP
            IF (EY(I).NE.0.) Y(I)=Y(I)/EY(I)
   10    CONTINUE
      ENDIF
*
      IF (IOPT(8).GT.0) CALL HXNORM (X)
*
*  SET UP BASIC FUNCTIONS TABLE
*
      IF (IOPT(6).EQ.2) THEN
         CALL UCOPY (ITERM(1,1),IQ(IBF),ND*NBF)
      ELSE
         CALL HSETBF (IQ(IBF),MAXPOW,SELLIM)
         IF (IFLAG.EQ.5) RETURN
      ENDIF
*
#if !defined(CERNLIB_DOUBLE)
      CALL HMUFIT (X,Y,EY,IQ(IBF),Q(IW),Q(IWT),Q(IWY),Q(IV),
     +             Q(IVT),Q(IVTT),Q(IDD),Q(IFF),Q(ICO),Q(ICT),IQ(IMB),
     +             IQ(IBM),R2MIN,FLEVEL)
#endif
#if defined(CERNLIB_DOUBLE)
      CALL HMUFIT (X,Y,EY,IQ(IBF),DQ(IW),DQ(IWT),DQ(IWY),
     +             DQ(IV),DQ(IVT),DQ(IVTT),DQ(IDD),DQ(IFF),DQ(ICO),
     +             DQ(ICT),IQ(IMB),IQ(IBM),R2MIN,FLEVEL)
#endif
      NCOEF=NCO
      DO 20 I=1,NCO
#if !defined(CERNLIB_DOUBLE)
         COEFF(I)=Q(ICO+I-1)
#endif
#if defined(CERNLIB_DOUBLE)
         COEFF(I)=DQ(ICO+I-1)
#endif
         COEFFI(I)=COEFF(I)
         DO 15 K=1,ND
            IBASFT(K,I)=IQ(IBF+ND*(I-1)+K-1)
   15    CONTINUE
   20 CONTINUE
      CALL UCOPY (IQ(IBF),ITERM(1,1),ND*NCO)
      IF (ITAP.NE.0) CALL HWRITF (ITAP)
  100 FORMAT (///,1X,40('*'),/,' *',38X,'*',/,' *   MULTIDIMENSIONAL'
     +        ,' PARAMETRIZATION   *'/,' *',38X,'*'/,1X,40('*'))
  110 FORMAT (//' FIT CHARACTERISTICS AND OPTIONS'/,1X,31('*'),/
     +      /,1X,I2,'-DIM DISTRIBUTION',/,' WORKING SPACE IN /PAWC/ = '
     +        ,I7,/,1X,I2,' USER-DEFINED BASIC FUNCTIONS'/,1X,I2,
     +        ' USER-DEFINED ELEMENTARY FUNCTIONS'/,' MAX NUMBER OF',
     +        ' REGRESSORS = ',I2,/,' MAX POWERS OF POLYNOMIALS IN ',
     +        ' EACH DIM = ',10(I2,2X))
  120 FORMAT (' AMOUNT OF OUTPUT = ',I1,/,' WEIGHTING TYPE = '
     +        ,I1,/,' CLASS OF POLYNOMIALS = ',I1,/,' CLASS OF BASIC'
     +        ,' FUNCTIONS = ',I1,/,' BASIC FUNCTION SELECTION MODE = ',
     +        I1,/,' REGRESSION MODE = ',I1,/,' X-NORMALIZATION TYPE = '
     +        ,I1,/,' POWER LIMITOR = ',F5.2,/,' F-TEST LEVEL = ',F6.2)
  130 FORMAT (/' FITTING PROCESS WILL STOP WHEN ALL CANDIDATE ',
     +        'BASIC FUNCTIONS ARE INCLUDED')
  140 FORMAT (/' FITTING PROCESS WILL STOP WHEN THE RESIDUAL VARIANCE'
     +        ,' HITS A MINIMUM')
  150 FORMAT (/' FITTING PROCESS WILL STOP WHEN THE MULTIPLE ',
     +        'CORRELATION COEFFICIENT GETS HIGHER THAN ',F7.4)
  160 FORMAT (' FORTRAN CODE FPARAM WRITTEN ON UNIT ',I2)
      END
