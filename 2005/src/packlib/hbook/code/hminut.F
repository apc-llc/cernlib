*
* $Id: hminut.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hminut.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 15/11/94  12.19.06  by  Rene Brun
*-- Author :
      SUBROUTINE HMINUT (FCN,UFCN,PARAM,WERR,ALIM,BLIM,CHOPT)
*.==========>
*.          TOP ROUTINE FOR HFITXX
*..=========> ( R.Brun, E.Lessner)
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION PAR,AL,BL,WE,FCNMH,ARGLIS,AMIN,
     +    EDM,ERRDEF,EPSIH,EPLUS,EMINUS,EPARAB,GLOBCC
#endif
      DIMENSION PARAM(*),ALIM(*),BLIM(*),WERR(*),ARGLIS(3)
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcpar0.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcfitd.inc"
#include "hbook/hcminpu.inc"
#include "hbook/hchoptm.inc"
*
      CHARACTER*(*) CHOPT
      CHARACTER*10 CHOPTT
      EXTERNAL UFCN,FCN
      SAVE LTOLD
      DATA LTOLD/0/
*_________________________________________________
*
*     Initializes Interface HBOOK4-MINUIT
*
      CHOPTM=CHOPT
      IFLFUN=INDEX(CHOPT,'U')
      IOPTE=INDEX(CHOPT,'E')
      IOPTZ=INDEX(CHOPT,'Z')
      IF(LTOLD.NE.LOUT)THEN
         LTOLD=LOUT
         IRD=5
         ISAV=7
         CALL MNINIT(IRD,LOUT,ISAV)
*        ARGLIS(1)=0.0000002
         ARGLIS(1)=1.E-7
         NARG=1
         CALL MNEXCM(FCN,'SET EPS',ARGLIS,NARG,IERFL,UFCN)
      ELSE
         CALL MNCLER
      ENDIF
*
*     Set error criterion for Chi**2 or Liklihood fits
*
      NARG=1
      ARGLIS(1) = 1.0
      IOPTL=INDEX(CHOPT,'L')
*
*-*  MINUIT ERRDEF should not be set to 0.5 in case of loglikelihood fit.
*-*  because the FCN is already multipiled by 2 in HFCNH
*
*      IF(IOPTL.NE.0)  ARGLIS(1)=0.5
      CALL MNEXCM(FCN,'SET ERR',ARGLIS,NARG,IERFL,UFCN)
*
*           Name parameters if not already done
*
      IF(FITNAM(1).EQ.' ')THEN
         DO 2 I=1,NFPAR
            IF(I.LT.10)THEN
               WRITE(FITNAM(I),10200)I
            ELSE
               WRITE(FITNAM(I),10300)I
            ENDIF
    2    CONTINUE
      ENDIF
*
*        Calculate iofset
*
      IOFSET=LOCF(PARAM(1))-LOCF(FITPAR(1))
*
*        Set initial step sizes for parameters
*
      STMIN=BINWID
      PLEPS=1.E-6
      DO 10 I=1,NFPAR
         IF (WERR(I).EQ.0.) GOTO 10
         IF (WERR(I).LT.0.) WERR(I)=0.3*ABS(PARAM(I))
         IF (WERR(I).LE.PLEPS*ABS(PARAM(I))) WERR(I)=STMIN
   10 CONTINUE
*
      NITMAX=200+100*NFPAR+10*NFPAR**2
      EPSIH=1.
      IF (FNIX.GT.0.) NITMAX=FNIX
      IF (FEPS.GT.0.) EPSIH=FEPS
      IF (ITFUM.GE.0) THEN
         CHOPTT=CHOPT
         IF(KSQUEZ.EQ.0)CALL HFORMA(3)
         WRITE (LOUT,10000) FITNAM(NFPAR+1)(1:6),ID,CHOPTT,EPSIH
      END IF
*
*     Initializations
*
      IF (ITFUM.LT.1) THEN
         ARGLIS(1)=-1.
         NARG=1
         CALL MNEXCM(FCN,'SET PRINT',ARGLIS,NARG,IERFL,UFCN)
         ARGLIS(1)=0.
         NARG=0
         CALL MNEXCM(FCN,'SET NOW',ARGLIS,NARG,IERFL,UFCN)
      ENDIF
*
      DO 30 I=1,NFPAR
         PAR= PARAM(I)
         AL = ALIM(I)
         BL = BLIM(I)
         WE = WERR(I)
         CALL MNPARM(I,FITNAM(I),PAR,WE,AL,BL,IERFLP)
         IF (IERFLP.GT.0) THEN
            WRITE (LERR,*) ' REQUEST FOR PARAMETERS FAILED.'
            WRITE (LERR,*) ' CHECK INPUT PARAMETERS.'
            ICSTAT=0
            RETURN
         ENDIF
   30 CONTINUE
*
*         Invoke interactive Minuit if option 'M'
*
      IF(INDEX(CHOPT,'M').NE.0)THEN
         IF(IADINP.EQ.0)THEN
            CALL MNREAD(FCN,3,IFLGUT,UFCN)
         ELSE
            CALL JUMPST(IADINP)
            CALL JUMPX2(FCN,UFCN)
         ENDIF
         IF(INDEX(CHOPT,'K').EQ.0)THEN
            NCMINP=0
         ENDIF
         GO TO 500
      ENDIF
*
*     Set gradient
*
      IF (IDER.NE.0) THEN
         IF (IDER.EQ.1) THEN
            ARGLIS(1)=1.
         ELSEIF (IDER.EQ.2) THEN
            ARGLIS(1)=0.
         ENDIF
         NARG=1
         CALL MNEXCM(FCN,'SET GRAD',ARGLIS,NARG,IERFLG,UFCN)
      ENDIF
*
*     Reset print level
*
      IF (ITFUM.GT.0) THEN
         IF (ITFUM.EQ.1) THEN
            ARGLIS(1)=0.
         ELSEIF (ITFUM.EQ.2) THEN
            ARGLIS(1)=1.
         ELSE
            ARGLIS(1)=2.
         ENDIF
         NARG=1
         CALL MNEXCM(FCN,'SET PRINT',ARGLIS,NARG,IERFL,UFCN)
      ENDIF
*
*     Perform minimization
*
      FCNMH=FLOAT(NITMAX)
      ARGLIS(1)=FCNMH
      ARGLIS(2)=EPSIH
      NARG=2
      CALL MNEXCM(FCN,'MIGRAD',ARGLIS,NARG,IERFLM,UFCN)
      IF(IOPTE.NE.0)THEN
         NARG=0
         CALL MNEXCM(FCN,'HESSE' ,ARGLIS,NARG,IERFLM,UFCN)
         CALL MNEXCM(FCN,'MINOS' ,ARGLIS,NARG,IERFLM,UFCN)
      ENDIF
*
*     Get status at return time
*
  500 CONTINUE
      DO 40 I=1,NFPAR
         CALL MNPOUT(I,FITNAM(I),PAR,WE,AL,BL,IVARB)
         FITPAD(I)=PAR
         PARAM(I) = PAR
         ALIM(I)  = AL
         BLIM(I)  = BL
         IF(IOPTE.EQ.0)THEN
            WERR(I)  = WE
         ELSE
            CALL MNERRS(I,EPLUS,EMINUS,EPARAB,GLOBCC)
            IF(EPLUS.GT.0..AND.EMINUS.LT.0.)THEN
               WERR(I)  = 0.5*(EPLUS-EMINUS)
            ELSE
               WERR(I)  = WE
            ENDIF
         ENDIF
   40 CONTINUE
      CALL MNSTAT(AMIN,EDM,ERRDEF,NVPAR,NPARX,ICSTAT)
*
*     Print final values of parameters, if ITFUM=0.
*
      IF (ITFUM.EQ.0) THEN
         IF(IOPTE.EQ.0)THEN
            CALL MNPRIN(3,AMIN)
         ELSE
            CALL MNPRIN(4,AMIN)
         ENDIF
      ENDIF
*
*   If log-likelihood, compute an equivalent chisquare
      IF(LINEAR.NE.0.AND.IOPTZ.EQ.0)THEN
         LINEAR=0
         CALL FCN(NFPAR,FITPAD,AMIN,FITPAD,1,UFCN)
      ENDIF
*
      IF (NPFITS-NVPAR.GT.0) THEN
         FITCHI=AMIN/(NPFITS-NVPAR)
      ELSE
         FITCHI=9999.
      ENDIF
      IF (ITFUM.GE.0) THEN
         WRITE (LOUT,10100)FITCHI,NPFITS
      ENDIF
*
*          Copy results in /HCFITS/
*
      DO 50 I=1,NFPAR
         FITPAR(I)=PARAM(I)
         FITSIG(I)=WERR(I)
   50 CONTINUE
*
10000 FORMAT (
     +/'     **********************************************',
     +/'     *                                            *',
     +/'     * Function minimization by SUBROUTINE ',A6,' *',
     +/'     * Variable-metric method                     *',
     +/'     * ID = ',I10,'  CHOPT = ',A,        '        *',
     +/'     *                                            *',
     +/'     **********************************************',
     +/' Convergence when estimated distance to minimum',
     +' (EDM) .LT. ',E9.2)
10100 FORMAT (/,' CHISQUARE =',E11.4,'  NPFIT =',I6,/)
10200 FORMAT('P',I1,6X)
10300 FORMAT('P',I2,5X)
*
      END
