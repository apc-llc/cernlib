*
* $Id: hminui.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hminui.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 24/08/94  09.02.15  by  Rene Brun
*-- Author :
      SUBROUTINE HMINUI (UFCN,PARAM,ALIM,BLIM,WERR,EMAT,IVARB)
*.==========>
*.          TOP ROUTINE FOR HFITXX
*..=========> ( E.Lessner,D.Lienart )
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION PARAM,ALIM,BLIM,WERR,FCNMH,ARGLIS,AMIN,
     +                 EDM,ERRDEF,RHOTOL,EMAT,UFCN,EPSIH
#endif
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcpar0.inc"
#include "hbook/hcfit2.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcfits.inc"
#include "hbook/hcrlf.inc"
*
      DIMENSION PARAM(NUP),ALIM(NUP),BLIM(NUP),WERR(NUP),ARGLIS(3),
     +          EMAT(NUP,NUP),IVARB(NUP)
      EXTERNAL UFCN,HFCN
      SAVE LTOLD
      DATA LTOLD/0/
      DATA NAMPO,NAMGA /2HPO,2HGA/
*_________________________________________________
*
*     Initializes Interface HBOOK4-MINUIT
*
      IF(LTOLD.NE.LOUT)THEN
         LTOLD=LOUT
         CALL MNINIT(IRD,LOUT,ISAV)
      ELSE
         CALL MNCLER
      ENDIF
*
*     Calculate iofset
*
      IOFSET=LOCF(PARAM(1))-LOCF(XVAR(1))
*
*    Set initial step sizes for parameters
*
      IF (NAMFUN.NE.NAMPO) THEN
         STMIN=0.1
         IF (IHIS.EQ.1) STMIN=BINWID
         PLEPS=1.E-6
         DO 9 I=1,NUP
            IF (WERR(I).EQ.0..AND.ILICO.LT.2) GOTO 9
            IF (WERR(I).LT.0.) WERR(I)=0.3*ABS(PARAM(I))
            IF (WERR(I).LE.PLEPS*ABS(PARAM(I))) WERR(I)=STMIN
    9    CONTINUE
         IF (NAMFUN.EQ.NAMGA) THEN
            WERR(2)=0.3*ABS(PARAM(2))+BINWID*5.
            WERR(3)=0.3*ABS(PARAM(3))+BINWID*5.
         END IF
      END IF
*
      NITMAX=200
      EPSIH=10.-9.*IMINU
      IF (FNIX.GT.0..AND.FNIX.LT.1000.) NITMAX=FNIX
      IF (FEPS.GT.0..AND.FEPS.LE.1000.) EPSIH=FEPS*10.
      NFCNMH=NITMAX+100*NUP+10*NUP**2
      RHOTOL=1.E-4*EPSIH
      IF (ITPR.GE.0) THEN
         WRITE (LOUT,470)CRLF, NAMFUN
         WRITE (LOUT,480)
         IF (IHIS.EQ.1) WRITE (LOUT,485) ID
         WRITE (LOUT,490) RHOTOL
      END IF
*
*     INITIALIZATIONS
*
      IF (ITPR.LT.1) THEN
         ARGLIS(1)=-1.
         NARG=1
         CALL MNEXCM(HFCN,'SET PRINT',ARGLIS,NARG,IERFL,UFCN)
         ARGLIS(1)=0.
         NARG=0
         CALL MNEXCM(HFCN,'SET NOW',ARGLIS,NARG,IERFL,UFCN)
      ENDIF
*
      IF(FITNAM(1).EQ.' ')THEN
         DO 5 I=1,NUP
            IF(I.LT.10)THEN
               WRITE(FITNAM(I),1000)I
            ELSE
               WRITE(FITNAM(I),2000)I
            ENDIF
    5    CONTINUE
      ENDIF
*
      DO 10 I=1,NUP
         CALL MNPARM(I,FITNAM(I),PARAM(I),WERR(I),ALIM(I),BLIM(I),
     +   IERFLP)
         IF (IERFLP.GT.0) THEN
            WRITE (LERR,*) ' REQUEST FOR PARAMETERS FAILED.'
            WRITE (LERR,*) ' CHECK INPUT PARAMETERS.'
            ICSTAT=0
            RETURN
         ENDIF
   10 CONTINUE
*
*     SET GRADIENT, IF IDER=1 (FORCING DERIVATIVES) OR IDER=2 (NOT FORCING)
*
      IF (IDER.NE.0) THEN
         IF (IDER.EQ.1) THEN
            ARGLIS(1)=1.
         ELSEIF (IDER.EQ.2) THEN
            ARGLIS(1)=0.
         ENDIF
         NARG=1
         CALL MNEXCM(HFCN,'SET GRAD',ARGLIS,NARG,IERFLG,UFCN)
      ENDIF
*
*     RESET PRINT LEVEL
*
      IF (ITPR.GT.0) THEN
         IF (ITPR.EQ.1) THEN
            ARGLIS(1)=0.
         ELSEIF (ITPR.EQ.2) THEN
            ARGLIS(1)=1.
         ELSE
            ARGLIS(1)=2.
         ENDIF
         NARG=1
         CALL MNEXCM(HFCN,'SET PRINT',ARGLIS,NARG,IERFL,UFCN)
      ENDIF
*
*     PERFORM MINIMIZATION
*
      FCNMH=FLOAT(NFCNMH)
      ARGLIS(1)=FCNMH
      ARGLIS(2)=EPSIH
      NARG=2
      CALL MNEXCM(HFCN,'MIGRAD',ARGLIS,NARG,IERFLM,UFCN)
*
*     GET STATUS AT RETURN TIME
*
      DO 20 I=1,NUP
         CALL MNPOUT(I,FITNAM(I),PARAM(I),WERR(I),ALIM(I),BLIM(I),
     +               IVARB(I))
   20 CONTINUE
      CALL MNSTAT(AMIN,EDM,ERRDEF,NUP,NPARX,ICSTAT)
      CHIM=AMIN
*
*     PRINT FINAL VALUES OF PARAMETERS, IF ITPR=0.
*
      IF (ITPR.EQ.0) THEN
         CALL MNPRIN(3,AMIN)
      ENDIF
*
*     COMPUTE COVARIANCE MATRIX IF REQUIRED
*
      IF (IMAT.EQ.0) GOTO 98
      IF (ICSTAT.EQ.0) THEN
         IF (ITPR.GE.0)
     +      WRITE (LOUT,*) ' COVARIANCE MATRIX NOT CALCULATED.'
      ELSEIF (ICSTAT.GT.0) THEN
         IF (ITPR.NE.0) THEN
            ARGLIS(1)=-1.
         ELSE
            ARGLIS(1)=1.
         ENDIF
         NARG=1
         CALL MNEXCM(HFCN,'SET PRINT',ARGLIS,NARG,IERFL,UFCN)
         CALL MNEMAT(EMAT,NUP)
      ENDIF
*
      IF (ITPR.EQ.0.OR.ITPR.EQ.1) CALL MNMATU(0)
   98 IF (NPFIT-NUP.GT.0) THEN
         CHISQ=AMIN/(NPFIT-NUP)
      ELSE
         CHISQ=9999.
      ENDIF
      IF (ITPR.GE.0) THEN
         WRITE (LOUT,500)CHISQ,NPFIT
      ENDIF
*
*          Copy results in /HCFITS/
*
      FITCHI=CHISQ
      NPFITS=NPFIT
      NFPAR =NUP
      DO 40 I=1,NUP
         FITPAR(I)=PARAM(I)
         FITSIG(I)=WERR(I)
   40 CONTINUE
*
  470 FORMAT (A,
     +/'     **********************************************',
     +/'     *                                            *',
     +/'     * FUNCTION MINIMIZATION BY SUBROUTINE HFIT',A2,' *')
  480 FORMAT ('     * VARIABLE-METRIC METHOD                     *')
  485 FORMAT ('     * ID = ',I10,'                            *')
  490 FORMAT ('     *                                            *',
     +/'     **********************************************',/
     +' CONVERGENCE WHEN ESTIMATED DISTANCE TO MINIMUM',
     +' (EDM) .LT. ',E9.2)
  500 FORMAT (/,' CHISQUARE =',E11.4,'  NPFIT =',I6,/)
 1000 FORMAT('P',I1,6X)
 2000 FORMAT('P',I2,5X)
*
      END
