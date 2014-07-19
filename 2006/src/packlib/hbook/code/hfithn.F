*
* $Id: hfithn.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfithn.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.19/00 28/04/93  11.39.30  by  Rene Brun
*-- Author :
      SUBROUTINE HFITHN(ID,CUFUNC,CHOPT,NPP,PARAM,STEP,PMIN,PMAX,SIGPAR
     +                  ,CHI2)
*.==========>
*.
*.           Fits the parametric function to the contents of the
*.           1- or 2-dimensional histogram IDD,and optionally super-
*.           imposes it to the 1-dimensional histogram when editing
*.
*.
*.           Input     IDD     = histogram identifier
*.           arguments
*.                     CUFUNC  = Parametric function (CHARACTER)
*.
*.                     CHOPT   = Character string for options
*.
*.                     NP      = Number of parameters
*.
*.                     PARAM   = Initial values of parameters(array)
*.
*.                     STEP    = initial step sizes for parameters
*.                                search (array)
*.
*.                     PMIN    = Lower parameter bounds (array)
*.                     PMAX    = Upper
*.
*.           Output    PARAM     = Final values of parameters (array)
*.           arguments
*.                     SIGPAR    = Standard deviations of parameters
*.                                  (array)
*.                     CHI2      = Chisquare of fit
*.
*.
*.
*.    ---------------------------REMARKS------------------------------
*.
*.                CHOPT : Possible options 'RQVNBLDW'
*.
*.    'R'  Fit a Restricted area of the histogram
*.          IFXLOW = IQUEST(11)
*.          IFXUP  = IQUEST(12)
*.          IFYLOW = IQUEST(13)
*.          IFYUP  = IQUEST(14)
*.
*.    'Q'  Quiet mode. No print
* .   'V'  Verbode mode. Results after each iteration are printed
*.         By default only final results are printed.
*.
*.    'N'  Do not store the result of the fit bin by bin with the histogram.
*.         By default the function is calculated at the centre of each bin
*.         in the specified range.
*.
*.    'B'  Some or all parameters are bounded. The array STEP,PMIN,PMAX
*.         must be specified.
*.         Default is: All parameters vary freely.
*.
*.    'M'  Interactive Minuit is invoked.
*.
*.    'L'  Use Log Likelihood.
*.         Default is chisquare method.
*.
*.    'D'  The user is assumed to compute derivatives analytically
*.         using the routine HDERIV.
*.         By default, derivatives are computed numerically.
*.
*.    'W'  Sets weights equal to 1.
*.         By default, weights are taken following statistical errors.
*.
*.    'T'  HFITHN is being called by another HBOOK routine
*.         In this case takes parameter names from HCFITS.
*.
*.    'U'  User function value is taken from /HCFITD/FITPAD(24),FITFUN
*.
*..=========> ( R.Brun)
*
#include "hbook/hcfitf.inc"
      COMMON/PAWPAR/PAR(200)
      DIMENSION PARAM(*),SIGPAR(*),STEP(*),PMIN(*),PMAX(*)
      DIMENSION IOPT(13)
      EQUIVALENCE (IOPTQ,IOPT(1)),(IOPTV,IOPT(2))
      EQUIVALENCE (IOPTB,IOPT(3)),(IOPTL,IOPT(4))
      EQUIVALENCE (IOPTD,IOPT(5)),(IOPTW,IOPT(6))
      EQUIVALENCE (IOPTR,IOPT(7)),(IOPTN,IOPT(8))
      EQUIVALENCE (IOPTT,IOPT(9)),(IOPTM,IOPT(10))
      EQUIVALENCE (IOPTF,IOPT(11))
      CHARACTER*(*) CHOPT,CUFUNC
      EXTERNAL HFITHC
*.___________________________________________
      CALL HUOPTC(CHOPT,'QVBLDWRNTEMUF',IOPT)
*
      NOPER=0
      CALL HFITF(CUFUNC,NP)
      IF(NOPER.EQ.0)GO TO 99
      IF(NOPER.EQ.1)THEN
         IC=12
         IF(IOPTN.NE.0)IC=IC-2
         IF(IOPTV.NE.0)IOPTQ=0
         IF(IOPTV.NE.0)IC=IC+10
         IF(IOPTQ.NE.0)IC=IC-10
         IF(IOPTW.NE.0)IC=IC+100
         IF(IOPTM.NE.0)IC=IC+10000
         IF(IOPTR.NE.0)IC=IC+100000
         IF(IOPTL.NE.0)IC=IC+1000000
      ENDIF
      IF(NOPER.GT.1.OR.NPP.EQ.NP)THEN
         DO 10 I=1,NP
            PAR(I)=PARAM(I)
  10     CONTINUE
         CALL HFITH(ID,HFITHC,CHOPT,NP,PAR,STEP,PMIN,PMAX,
     +              SIGPAR,CHI2)
      ELSE
         IF(IFTYPE(1).EQ.1)THEN
            CALL HFITGA(ID,PAR(1),PAR(2),PAR(3),CHI2,IC,SIGPAR)
         ELSEIF(IFTYPE(1).EQ.2)THEN
            CALL HFITEX(ID,PAR(1),PAR(2),CHI2,IC,SIGPAR)
         ELSEIF(IFTYPE(1).EQ.3)THEN
            CALL HFITPO(ID,NP,PAR,CHI2,IC,SIGPAR)
         ENDIF
      ENDIF
*
      DO 20 I=1,NP
         PARAM(I)=PAR(I)
  20  CONTINUE
*
  99  END
