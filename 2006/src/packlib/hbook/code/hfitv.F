*
* $Id: hfitv.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitv.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 23/08/94  14.17.45  by  Rene Brun
*-- Author :
      SUBROUTINE HFITV (N,NDIM,NVAR,X,Y,EY,UFUNC,CHOPT,NP,PARAM
     +                 ,STEP,PMIN,PMAX,SIGPAR,CHI2)
*.==========>
*.
*.           Fits the parametric function to the contents of the
*.           N points in vectors X,Y with associated error bars in EY.
*.
*.
*.           Input     N       = Number of points
*.           arguments NDIM    = Declared first dimension of X
*.                     NVAR    = Dimension of the distribution
*.                     X       = X coordinates (array of N points)
*.                     Y       = Y coordinates (array of N points)
*.                     EY      = Array of errors on Y (array of N values)
*.
*.                     UFUNC   = Parametric function (EXTERNAL)
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
*.                CHOPT : Possible options 'QVBLDW'
*.
*.    'Q'  Quiet mode. No print
* .   'V'  Verbode mode. Results after each iteration are printed
*.         By default only final results are printed.
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
*.         In this case, array EY is ignored.
*.
*.    'Z'  UFUNC is the user FCN (user own fitting model)
*.
*.    'K'  Do not reset the Application Hminuit settings for option 'M'
*.
*..=========> ( R.Brun ,E.Lessner)
*
#include "hbook/hcfit2.inc"
#include "hbook/hcfit6.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
      PARAMETER(MAXPAR=34)
      DIMENSION PARAM(*),SIGPAR(*),STEP(*),PMIN(*),PMAX(*)
      DIMENSION X(NDIM,NVAR),Y(*),EY(*)
      DIMENSION STE(MAXPAR),PMI(MAXPAR),PMA(MAXPAR)
      DIMENSION IOPT(7)
      EQUIVALENCE (IOPTQ,IOPT(1)),(IOPTV,IOPT(2))
      EQUIVALENCE (IOPTB,IOPT(3)),(IOPTL,IOPT(4))
      EQUIVALENCE (IOPTD,IOPT(5)),(IOPTW,IOPT(6))
      EQUIVALENCE (IOPTZ,IOPT(7))
      EXTERNAL UFUNC,HFCNV
      CHARACTER*(*) CHOPT
*.___________________________________________
      IF(NP.LE.0.OR.NP.GT.MAXPAR)THEN
         CALL HBUG('Wrong number of parameters','HFITV',IDD)
         GO TO 999
      ENDIF
      NUMEP=N
      ID=0
      IDLAST=0
*
      NX=NVAR
      NY=N
*
      FITNAM(1)=' '
      FITNAM(NP+1)='HFITV'
      IDIMPN=2+NVAR
      CALL HUOPTC(CHOPT,'QVBLDWZ',IOPT)
      IFLSF = 0
      IDER  = IOPTD
      IWEIGH= IOPTW
      ITFUM = 0
      LINEAR= IOPTZ
      IF(IOPTQ.NE.0)ITFUM=-1
      IF(IOPTV.NE.0)ITFUM=1
*
*             Set addresses of working vectors
*
      LAHFIT=NX*NY+2*NY+100
      CALL HWORK(LAHFIT,ILXE,'HFITV')
      IF(ILXE.EQ.0)GO TO 999
      ILYE=ILXE+NX*NY
      ILEY=ILYE+NY
*
*
*             Computes ALLCHA and WGTMAX
*
      BINWID=1.
      ALLCHA=0.
      WGTMAX=0.
      DO 10 L1=1,N
         IF (ABS(Y(L1)).GT.WGTMAX) WGTMAX=ABS(Y(L1))
   10 CONTINUE
*
      DO 30 I=1,NX
         DO 20 J=1,NY
            Q(ILXE+J-1+NY*(I-1))=X(J,I)
   20    CONTINUE
   30 CONTINUE
      DO 40 I=1,N
         Q(ILYE+I-1)=Y(I)
         IF(IWEIGH.EQ.0)THEN
            Q(ILEY+I-1)=EY(I)
         ELSE
            Q(ILEY+I-1)=1.
         ENDIF
   40 CONTINUE
*
*             Set boundary sizes for parameters
*
      DO 50 I=1,NP
         IF (IOPTB.EQ.0) THEN
            PMI(I)=0.
            PMA(I)=0.
            STE(I)=-1.
         ELSE
            PMI(I)=PMIN(I)
            PMA(I)=PMAX(I)
            STE(I)=STEP(I)
         END IF
   50 CONTINUE
*
*             Other initializations
*
      NFPAR=NP
      CALL HMINUT (HFCNV,UFUNC,PARAM,STE,PMI,PMA,CHOPT)
*
      CHI2=FITCHI
      DO 60 I=1,NFPAR
         SIGPAR(I)=STE(I)
   60 CONTINUE
*
  999 END
