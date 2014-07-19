*
* $Id: hfith.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfith.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.40.57  by  Julian Bunn
*-- Author :
      SUBROUTINE HFITH (IDD,UFUNC,CHOPT,NP,PARAM,STEP,PMIN,PMAX,SIGPAR
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
*.                CHOPT : Possible options 'RQVNFBMLDWTE'
*.
*.    'R'  Fit a Restricted area of the histogram - 1-D only.
*.          IFXLOW = IQUEST(11)
*.          IFYUP  = IQUEST(12)
*.          IFYLOW = IQUEST(13)
*.          IFYUP  = IQUEST(14)
*.
*.    'Q'  Quiet mode. No print
* .   'V'  Verbode mode. Results after each iteration are printed
*.         By default only final results are printed.
*.
*.    'N'  Do not store the result of the fit bin by bin with the histogram.
*.         By default, for 1-D, the function is calculated at the centre of
*.         each bin in the specified range and stored.
*.
*.    'F'  Force storing of the result of the fit bin by bin with the
*.         histogram for any-dimension histogram.
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
*.    'W'  Sets weights equal to 1 for the chisquare method.
*.         By default, weights are taken following statistical errors.
*.         If the 'L' option is given (Log Likelihood), bins with errors=0
*.         are excluded of the fit.
*.
*.    'T'  HFITH is being called by another HBOOK routine
*.         In this case takes parameter names from HCFITS.
*.
*.    'E'  Performs a better error evaluation, calling HESSE and MINOS
*.
*.    'U'  User function value is taken from /HCFITD/FITPAD(24),FITFUN
*.
*.    'K'  Do not reset the Application Hminuit settings for option 'M'
*.
*..=========> ( R.Brun ,E.Lessner)
*
#include "hbook/hcfit2.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfitr.inc"
#include "hbook/hcfits.inc"
      COMMON/QUEST/IQUEST(100)
      PARAMETER(MAXPAR=34)
      DIMENSION PARAM(*),SIGPAR(*),STEP(*),PMIN(*),PMAX(*)
      DIMENSION STE(MAXPAR),PMI(MAXPAR),PMA(MAXPAR)
      DIMENSION IOPT(13)
      EQUIVALENCE (IOPTQ,IOPT(1)),(IOPTV,IOPT(2))
      EQUIVALENCE (IOPTB,IOPT(3)),(IOPTL,IOPT(4))
      EQUIVALENCE (IOPTD,IOPT(5)),(IOPTW,IOPT(6))
      EQUIVALENCE (IOPTR,IOPT(7)),(IOPTN,IOPT(8))
      EQUIVALENCE (IOPTT,IOPT(9)),(IOPTE,IOPT(10))
      EQUIVALENCE (IOPTM,IOPT(11)),(IOPTU,IOPT(12))
      EQUIVALENCE (IOPTF,IOPT(13))
      EXTERNAL UFUNC,HFCNH
      CHARACTER*(*) CHOPT
*.___________________________________________
      CALL HUOPTC(CHOPT,'QVBLDWRNTEMUF',IOPT)
*
*          Skip this part if HFITH called by another routine
*
      NDIM=1
      IF(IOPTT.EQ.0)THEN
         IF(NP.LE.0.OR.NP.GT.MAXPAR)THEN
            CALL HBUG('Wrong number of parameters','HFITH',IDD)
            GO TO 999
         ENDIF
         CALL HFIND(IDD,'HFITH ')
         IF(LCID.EQ.0)GO TO 999
         IF(IQ(LCONT+KNOENT).EQ.0)THEN
            CALL HBUG('Empty histogram','HFITH',IDD)
            GO TO 999
         ENDIF
         CALL HDCOFL
*
         FITNAM(1)   = ' '
         FITNAM(NP+1)= 'HFITH'
*
         NDIM=1
         IF(I1.EQ.0)THEN
            IF(LCONT.EQ.LQ(LCID-1)) NDIM=2
         ENDIF
      ENDIF
*
      IDIMPN=2+NDIM
      IFLSF = 0
      IDER  = IOPTD
      IWEIGH= IOPTW
      ITFUM = 0
      IF(IOPTQ.NE.0)ITFUM=-1
      IF(IOPTV.NE.0)ITFUM=1
      LINEAR=IOPTL
*
      BINWID=(Q(LPRX+2)-Q(LPRX+1))/FLOAT(IQ(LPRX))
      IF(IOPTR.NE.0)THEN
         IFTRNG=1
         IFXLOW=IQUEST(11)
         IFXUP =IQUEST(12)
         ICX1=IFXLOW
         IF(IFXUP.GT.IQ(LPRX))IFXUP=IQ(LPRX)
         ICX2=IFXUP
         NCHANX=ICX2-ICX1+1
         XMIN=Q(LPRX+1)+(ICX1-1)*BINWID
      ELSE
         IFTRNG=0
         NCHANX=IQ(LPRX)
         ICX1=1
         ICX2=NCHANX
         XMIN=Q(LPRX+1)
      ENDIF
      NCHANY=1
      ICY1=1
      ICY2=1
      IF(IDIMPN.NE.3)THEN
         BINWIY=(Q(LPRY+2)-Q(LPRY+1))/FLOAT(IQ(LPRY))
         IF(IOPTR.NE.0)THEN
            IFYLOW=IQUEST(13)
            IFYUP =IQUEST(14)
            ICY1=IFYLOW
            IF(IFYUP.GT.IQ(LPRY))IFYUP=IQ(LPRY)
            ICY2=IFYUP
            NCHANY=ICY2-ICY1+1
            YMIN=Q(LPRY+1)+(ICY1-1)*BINWIY
         ELSE
            NCHANY=IQ(LPRY)
            ICY1=1
            ICY2=NCHANY
            YMIN=Q(LPRY+1)
         ENDIF
      ENDIF
      NUMEP=NCHANX*NCHANY
*
*
*             Computes ALLCHA and WGTMAX
*
      ALLCHA=0.
      WGTMAX=0.
      DO 20 J=ICY1,ICY2
         DO 10 I=ICX1,ICX2
            IF(IDIMPN.EQ.3)CONTEN=HCX(I,1)
            IF(IDIMPN.EQ.4)CONTEN=HCXY(I,J,1)
            IF(CONTEN.LT.0.AND.LQ(LCONT).EQ.0)IWEIGH=1
            IF(ABS(CONTEN).GT.WGTMAX)WGTMAX=ABS(CONTEN)
            ALLCHA=ALLCHA+CONTEN
   10    CONTINUE
   20 CONTINUE
*
      IF(WGTMAX.EQ.0)THEN
         CALL HBUG('Empty histogram','HFITH',ID)
         GO TO 999
      ENDIF
*
*             Create bank for function values
*
      IF(IDIMPN.EQ.3)THEN
         CALL HSUPIS(UFUNC,0,ICX1,ICX2)
         IF(IOPTN.EQ.0)CALL HSUPIS(UFUNC,1,ICX1,ICX2)
      ENDIF
*
*             Set boundary sizes for parameters
*
      DO 30 I=1,NP
         IF (IOPTB.EQ.0) THEN
            PMI(I)=0.
            PMA(I)=0.
            STE(I)=-1.
         ELSE
            PMI(I)=PMIN(I)
            PMA(I)=PMAX(I)
            STE(I)=STEP(I)
         END IF
   30 CONTINUE
*
*             Perform minimization
*
      NFPAR=NP
      CALL HMINUT (HFCNH,UFUNC,PARAM,STE,PMI,PMA,CHOPT)
*
      IFTRNG=0
      CHI2=FITCHI
      DO 40 I=1,NFPAR
         SIGPAR(I)=STE(I)
   40 CONTINUE
      IF(FITNAM(NP+1).EQ.'HFITGA')FITPAR(3)=ABS(FITPAR(3))
*
      IF(IOPTN.EQ.0)THEN
         CALL HSUPIS(UFUNC,2,ICX1,ICX2)
      ELSEIF(IOPTF.EQ.0)THEN
         CALL HSUPIS(UFUNC,3,ICX1,ICX2)
      ENDIF
*
  999 END
