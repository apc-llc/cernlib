*
* $Id: hfitex.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitex.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 17/08/89  08.50.24  by  Rene Brun
*-- Author :
      SUBROUTINE HFITEX(IDD,AA,BB,CHI2,ICONTR,SIGPAR)
*.==========>
*.                     Fits an exponential function
*.                      E(X)=EXP(AA+BB*X)
*.           to the contents of 1-dim histogram IDD and
*.           optionally superimposes it to the histogram
*.
*.
*.           Input     IDD     = Histogram identifier
*.           arguments
*.                     ICONTR  = control word(see old HFITL)
*.
*.           Output    AA      =
*.           arguments           parameters of the exponential
*.                     BB      =
*.
*.                     CHI2    = chisquare of fit
*.
*.                     SIGPAR  = standard deviations of parameters
*.                                      (array)
*.
*..=========> ( R.Brun,E.Lessner)
#include "hbook/hcfit2.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
      DIMENSION SIGPAR(2)
      EXTERNAL HDEXPO
      CHARACTER*8 CHOPT
*.___________________________________________
      CALL HFIND(IDD,'HFITEX')
      IF(LCID.EQ.0)GO TO 99
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         CALL HBUG('Empty histogram','HFITEX',IDD)
         GO TO 99
      ENDIF
      CALL HDCOFL
      IF(I1.EQ.0.AND.LFIX.EQ.0)GO TO 99
*
      NP=2
      FITNAM(1)='Constant'
      FITNAM(2)='Slope'
      FITNAM(3)='HFITEX'
      CALL HGETFF(ICONTR,IFLRET,CHOPT)
*
*           Set initial values of parameters
*
      CALL HLLSQ(-NCHANX,FITPAR(1),FITPAR(2),IFAIL)
      IF (IFAIL.NE.0) GO TO 99
*
*           Minimization
*
      CALL HFITH(IDD,HDEXPO,CHOPT,NP,FITPAR,STEP,PMIN,PMAX,
     +           SIGPAR,CHI2)
      AA = FITPAR(1)
      BB = FITPAR(2)
*
   99 END
