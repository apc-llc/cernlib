*
* $Id: hfitpo.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitpo.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 17/08/89  08.50.24  by  Rene Brun
*-- Author :
      SUBROUTINE HFITPO(IDD,NP,A,CHI2,ICONTR,SIGPAR)
*.==========>
*.                     Fits a polynomial
*.           P(X)=A(1)+A(2)*X +...+ A(NP)*X**(NP-1)
*.           to the contents of 1-dim histogram idd and
*.           optionally superimposes it to the histogram
*.
*.
*.           Input     IDD     = Histogram identifier
*.           arguments
*.                     NP      = number of coefficients (order+1)
*.
*.                     ICONTR  = Control word(see old HFITL)
*.
*.           Output    A       = coefficients,increasing order
*.           arguments                  of x power (array)
*.
*.                     CHI2    = chisquare of fit
*.
*.                     SIGPAR  = standard deviations of coefficients
*.                                       (array)
*.
*..=========> ( R.Brun,E.Lessner)
#include "hbook/hcfit2.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
      DIMENSION SIGPAR(*),A(*)
      EXTERNAL HPOLYN
      CHARACTER*8 CHOPT
*.___________________________________________
      CALL HFIND(IDD,'HFITPO')
      IF(LCID.EQ.0)GO TO 99
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         CALL HBUG('Empty histogram','HFITPO',IDD)
         GO TO 99
      ENDIF
      CALL HDCOFL
      IF(I1.EQ.0.AND.LFIX.EQ.0)GO TO 99
*
      IF (NP.LE.0.OR.NP.GT.18)THEN
         CALL HBUG('Wrong number of coefficients.LE.0','HFITPO',IDD)
         GOTO 99
      ENDIF
      IF (IQ(LPRX).LT.NP)THEN
         CALL HBUG('Number of channels.LE.Degree of polynomial'
     +             ,'HFITPO',IDD)
         GOTO 99
      ENDIF
*
      FITNAM(1)='A0'
      DO 2 I=2,NP
         IF(I.LT.11)THEN
            WRITE(FITNAM(I),10200)I-1
         ELSE
            WRITE(FITNAM(I),10300)I-1
         ENDIF
   2  CONTINUE
      FITNAM(NP+1)='HFITPO'
      CALL HGETFF(ICONTR,IFLRET,CHOPT)
*
*           Set initial values of parameters
*
      IF(NCHANX.LE.1.OR.NP.EQ.1)THEN
         FITPAR(1)=ALLCHA/FLOAT(NCHANX)
      ELSE
         CALL HLSQ(NCHANX,NP,FITPAR)
      ENDIF
*
*           Minimization
*
      CALL HFITH(IDD,HPOLYN,CHOPT,NP,FITPAR,STEP,PMIN,PMAX,
     +           SIGPAR,CHI2)
*
      DO 80 I=1,NP
         A(I)=FITPAR(I)
  80  CONTINUE
*
10200 FORMAT('A',I1,6X)
10300 FORMAT('A',I2,5X)
  99  END
