*
* $Id: hfitga.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfitga.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          27/07/95  16.15.05  by  Julian Bunn
*-- Author :
      SUBROUTINE HFITGA(IDD,COEFF,AVERAG,SIGM,CHI2,ICONTR,SIGPAR)
*.==========>
*.                      Fits a gaussian
*.           G(X)=COEFF*EXP(-0.5*((X-AVERAG)/SIGM)**2)
*.           to the contents of 1-dim histogram IDD and
*.           optionally superimposes it to the histogram
*.
*.
*.           Input     IDD     = histogram identifier
*.           arguments
*.                     ICONTR  = control word
*.
*.           Output    COEFF   =
*.           arguments
*.                     AVERAG  = parameters of the gaussian
*.
*.                     SIGM   =
*.
*.                     CHI2    = chisquare of fit
*.
*.                     SIGPAR  = standard deviations of parameters
*.                                      (array)
*.
*..=========> ( R.Brun,E.Lessner)
*
#include "hbook/hcfit2.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcfits.inc"
      DIMENSION SIGPAR(3),STEP(3),PMIN(3),PMAX(3)
      EXTERNAL HGAUS
      CHARACTER*8 CHOPT
      DATA SQRT2P /2.506628/
*.___________________________________________
*
C Does the min value for par(3) need to be set to zero ??
      CALL HFIND(IDD,'HFITGA')
      IF(LCID.EQ.0)GO TO 99
      IF(IQ(LCONT+KNOENT).EQ.0)THEN
         CALL HBUG('Empty histogram','HFITGA',IDD)
         GO TO 99
      ENDIF
      CALL HDCOFL
      IF(I1.EQ.0.AND.LFIX.EQ.0)GO TO 99
*
      NP=3
      FITNAM(1)='Constant'
      FITNAM(2)='Mean'
      FITNAM(3)='Sigma'
      FITNAM(4)='HFITGA'
      CALL HGETFF(ICONTR,IFLRET,CHOPT)
*
*           Set initial values of parameters
*
      FITPAR(2)=HSTATI (IDD,1,'HIST',1)
      FITPAR(3)=HSTATI (IDD,2,'HIST',1)
      IF (FITPAR(3).EQ.0.) THEN
         FITPAR(3)=BINWID*NCHANX*0.25
      END IF
      FITPAR(1)=BINWID*ALLCHA/(SQRT2P*FITPAR(3))
*
*           Minimization
*
      CALL HFITH(IDD,HGAUS,CHOPT,NP,FITPAR,STEP,PMIN,PMAX,
     +           SIGPAR,CHI2)
      COEFF = FITPAR(1)
      AVERAG= FITPAR(2)
      SIGM  = FITPAR(3)
*
   99 END
