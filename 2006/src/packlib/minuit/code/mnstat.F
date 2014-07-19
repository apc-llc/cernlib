*
* $Id: mnstat.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnstat.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNSTAT(FMIN,FEDM,ERRDEF,NPARI,NPARX,ISTAT)
#include "minuit/d506dp.inc"
CC       User-called
CC       Provides the user with information concerning the current status
CC          of the current minimization. Namely, it returns:
CC        FMIN: the best function value found so far
CC        FEDM: the estimated vertical distance remaining to minimum
CC        ERRDEF: the value of UP defining parameter uncertainties
CC        NPARI: the number of currently variable parameters
CC        NPARX: the highest (external) parameter number defined by user
CC        ISTAT: a status integer indicating how good is the covariance
CC           matrix:  0= not calculated at all
CC                    1= approximation only, not accurate
CC                    2= full matrix, but forced positive-definite
CC                    3= full accurate covariance matrix
CC
#include "minuit/d506cm.inc"
      FMIN = AMIN
      FEDM = EDM
      ERRDEF = UP
      NPARI = NPAR
      NPARX = NU
      ISTAT = ISW(2)
        IF (EDM  .EQ. BIGEDM)  THEN
            FEDM = UP
        ENDIF
        IF (AMIN .EQ. UNDEFI)  THEN
            FMIN = 0.0
            FEDM = UP
            ISTAT= 0
        ENDIF
      RETURN
      END
