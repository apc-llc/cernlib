*
* $Id: mnpout.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnpout.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNPOUT(IUEXT,CHNAM,VAL,ERR,XLOLIM,XUPLIM,IUINT)
#include "minuit/d506dp.inc"
CC     User-called
CC   Provides the user with information concerning the current status
CC          of parameter number IUEXT. Namely, it returns:
CC        CHNAM: the name of the parameter
CC        VAL: the current (external) value of the parameter
CC        ERR: the current estimate of the parameter uncertainty
CC        XLOLIM: the lower bound (or zero if no limits)
CC        XUPLIM: the upper bound (or zero if no limits)
CC        IUINT: the internal parameter number (or zero if not variable,
CC           or negative if undefined).
CC  Note also:  If IUEXT is negative, then it is -internal parameter
CC           number, and IUINT is returned as the EXTERNAL number.
CC     Except for IUINT, this is exactly the inverse of MNPARM
CC
#include "minuit/d506cm.inc"
      CHARACTER*(*) CHNAM
      XLOLIM = 0.
      XUPLIM = 0.
      ERR = 0.
      IF (IUEXT .EQ. 0)  GO TO 100
      IF (IUEXT .LT. 0)  THEN
C                   internal parameter number specified
         IINT = -IUEXT
         IF (IINT .GT. NPAR) GO TO 100
         IEXT = NEXOFI(IINT)
         IUINT = IEXT
      ELSE
C                    external parameter number specified
         IEXT = IUEXT
         IF (IEXT .EQ. 0)   GO TO 100
         IF (IEXT .GT. NU)  GO TO 100
         IINT = NIOFEX(IEXT)
         IUINT = IINT
      ENDIF
C                     in both cases
         NVL = NVARL(IEXT)
         IF (NVL .LT. 0) GO TO 100
      CHNAM = CPNAM(IEXT)
      VAL = U(IEXT)
      IF (IINT .GT. 0)  ERR = WERR(IINT)
      IF (NVL .EQ. 4) THEN
         XLOLIM = ALIM(IEXT)
         XUPLIM = BLIM(IEXT)
      ENDIF
      RETURN
C                parameter is undefined
  100 IUINT = -1
      CHNAM = 'undefined'
      VAL = 0.
      RETURN
      END
