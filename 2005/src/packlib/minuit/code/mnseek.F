*
* $Id: mnseek.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnseek.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNSEEK(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC   Performs a rough (but global) minimization by monte carlo search.
CC        Each time a new minimum is found, the search area is shifted
CC        to be centered at the best value.  Random points are chosen
CC        uniformly over a hypercube determined by current step sizes.
CC   The Metropolis algorithm accepts a worse point with probability
CC      exp(-d/UP), where d is the degradation.  Improved points
CC      are of course always accepted.  Actual steps are random
CC      multiples of the nominal steps (DIRIN).
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      PARAMETER (TWOPI=2.0*3.141593)
      DIMENSION  XBEST(MNI), XMID(MNI)
      MXFAIL = WORD7(1)
      IF (MXFAIL .LE. 0)  MXFAIL=100+20*NPAR
      MXSTEP = 10*MXFAIL
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      ALPHA = WORD7(2)
      IF (ALPHA .LE. ZERO)  ALPHA=3.
      IF (ISW(5) .GE. 1)  WRITE (ISYSWR, 3) MXFAIL,MXSTEP,ALPHA
    3 FORMAT (' MNSEEK: MONTE CARLO MINIMIZATION USING METROPOLIS',
     + ' ALGORITHM'/' TO STOP AFTER',I6,' SUCCESSIVE FAILURES, OR',
     + I7,' STEPS'/' MAXIMUM STEP SIZE IS',F9.3,' ERROR BARS.')
      CSTATU= 'INITIAL  '
      IF (ISW(5) .GE. 2)  CALL MNPRIN(2,AMIN)
      CSTATU = 'UNCHANGED '
      IFAIL = 0
      RNUM = ZERO
      RNUM1 = ZERO
      RNUM2 = ZERO
      NPARX = NPAR
      FLAST = AMIN
C              set up step sizes, starting values
      DO 10 IPAR =  1, NPAR
      IEXT = NEXOFI(IPAR)
      DIRIN(IPAR) = 2.0*ALPHA*WERR(IPAR)
      IF (NVARL(IEXT) .GT. 1)  THEN
C              parameter with limits
         CALL MNDXDI(X(IPAR),IPAR,DXDI)
         IF (DXDI .EQ. ZERO)  DXDI=1.
         DIRIN(IPAR) = 2.0*ALPHA*WERR(IPAR)/DXDI
         IF (ABS(DIRIN(IPAR)).GT.TWOPI)  DIRIN(IPAR)=TWOPI
         ENDIF
      XMID(IPAR) = X(IPAR)
   10 XBEST(IPAR) = X(IPAR)
C                              search loop
      DO 500 ISTEP= 1, MXSTEP
      IF (IFAIL .GE. MXFAIL)  GO TO 600
        DO 100 IPAR= 1, NPAR
        CALL MNRN15(RNUM1,ISEED)
        CALL MNRN15(RNUM2,ISEED)
  100   X(IPAR) = XMID(IPAR) + 0.5*(RNUM1+RNUM2-1.)*DIRIN(IPAR)
      CALL MNINEX(X)
      CALL FCN(NPARX,GIN,FTRY,U,4,FUTIL)
      NFCN = NFCN + 1
      IF (FTRY .LT. FLAST)  THEN
         IF (FTRY .LT. AMIN)  THEN
            CSTATU = 'IMPROVEMNT'
            AMIN = FTRY
            DO 200 IB= 1, NPAR
  200       XBEST(IB) = X(IB)
            IFAIL = 0
            IF (ISW(5) .GE. 2) CALL MNPRIN(2,AMIN)
            ENDIF
         GO TO 300
      ELSE
         IFAIL = IFAIL + 1
C                   Metropolis algorithm
         BAR = (AMIN-FTRY)/UP
         CALL MNRN15(RNUM,ISEED)
         IF (BAR .LT. LOG(RNUM))  GO TO 500
      ENDIF
C                    Accept new point, move there
  300 CONTINUE
      DO 350 J= 1, NPAR
      XMID(J) = X(J)
  350 CONTINUE
      FLAST = FTRY
  500 CONTINUE
C                               end search loop
  600 CONTINUE
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,601) IFAIL
  601 FORMAT(' MNSEEK:',I5,' SUCCESSIVE UNSUCCESSFUL TRIALS.')
      DO 700 IB= 1, NPAR
  700 X(IB) = XBEST(IB)
      CALL MNINEX(X)
      IF (ISW(5) .GE. 1)  CALL MNPRIN(2,AMIN)
      IF (ISW(5) .EQ. 0)  CALL MNPRIN(0,AMIN)
      RETURN
      END
