*
* $Id: mnscan.F,v 1.2 1996/03/15 18:02:51 james Exp $
*
* $Log: mnscan.F,v $
* Revision 1.2  1996/03/15 18:02:51  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNSCAN(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC        Scans the values of FCN as a function of one parameter
CC        and plots the resulting values as a curve using MNPLOT.
CC        It may be called to scan one parameter or all parameters.
CC        retains the best function and parameter values found.
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      XLREQ = MIN(WORD7(3),WORD7(4))
      XHREQ = MAX(WORD7(3),WORD7(4))
      NCALL = WORD7(2) + 0.01
      IF (NCALL .LE. 1)  NCALL = 41
      IF (NCALL .GT. MAXCPT)  NCALL = MAXCPT
      NCCALL = NCALL
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      IPARWD = WORD7(1) + 0.1
      IPAR = MAX(IPARWD, 0)
      IINT = NIOFEX(IPAR)
      CSTATU = 'NO CHANGE'
      IF (IPARWD .GT. 0)  GO TO 200
C
C         equivalent to a loop over parameters requested
  100 IPAR = IPAR + 1
      IF (IPAR .GT. NU)  GO TO 900
      IINT = NIOFEX(IPAR)
      IF (IINT .LE. 0)  GO TO 100
C         set up range for parameter IPAR
  200 CONTINUE
      UBEST = U(IPAR)
      XPT(1) = UBEST
      YPT(1) = AMIN
      CHPT(1)= ' '
      XPT(2) = UBEST
      YPT(2) = AMIN
      CHPT(2)= 'X'
      NXYPT = 2
      IF (NVARL(IPAR) .GT. 1)  GO TO 300
C         no limits on parameter
      IF (XLREQ .EQ. XHREQ)  GO TO 250
      UNEXT = XLREQ
      STEP = (XHREQ-XLREQ)/FLOAT(NCALL-1)
      GO TO 500
  250 CONTINUE
      XL = UBEST - WERR(IINT)
      XH = UBEST+  WERR(IINT)
      CALL MNBINS(XL,XH,NCALL, UNEXT,UHIGH,NBINS,STEP)
      NCCALL = NBINS + 1
      GO TO 500
C         limits on parameter
  300 CONTINUE
      IF (XLREQ .EQ. XHREQ)  GO TO 350
      XL = MAX(XLREQ,ALIM(IPAR))
      XH = MIN(XHREQ,BLIM(IPAR))
      IF (XL .GE. XH)  GO TO 700
      UNEXT = XL
      STEP = (XH-XL)/FLOAT(NCALL-1)
      GO TO 500
  350 CONTINUE
      UNEXT = ALIM(IPAR)
      STEP = (BLIM(IPAR)-ALIM(IPAR))/FLOAT(NCALL-1)
C         main scanning loop over parameter IPAR
  500 CONTINUE
      DO 600 ICALL = 1, NCCALL
      U(IPAR) = UNEXT
      NPARX = NPAR
      CALL FCN(NPARX,GIN,FNEXT,U,4,FUTIL)
      NFCN = NFCN + 1
      NXYPT = NXYPT + 1
      XPT(NXYPT) = UNEXT
      YPT(NXYPT) = FNEXT
      CHPT(NXYPT) = '*'
      IF (FNEXT .LT. AMIN)  THEN
        AMIN = FNEXT
        UBEST = UNEXT
        CSTATU= 'IMPROVED  '
        ENDIF
  530 CONTINUE
      UNEXT = UNEXT + STEP
  600 CONTINUE
C         finished with scan of parameter IPAR
      U(IPAR) = UBEST
      CALL MNEXIN(X)
      IF (ISW(5) .GE. 1)  THEN
        WRITE (ISYSWR,1001)  NEWPAG,IPAR,CPNAM(IPAR)
        NUNIT = ISYSWR
        CALL MNPLOT(XPT,YPT,CHPT,NXYPT,NUNIT,NPAGWD,NPAGLN)
      ENDIF
      GO TO 800
  700 CONTINUE
      WRITE (ISYSWR,1000) IPAR
  800 CONTINUE
      IF (IPARWD .LE. 0)  GO TO 100
C         finished with all parameters
  900 CONTINUE
      IF (ISW(5) .GE. 0) CALL MNPRIN(5,AMIN)
      RETURN
 1000 FORMAT (46H REQUESTED RANGE OUTSIDE LIMITS FOR PARAMETER  ,I3/)
 1001 FORMAT (I1,'SCAN OF PARAMETER NO.',I3,3H,   ,A10)
      END
