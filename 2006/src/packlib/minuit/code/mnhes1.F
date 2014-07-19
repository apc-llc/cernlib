*
* $Id: mnhes1.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mnhes1.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNHES1(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC      Called from MNHESS and MNGRAD
CC      Calculate first derivatives (GRD) and uncertainties (DGRD)
CC         and appropriate step sizes GSTEP
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      LOGICAL LDEBUG
      CHARACTER CBF1*22
      LDEBUG = (IDBG(5) .GE. 1)
      IF (ISTRAT .LE. 0) NCYC = 1
      IF (ISTRAT .EQ. 1) NCYC = 2
      IF (ISTRAT .GT. 1) NCYC = 6
      IDRV = 1
      NPARX = NPAR
      DFMIN = 4.*EPSMA2*(ABS(AMIN)+UP)
C                                     main loop over parameters
      DO 100 I= 1, NPAR
      XTF = X(I)
      DMIN = 4.*EPSMA2*ABS(XTF)
      EPSPRI = EPSMA2 + ABS(GRD(I)*EPSMA2)
      OPTSTP = SQRT(DFMIN/(ABS(G2(I))+EPSPRI))
      D = 0.2 * ABS(GSTEP(I))
      IF (D .GT. OPTSTP)  D = OPTSTP
      IF (D .LT. DMIN)  D = DMIN
      CHGOLD = 10000.
C                                       iterate reducing step size
      DO 50 ICYC= 1, NCYC
      X(I) = XTF + D
      CALL MNINEX(X)
      CALL FCN(NPARX,GIN,FS1,U,4,FUTIL)
      NFCN = NFCN + 1
      X(I) = XTF - D
      CALL MNINEX(X)
      CALL FCN(NPARX,GIN,FS2,U,4,FUTIL)
      NFCN = NFCN + 1
      X(I) = XTF
C                                       check if step sizes appropriate
      SAG = 0.5*(FS1+FS2-2.0*AMIN)
      GRDOLD = GRD(I)
      GRDNEW = (FS1-FS2)/(2.0*D)
      DGMIN = EPSMAC*(ABS(FS1)+ABS(FS2))/D
      IF (LDEBUG) WRITE (ISYSWR,11) I,IDRV,GSTEP(I),D,G2(I),GRDNEW,SAG
   11 FORMAT (I4,I2,6G12.5)
      IF (GRDNEW .EQ. ZERO)  GO TO 60
      CHANGE = ABS((GRDOLD-GRDNEW)/GRDNEW)
      IF (CHANGE.GT.CHGOLD .AND. ICYC.GT.1)  GO TO 60
      CHGOLD = CHANGE
      GRD(I) = GRDNEW
      GSTEP(I) = SIGN(D,GSTEP(I))
C                  decrease step until first derivative changes by <5%
      IF (CHANGE .LT. 0.05) GO TO 60
      IF (ABS(GRDOLD-GRDNEW) .LT. DGMIN)  GO TO 60
      IF (D .LT. DMIN)  THEN
         CALL MNWARN('D','MNHES1','Step size too small for 1st drv.')
         GO TO 60
      ENDIF
      D = 0.2*D
   50 CONTINUE
C                                       loop satisfied = too many iter
      WRITE (CBF1,'(2G11.3)') GRDOLD,GRDNEW
      CALL MNWARN('D','MNHES1','Too many iterations on D1.'//CBF1)
   60 CONTINUE
      DGRD(I) = MAX(DGMIN,ABS(GRDOLD-GRDNEW))
  100 CONTINUE
C                                        end of first deriv. loop
      CALL MNINEX(X)
      RETURN
      END
