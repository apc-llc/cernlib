*
* $Id: mnmnot.F,v 1.1.1.1 1996/03/07 14:31:30 mclareni Exp $
*
* $Log: mnmnot.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNMNOT(FCN,ILAX,ILAX2,VAL2PL,VAL2MI,FUTIL)
#include "minuit/d506dp.inc"
CC        Performs a MINOS error analysis on one parameter.
CC        The parameter ILAX is varied, and the minimum of the
CC        function with respect to the other parameters is followed
CC        until it crosses the value FMIN+UP.
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      DIMENSION XDEV(MNI),W(MNI),GCC(MNI)
      CHARACTER*4 CPOS,CNEG,CSIG
      PARAMETER (CPOS='POSI',CNEG='NEGA')
C                                        . . save and prepare start vals
      ISW2 = ISW(2)
      ISW4 = ISW(4)
      SIGSAV = EDM
      ISTRAV = ISTRAT
      DC = DCOVAR
      LNEWMN = .FALSE.
      APSI  = EPSI*0.5
      ABEST=AMIN
      MPAR=NPAR
      NFMXIN = NFCNMX
      DO 125 I= 1, MPAR
  125 XT(I) = X(I)
      DO 130 J= 1, MPAR*(MPAR+1)/2
  130 VTHMAT(J) = VHMAT(J)
      DO 135 I= 1, MPAR
      GCC(I) = GLOBCC(I)
  135 W(I) = WERR(I)
      IT = NIOFEX(ILAX)
      ERP(IT) = 0.
      ERN(IT) = 0.
      CALL MNINEX(XT)
      UT = U(ILAX)
      IF (NVARL(ILAX) .EQ. 1) THEN
         ALIM(ILAX) = UT -100.*W(IT)
         BLIM(ILAX) = UT +100.*W(IT)
         ENDIF
      NDEX = IT*(IT+1)/2
      XUNIT = SQRT(UP/VTHMAT(NDEX))
      MARC = 0
      DO 162 I= 1, MPAR
      IF (I .EQ. IT)  GO TO 162
      MARC = MARC + 1
         IMAX = MAX(IT,I)
         INDX = IMAX*(IMAX-1)/2 + MIN(IT,I)
      XDEV(MARC) = XUNIT*VTHMAT(INDX)
  162 CONTINUE
C                           fix the parameter in question
      CALL MNFIXP (IT,IERR)
      IF (IERR .GT. 0)  THEN
         WRITE (ISYSWR,'(A,I5,A,I5)')
     +    ' MINUIT ERROR. CANNOT FIX PARAMETER',ILAX,'    INTERNAL',IT
         GO TO 700
      ENDIF
C                       . . . . . Nota Bene: from here on, NPAR=MPAR-1
C      Remember: MNFIXP squeezes IT out of X, XT, WERR, and VHMAT,
C                                                    not W, VTHMAT
      DO 500 ISIG= 1,2
      IF (ISIG .EQ. 1) THEN
         SIG = 1.0
         CSIG = CPOS
      ELSE
         SIG = -1.0
         CSIG = CNEG
      ENDIF
C                                        . sig=sign of error being calcd
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,806)  CSIG,ILAX,CPNAM(ILAX)
  806 FORMAT (/' DETERMINATION OF ',A4,'TIVE MINOS ERROR FOR PARAMETER',
     +    I3, 2X ,A)
      IF (ISW(2).LE.0) CALL MNWARN('D','MINOS','NO COVARIANCE MATRIX.')
      NLIMIT = NFCN + NFMXIN
      ISTRAT = MAX(ISTRAV-1,0)
      DU1 = W(IT)
      U(ILAX) = UT + SIG*DU1
      U(ILAX) = MIN(U(ILAX),BLIM(ILAX))
      U(ILAX) = MAX(U(ILAX),ALIM(ILAX))
      DELU = U(ILAX) - UT
C         stop if already at limit with negligible step size
      IF (ABS(DELU)/(ABS(UT)+ABS(U(ILAX))) .LT. EPSMAC)  GO TO 440
      FAC = DELU/W(IT)
         DO 185 I= 1, NPAR
  185    X(I) = XT(I) + FAC*XDEV(I)
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,801)  ILAX,UT,DELU,U(ILAX)
  801 FORMAT (/' PARAMETER',I4,' SET TO',E11.3,' + ',E10.3,' = ',E12.3)
C                                        loop to hit AMIN+UP
      KE1CR = ILAX
      KE2CR = 0
      XMIDCR = U(ILAX)
      XDIRCR = DELU
C
      AMIN = ABEST
      NFCNMX = NLIMIT - NFCN
      CALL MNCROS(FCN,AOPT,IERCR,FUTIL)
      IF (ABEST-AMIN .GT. 0.01*UP)  GO TO 650
      IF (IERCR .EQ. 1)  GO TO 440
      IF (IERCR .EQ. 2)  GO TO 450
      IF (IERCR .EQ. 3)  GO TO 460
C                                        . error successfully calculated
      EROS = XMIDCR-UT + AOPT*XDIRCR
      IF (ISW(5) .GT. 1) WRITE (ISYSWR,808)  CSIG,ILAX,CPNAM(ILAX),EROS
  808 FORMAT (/9X,4HTHE ,A4,  29HTIVE MINOS ERROR OF PARAMETER,I3,   2H
     +, ,A10,      4H, IS ,E12.4)
      GO TO 480
C                                        . . . . . . . . failure returns
  440 IF (ISW(5) .GE. 1) WRITE(ISYSWR,807)  CSIG,ILAX,CPNAM(ILAX)
  807 FORMAT (5X,'THE ',A4,'TIVE MINOS ERROR OF PARAMETER',I3,', ',A,
     +', EXCEEDS ITS LIMIT.'/)
      EROS = UNDEFI
      GO TO 480
  450 IF (ISW(5) .GE. 1) WRITE (ISYSWR, 802)  CSIG,ILAX,NFMXIN
  802 FORMAT (9X,'THE ',A,'TIVE MINOS ERROR',I4,' REQUIRES MORE THAN',
     +   I5,' FUNCTION CALLS.'/)
      EROS = 0.
      GO TO 480
  460 IF (ISW(5) .GE. 1) WRITE (ISYSWR, 805) CSIG,ILAX
  805 FORMAT (25X,A,'TIVE MINOS ERROR NOT CALCULATED FOR PARAMETER',I4/)
      EROS = 0.
C
  480 IF (ISW(5) .GT. 1) WRITE (ISYSWR,'(5X, 74(1H*))')
      IF (SIG .LT. ZERO)  THEN
         ERN(IT) = EROS
         IF (ILAX2.GT.0 .AND. ILAX2.LE.NU)  VAL2MI = U(ILAX2)
      ELSE
         ERP(IT) = EROS
         IF (ILAX2.GT.0 .AND. ILAX2.LE.NU)  VAL2PL = U(ILAX2)
      ENDIF
  500 CONTINUE
C                                        . . parameter finished. reset v
C                       normal termination
      ITAUR = 1
      CALL MNFREE(1)
      DO 550 J= 1, MPAR*(MPAR+1)/2
  550 VHMAT(J) = VTHMAT(J)
      DO 595 I= 1, MPAR
      WERR(I) = W(I)
      GLOBCC(I) = GCC(I)
  595 X(I) = XT(I)
      CALL MNINEX (X)
      EDM = SIGSAV
      AMIN = ABEST
      ISW(2) = ISW2
      ISW(4) = ISW4
      DCOVAR = DC
      GO TO 700
C                       new minimum
  650 LNEWMN = .TRUE.
      ISW(2) = 0
      DCOVAR = 1.
      ISW(4) = 0
      SAV = U(ILAX)
      ITAUR = 1
      CALL MNFREE(1)
      U(ILAX) = SAV
      CALL MNEXIN(X)
      EDM = BIGEDM
C                       in any case
  700 CONTINUE
      ITAUR = 0
      NFCNMX = NFMXIN
      ISTRAT = ISTRAV
      RETURN
      END
