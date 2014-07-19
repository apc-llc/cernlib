*
* $Id: mncont.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mncont.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNCONT(FCN,KE1,KE2,NPTU,XPTU,YPTU,IERRF,FUTIL)
#include "minuit/d506dp.inc"
CC       Find NPTU points along a contour where the function
CC             FMIN (X(KE1),X(KE2)) =  AMIN+UP
CC       where FMIN is the minimum of FCN with respect to all
CC       the other NPAR-2 variable parameters (if any).
CC   IERRF on return will be equal to the number of points found:
CC     NPTU if normal termination with NPTU points found
CC     -1   if errors in the calling sequence (KE1, KE2 not variable)
CC      0   if less than four points can be found (using MNMNOT)
CC     n>3  if only n points can be found (n < NPTU)
CC
#include "minuit/d506cm.inc"
      DIMENSION XPTU(NPTU), YPTU(NPTU), W(MNI),GCC(MNI)
      CHARACTER CHERE*10
      PARAMETER (CHERE='MNContour ')
      LOGICAL LDEBUG
      EXTERNAL FCN,FUTIL
C                 input arguments: parx, pary, devs, ngrid
      LDEBUG = (IDBG(6) .GE. 1)
      IF (KE1.LE.0 .OR. KE2.LE.0)  GO TO 1350
      IF (KE1.GT.NU .OR. KE2.GT.NU)  GO TO 1350
      KI1 = NIOFEX(KE1)
      KI2 = NIOFEX(KE2)
      IF (KI1.LE.0 .OR. KI2.LE.0)  GO TO 1350
      IF (KI1 .EQ. KI2)  GO TO 1350
      IF (NPTU .LT. 4)  GO TO 1400
C
      NFCNCO = NFCN
      NFCNMX = 100*(NPTU+5)*(NPAR+1)
C           The minimum
      CALL MNCUVE(FCN,FUTIL)
      U1MIN = U(KE1)
      U2MIN = U(KE2)
      IERRF = 0
      CFROM = CHERE
      NFCNFR = NFCNCO
      IF (ISW(5) .GE. 0)  THEN
         WRITE (ISYSWR,'(1X,A,I4,A)')
     +   'START MNCONTOUR CALCULATION OF',NPTU,' POINTS ON CONTOUR.'
         IF (NPAR .GT. 2) THEN
            IF (NPAR .EQ. 3) THEN
              KI3 = 6 - KI1 - KI2
              KE3 = NEXOFI(KI3)
              WRITE (ISYSWR,'(1X,A,I3,2X,A)')
     +        'EACH POINT IS A MINIMUM WITH RESPECT TO PARAMETER ',
     +        KE3, CPNAM(KE3)
            ELSE
              WRITE (ISYSWR,'(1X,A,I3,A)')
     +        'EACH POINT IS A MINIMUM WITH RESPECT TO THE OTHER',
     +        NPAR-2, ' VARIABLE PARAMETERS.'
            ENDIF
         ENDIF
      ENDIF
C
C           Find the first four points using MNMNOT
C              ........................ first two points
      CALL MNMNOT(FCN,KE1,KE2,VAL2PL,VAL2MI,FUTIL)
      IF (ERN(KI1) .EQ. UNDEFI)  THEN
         XPTU(1) = ALIM(KE1)
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')
      ELSE
         IF (ERN(KI1) .GE. ZERO)  GO TO 1500
         XPTU(1) = U1MIN+ERN(KI1)
      ENDIF
      YPTU(1) = VAL2MI
C
      IF (ERP(KI1) .EQ. UNDEFI)  THEN
         XPTU(3) = BLIM(KE1)
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')
      ELSE
         IF (ERP(KI1) .LE. ZERO)  GO TO 1500
         XPTU(3) = U1MIN+ERP(KI1)
      ENDIF
      YPTU(3) = VAL2PL
      SCALX = 1.0/(XPTU(3) - XPTU(1))
C              ........................... next two points
      CALL MNMNOT(FCN,KE2,KE1,VAL2PL,VAL2MI,FUTIL)
      IF (ERN(KI2) .EQ. UNDEFI)  THEN
         YPTU(2) = ALIM(KE2)
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')
      ELSE
         IF (ERN(KI2) .GE. ZERO)  GO TO 1500
         YPTU(2) = U2MIN+ERN(KI2)
      ENDIF
      XPTU(2) = VAL2MI
      IF (ERP(KI2) .EQ. UNDEFI)  THEN
         YPTU(4) = BLIM(KE2)
         CALL MNWARN('W',CHERE,'Contour squeezed by parameter limits.')
      ELSE
         IF (ERP(KI2) .LE. ZERO)  GO TO 1500
         YPTU(4) = U2MIN+ERP(KI2)
      ENDIF
      XPTU(4) = VAL2PL
      SCALY = 1.0/(YPTU(4) - YPTU(2))
      NOWPTS = 4
      NEXT = 5
      IF (LDEBUG) THEN
         WRITE (ISYSWR,'(A)') ' Plot of four points found by MINOS'
         XPT(1) = U1MIN
         YPT(1) = U2MIN
         CHPT(1) = ' '
         NALL = MIN(NOWPTS+1,MAXCPT)
         DO 85 I= 2, NALL
           XPT(I) = XPTU(I-1)
           YPT(I) = YPTU(I-1)
   85    CONTINUE
           CHPT(2)= 'A'
           CHPT(3)= 'B'
           CHPT(4)= 'C'
           CHPT(5)= 'D'
         CALL MNPLOT(XPT,YPT,CHPT,NALL,ISYSWR,NPAGWD,NPAGLN)
      ENDIF
C
C               ..................... save some values before fixing
      ISW2 = ISW(2)
      ISW4 = ISW(4)
      SIGSAV = EDM
      ISTRAV = ISTRAT
      DC = DCOVAR
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
C                           fix the two parameters in question
      KINTS = NIOFEX(KE1)
      CALL MNFIXP (KINTS,IERR)
      KINTS = NIOFEX(KE2)
      CALL MNFIXP (KINTS,IERR)
C               ......................Fill in the rest of the points
      DO 900 INEW= NEXT, NPTU
C            find the two neighbouring points with largest separation
      BIGDIS = 0.
         DO 200  IOLD = 1, INEW-1
         I2 = IOLD + 1
         IF (I2 .EQ. INEW) I2 = 1
         DIST = (SCALX*(XPTU(IOLD)-XPTU(I2)))**2 +
     +          (SCALY*(YPTU(IOLD)-YPTU(I2)))**2
         IF (DIST .GT. BIGDIS) THEN
            BIGDIS = DIST
            IDIST = IOLD
         ENDIF
  200    CONTINUE
      I1 = IDIST
      I2 = I1 + 1
      IF (I2 .EQ. INEW) I2 = 1
C                   next point goes between I1 and I2
      A1 = HALF
      A2 = HALF
  300 XMIDCR = A1*XPTU(I1) + A2*XPTU(I2)
      YMIDCR = A1*YPTU(I1) + A2*YPTU(I2)
      XDIR = YPTU(I2) - YPTU(I1)
      YDIR = XPTU(I1) - XPTU(I2)
      SCLFAC = MAX(ABS(XDIR*SCALX), ABS(YDIR*SCALY))
      XDIRCR = XDIR/SCLFAC
      YDIRCR = YDIR/SCLFAC
      KE1CR = KE1
      KE2CR = KE2
C                Find the contour crossing point along DIR
      AMIN = ABEST
      CALL MNCROS(FCN,AOPT,IERCR,FUTIL)
      IF (IERCR .GT. 1)  THEN
C              If cannot find mid-point, try closer to point 1
         IF (A1 .GT. HALF) THEN
            IF (ISW(5) .GE. 0)
     +      WRITE (ISYSWR,'(A,A,I3,A)') ' MNCONT CANNOT FIND NEXT',
     +           ' POINT ON CONTOUR.  ONLY ',NOWPTS,' POINTS FOUND.'
            GO TO 950
         ENDIF
         CALL MNWARN('W',CHERE,'Cannot find midpoint, try closer.')
         A1 = 0.75
         A2 = 0.25
         GO TO 300
      ENDIF
C                Contour has been located, insert new point in list
         DO 830 MOVE= NOWPTS,I1+1,-1
         XPTU(MOVE+1) = XPTU(MOVE)
         YPTU(MOVE+1) = YPTU(MOVE)
  830    CONTINUE
      NOWPTS = NOWPTS + 1
      XPTU(I1+1) = XMIDCR + XDIRCR*AOPT
      YPTU(I1+1) = YMIDCR + YDIRCR*AOPT
  900 CONTINUE
  950 CONTINUE
C
      IERRF = NOWPTS
      CSTATU = 'SUCCESSFUL'
      IF (NOWPTS .LT. NPTU)  CSTATU = 'INCOMPLETE'
C                make a lineprinter plot of the contour
      IF (ISW(5) .GE. 0) THEN
         XPT(1) = U1MIN
         YPT(1) = U2MIN
         CHPT(1) = ' '
         NALL = MIN(NOWPTS+1,MAXCPT)
         DO 1000 I= 2, NALL
           XPT(I) = XPTU(I-1)
           YPT(I) = YPTU(I-1)
           CHPT(I)= 'X'
 1000    CONTINUE
         WRITE (ISYSWR,'(A,I3,2X,A)') ' Y-AXIS: PARAMETER ',KE2,
     +        CPNAM(KE2)
         CALL MNPLOT(XPT,YPT,CHPT,NALL,ISYSWR,NPAGWD,NPAGLN)
         WRITE (ISYSWR,'(25X,A,I3,2X,A)') 'X-AXIS: PARAMETER ',
     +         KE1,CPNAM(KE1)
      ENDIF
C                 print out the coordinates around the contour
      IF (ISW(5) .GE. 1)  THEN
         NPCOL = (NOWPTS+1)/2
         NFCOL = NOWPTS/2
         WRITE (ISYSWR,'(/I5,A,G13.5,A,G11.3)') NOWPTS,
     +    ' POINTS ON CONTOUR.   FMIN=',ABEST,'   ERRDEF=',UP
         WRITE (ISYSWR,'(9X,A,3X,A,18X,A,3X,A)')
     +         CPNAM(KE1),CPNAM(KE2),CPNAM(KE1),CPNAM(KE2)
         DO 1050 LINE = 1, NFCOL
           LR = LINE + NPCOL
           WRITE (ISYSWR,'(1X,I5,2G13.5,10X,I5,2G13.5)')
     +     LINE,XPTU(LINE),YPTU(LINE),LR,XPTU(LR),YPTU(LR)
 1050    CONTINUE
         IF (NFCOL .LT. NPCOL) WRITE (ISYSWR,'(1X,I5,2G13.5)')
     +                         NPCOL,XPTU(NPCOL),YPTU(NPCOL)
      ENDIF
C                                    . . contour finished. reset v
      ITAUR = 1
      CALL MNFREE(1)
      CALL MNFREE(1)
      DO 1100 J= 1, MPAR*(MPAR+1)/2
 1100 VHMAT(J) = VTHMAT(J)
      DO 1120 I= 1, MPAR
      GLOBCC(I) = GCC(I)
      WERR(I) = W(I)
 1120 X(I) = XT(I)
      CALL MNINEX (X)
      EDM = SIGSAV
      AMIN = ABEST
      ISW(2) = ISW2
      ISW(4) = ISW4
      DCOVAR = DC
      ITAUR = 0
      NFCNMX = NFMXIN
      ISTRAT = ISTRAV
      U(KE1) = U1MIN
      U(KE2) = U2MIN
      GO TO 2000
C                                     Error returns
 1350 WRITE (ISYSWR,'(A)') ' INVALID PARAMETER NUMBERS.'
      GO TO 1450
 1400 WRITE (ISYSWR,'(A)') ' LESS THAN FOUR POINTS REQUESTED.'
 1450 IERRF = -1
      CSTATU = 'USER ERROR'
      GO TO 2000
 1500 WRITE (ISYSWR,'(A)') ' MNCONT UNABLE TO FIND FOUR POINTS.'
      U(KE1) = U1MIN
      U(KE2) = U2MIN
      IERRF = 0
      CSTATU = 'FAILED'
 2000 CONTINUE
      CFROM = CHERE
      NFCNFR = NFCNCO
      RETURN
      END
