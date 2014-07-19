*
* $Id: mncros.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mncros.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNCROS(FCN,AOPT,IERCR,FUTIL)
#include "minuit/d506dp.inc"
CC       Find point where MNEVAL=AMIN+UP, along the line through
CC       XMIDCR,YMIDCR with direction XDIRCR,YDIRCR,   where X and Y 
CC       are parameters KE1CR and KE2CR.  If KE2CR=0 (from MINOS),
CC       only KE1CR is varied.  From MNCONT, both are varied.
CC       Crossing point is at
CC        (U(KE1),U(KE2)) = (XMID,YMID) + AOPT*(XDIR,YDIR)
CC
#include "minuit/d506cm.inc"
      CHARACTER CHERE*10, CHARAL*28, CHSIGN*4
      PARAMETER (CHERE='MNCROS    ', MLSB=3, MAXITR=15, TLR=0.01)
      DIMENSION FLSB(MLSB),ALSB(MLSB), COEFF(3)
      LOGICAL LDEBUG
      EXTERNAL FCN,FUTIL
      DATA  CHARAL/' .ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      LDEBUG = (IDBG(6) .GE. 1)
      AMINSV = AMIN
C        convergence when F is within TLF of AIM and next prediction
C        of AOPT is within TLA of previous value of AOPT
      AIM = AMIN + UP
      TLF = TLR*UP
      TLA = TLR
      XPT(1) = 0.0
      YPT(1) = AIM
      CHPT(1) = ' '
      IPT = 1
      IF (KE2CR .EQ. 0) THEN
        XPT(2) = -1.0
        YPT(2) = AMIN
        CHPT(2) = '.'
        IPT = 2
      ENDIF
C                    find the largest allowed A
      AULIM = 100.
      DO 100 IK= 1, 2
         IF (IK .EQ. 1)  THEN
            KEX = KE1CR
            ZMID = XMIDCR
            ZDIR = XDIRCR
         ELSE
            IF (KE2CR .EQ. 0)  GO TO 100
            KEX = KE2CR
            ZMID = YMIDCR
            ZDIR = YDIRCR
         ENDIF
         IF (NVARL(KEX) .LE. 1) GO TO 100
         IF (ZDIR .EQ. ZERO)      GO TO 100
         ZLIM = ALIM(KEX)
         IF (ZDIR .GT. ZERO) ZLIM = BLIM(KEX)
         AULIM = MIN(AULIM,(ZLIM-ZMID)/ZDIR)
  100 CONTINUE
C                  LSB = Line Search Buffer
C          first point
      ANEXT = 0.
      AOPT = ANEXT
      LIMSET = .FALSE.
        IF (AULIM .LT. AOPT+TLA)  LIMSET = .TRUE.
      CALL MNEVAL(FCN,ANEXT,FNEXT,IEREV,FUTIL)
C debug printout:
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT
      IF (IEREV .GT. 0)  GO TO 900
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930
      IPT = IPT + 1
      XPT(IPT) = ANEXT
      YPT(IPT) = FNEXT
      CHPT(IPT)= CHARAL(IPT:IPT)
      ALSB(1) = ANEXT
      FLSB(1) = FNEXT
      FNEXT = MAX(FNEXT,AMINSV+0.1*UP)
      AOPT =  SQRT((UP)/(FNEXT-AMINSV)) - 1.0
      IF (ABS(FNEXT-AIM) .LT. TLF)  GO TO 800
C
      IF (AOPT .LT. -HALF)  AOPT = -HALF
      IF (AOPT .GT. ONE)    AOPT = ONE
      LIMSET = .FALSE.
      IF (AOPT .GT. AULIM)  THEN
              AOPT = AULIM
              LIMSET = .TRUE.
      ENDIF
      CALL MNEVAL(FCN,AOPT,FNEXT,IEREV,FUTIL)
C debug printout:
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT
      IF (IEREV .GT. 0)  GO TO 900
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930
      ALSB(2) = AOPT
      IPT = IPT + 1
      XPT(IPT) = ALSB(2)
      YPT(IPT) = FNEXT
      CHPT(IPT)= CHARAL(IPT:IPT)
      FLSB(2) = FNEXT
      DFDA = (FLSB(2)-FLSB(1))/ (ALSB(2)-ALSB(1))
C                   DFDA must be positive on the contour
      IF (DFDA .GT. ZERO)  GO TO 460
  300    CALL MNWARN('D',CHERE,'Looking for slope of the right sign')
         MAXLK = MAXITR - IPT
         DO 400 IT= 1, MAXLK
            ALSB(1) = ALSB(2)
            FLSB(1) = FLSB(2)
            AOPT = ALSB(1) + 0.2*REAL(IT)
            LIMSET = .FALSE.
            IF (AOPT .GT. AULIM)  THEN
              AOPT = AULIM
              LIMSET = .TRUE.
            ENDIF
            CALL MNEVAL(FCN,AOPT,FNEXT,IEREV,FUTIL)
C debug printout:
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT
            IF (IEREV .GT. 0)  GO TO 900
            IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930
               ALSB(2) = AOPT
               IPT = IPT + 1
               XPT(IPT) = ALSB(2)
               YPT(IPT) = FNEXT
               CHPT(IPT)= CHARAL(IPT:IPT)
            FLSB(2) = FNEXT
            DFDA = (FLSB(2)-FLSB(1))/ (ALSB(2)-ALSB(1))
            IF (DFDA .GT. ZERO)  GO TO 450
  400    CONTINUE
         CALL MNWARN('W',CHERE,'Cannot find slope of the right sign')
         GO TO 950
  450    CONTINUE
C                    we have two points with the right slope
  460 AOPT = ALSB(2) + (AIM-FLSB(2))/DFDA
      FDIST = MIN(ABS(AIM -FLSB(1)),ABS(AIM -FLSB(2)))
      ADIST = MIN(ABS(AOPT-ALSB(1)),ABS(AOPT-ALSB(2)))
      TLA = TLR
      IF (ABS(AOPT) .GT. ONE)  TLA = TLR*ABS(AOPT)
      IF (ADIST .LT. TLA .AND. FDIST .LT. TLF) GO TO 800
      IF (IPT .GE. MAXITR)  GO TO 950
      BMIN = MIN(ALSB(1),ALSB(2)) - 1.0
      IF (AOPT .LT. BMIN)  AOPT = BMIN
      BMAX = MAX(ALSB(1),ALSB(2)) + 1.0
      IF (AOPT .GT. BMAX)  AOPT = BMAX
C                    Try a third point
      LIMSET = .FALSE.
      IF (AOPT .GT. AULIM) THEN
         AOPT = AULIM
         LIMSET = .TRUE.
      ENDIF
      CALL MNEVAL(FCN,AOPT,FNEXT,IEREV,FUTIL)
C debug printout:
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT
      IF (IEREV .GT. 0)  GO TO 900
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930
      ALSB(3) = AOPT
      IPT = IPT + 1
      XPT(IPT) = ALSB(3)
      YPT(IPT) = FNEXT
      CHPT(IPT)= CHARAL(IPT:IPT)
      FLSB(3) = FNEXT
      INEW = 3
C                now we have three points, ask how many <AIM
      ECARMN = ABS(FNEXT-AIM)
      IBEST = 3
      ECARMX = 0.
      NOLESS = 0
      DO 480 I= 1, 3
         ECART = ABS(FLSB(I) - AIM)
         IF (ECART .GT. ECARMX) THEN
            ECARMX = ECART
            IWORST = I
         ENDIF
         IF (ECART .LT. ECARMN) THEN
            ECARMN = ECART
            IBEST = I
         ENDIF
         IF (FLSB(I) .LT. AIM) NOLESS = NOLESS + 1
  480 CONTINUE
      INEW = IBEST
C           if at least one on each side of AIM, fit a parabola
      IF (NOLESS.EQ.1 .OR. NOLESS.EQ.2) GO TO 500
C           if all three are above AIM, third must be closest to AIM
      IF (NOLESS .EQ. 0 .AND. IBEST .NE. 3)  GO TO 950
C           if all three below, and third is not best, then slope
C             has again gone negative, look for positive slope.
      IF (NOLESS .EQ. 3 .AND. IBEST .NE. 3) THEN
          ALSB(2) = ALSB(3)
          FLSB(2) = FLSB(3)
          GO TO 300
      ENDIF
C           in other cases, new straight line thru last two points
      ALSB(IWORST) = ALSB(3)
      FLSB(IWORST) = FLSB(3)
      DFDA = (FLSB(2)-FLSB(1))/ (ALSB(2)-ALSB(1))
      GO TO 460
C                parabola fit
  500 CALL MNPFIT(ALSB,FLSB,3,COEFF,SDEV)
      IF (COEFF(3) .LE. ZERO)  CALL MNWARN ('D',CHERE,
     +             'Curvature is negative near contour line.')
      DETERM =  COEFF(2)**2 - 4.*COEFF(3)*(COEFF(1)-AIM)
      IF (DETERM .LE. ZERO)   THEN
          CALL MNWARN('D',CHERE,'Problem 2, impossible determinant')
          GO TO 950
      ENDIF
C                Find which root is the right one
      RT = SQRT(DETERM)
      X1 = (-COEFF(2) + RT)/(2.*COEFF(3))
      X2 = (-COEFF(2) - RT)/(2.*COEFF(3))
      S1 = COEFF(2) + 2.*X1*COEFF(3)
      S2 = COEFF(2) + 2.*X2*COEFF(3)
      IF (S1*S2 .GT. ZERO) WRITE (ISYSWR,'(A)') ' MNCONTour problem 1'
      AOPT = X1
      SLOPE = S1
      IF (S2 .GT. ZERO)  THEN
         AOPT = X2
         SLOPE = S2
      ENDIF
C         ask if converged
      TLA = TLR
      IF (ABS(AOPT) .GT. ONE)  TLA = TLR*ABS(AOPT)
      IF (ABS(AOPT-ALSB(IBEST)) .LT. TLA  .AND. 
     &    ABS(FLSB(IBEST)-AIM)  .LT. TLF)  GO TO 800
      IF (IPT .GE. MAXITR)  GO TO 950
C         see if proposed point is in acceptable zone between L and R
C         first find ILEFT, IRIGHT, IOUT and IBEST
      ILEFT = 0
      IRIGHT = 0
      IBEST = 1
      ECARMX = 0.
      ECARMN = ABS(AIM-FLSB(1))
      DO 550 I= 1, 3
      ECART = ABS(FLSB(I) - AIM)
      IF (ECART .LT. ECARMN) THEN
         ECARMN = ECART
         IBEST = I
      ENDIF
      IF (ECART .GT. ECARMX) ECARMX = ECART
      IF (FLSB(I) .GT. AIM)  THEN
         IF (IRIGHT .EQ. 0)  THEN
            IRIGHT = I
         ELSE IF (FLSB(I) .GT. FLSB(IRIGHT)) THEN
            IOUT = I
         ELSE
            IOUT = IRIGHT
            IRIGHT = I
         ENDIF
      ELSE IF (ILEFT .EQ. 0)  THEN
         ILEFT = I
      ELSE IF (FLSB(I) .LT. FLSB(ILEFT)) THEN
         IOUT = I
      ELSE     
         IOUT = ILEFT
         ILEFT = I
      ENDIF
  550 CONTINUE 
C       avoid keeping a very bad point next time around
      IF (ECARMX .GT. 10.*ABS(FLSB(IOUT)-AIM))
     &    AOPT = HALF*AOPT + HALF*HALF*(ALSB(IRIGHT)+ALSB(ILEFT))      
C         knowing ILEFT and IRIGHT, get acceptable window
      SMALLA = 0.1*TLA
      IF (SLOPE*SMALLA .GT. TLF)  SMALLA = TLF/SLOPE
      ALEFT  = ALSB(ILEFT)  + SMALLA
      ARIGHT = ALSB(IRIGHT) - SMALLA
C         move proposed point AOPT into window if necessary
      IF (AOPT .LT. ALEFT)  AOPT = ALEFT
      IF (AOPT .GT. ARIGHT) AOPT = ARIGHT
      IF (ALEFT .GT. ARIGHT)AOPT = HALF*(ALEFT + ARIGHT)
C         see if proposed point outside limits (should be impossible!)
      LIMSET = .FALSE.
      IF (AOPT .GT. AULIM)  THEN
              AOPT = AULIM
              LIMSET = .TRUE.
      ENDIF
C                  Evaluate function at new point AOPT
      CALL MNEVAL(FCN,AOPT,FNEXT,IEREV,FUTIL)
C debug printout:
      IF (LDEBUG) WRITE (ISYSWR,'(A,I8,A,F10.5,A,2F10.5)')
     + ' MNCROS: calls=',NFCN,'   AIM=',AIM,'  F,A=',FNEXT,AOPT
      IF (IEREV .GT. 0)  GO TO 900
      IF (LIMSET .AND. FNEXT .LE. AIM)  GO TO 930
      IPT = IPT + 1
      XPT(IPT) = AOPT
      YPT(IPT) = FNEXT
      CHPT(IPT)= CHARAL(IPT:IPT)
C                Replace odd point by new one
      ALSB(IOUT) = AOPT
      FLSB(IOUT) = FNEXT
C          the new point may not be the best, but it is the only one
C          which could be good enough to pass convergence criteria
      IBEST = IOUT
      GO TO 500
C
C       Contour has been located, return point to MNCONT OR MINOS
  800 CONTINUE
      IERCR = 0
      GO TO 1000
C                error in the minimization
  900 IF (IEREV .EQ. 1)  GO TO 940
      GO TO 950
C                parameter up against limit
  930 IERCR = 1
      GO TO 1000
C                too many calls to FCN
  940 IERCR = 2
      GO TO 1000
C                cannot find next point
  950 IERCR = 3
C                in any case
 1000 CONTINUE
      IF (LDEBUG) THEN
         ITOOHI = 0
         DO 1100 I= 1, IPT
         IF (YPT(I) .GT. AIM+UP) THEN
            YPT(I) = AIM+UP
            CHPT(I) = '+'
            ITOOHI = 1
         ENDIF
 1100    CONTINUE
         CHSIGN = 'POSI'
         IF (XDIRCR .LT. ZERO)  CHSIGN = 'NEGA'
         IF (KE2CR .EQ. 0)  WRITE (ISYSWR, '(2X,A,A,I3)')
     +            CHSIGN,'TIVE MINOS ERROR, PARAMETER ',KE1CR
         IF (ITOOHI .EQ. 1)  WRITE (ISYSWR, '(10X,A)')
     +            'POINTS LABELLED "+" WERE TOO HIGH TO PLOT.'
         IF (IERCR .EQ. 1) WRITE (ISYSWR,'(10X,A)')
     +            'RIGHTMOST POINT IS UP AGAINST LIMIT.'
         CALL MNPLOT(XPT,YPT,CHPT,IPT,ISYSWR,NPAGWD,NPAGLN)
      ENDIF
      RETURN
      END
