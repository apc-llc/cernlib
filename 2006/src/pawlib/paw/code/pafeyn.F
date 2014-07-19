*
* $Id: pafeyn.F,v 1.1.1.1 1996/03/01 11:38:39 mclareni Exp $
*
* $Log: pafeyn.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:39  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/09 16/01/95  10.26.32  by  O.Couet
*-- Author :    O.Couet   14/09/92
      SUBROUTINE PAFEYN
*
*          /GRAPHICS_BASIC/PRIMITIVES : Feynman diagrams
*
#include "paw/pcpatl.inc"
#include "paw/pcbuff.inc"
#include "paw/quest.inc"
*
      REAL XX(51),YY(51)
      REAL UXX(3),UYY(3)
      EQUIVALENCE (PAWBUF(1000),XX(1)),(PAWBUF(1100),YY(1))
      EQUIVALENCE (PAWBUF(1200),UXX(1)),(PAWBUF(1300),UYY(1))
      PARAMETER (PI=3.14159265,TWOPI=2.*PI,PIHALF=PI/2.,DEGRAD=PI/180.)
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)
      IQUEST(1) = 0
C
C           HELIX
C
      IF(CHPATL.EQ.'HELIX')THEN
         CALL KUGETR(X1)
         CALL KUGETR(Y1)
         CALL KUGETR(X2)
         CALL KUGETR(Y2)
         CALL KUGETR(R)
         R = ABS(R)
         CALL KUGETR(WI)
         WI = ABS(WI)
         CALL KUGETR(PHI)
         PHI = MOD(PHI,90.)
         IF (PHI .GT. 89.) PHI = 89.
         IPARTS = 50 * INT(WI)
         IF (ABS(X2-X1) .LT. 1.E-17 ) THEN
            THE = PIHALF
            IF ( (Y2-Y1) .LT. 0 ) THE = THE + PI
         ELSE
            IF ((X2-X1).EQ.0.) THEN
               CALL IGERR('Invalid parameters X1 and X2','HELIX')
               RETURN
            ENDIF
            THE = ATAN( (Y2-Y1)/(X2-X1) )
         ENDIF
         IF( (X2-X1) .LT. 0.) THE = THE + PI
         CT = COS(THE)
         ST = SIN(THE)
         CP = COS(PHI*DEGRAD)
         SP = SIN(PHI*DEGRAD)
* z - length of helix
         IF (CP.EQ.0..OR.INT(WI).EQ.0.OR.R.EQ.0..OR.IPARTS.EQ.0) THEN
            CALL IGERR('Invalid parameters','HELIX')
            RETURN
         ENDIF
         WZ = SQRT((X1-X2)**2+(Y1-Y2)**2) / CP
         WZ1 = WZ/FLOAT(INT(WI))
         ZSLP = WZ1/(2.*PI*R)
         DUZ = WZ / FLOAT(IPARTS)
         UZ = DUZ
         CALL PAHEXY(R,ZSLP,UZ,UX,UY)
         WX = UY * SP + UZ * CP
         WY = UX
* rotation
         UX = WX * CT - WY * ST
         UY = WX * ST + WY * CT
* translation
         XX(1) = UX + X1
         YY(1) = UY + Y1
         DO 20 K = 1 , INT(WI)
            DO 10 I = 2 , 51
               CALL PAHEXY(R,ZSLP,UZ,UX,UY)
               WX = UY * SP + UZ * CP
               WY = UX
* rotation
               UX = WX * CT - WY * ST
               UY = WX * ST + WY * CT
* translation
               XX(I) = UX + X1
               YY(I) = UY + Y1
               UZ = UZ + DUZ
   10       CONTINUE
            CALL IPL(51,XX,YY)
            XX(1) = XX(51)
            YY(1) = YY(51)
   20    CONTINUE
      ENDIF
C
C           ARCHELIX
C
      IF(CHPATL.EQ.'ARCHELIX')THEN
         CALL KUGETR(X1)
         CALL KUGETR(Y1)
         CALL KUGETR(X2)
         CALL KUGETR(Y2)
         CALL KUGETR(R)
         R = ABS(R)
         CALL KUGETR(WI)
         WI = ABS(WI)
         CALL KUGETR(PHI)
         CALL KUGETR(RL)
         RL = ABS(RL)
         RL = MAX(1.E-7,RL)
         RL = MAX(SQRT((X2-X1)**2+(Y2-Y1)**2)/2.,RL)
         PHI = MOD(PHI,90.)
         IF (PHI .GT. 89.) PHI = 89.
         IPARTS = 50 * INT(WI)
         IF (ABS(X2-X1) .LT. 1.E-17 ) THEN
            THE = PIHALF
            IF ( (Y2-Y1) .LT. 0 ) THE = THE + PI
         ELSE
            IF ((X2-X1).EQ.0.) THEN
               CALL IGERR('Invalid parameters X1 and X2','ARCHELIX')
               RETURN
            ENDIF
            THE = ATAN( (Y2-Y1)/(X2-X1) )
         ENDIF
         IF( (X2-X1) .LT. 0.) THE = THE + PI
         CT = COS(THE)
         ST = SIN(THE)
         CP = COS(PHI*DEGRAD)
         SP = SIN(PHI*DEGRAD)
* z - length of helix
         WZ = SQRT((X1-X2)**2+(Y1-Y2)**2)
* loop
         IF (RL.EQ.0..OR.CP.EQ.0..OR.INT(WI).EQ.0.OR.R.EQ.0..OR.
     +       IPARTS.EQ.0) THEN
            CALL IGERR('Invalid parameters','ARCHELIX')
            RETURN
         ENDIF
         XI0  = MIN(1.,WZ/2./RL)
         XI0  = ASIN(XI0)*2.
         WZ   = XI0*RL /CP
         WZ1  = WZ/FLOAT(INT(WI))
         ZSLP = WZ1/(2.*PI*R)
         DUZ  = WZ / FLOAT(IPARTS)
         UZ   = DUZ
         CALL PAHEXY(R,ZSLP,UZ,UX,UY)
         WX   = UY * SP + UZ * CP
         WY   = UX
* loop'en
         XI   = - WX/RL
         UX   = (RL+WY)*SIN(XI)
         UY   = (RL+WY)*COS(XI)
         WX   = UX
         WY   = UY - RL
         SXI0 = SIN(XI0/2.)
         CXI0 = COS(XI0/2.)
         UX   = (WX)*CXI0-(WY)*SXI0
         UY   = (WX)*SXI0+(WY)*CXI0
         WX   = UX
         WY   = UY
* rotation
         UX   = WX * CT - WY * ST
         UY   = WX * ST + WY * CT
* translation
         XX(1) = UX + X1
         YY(1) = UY + Y1
         DO 40 K = 1 , INT(WI)
            DO 30 I = 2 , 51
               CALL PAHEXY(R,ZSLP,UZ,UX,UY)
               WX = UY * SP + UZ * CP
               WY = UX
* loop'en
               XI = - WX/RL
               UX = -(RL+WY)*SIN(XI)
               UY = (RL+WY)*COS(XI)
               WX = UX
               WY = UY - RL
               UX = (WX)*CXI0-(WY)*SXI0
               UY = (WX)*SXI0+(WY)*CXI0
               WX = UX
               WY = UY
* rotation
               UX = WX * CT - WY * ST
               UY = WX * ST + WY * CT
* translation
               XX(I) = UX + X1
               YY(I) = UY + Y1
               UZ = UZ + DUZ
   30       CONTINUE
            CALL IPL(51,XX,YY)
            XX(1) = XX(51)
            YY(1) = YY(51)
   40    CONTINUE
      ENDIF
C
C           ARLINE
C
      IF(CHPATL.EQ.'ARLINE')THEN
         CALL KUGETR(X1)
         CALL KUGETR(Y1)
         CALL KUGETR(X2)
         CALL KUGETR(Y2)
         CALL KUGETR(H)
         H = H/2.
         XX(1) = X1
         XX(2) = X2
         YY(1) = Y1
         YY(2) = Y2
         CALL IPL(2,XX,YY)
         IF ( ABS(X2-X1) .GT. 1E-8 ) THEN
            THE = ATAN( (Y2-Y1)/(X2-X1) )
            IF( (X2-X1) .LT. 0.) THE = THE + PI
            CT = COS(THE)
            ST = SIN(THE)
            UXX(1) = H
            UXX(2) = -H
            UXX(3) = -H
            UYY(1) = 0.
            UYY(2) = H
            UYY(3) = -H
            A = (Y2-Y1)/(X2-X1)
            B = Y1 - A*X1
            RMX = (X2 - X1)/2.+ X1
            RMY = A * RMX + B
            DO 50 I = 1 , 3
               XX(I) = UXX(I) * CT - UYY(I) * ST
               YY(I) = UXX(I) * ST + UYY(I) * CT
* translation
               XX(I) = XX(I) + RMX
               YY(I) = YY(I) + RMY
   50       CONTINUE
            CALL IFA(3,XX,YY)
         ELSE
            RMY = (Y2 - Y1)/2. + Y1
            RMX = X1
            IF ( Y2 .GT. Y1 ) THEN
               YY(1) = RMY + H
               YY(2) = RMY - H
               YY(3) = RMY - H
               XX(1) = X1
               XX(2) = X1 + H
               XX(3) = X1 - H
            ELSE
               YY(1) = RMY - H
               YY(2) = RMY + H
               YY(3) = RMY + H
               XX(1) = X1
               XX(2) = X1 + H
               XX(3) = X1 - H
            ENDIF
            CALL IFA(3,XX,YY)
         ENDIF
      ENDIF
C
C           FPOINT
C
      IF(CHPATL.EQ.'FPOINT')THEN
         CALL KUGETR(X1)
         CALL KUGETR(Y1)
         CALL KUGETR(R)
         ALPHA = 0.
         DA    = TWOPI/50.
         XX(1) = -R*SIN(ALPHA) + X1
         YY(1) = R*COS(ALPHA) + Y1
         DO 60 I = 2 , 51
            ALPHA = ALPHA + DA
            XX(I) = -R*SIN(ALPHA) +X1
            YY(I) = R*COS(ALPHA) +Y1
   60    CONTINUE
         CALL IFA(51,XX,YY)
      ENDIF
      END
