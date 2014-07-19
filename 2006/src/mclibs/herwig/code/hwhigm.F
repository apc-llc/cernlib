CDECK  ID>, HWHIGM.
*CMZ :-        -02/05/91  11.17.14  by  Federico Carminati
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHIGM(EM,WEIGHT)
C-----------------------------------------------------------------------
C     CHOOSE HIGGS MASS:
C     IF (IOPHIG.EQ.0.OR.IOPHIG.EQ.2) THEN
C       CHOOSE HIGGS MASS ACCORDING TO
C       EM**4       /  (EM**2-EMH**2)**2 + (GAMH*EMH)**2
C     ELSE
C       CHOOSE HIGGS MASS ACCORDING TO
C       EMH * GAMH  /  (EM**2-EMH**2)**2 + (GAMH*EMH)**2
C     ENDIF
C     IF (IOPHIG.EQ.0.OR.IOPHIG.EQ.1) THEN
C       SUPPLY WEIGHT FACTOR TO YIELD
C       EM * GAM(EM)/  (EM**2-EMH**2)**2 + (GAM(EM)*EM)**2
C     ELSE
C       SUPPLY WEIGHT FACTOR TO YIELD
C       EM*(EMH/EM)**4 * GAM(EM)
C                   /  (EM**2-EMH**2)**2 + (GAM(EM)*EMH**2/EM)**2
C       AS SUGGESTED IN M.H.SEYMOUR, PHYS.LETT.B354(1995)409.
C     ENDIF
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRUNI,EM,WEIGHT,EMH,DIF,FUN,THETA,T,EMHLST,W0,
     & W1,EMM,GAMEM,T0,TMIN,TMAX,THEMIN,THEMAX,ZMIN,ZMAX,Z,F,GAMOFS
      INTEGER I
      EXTERNAL HWRUNI
      SAVE EMHLST,GAMEM,T0,TMIN,TMAX,THEMIN,THEMAX,ZMIN,ZMAX,W0,W1
      EQUIVALENCE (EMH,RMASS(201))
      DATA EMHLST/0D0/
C---SET UP INTEGRAND AND INDEFINITE INTEGRAL OF DISTRIBUTION
C   THETA=ATAN((EM**2-EMH**2)/(GAMH*EMH)); T=TAN(THETA); T0=EMH/GAMH
      DIF(T,T0)=(T+T0)**2
      FUN(THETA,T,T0)=T + (T0*T0-1)*THETA + T0*LOG(1+T*T)
C---SET UP CONSTANTS
      IF (EMH.NE.EMHLST .OR. FSTWGT) THEN
        EMHLST=EMH
        GAMEM=GAMH*EMH
        T0=EMH/GAMH
        TMIN=(MAX(ONE*1E-10,EMH-GAMMAX*GAMH))**2/GAMEM-T0
        TMAX=(              EMH+GAMMAX*GAMH )**2/GAMEM-T0
        THEMIN=ATAN(TMIN)
        THEMAX=ATAN(TMAX)
        ZMIN=FUN(THEMIN,TMIN,T0)
        ZMAX=FUN(THEMAX,TMAX,T0)
        W0=(ZMAX-ZMIN) / PIFAC * GAMEM
        W1=(THEMAX-THEMIN) / PIFAC
      ENDIF
C---CHOOSE HIGGS MASS
      IF (IOPHIG.EQ.0.OR.IOPHIG.EQ.2) THEN
 1      EM=0
        WEIGHT=0
        Z=HWRUNI(1,ZMIN,ZMAX)
C---SOLVE FUN(THETA,TAN(THETA))=Z BY NEWTON'S METHOD
        THETA=MAX(THEMIN, MIN(THEMAX, Z/T0**2 ))
        I=1
        F=0
 10     IF (I.LE.20 .AND. ABS(1-F/Z).GT.1E-4) THEN
          I=I+1
          IF (2*ABS(THETA).GT.PIFAC) CALL HWWARN('HWHIGM',51,*999)
          T=TAN(THETA)
          F=FUN(THETA,T,T0)
          THETA=THETA-(F-Z)/DIF(T,T0)
          GOTO 10
        ENDIF
        IF (I.GT.20) CALL HWWARN('HWHIGM',1,*999)
      ELSE
        THETA=HWRUNI(0,THEMIN,THEMAX)
      ENDIF
      EM=SQRT(GAMEM*(T0+TAN(THETA)))
C---NOW CALCULATE WEIGHT FACTOR FOR NON-CONSTANT HIGGS WIDTH
      GAMOFS=EM
      CALL HWDHIG(GAMOFS)
      IF (IOPHIG.EQ.0) THEN
        WEIGHT=W0*GAMOFS*EM /EM**4 *((EM**2-EMH**2)**2 + GAMEM**2)
     &                             /((EM**2-EMH**2)**2 +(GAMOFS*EM)**2)
      ELSEIF (IOPHIG.EQ.1) THEN
        WEIGHT=W1*GAMOFS*EM /GAMEM *((EM**2-EMH**2)**2 + GAMEM**2)
     &                             /((EM**2-EMH**2)**2 +(GAMOFS*EM)**2)
      ELSEIF (IOPHIG.EQ.2) THEN
        EMM=EM*(EMH/EM)**4
        WEIGHT=W0*GAMOFS*EMM/EM**4 *((EM**2-EMH**2)**2 + GAMEM**2)
     &                             /((EM**2-EMH**2)**2 +(GAMOFS*EMM)**2)
      ELSEIF (IOPHIG.EQ.3) THEN
        EMM=EM*(EMH/EM)**4
        WEIGHT=W1*GAMOFS*EMM/GAMEM *((EM**2-EMH**2)**2 + GAMEM**2)
     &                             /((EM**2-EMH**2)**2 +(GAMOFS*EMM)**2)
      ELSE
        CALL HWWARN('HWHIGM',500,*999)
      ENDIF
 999  END
