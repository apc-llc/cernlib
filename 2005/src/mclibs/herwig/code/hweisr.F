CDECK  ID>, HWEISR.
*CMZ :-        -26/04/94  16.20.17  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWEISR(IHEP)
C-----------------------------------------------------------------------
C     GENERATES AN ISR PHOTON FROM INCOMING E+, E-, MU+ OR MU-
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,QSQMAX,QSQMIN,A,B,B1,B2,B3,B4,B5,B6,B7,B8,
     $ R,AA,T0,T1,C1,C2,T,Z(2),QSQ(2),PHI(2),C
      INTEGER IHEP,I,J
      EXTERNAL HWRGEN
      SAVE Z,QSQ,PHI
C---IF ZMXISR IS ZERO, THERE CAN BE NO ISR
      IF (ZMXISR.EQ.0 .OR. IPRO.GT.3) RETURN
C---CHECK CONSISTENCY OF TMNISR AND ZMXISR
      IF (ZMXISR**2.LT.TMNISR) CALL HWWARN('HWEISR',200,*999)
C---CALCULATE VIRTUALITY LIMITS
      QSQMAX=4*PHEP(4,IHEP)**2
      QSQMIN=PHEP(5,IHEP)**2
C---AND THEREFORE THE Z DEPENDENCE
      A=ALPHEM/PIFAC
      B=A*(LOG(QSQMAX/QSQMIN)-1)
C---DECIDE HOW MUCH WEIGHT TO GIVE THE Z RESONANCE
      IF (IHEP.EQ.1.AND.IPRO.LE.3) THEN
        IF (IPRO.EQ.1) THEN
          AA=10
        ELSEIF (IPRO.EQ.2) THEN
          AA=0
        ELSEIF (IPRO.EQ.3) THEN
          AA=1
        ELSE
          RETURN
        ENDIF
        T0=RMASS(200)**2/QSQMAX
        T1=GAMZ*RMASS(200)/QSQMAX
        IF (T0.GT.1) THEN
          T0=0
          AA=0
        ENDIF
        AA=AA*(1-T0)
C---GENERATE A T VALUE BETWEEN TMNISR AND 1 ACCORDING TO:
C   ( b**2*log(zmxisr**2/t)/t + 2*b*(1-(1-zmxisr)**b)*((1-t)**(2*b-1)+1/t
C     +(1-t0)**(2b-1)*aa*t1/((t-t0)**2+t1**2)) ) *theta(zmxisr**2-t)
C  +( 2*b*(1-zmxisr)**b*((1-t)**(b-1)+1/t
C     +(1-t0)**(b-1)*aa*t1/((t-t0)**2+t1**2))  ) *theta(zmxisr-t)
C  +( (1-zmxisr)**(2*b)                        ) *delta(1-t)
        B1=(1-ZMXISR)**(2*B)
        B2=B1+2*(1-ZMXISR)**B*((1-TMNISR)**B-(1-ZMXISR)**B)
        B3=B2+2*B*(1-ZMXISR)**B*LOG(ZMXISR/TMNISR)
        B4=B3+2*B*(1-ZMXISR)**B*AA*(1-T0)**(B-1)
     $       *(ATAN((ZMXISR-T0)/T1)-ATAN((TMNISR-T0)/T1))
        B5=B4+(1-(1-ZMXISR)**B)*((1-TMNISR)**(2*B)-(1-ZMXISR**2)**(2*B))
        B6=B5+2*B*(1-(1-ZMXISR)**B)*LOG(ZMXISR**2/TMNISR)
        B7=B6+B**2*LOG(ZMXISR**2/TMNISR)**2/2
        B8=B7+2*B*(1-(1-ZMXISR)**B)*AA*(1-T0)**(2*B-1)
     $       *(ATAN((ZMXISR**2-T0)/T1)-ATAN((TMNISR-T0)/T1))
        R=B8*HWRGEN(0)
        IF (R.LE.B1) THEN
C---NEITHER EMITS
          T=1
          GAMWT=GAMWT*B8/B1
          Z(1)=1
        ELSEIF (R.LE.B4) THEN
C---ONE EMITS
          IF (R.LE.B2) THEN
            R=(R-B1)/(B2-B1)
            T=1-(1-TMNISR)*(1-R*(1-((1-ZMXISR)/(1-TMNISR))**B))**(1/B)
          ELSEIF (R.LE.B3) THEN
            R=(R-B2)/(B3-B2)
            T=(TMNISR/ZMXISR)**R*ZMXISR
          ELSE
            R=(R-B3)/(B4-B3)
            T=T0+T1*TAN(
     $           ATAN((ZMXISR-T0)/T1)*R+ATAN((TMNISR-T0)/T1)*(1-R))
          ENDIF
          GAMWT=GAMWT*B8/(2*B*(1-ZMXISR)**B*((1-T)**(B-1)+1/T+
     $         (1-T0)**(B-1)*AA*T1/((T-T0)**2+T1**2)))
          Z(1)=1
          IF (HWRGEN(1).GT.0.5) Z(1)=T
          GAMWT=GAMWT*2
        ELSE
C---BOTH EMIT
          IF (R.LE.B5) THEN
            R=(R-B4)/(B5-B4)
            T=1-(1-TMNISR)*
     $           (1-R*(1-((1-ZMXISR**2)/(1-TMNISR))**(2*B)))**(.5/B)
          ELSEIF (R.LE.B6) THEN
            R=(R-B5)/(B6-B5)
            T=(TMNISR/ZMXISR**2)**R*ZMXISR**2
          ELSEIF (R.LE.B7) THEN
            R=(R-B6)/(B7-B6)
            T=(TMNISR/ZMXISR**2)**SQRT(R)*ZMXISR**2
          ELSE
            R=(R-B7)/(B8-B7)
            T=T0+T1*TAN(
     $           ATAN((ZMXISR**2-T0)/T1)*R+ATAN((TMNISR-T0)/T1)*(1-R))
          ENDIF
          GAMWT=GAMWT*B8/(B**2*LOG(ZMXISR**2/T)/T
     $         + 2*B*(1-(1-ZMXISR)**B)*((1-T)**(2*B-1)+1/T+
     $         (1-T0)**(B-1)*AA*T1/((T-T0)**2+T1**2)))
C---GENERATE A Z VALUE BETWEEN T/ZMXISR AND ZMXISR ACCORDING TO:
C   1/z+(1-z)**(b-1)+t/z**2*(1-t/z)**(b-1)
          C1=LOG(ZMXISR**2/T)
          C2=C1+2/B*((1-T/ZMXISR)**B-(1-ZMXISR)**B)
          IF (C2.GT.0) THEN
            R=C2*HWRGEN(4)
            IF (R.LE.C1) THEN
              Z(1)=(T/ZMXISR**2)**HWRGEN(5)*ZMXISR
            ELSE
              Z(1)=1-(1-T/ZMXISR)*
     $             (1-HWRGEN(6)*(1-((1-ZMXISR)/(1-T/ZMXISR))**B))**(1/B)
              IF (2*R.LE.C2+C1) Z(1)=T/Z(1)
            ENDIF
          ELSE
            Z(1)=SQRT(T)
          ENDIF
          GAMWT=GAMWT*C2/Z(1)
     $         /(1/Z(1)+(1-Z(1))**(B-1)+T/Z(1)**2*(1-T/Z(1))**(B-1))
        ENDIF
C---INCLUDE DISTRIBUTION FUNCTIONS
        Z(2)=T/Z(1)
        DO 10 I=1,2
          IF (Z(I).GT.ZMXISR) THEN
            Z(I)=1
            GAMWT=GAMWT*(1-ZMXISR)**B*EXP(3*B/4)*(1-B**2*PIFAC**2/12)
          ELSE
            GAMWT=GAMWT*(B*(1-Z(I))**(B-1)*(1+Z(I)**2)/2
     $           *EXP(B*Z(I)/2*(1+Z(I)/2))*(1-B**2*PIFAC**2/12)
     $           +B**2/8*((1+Z(I))*((1+Z(I))**2+3*LOG(Z(I)))
     $           -4*LOG(Z(I))/(1-Z(I))))
          ENDIF
  10    CONTINUE
C---CHOOSE BOTH QSQ VALUES
        DO 30 I=1,2
          IF (Z(I).GT.ZMXISR .OR. COLISR) THEN
            QSQ(I)=0
          ELSE
            J=3-I
C---ACCORDING TO 1/(QSQ+QSQMIN) FROM 0 TO (1-Z)*(T/(Z+T))*QSQMAX
 20         QSQ(I)=(((1-Z(I))*(T/(Z(I)+T))
     $           *QSQMAX/QSQMIN+1)**HWRGEN(7)-1)*QSQMIN
C---AND REJECT TO QSQ/(QSQ+QSQMIN)**2
            IF (HWRGEN(8)*(QSQ(I)+QSQMIN).GT.QSQ(I)) GOTO 20
          ENDIF
 30     CONTINUE
C---CHOOSE BOTH AZIMUTHS
        PHI(1)=HWRGEN(9)*2*PIFAC
        PHI(2)=HWRGEN(10)*2*PIFAC
C---USE S-HAT PRESCRIPTION TO MODIFY Z VALUES
        I=0
        IF ((1-Z(1))*QSQ(1).GT.(1-Z(2))*QSQ(2)) I=1
        IF ((1-Z(2))*QSQ(2).GT.(1-Z(1))*QSQ(1)) I=2
        IF (I.GT.0) THEN
          J=3-I
          Z(I)=Z(I)+QSQ(I)/QSQMAX
          IF (QSQ(J).GT.0) THEN
            Z(J)=((QSQ(I)*QSQMAX+QSQ(J)*QSQMAX
     $           -QSQ(I)*QSQ(J))/QSQMAX**2+T)/Z(I)
            C=COS(PHI(1)-PHI(2))*SQRT(QSQ(1)*QSQ(2))/QSQMAX
            Z(J)=Z(J)+(-2*C**2*(1-Z(I))+2*C*SQRT((1-Z(I))
     $           *(C**2*(1-Z(I))+Z(I)**2*(1-Z(J)))))/Z(I)**2
          ENDIF
        ENDIF
      ELSEIF (IHEP.EQ.2) THEN
C---EVERYTHING WAS GENERATED LAST TIME
      ELSE
C---ROUTINE CALLED UNEXPECTEDLY
        CALL HWWARN('HWEISR',201,*999)
      ENDIF
C---IF Z IS TOO LARGE THERE IS NO EMISSION
      IF (Z(IHEP).GT.ZMXISR) RETURN
C---PUT NEW LEPTON IN EVENT RECORD
      NHEP=NHEP+1
      IDHW(NHEP)=IDHW(IHEP)
      IDHEP(NHEP)=IDHEP(IHEP)
      ISTHEP(NHEP)=3
      JMOHEP(1,NHEP)=IHEP
      JMOHEP(2,NHEP)=0
      JDAHEP(1,NHEP)=0
      JDAHEP(2,NHEP)=0
      JDAHEP(1,IHEP)=NHEP
C---AND OUTGOING PHOTON
      NHEP=NHEP+1
      IDHW(NHEP)=59
      IDHEP(NHEP)=22
      ISTHEP(NHEP)=1
      JMOHEP(1,NHEP)=IHEP
      JMOHEP(2,NHEP)=0
      JDAHEP(1,NHEP)=0
      JDAHEP(2,NHEP)=0
      JDAHEP(2,IHEP)=NHEP
C---RECONSTRUCT PHOTON KINEMATICS (Z IS LIGHT-CONE MOMENTUM FRACTION)
      PHEP(1,NHEP)=SQRT(QSQ(IHEP)*(1-Z(IHEP)))*COS(PHI(IHEP))
      PHEP(2,NHEP)=SQRT(QSQ(IHEP)*(1-Z(IHEP)))*SIN(PHI(IHEP))
      PHEP(3,NHEP)=(1-Z(IHEP))*PHEP(4,IHEP)-QSQ(IHEP)/(4*PHEP(4,IHEP))
      IF (IHEP.EQ.2) PHEP(3,NHEP)=-PHEP(3,NHEP)
      PHEP(4,NHEP)=(1-Z(IHEP))*PHEP(4,IHEP)+QSQ(IHEP)/(4*PHEP(4,IHEP))
      PHEP(5,NHEP)=0
C---AND LEPTON
      CALL HWVDIF(4,PHEP(1,IHEP),PHEP(1,NHEP),PHEP(1,NHEP-1))
      CALL HWUMAS(PHEP(1,NHEP-1))
C---UPDATE OVERALL CM FRAME
      JMOHEP(IHEP,3)=NHEP-1
      CALL HWVDIF(4,PHEP(1,3),PHEP(1,IHEP),PHEP(1,3))
      CALL HWVSUM(4,PHEP(1,NHEP-1),PHEP(1,3),PHEP(1,3))
      CALL HWUMAS(PHEP(1,3))
 999  END
