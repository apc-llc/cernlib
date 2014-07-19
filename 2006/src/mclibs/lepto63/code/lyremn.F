
C **********************************************************************

      SUBROUTINE LYREMN(IPU1,IPU2)

C...ADDS ON TARGET REMNANTS (ONE OR TWO FROM EACH SIDE) AND
C...INCLUDES PRIMORDIAL KT.
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),XLP,YLP,W2LP,Q2LP,ULP
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON /LYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      COMMON /LYPROC/ ISUB,KFL(3,2),X(2),SH,TH,UH,Q2,XSEC(0:40)
      DIMENSION KFLCH(2),KFLSP(2),CHI(2),PMS(6),IS(2),ROBO(5)
      DOUBLE PRECISION DBETAX,DBETAZ,DROBO(5)
      DATA IPU,IQ/0,0/,PEI,PE,PZI,PZ,SHS,PZH,PEH/7*0./

C...FIND EVENT TYPE, SET POINTERS
      IF(IPU1.EQ.0.AND.IPU2.EQ.0) RETURN
      ILEP=0
      IF(IPU1.EQ.0) ILEP=1
      IF(IPU2.EQ.0) ILEP=2
      IF(ISUB.EQ.7) ILEP=-1
      IF(ILEP.EQ.1) IQ=21
      IF(ILEP.EQ.2) IQ=23
      IP=MAX(IPU1,IPU2)
      NS=N

C...DEFINE INITIAL PARTONS, INCLUDING PRIMORDIAL KT
  100 DO 120 I=3,4
      IF(I.EQ.3) IPU=IPU1
      IF(I.EQ.4) IPU=IPU2
      K(I,1)=21
      K(I,3)=I-2
      DO 110 J=1,5
  110 P(I,J)=0.
      IF(ISUB.EQ.7) THEN
        K(I,2)=21
        SHS=0.
      ELSEIF(IPU.NE.0) THEN
        K(I,2)=K(IPU,2)
        P(I,5)=P(IPU,5)
        CALL LPRIKT(PARL(3),PTSPL,PHISPL)
        P(I,1)=PTSPL*COS(PHISPL)
        P(I,2)=PTSPL*SIN(PHISPL)
        PMS(I-2)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      ELSE
        K(I,2)=K(IQ,2)
        P(I,5)=-SQRT(Q2)
        PMS(I-2)=-Q2
        SHS=(1.-X(5-I))*Q2/X(5-I)+PYVAR(7-I)**2
      ENDIF
  120 CONTINUE

C...KINEMATICS CONSTRUCTION FOR INITIAL PARTONS
      IF(ILEP.EQ.0) SHS=PYVAR(31)*PYVAR(32)*PYVAR(2)+
     &(P(3,1)+P(4,1))**2+(P(3,2)+P(4,2))**2
      SHR=SQRT(MAX(0.,SHS))
      IF(ILEP.EQ.0) THEN
        IF((SHS-PMS(1)-PMS(2))**2-4.*PMS(1)*PMS(2).LE.0.) GOTO 100
        P(3,4)=0.5*(SHR+(PMS(1)-PMS(2))/SHR)
        P(3,3)=SQRT(MAX(0.,P(3,4)**2-PMS(1)))
        P(4,4)=SHR-P(3,4)
        P(4,3)=-P(3,3)
      ELSEIF(ILEP.EQ.1) THEN
        P(3,4)=P(IQ,4)
        P(3,3)=P(IQ,3)
        P(4,4)=P(IP,4)
        P(4,3)=P(IP,3)
      ELSEIF(ILEP.EQ.2) THEN
        P(3,4)=P(IP,4)
        P(3,3)=P(IP,3)
        P(4,4)=P(IQ,4)
        P(4,3)=P(IQ,3)
      ENDIF

C...TRANSFORM PARTONS TO OVERALL CM-FRAME (NOT FOR LEPTOPRODUCTION)
      IF(ILEP.EQ.0) THEN
        MSTU(1)=3
        MSTU(2)=4
        DROBO(3)=(P(3,1)+P(4,1))/SHR
        DROBO(4)=(P(3,2)+P(4,2))/SHR
        CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,-DROBO(3),-DROBO(4),0.D0)
        ROBO(2)=ULANGL(P(3,1),P(3,2))
        CALL LUDBRB(MSTU(1),MSTU(2),0.,-ROBO(2),0.D0,0.D0,0.D0)
        ROBO(1)=ULANGL(P(3,3),P(3,1))
        CALL LUDBRB(MSTU(1),MSTU(2),-ROBO(1),0.,0.D0,0.D0,0.D0)
        MSTU(2)=MAX(IPY(47),IPU1,IPU2)
        CALL LUDBRB(MSTU(1),MSTU(2),
     &  ROBO(1),ROBO(2),DROBO(3),DROBO(4),0.D0)
        DROBO(5)=MAX(-0.999999,MIN(0.999999,(PYVAR(31)-PYVAR(32))/
     &  (PYVAR(31)+PYVAR(32))))
        CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,0.D0,0.D0,DROBO(5))
        MSTU(1)=0
        MSTU(2)=0
      ENDIF

C...CHECK INVARIANT MASS OF REMNANT SYSTEM:
C...HADRONIC EVENTS OR LEPTOPRODUCTION
      IF(ILEP.LE.0) THEN
        IF(IPY(12).LE.0.OR.ISUB.EQ.7) PYVAR(33)=0.
        IF(IPY(12).LE.0.OR.ISUB.EQ.7) PYVAR(34)=0.
        PEH=P(3,4)+P(4,4)+0.5*PYVAR(1)*(PYVAR(33)+PYVAR(34))
        PZH=P(3,3)+P(4,3)+0.5*PYVAR(1)*(PYVAR(33)-PYVAR(34))
        SHH=(PYVAR(1)-PEH)**2-(P(3,1)+P(4,1))**2-(P(3,2)+P(4,2))**2-
     &  PZH**2
        MSTJ(93)=1
        AMK32=ULMASS(K(3,2))
        MSTJ(93)=1
        AMK42=ULMASS(K(4,2))
        PMMIN=P(1,5)+P(2,5)+AMK32+AMK42
        IF(SHR.GE.PYVAR(1).OR.SHH.LE.(PMMIN+PYPAR(12))**2) THEN
          IPY(48)=1
          RETURN
        ENDIF
        SHR=SQRT(SHH+(P(3,1)+P(4,1))**2+(P(3,2)+P(4,2))**2)
      ELSE
        PEI=P(IQ,4)+P(IP,4)
        PZI=P(IQ,3)+P(IP,3)
        PMS(ILEP)=MAX(0.,PEI**2-PZI**2+P(5-ILEP,1)**2+P(5-ILEP,2)**2)
        MSTJ(93)=1
        PMMIN=P(3-ILEP,5)+ULMASS(K(5-ILEP,2))+SQRT(PMS(ILEP))
        IF(SHR.LE.PMMIN+PYPAR(12)) THEN
          IPY(48)=1
          RETURN
        ENDIF
      ENDIF

C...SUBDIVIDE REMNANT IF NECESSARY, STORE FIRST PARTON
  130 I=NS-1
      DO 160 JT=1,2
      IF(JT.EQ.ILEP) GOTO 160
      IF(JT.EQ.1) IPU=IPU1
      IF(JT.EQ.2) IPU=IPU2
      CALL LYSPLI(IPY(40+JT),KFL(1,JT),KFLCH(JT),KFLSP(JT))
      I=I+2
      IS(JT)=I
      K(I,1)=3
      K(I,2)=KFLSP(JT)
      K(I,3)=JT
      MSTJ(93)=1
      P(I,5)=ULMASS(K(I,2))
CJR--
      KFI2=LUCOMP(K(I,2))
      IF (KFI2.EQ.90) THEN
         P(I,5)=P(I,5)-2.*PARL(20)
      ELSEIF (1.LE.KFI2 .AND. KFI2.LE.6) THEN
         P(I,5)=P(I,5)-PARL(20)
      ENDIF
CJR--
C...FIRST PARTON COLOUR CONNECTIONS AND TRANSVERSE MASS
      K(I+1,1)=-1
      K(I+1,3)=I
      K(I+1,2)=1000
      IF(IPY(34).GE.1) K(I+1,2)=1000+JT
      DO 140 J=1,5
  140 P(I+1,J)=0.
      IF(KFLSP(JT).EQ.21) THEN
        P(I+1,3)=IPU
        P(I+1,4)=IPU
        P(IPU+1,1)=I
        P(IPU+1,2)=I
        K(I,4)=IPU+IPU*MSTU(5)
        K(I,5)=IPU+IPU*MSTU(5)
        K(IPU,4)=MOD(K(IPU,4),MSTU(5))+I*MSTU(5)
        K(IPU,5)=MOD(K(IPU,5),MSTU(5))+I*MSTU(5)
      ELSE
        IFLS=(3-ISIGN(1,KFLSP(JT)*(1102-IABS(KFLSP(JT)))))/2
        P(I+1,IFLS+2)=IPU
        P(IPU+1,3-IFLS)=I
        K(I,IFLS+3)=IPU
        K(IPU,6-IFLS)=MOD(K(IPU,6-IFLS),MSTU(5))+I*MSTU(5)
      ENDIF
      IF(KFLCH(JT).EQ.0) THEN
        P(I,1)=-P(JT+2,1)
        P(I,2)=-P(JT+2,2)
        PMS(JT)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      ELSE
C...WHEN EXTRA REMNANT PARTON OR HADRON: FIND RELATIVE PT, STORE
C...PRIMORDIAL KT SPLIT SHARED BETWEEN REMNANTS
        CALL LPRIKT(PARL(14),PTSPL,PHISPL)
C...RELATIVE DISTRIBUTION OF ENERGY; EXTRA PARTON COLOUR CONNECTION
        CALL LREMH(0,PTSPL,KFLSP(JT),KFLCH(JT),CHI(JT))
        P(I,1)=-P(JT+2,1)*(1.-CHI(JT))+PTSPL*COS(PHISPL)
        P(I,2)=-P(JT+2,2)*(1.-CHI(JT))+PTSPL*SIN(PHISPL)
        PMS(JT+2)=P(I,5)**2+P(I,1)**2+P(I,2)**2
        I=I+2
        DO 150 J=1,5
        K(I,J)=0
        K(I+1,J)=0
        P(I,J)=0.
  150   P(I+1,J)=0.
        K(I,1)=1
        K(I,2)=KFLCH(JT)
        K(I,3)=JT
        MSTJ(93)=1
        P(I,5)=ULMASS(K(I,2))
CJR--
        KFI2=LUCOMP(K(I,2))
        IF (KFI2.EQ.90) THEN
           P(I,5)=P(I,5)-2.*PARL(20)
        ELSEIF (1.LE.KFI2 .AND. KFI2.LE.6) THEN
           P(I,5)=P(I,5)-PARL(20)
        ENDIF
CJR--
        P(I,1)=-P(JT+2,1)*CHI(JT)-PTSPL*COS(PHISPL)
        P(I,2)=-P(JT+2,2)*CHI(JT)-PTSPL*SIN(PHISPL)
        PMS(JT+4)=P(I,5)**2+P(I,1)**2+P(I,2)**2
C...end of update
        PMS(JT)=PMS(JT+4)/CHI(JT)+PMS(JT+2)/(1.-CHI(JT))
        K(I+1,1)=-1
        K(I+1,3)=I
        K(I+1,2)=1000
        IF(IPY(34).GE.1) K(I+1,2)=1000+JT
        IF((IABS(KFLCH(JT)).GE.1.AND.IABS(KFLCH(JT)).LE.8).OR.
     &  IABS(KFLCH(JT)).EQ.21.OR.LUCOMP(IABS(KFLCH(JT))).EQ.90) THEN
          IFLS=(3-ISIGN(1,KFLCH(JT)*(1102-IABS(KFLCH(JT)))))/2
          P(I+1,IFLS+2)=IPU
          P(IPU+1,3-IFLS)=I
          K(I,1)=3
          K(I,IFLS+3)=IPU
          K(IPU,6-IFLS)=MOD(K(IPU,6-IFLS),MSTU(5))+I*MSTU(5)
        ELSE
          IF(IPY(34).GE.1) THEN
            K(I,1)=1
            K(I,3)=JT
          ENDIF
        ENDIF
      ENDIF
  160 CONTINUE
      IF(SHR.LE.SQRT(PMS(1))+SQRT(PMS(2))) GOTO 130
      N=I+1

C...RECONSTRUCT KINEMATICS OF REMNANTS
      DO 170 JT=1,2
      IF(JT.EQ.ILEP) GOTO 170
      PE=0.5*(SHR+(PMS(JT)-PMS(3-JT))/SHR)
      PZ=SQRT(PE**2-PMS(JT))
      IF(KFLCH(JT).EQ.0) THEN
        P(IS(JT),4)=PE
        P(IS(JT),3)=PZ*(-1)**(JT-1)
      ELSE
        PW1=CHI(JT)*(PE+PZ)
        P(IS(JT)+2,4)=0.5*(PW1+PMS(JT+4)/PW1)
        P(IS(JT)+2,3)=0.5*(PW1-PMS(JT+4)/PW1)*(-1)**(JT-1)
        P(IS(JT),4)=PE-P(IS(JT)+2,4)
        P(IS(JT),3)=PZ*(-1)**(JT-1)-P(IS(JT)+2,3)
      ENDIF
  170 CONTINUE

C     CALL GULIST(31,2)
C...HADRONIC EVENTS: BOOST REMNANTS TO CORRECT LONGITUDINAL FRAME
      IF(ILEP.LE.0) THEN
        MSTU(1)=NS+1
        CALL LUDBRB(MSTU(1),MSTU(2),
     &  0.,0.,0.D0,0.D0,-DBLE(PZH)/(DBLE(PYVAR(1))-DBLE(PEH)))
        MSTU(1)=0
C...LEPTOPRODUCTION EVENTS: BOOST COLLIDING SUBSYSTEM
      ELSE
        IMIN=21
        IMAX=MAX(IP,IPY(47))
        PEF=SHR-PE
        PZF=PZ*(-1)**(ILEP-1)
        PT2=P(5-ILEP,1)**2+P(5-ILEP,2)**2
        PHIPT=ULANGL(P(5-ILEP,1),P(5-ILEP,2))
        CALL LUDBRB(IMIN,IMAX,0.,-PHIPT,0.D0,0.D0,0.D0)
        RQP=P(IQ,3)*(PT2+PEI**2)-P(IQ,4)*PEI*PZI
        SINTH=P(IQ,4)*SQRT(PT2*(PT2+PEI**2)/(RQP**2+PT2*
     &  P(IQ,4)**2*PZI**2))*SIGN(1.,-RQP)
        CALL LUDBRB(IMIN,IMAX,ASIN(SINTH),0.,0.D0,0.D0,0.D0)
        DBETAX=(-DBLE(PEI)*PZI*SINTH+
     &  SQRT(DBLE(PT2)*(PT2+PEI**2-(PZI*SINTH)**2)))/
     &  (DBLE(PT2)+PEI**2)
        CALL LUDBRB(IMIN,IMAX,0.,0.,DBETAX,0.D0,0.D0)
        CALL LUDBRB(IMIN,IMAX,0.,PHIPT,0.D0,0.D0,0.D0)
        PEM=P(IQ,4)+P(IP,4)
        PZM=P(IQ,3)+P(IP,3)
        DBETAZ=(-DBLE(PEM)*PZM+
     &  PZF*SQRT(DBLE(PZF)**2+PEM**2-PZM**2))/(DBLE(PZF)**2+PEM**2)
        CALL LUDBRB(IMIN,IMAX,0.,0.,0.D0,0.D0,DBETAZ)
C...Avoid double application of kt
        P(4,1)=0.
        P(4,2)=0.
        CALL LUDBRB(3,4,ASIN(SINTH),0.,DBETAX,0.D0,0.D0)
        CALL LUDBRB(3,4,0.,PHIPT,0.D0,0.D0,DBETAZ)
      ENDIF

      RETURN
      END
