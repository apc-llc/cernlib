*
* $Id: pyinki.F,v 1.1.1.1 1996/01/11 14:05:27 mclareni Exp $
*
* $Log: pyinki.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:27  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYINKI(CHFRAM,CHBEAM,CHTARG,WIN)
 
C...Identifies the two incoming particles and sets up kinematics,
C...including rotations and boosts to/from CM frame.
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /LUJETS/,/LUDAT1/,/LUDAT2/
      SAVE /PYSUBS/,/PYPARS/,/PYINT1/
      CHARACTER CHFRAM*8,CHBEAM*8,CHTARG*8,CHCOM(3)*8,CHALP(2)*26,
     &CHIDNT(3)*8,CHTEMP*8,CHCDE(19)*8,CHINIT*76
      DIMENSION LEN(3),KCDE(19)
      DATA CHALP/'abcdefghijklmnopqrstuvwxyz',
     &'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA CHCDE/'e-      ','e+      ','nu_e    ','nu_e~   ',
     &'mu-     ','mu+     ','nu_mu   ','nu_mu~  ','tau-    ',
     &'tau+    ','nu_tau  ','nu_tau~ ','pi+     ','pi-     ',
     &'n0      ','n~0     ','p+      ','p~-     ','gamma   '/
      DATA KCDE/11,-11,12,-12,13,-13,14,-14,15,-15,16,-16,
     &211,-211,2112,-2112,2212,-2212,22/
 
C...Convert character variables to lowercase and find their length.
      CHCOM(1)=CHFRAM
      CHCOM(2)=CHBEAM
      CHCOM(3)=CHTARG
      DO 120 I=1,3
      LEN(I)=8
      DO 100 LL=8,1,-1
      IF(LEN(I).EQ.LL.AND.CHCOM(I)(LL:LL).EQ.' ') LEN(I)=LL-1
      DO 100 LA=1,26
  100 IF(CHCOM(I)(LL:LL).EQ.CHALP(2)(LA:LA)) CHCOM(I)(LL:LL)=
     &CHALP(1)(LA:LA)
      CHIDNT(I)=CHCOM(I)
 
C...Fix up bar, underscore and charge in particle name (if needed).
      DO 110 LL=1,6
      IF(CHIDNT(I)(LL:LL+2).EQ.'bar') THEN
        CHTEMP=CHIDNT(I)
        CHIDNT(I)=CHTEMP(1:LL-1)//'~'//CHTEMP(LL+3:8)//'  '
      ENDIF
  110 CONTINUE
      IF(CHIDNT(I)(1:2).EQ.'nu'.AND.CHIDNT(I)(3:3).NE.'_') THEN
        CHTEMP=CHIDNT(I)
        CHIDNT(I)='nu_'//CHTEMP(3:7)
      ELSEIF(CHIDNT(I)(1:2).EQ.'n ') THEN
        CHIDNT(I)(1:3)='n0 '
      ELSEIF(CHIDNT(I)(1:2).EQ.'n~') THEN
        CHIDNT(I)(1:3)='n~0'
      ELSEIF(CHIDNT(I)(1:2).EQ.'p ') THEN
        CHIDNT(I)(1:3)='p+ '
      ELSEIF(CHIDNT(I)(1:2).EQ.'p~'.OR.CHIDNT(I)(1:2).EQ.'p-') THEN
        CHIDNT(I)(1:3)='p~-'
      ENDIF
  120 CONTINUE
 
C...Identify free initialization.
      IF(CHCOM(1)(1:2).EQ.'no') THEN
        MINT(65)=1
        RETURN
      ENDIF
 
C...Set initial state. Error for unknown codes. Reset variables.
      N=2
      DO 140 I=1,2
      K(I,1)=1
      K(I,2)=0
      DO 130 J=1,19
  130 IF(CHIDNT(I+1).EQ.CHCDE(J)) K(I,2)=KCDE(J)
      P(I,5)=ULMASS(K(I,2))
      MINT(40+I)=1
      IF(IABS(K(I,2)).GT.100.OR.K(I,2).EQ.22) MINT(40+I)=2
      MINT(44+I)=MINT(40+I)
      IF(MSTP(11).GE.1.AND.IABS(K(I,2)).EQ.11) MINT(44+I)=3
      DO 140 J=1,5
  140 V(I,J)=0.
      IF(K(1,2).EQ.0) WRITE(MSTU(11),5000) CHBEAM(1:LEN(2))
      IF(K(2,2).EQ.0) WRITE(MSTU(11),5100) CHTARG(1:LEN(3))
      IF(K(1,2).EQ.0.OR.K(2,2).EQ.0) STOP
      DO 150 J=6,10
  150 VINT(J)=0.
      CHINIT=' '
 
C...Set up kinematics for events defined in CM frame.
      IF(CHCOM(1)(1:2).EQ.'cm') THEN
        IF(CHCOM(2)(1:1).NE.'e') THEN
          LOFFS=(31-(LEN(2)+LEN(3)))/2
          CHINIT(LOFFS+1:76)='PYTHIA will be initialized for a '//
     &    CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//' collider'//
     &    ' '
        ELSE
          LOFFS=(30-(LEN(2)+LEN(3)))/2
          CHINIT(LOFFS+1:76)='PYTHIA will be initialized for an '//
     &    CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//' collider'//
     &    ' '
        ENDIF
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5200) CHINIT
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5300) WIN
        S=WIN**2
        P(1,1)=0.
        P(1,2)=0.
        P(2,1)=0.
        P(2,2)=0.
        P(1,3)=SQRT(((S-P(1,5)**2-P(2,5)**2)**2-(2.*P(1,5)*P(2,5))**2)/
     &  (4.*S))
        P(2,3)=-P(1,3)
        P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,3)**2+P(2,5)**2)
 
C...Set up kinematics for fixed target events.
      ELSEIF(CHCOM(1)(1:3).EQ.'fix') THEN
        LOFFS=(29-(LEN(2)+LEN(3)))/2
        CHINIT(LOFFS+1:76)='PYTHIA will be initialized for '//
     &  CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &  ' fixed target'//' '
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5200) CHINIT
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5400) WIN
        P(1,1)=0.
        P(1,2)=0.
        P(2,1)=0.
        P(2,2)=0.
        P(1,3)=WIN
        P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
        P(2,3)=0.
        P(2,4)=P(2,5)
        S=P(1,5)**2+P(2,5)**2+2.*P(2,4)*P(1,4)
        VINT(10)=P(1,3)/(P(1,4)+P(2,4))
        CALL LUROBO(0.,0.,0.,0.,-VINT(10))
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5500) SQRT(S)
 
C...Set up kinematics for events in user-defined frame.
      ELSEIF(CHCOM(1)(1:3).EQ.'use') THEN
        LOFFS=(12-(LEN(2)+LEN(3)))/2
        CHINIT(LOFFS+1:76)='PYTHIA will be initialized for '//
     &  CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &  ' user-specified configuration'//' '
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5200) CHINIT
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5600)
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5700) CHCOM(2),P(1,1),
     &  P(1,2),P(1,3)
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5700) CHCOM(3),P(2,1),
     &  P(2,2),P(2,3)
        P(1,4)=SQRT(P(1,1)**2+P(1,2)**2+P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,1)**2+P(2,2)**2+P(2,3)**2+P(2,5)**2)
        DO 160 J=1,3
  160   VINT(7+J)=(DBLE(P(1,J))+DBLE(P(2,J)))/DBLE(P(1,4)+P(2,4))
        CALL LUROBO(0.,0.,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=ULANGL(P(1,1),P(1,2))
        CALL LUROBO(0.,-VINT(7),0.,0.,0.)
        VINT(6)=ULANGL(P(1,3),P(1,1))
        CALL LUROBO(-VINT(6),0.,0.,0.,0.)
        S=P(1,5)**2+P(2,5)**2+2.*(P(1,4)*P(2,4)-P(1,3)*P(2,3))
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5500) SQRT(S)
 
C...Unknown frame. Error for too low CM energy.
      ELSE
        WRITE(MSTU(11),5800) CHFRAM(1:LEN(1))
        STOP
      ENDIF
      IF(S.LT.PARP(2)**2) THEN
        WRITE(MSTU(11),5900) SQRT(S)
        STOP
      ENDIF
 
C...Save information on incoming particles.
      MINT(11)=K(1,2)
      MINT(12)=K(2,2)
      MINT(43)=2*MINT(41)+MINT(42)-2
      MINT(44)=MINT(43)
      IF(MINT(11).EQ.22) MINT(44)=MINT(44)-2
      IF(MINT(12).EQ.22) MINT(44)=MINT(44)-1
      MINT(47)=2*MIN(2,MINT(45))+MIN(2,MINT(46))-2
      IF(MIN(MINT(45),MINT(46)).EQ.3) MINT(47)=5
      VINT(1)=SQRT(S)
      VINT(2)=S
      VINT(3)=P(1,5)
      VINT(4)=P(2,5)
      VINT(5)=P(1,3)
 
C...Store constants to be used in generation.
      IF(MSTP(82).LE.1) VINT(149)=4.*PARP(81)**2/S
      IF(MSTP(82).GE.2) VINT(149)=4.*PARP(82)**2/S
 
C...Formats for initialization and error information.
 5000 FORMAT(1X,'Error: unrecognized beam particle ''',A,'''.'/
     &1X,'Execution stopped!')
 5100 FORMAT(1X,'Error: unrecognized target particle ''',A,'''.'/
     &1X,'Execution stopped!')
 5200 FORMAT(/1X,78('=')/1X,'I',76X,'I'/1X,'I',A76,'I')
 5300 FORMAT(1X,'I',18X,'at',1X,F10.3,1X,'GeV center-of-mass energy',
     &19X,'I'/1X,'I',76X,'I'/1X,78('='))
 5400 FORMAT(1X,'I',22X,'at',1X,F10.3,1X,'GeV/c lab-momentum',22X,'I')
 5500 FORMAT(1X,'I',76X,'I'/1X,'I',11X,'corresponding to',1X,F10.3,1X,
     &'GeV center-of-mass energy',12X,'I'/1X,'I',76X,'I'/1X,78('='))
 5600 FORMAT(1X,'I',76X,'I'/1X,'I',25X,'px (GeV/c)',3X,'py (GeV/c)',3X,
     &'pz (GeV/c)',15X,'I')
 5700 FORMAT(1X,'I',15X,A8,3(2X,F10.3,1X),14X,'I')
 5800 FORMAT(1X,'Error: unrecognized coordinate frame ''',A,'''.'/
     &1X,'Execution stopped!')
 5900 FORMAT(1X,'Error: too low CM energy,',F8.3,' GeV for event ',
     &'generation.'/1X,'Execution stopped!')
 
      RETURN
      END
