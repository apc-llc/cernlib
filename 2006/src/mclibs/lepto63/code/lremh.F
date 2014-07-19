
C **********************************************************************

      SUBROUTINE LREMH(IFLRO,PT,IFLR,K2,Z)

C...Gives flavour and energy-momentum fraction Z for the particle
C...to be produced out of the target remnant when that is not a
C...simple diquark.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)

C...Flavours fixed when calling from LQQBEV OR LYREMN
      IF(IFLRO.EQ.0) GOTO 200

C...Split target remnant qqqQ -> qqQ + q or qqqQbar -> qQbar + qq
C...Q (Qbar) is the partner to the struck sea quark
C...qqq are the nucleon valence quarks from which a quark q or a
C...diquark qq is chosen at random to form a jet system with the
C...scattered sea antiquark or quark, respectively, the other parton
C...forms a baryon qqQ or meson qQbar, respectively.
  100 IFLQ=INT(1.+LST(22)/3.+RLU(0))
      IF(IFLQ.EQ.LST(22)) THEN
        IFLQQ=2101
        IF(RLU(0).GT.PARL(4)) IFLQQ =2103
      ELSE
        IFLQQ=1000*IFLQ+100*IFLQ+3
      ENDIF
      IFLQ=3-IFLQ

C...Choose flavour of hadron and parton for jet system
      IF(IFLRO.GT.0) THEN
        CALL LUKFDI(IFLQQ,IFLRO,IDUM,K2)
        IF(K2.EQ.0) GOTO 100
        IFLR=IFLQ
      ELSE
        CALL LUKFDI(IFLQ,IFLRO,IDUM,K2)
        IF(K2.EQ.0) GOTO 100
        IFLR=IFLQQ
      ENDIF

C...Entry from LQQBEV & LYREMN with flavours given, choose E-p fraction
  200 KSP=IFLR
C...Split energy-momentum of target remnant according to functions P(z)
C...z=E-pz fraction for qq (q) forming jet-system with struck Q (Qbar)
C...1-z=E-pz fraction for qQbar (qqQ) hadron
C...mq=mass of (light) parton remnant q (qq) in jet system
C...mQ=mass of produced (heavy flavour) hadron
      MSTJ(93)=1
      AMSP=ULMASS(KSP)
      MSTJ(93)=1
      AMK2=ULMASS(K2)
      IF(LST(14).EQ.1) THEN
C...P(z)=(a+1)(1-z)**a with <z>=1/(a+2)=1/3 since a=1 fixed
        Z=1.-SQRT(RLU(0))
C...Flip if baryon produced
        KC2=IABS(LUCOMP(K2))
        IF(KC2.GE.301.AND.KC2.LE.400) Z=1.-Z
      ELSEIF(LST(14).EQ.2) THEN
C...P(z)=(a+1)(1-z)**a with <z>=1/(a+2)=mq/(mq+mQ) --> a=a(mq,mQ)
        A=(AMSP+AMK2)/AMSP - 2.
        Z=RLU(0)**(1./(A+1.))
      ELSEIF(LST(14).EQ.3) THEN
C...Using Peterson fragmentation function
C...P(z)=N/(z(1-1/z-c/(1-z))**2)  where c=(mq/mQ)**2  (FC=-c)
        FC=-(AMSP/AMK2)**2
  300   Z=RLU(0)
        IF(-4.*FC*Z*(1.-Z)**2.LT.RLU(0)*((1.-Z)**2-FC*Z)**2) GOTO 300
      ELSEIF(LST(14).EQ.4) THEN
C...Using chosen fragmentation function in JETSET
        TM2=AMK2**2+PT**2
        CALL LUZDIS(1,0,TM2,Z)
      ENDIF
      LST(27)=1
      K2A=IABS(K2)
      IF((K2A.GE.1.AND.K2A.LE.8).OR.K2A.EQ.21.OR.LUCOMP(K2A).EQ.90)
     &LST(27)=2

      RETURN
      END
