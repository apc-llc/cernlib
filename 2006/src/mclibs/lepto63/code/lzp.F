
C **********************************************************************

      SUBROUTINE LZP(XP,ZP,IFAIL)

C...Choose value of ZP according to QCD matrix elements.

      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      DATA C1,C2/0.2122066,0.0795775/,DZPMAX,SZP,CP/3*0./
      FQG(DZ,DX,DA,DB,DC)=DA*(DZ**2+DX**2)/(1.-DX)+2.*DA*DX*DZ*(1.-DZ)
     &+2.*DA*(1.-DZ)+4.*DB*DX*DZ*(1.-DZ)+DC*(DZ**2+DX**2)/(1.-DX)+
     &2.*DC*(DX+DZ)*(1.-DZ)
      FQQ(DZ,DX,DA,DB,DC,DD,DE)=DA*DD*(DZ**2+(1.-DZ)**2)+DB*DE*DZ*
     &(1.-DZ)+DC*DD*(2.*DZ-1.)

      IFAIL=1
      IH=1
      IF(LST(30).EQ.1) IH=2
CAE.Scheme for ME cutoff: W2, Q2, mixed, z-shat
      IF(LST(20).LE.1) THEN
        ZPMIN=(1.-X)*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).EQ.2) THEN
        ZPMIN=X*XP/(XP-X)*PARL(27)
      ELSEIF(LST(20).GE.3.AND.LST(20).LE.5) THEN
        ZPMIN=PARL(27)
      ELSEIF(LST(20).EQ.6) THEN
        ZPMIN=PARL(8)
      ELSE
        WRITE(6,*) 'LZP: No such jet scheme!'
      ENDIF
CAE
      IF(ZPMIN.LE.0..OR.ZPMIN.GE.0.5) RETURN
      ZPMAX=1.-ZPMIN
      I=IABS(LST(25))
      AP=1.-ZPMIN
      BP=ZPMIN/AP
      IF(LST(23).EQ.2) THEN
        A=PARI(24)
        B=PARI(25)
        CSIGN=-LST(30)*ISIGN(1,LST(25))*PARI(26)
      ELSE
        A=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(24)
        B=(EWQC(1,IH,I)+EWQC(2,IH,I))*PARI(25)
        C=(EWQC(1,IH,I)-EWQC(2,IH,I))*PARI(26)
        CSIGN=-C*LST(30)*ISIGN(1,LST(25))
      ENDIF
      IF(LST(24).EQ.2) THEN
        DZPMAX=MAX(FQG(ZPMIN,XP,A,B,CSIGN),FQG(ZPMAX,XP,A,B,CSIGN))
        AA=2.*(A+CSIGN)/(1.-XP)-4.*A*XP-8.*B*XP-4.*CSIGN
        IF(ABS(AA).GT.1.E-20) THEN
          BB=2.*A*(XP-1.)+4.*B*XP+2.*CSIGN*(1.-XP)
          Z1=-BB/AA
          IF(Z1.GT.ZPMIN.AND.Z1.LT.ZPMAX)
     &    DZPMAX=MAX(DZPMAX,FQG(Z1,XP,A,B,CSIGN))
        ENDIF
        DZPMAX=DZPMAX*C1*1.05
      ELSEIF(LST(24).EQ.3) THEN
        CP=1./BP**2
        D=XP**2+(1.-XP)**2
        E=8.*XP*(1-XP)
        DZPMAX=MAX(FQQ(ZPMIN,XP,A,B,CSIGN,D,E),
     &  FQQ(ZPMAX,XP,A,B,CSIGN,D,E))
        AA=4.*A*D-2.*B*E
        IF(ABS(AA).GT.1.E-20) THEN
          BB=B*E-2.*A*D+2.*CSIGN*D
          Z1=-BB/AA
          IF(Z1.GT.ZPMIN.AND.Z1.LT.ZPMAX)
     &    DZPMAX=MAX(DZPMAX,FQQ(Z1,XP,A,B,CSIGN,D,E))
        ENDIF
        DZPMAX=DZPMAX*C2*1.05
      ENDIF
      IPART=LST(24)-1
      LOOP=0
  100 LOOP=LOOP+1
      IF(LOOP.GT.1000) RETURN
      IF(LST(24).EQ.2) THEN
        ZP=1.-AP*BP**RLU(0)
        SZP=1.-ZP
      ELSEIF(LST(24).EQ.3) THEN
        DP=BP*CP**RLU(0)
        ZP=DP/(1.+DP)
        SZP=ZP*(1.-ZP)
      ENDIF
      ZPWEIT=SZP*(A*DQCD(0,IPART,1,XP,ZP,0.)+B*DQCD(0,IPART,2,XP,ZP,0.)
     &+CSIGN*DQCD(0,IPART,3,XP,ZP,0.))/DZPMAX
     
      IF(ZPWEIT.LT.RLU(0)) GOTO 100
      IFAIL=0
      RETURN
      END
