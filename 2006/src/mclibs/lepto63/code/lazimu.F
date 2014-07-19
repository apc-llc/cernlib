
C **********************************************************************

      SUBROUTINE LAZIMU(XP,ZP)

C...Choose azimuthal angle (PHI) according to QCD matrix elements.
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      COMMON /LINTER/ PARI(40),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)

      J=LST(24)-1
      SGN=SIGN(1.,2.5-LST(24))
      IFL=LST(25)
      I=IABS(IFL)
      IH=1
      IF(LST(30).EQ.1) IH=2

      IF(LST(23).EQ.2) THEN
        A=PARI(24)*DQCD(0,J,1,XP,ZP,Y)+PARI(25)*DQCD(0,J,2,XP,ZP,Y)
     &  -LST(30)*ISIGN(1,IFL)*PARI(26)*DQCD(0,J,3,XP,ZP,Y)
        B=DQCD(1,J,1,XP,ZP,Y)
     &  +SGN*LST(30)*ISIGN(1,IFL)*DQCD(1,J,3,XP,ZP,Y)
        C=DQCD(2,J,1,XP,ZP,Y)
      ELSE
        A=(EWQC(1,IH,I)+EWQC(2,IH,I))*(PARI(24)*DQCD(0,J,1,XP,ZP,Y)+
     &    PARI(25)*DQCD(0,J,2,XP,ZP,Y))
     &    -LST(30)*ISIGN(1,IFL)*(EWQC(1,IH,I)-EWQC(2,IH,I))
     &    *PARI(26)*DQCD(0,J,3,XP,ZP,Y)
        B=(EWQC(1,IH,I)+EWQC(2,IH,I))*DQCD(1,J,1,XP,ZP,Y)
     &    +SGN*LST(30)*ISIGN(1,IFL)*(EWQC(1,IH,I)-EWQC(2,IH,I))
     &    *DQCD(1,J,3,XP,ZP,Y)
        C=(EWQC(1,IH,I)+EWQC(2,IH,I))*DQCD(2,J,1,XP,ZP,Y)
      ENDIF

      PHIMAX=ABS(A)+ABS(B)+ABS(C)
  100 PHI=6.2832*RLU(0)
      IF(A+B*COS(PHI)+C*COS(2.*PHI).LT.RLU(0)*PHIMAX) GOTO 100
      CALL LUROBO(0.,PHI,0.,0.,0.)

CAE.Store ME variables
      PARL(28)=XP
      PARL(29)=ZP
      PARL(30)=PHI
CAE
      RETURN
      END
