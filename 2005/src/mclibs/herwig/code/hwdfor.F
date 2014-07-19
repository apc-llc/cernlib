CDECK  ID>, HWDFOR.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWDFOR(P0,P1,P2,P3,P4)
C-----------------------------------------------------------------------
C     Generates 4-body decay 0->1+2+3+4 using pure phase space
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,P0(5),P1(5),P2(5),P3(5),P4(5),B,C,AA,BB,
     & CC,DD,EE,TT,S1,RS1,FF,S2,PP,QQ,RR,P1CM,P234(5),P2CM,P34(5),P3CM
      EXTERNAL HWRGEN
      B=P0(5)-P1(5)
      C=P2(5)+P3(5)+P4(5)
      IF (B.LT.C) CALL HWWARN('HWDFOR',100,*999)
      AA=(P0(5)+P1(5))**2
      BB=B**2
      CC=C**2
      DD=(P3(5)+P4(5))**2
      EE=(P3(5)-P4(5))**2
      TT=(B-C)*P0(5)**3
C Select squared masses S1 and S2 of 234 and 34 subsystems
  10  S1=BB+HWRGEN(1)*(CC-BB)
      RS1=SQRT(S1)
      FF=(RS1-P2(5))**2
      S2=DD+HWRGEN(2)*(FF-DD)
      PP=(AA-S1)*(BB-S1)
      QQ=((RS1+P2(5))**2-S2)*(FF-S2)/S1
      RR=(S2-DD)*(S2-EE)/S2
      IF (PP*QQ*RR.LT.TT*S1*S2*HWRGEN(3)**2) GOTO 10
C Do two body decays: 0-->1+234, 234-->2+34 and 34-->3+4
      P1CM=SQRT(PP/4)/P0(5)
      P234(5)=RS1
      P2CM=SQRT(QQ/4)
      P34(5)=SQRT(S2)
      P3CM=SQRT(RR/4)
      CALL HWDTWO(P0  ,P1,P234,P1CM,TWO,.TRUE.)
      CALL HWDTWO(P234,P2,P34 ,P2CM,TWO,.TRUE.)
      CALL HWDTWO(P34 ,P3,P4  ,P3CM,TWO,.TRUE.)
  999 END
