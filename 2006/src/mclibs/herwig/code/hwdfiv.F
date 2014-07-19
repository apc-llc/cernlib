CDECK  ID>, HWDFIV.
*CMZ :-        -
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWDFIV(P0,P1,P2,P3,P4,P5)
C-----------------------------------------------------------------------
C     Generates 5-body decay 0->1+2+3+4+5 using pure phase space
C-----------------------------------------------------------------------
      DOUBLE PRECISION HWRGEN,P0(5),P1(5),P2(5),P3(5),P4(5),P5(5),B,C,
     & AA,BB,CC,DD,EE,FF,TT,S1,RS1,GG,S2,RS2,HH,S3,PP,QQ,RR,SS,P1CM,
     & P2345(5),P2CM,P345(5),P3CM,P45(5),P4CM
      EXTERNAL HWRGEN
      B=P0(5)-P1(5)
      C=P2(5)+P3(5)+P4(5)+P5(5)
      IF (B.LT.C) CALL HWWARN('HWDFIV',100,*999)
      AA=(P0(5)+P1(5))**2
      BB=B**2
      CC=C**2
      DD=(P3(5)+P4(5)+P5(5))**2
      EE=(P4(5)+P5(5))**2
      FF=(P4(5)-P5(5))**2
      TT=(B-C)*P0(5)**3
C Select squared masses S1, S2 and S3 of 2345, 345 and 45 subsystems
  10  S1=BB+HWRGEN(1)*(CC-BB)
      RS1=SQRT(S1)
      GG=(RS1-P2(5))**2
      S2=DD+HWRGEN(2)*(GG-DD)
      RS2=SQRT(S2)
      HH=(RS2-P3(5))**2
      S3=EE+HWRGEN(3)*(HH-EE)
      PP=(AA-S1)*(BB-S1)
      QQ=((RS1+P2(5))**2-S2)*(GG-S2)/S1
      RR=((RS2+P3(5))**2-S3)*(HH-S3)/S2
      SS=(S3-EE)*(S3-FF)/S3
      IF (PP*QQ*RR*SS.LT.TT*S1*S2*S3*HWRGEN(4)**2) GOTO 10
C Do two body decays: 0-->1+2345, 2345-->2+345, 345-->3+45 and 45-->4+5
      P1CM=SQRT(PP/4)/P0(5)
      P2345(5)=RS1
      P2CM=SQRT(QQ/4)
      P345(5)=RS2
      P3CM=SQRT(RR/4)
      P45(5)=SQRT(S3)
      P4CM=SQRT(SS/4)
      CALL HWDTWO(P0   ,P1,P2345,P1CM,TWO,.TRUE.)
      CALL HWDTWO(P2345,P2,P345 ,P2CM,TWO,.TRUE.)
      CALL HWDTWO(P345 ,P3,P45  ,P3CM,TWO,.TRUE.)
      CALL HWDTWO(P45  ,P4,P5   ,P4CM,TWO,.TRUE.)
  999 END
