*
* $Id: frnucor.F,v 1.1.1.1 1996/01/11 14:05:19 mclareni Exp $
*
* $Log: frnucor.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:19  mclareni
* Fritiof
*
*
C********************************* END FRAACOL ***************************
 
 
C********************************* FRNUCOR *******************************
 
      SUBROUTINE FRNUCOR(L,NA,RMIN2,FMM,RMM,COR)
 
C --- this subroutine determines nucleon coordinates inside a nucleus
C     and recenter the sampled nucleons with respect to the rest frame of
C     the nucleus center. L=1 for proj, L=2 for target.
 
      PARAMETER (KSZ2=300)
      DIMENSION COR(KSZ2,3),SUM(3)
 
      DO 150 J=1,NA
C --- sample a nucleon from the nucleus
C --- first, sample r
100   RR1=RMM*RLU(0)
      RR2=FMM*RLU(0)
 
      FR = FRROR(L,RR1)
 
        IF (RR2.LT.FR) THEN
      R=RR1
        ELSE
      GOTO 100
        ENDIF
 
C --- then sample COS(sita) & fai
        DO 140 NUM=1,10
      CTHITA=1.-2.*RLU(0)
      STHITA=FRSQR(MAX(0.,1.-CTHITA**2), 'SITFAI')
      FAI=6.2832*RLU(0)
      COR(J,1)=R*STHITA*COS(FAI)
      COR(J,2)=R*STHITA*SIN(FAI)
      COR(J,3)=R*CTHITA
        IF (J.EQ.1) GOTO 150
C --- check if there are two nucleons too close each other
        DO 130 J1=1,J-1
      DICX=COR(J,1)-COR(J1,1)
      DICY=COR(J,2)-COR(J1,2)
      DICZ=COR(J,3)-COR(J1,3)
      DIC2=DICX*DICX+DICY*DICY+DICZ*DICZ
C --- if two nucleons too close each other, sample THITA & FAI once more
      IF (DIC2.LT.RMIN2) GOTO 140
130   CONTINUE
      GOTO 150
140   CONTINUE
C --- if 10 times of repeated saplings don't help, then sample R again
      GOTO 100
150   CONTINUE
 
C --- recenter the sampled nucleons within a nucleus
      SUM(1)=0.
      SUM(2)=0.
      SUM(3)=0.
        DO 170 J=1,NA
      SUM(1)=SUM(1)+COR(J,1)
      SUM(2)=SUM(2)+COR(J,2)
      SUM(3)=SUM(3)+COR(J,3)
170     CONTINUE
        DO 180 J=1,NA
      COR(J,1)=COR(J,1)-SUM(1)/NA
      COR(J,2)=COR(J,2)-SUM(2)/NA
      COR(J,3)=COR(J,3)-SUM(3)/NA
180     CONTINUE
 
C --- order the nucleons on increasing z-coordinates
      DO 220 J1=2,NA
        ZCO=COR(J1,3)
        DO 210 J2=1,J1-1
        IF (ZCO.LT.COR(J2,3)) THEN
          XCO=COR(J1,1)
          YCO=COR(J1,2)
          DO 200 J3=1,J1-J2
            COR(J1+1-J3,1)=COR(J1-J3,1)
            COR(J1+1-J3,2)=COR(J1-J3,2)
            COR(J1+1-J3,3)=COR(J1-J3,3)
200       CONTINUE
          COR(J2,1)=XCO
          COR(J2,2)=YCO
          COR(J2,3)=ZCO
          GOTO 220
          ENDIF
210     CONTINUE
220   CONTINUE
 
      RETURN
      END
