*
* $Id: frnucod.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frnucod.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRNUCOR ***************************
 
C********************************* FRNUCOD *******************************
 
      SUBROUTINE FRNUCOD(NA,RMIN2,A0,A2,A4,RMAX3,COR)
 
C --- this subroutine determines nucleon coordinates inside a deformed
C     nucleus and recenter the sampled nucleons
C     with respect to the rest frame of the nucleus center
 
      PARAMETER (KSZ2=300)
      DIMENSION COR(KSZ2,3),R0(38),FM(38),RM(38),SUM(3)
      DATA R0/5.8,5.9,6.0,6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9,7.0,7.1,
     $    7.2,7.3,7.4,7.5,7.6,7.7,7.8,7.9,8.0,8.1,8.2,8.3,8.4,8.5,8.6,
     $    8.7,8.8,8.9,9.0,9.1,9.2,9.3,9.4,9.5/
      DATA FM/  20.32,21.13,21.96,22.80,23.66,24.52,25.43,26.35,27.28,
     $    28.23,29.19,30.18,31.18,32.20,33.24,34.30,35.37,36.47,37.58,
     $    38.71,39.86,41.02,42.21,43.41,44.63,45.87,47.13,48.41,49.70,
     $    51.02,52.35,53.70,55.07,56.46,57.86,59.29,60.73,62.19/
      DATA RM/  16.49,16.59,16.70,16.81,16.91,17.02,17.13,17.23,17.34,
     $    17.45,17.56,17.66,17.77,17.87,17.98,18.09,18.19,18.30,18.41,
     $    18.51,18.62,18.73,18.83,18.94,19.04,19.15,19.26,19.36,19.47,
     $    19.57,19.68,19.79,19.89,20.00,20.10,20.21,20.32,20.42/
 
      DO 150 J=1,NA
C --- sample a nucleon from the target
C --- first sample sita from R(sita)**3. W1:cos(sita)
50    W1=-1.+2.*RLU(0)
      W2=RMAX3*RLU(0)
      W12=W1*W1
      W14=W12*W12
      RSITA=A0+A2*W12+A4*W14
      RSITA3=RSITA*RSITA*RSITA
      IF (RSITA3.LT.W2) GOTO 50
      CTHITA=W1
      STHITA=FRSQR(MAX(0.,1.-W12), 'STHW12')
      FAI=6.2832*RLU(0)
 
C --- then sample r
 
      RT0=RSITA
      FMM=FRINT(FM,R0,38,RT0)
      RMM=FRINT(RM,R0,38,RT0)
 
        DO 140 NUM=1,10
100   RR1=RMM*RLU(0)
      RR2=FMM*RLU(0)
      RR1S=RR1*RR1
      FR=RR1S/(1.+FRREX((RR1-RT0)/.55))
        IF (RR2.LT.FR) THEN
      R=RR1
        ELSE
      GOTO 100
        ENDIF
 
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
C --- if two nucleons too close each other, sample R once more
      IF (DIC2.LT.RMIN2) GOTO 140
130   CONTINUE
      GOTO 150
140   CONTINUE
C --- if 10 times repeated don't help, then sample THITA and FAI again
      GOTO 50
150   CONTINUE
 
C --- recenter the sampled nucleons within a nucleus
      SUM(1)=0.
      SUM(2)=0.
      SUM(3)=0.
        DO 160 J=1,NA
      SUM(1)=SUM(1)+COR(J,1)
      SUM(2)=SUM(2)+COR(J,2)
      SUM(3)=SUM(3)+COR(J,3)
160     CONTINUE
        DO 170 J=1,NA
      COR(J,1)=COR(J,1)-SUM(1)/NA
      COR(J,2)=COR(J,2)-SUM(2)/NA
      COR(J,3)=COR(J,3)-SUM(3)/NA
170     CONTINUE
 
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
