C ********************************************************************

      SUBROUTINE LSMALL
C--                                                                  --C
C--   Created:     951031                                            --C
C--   Last update: 960109                                            --C
C--   Purpose:     Take care of small mass systems with one diquark  --C
C--                                                                  --C

      IMPLICIT NONE

C--       global variables
      INTEGER N,K
      REAL P,V
      COMMON /LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      INTEGER LST
      REAL CUT,PARL,X,Y,W2,Q2,U
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,Q2,U
      INTEGER MSTU,MSTJ
      REAL PARU,PARJ
      COMMON /LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER KCHG
      REAL PMAS,PARF,VCKM
      COMMON /LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4) 

C--       functions
      REAL RLU,ULMASS
      INTEGER LUCOMP
C--       local variables
      INTEGER I,IFIRST,I1,I2,IDQ,IQ,NTRY1,NTRY2,KFB,KFH1,KFH2
      INTEGER KIDQ,KIQ,KDUMMY
      REAL*8 ENERGY,MINENERGY,MAXENERGY,INVMASS,MININVMASS,MAXINVMASS
      REAL*8 PLIGHT(5),PSUM(5)
      REAL*8 TOT2,M1,M2,ROTARG,PABS,COSTHE,PTEMP,PHI,PI
      REAL*8 PCPS,PC2,PN2,PS2,A,B,C,EPS2,EPS1
      LOGICAL FIRST
      DATA PI/3.14159265359D0/

C--       find lightest singlet system
      FIRST=.TRUE.
      MINENERGY=PARL(21)
      DO 20 I=1,N
         IF (K(I,1).EQ.2) THEN
            IF (FIRST) THEN
               PSUM(1)=P(I,1)
               PSUM(2)=P(I,2)
               PSUM(3)=P(I,3)
               PSUM(4)=P(I,4)
               PSUM(5)=P(I,5)
               MSTJ(93)=1
               PSUM(5)=ULMASS(K(I,2)) 
               FIRST=.FALSE.
               IFIRST=I
            ELSE
               PSUM(1)=PSUM(1)+P(I,1)
               PSUM(2)=PSUM(2)+P(I,2)
               PSUM(3)=PSUM(3)+P(I,3)
               PSUM(4)=PSUM(4)+P(I,4)
               PSUM(5)=PSUM(5)+P(I,5)
            ENDIF
         ELSEIF (K(I,1).EQ.1 .AND. .NOT. FIRST) THEN
            PSUM(1)=PSUM(1)+P(I,1)
            PSUM(2)=PSUM(2)+P(I,2)
            PSUM(3)=PSUM(3)+P(I,3)
            PSUM(4)=PSUM(4)+P(I,4)
            PSUM(5)=PSUM(5)+P(I,5)
            MSTJ(93)=1
            PSUM(5)=PSUM(5)+ULMASS(K(I,2)) 
            INVMASS=PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-PSUM(3)**2
            ENERGY=SQRT(MAX(0.D0,INVMASS))-PSUM(5)
            FIRST=.TRUE.
C--       only system with a diquark are of interest
            IF (ENERGY.LT.MINENERGY .AND. 
     &         LUCOMP(K(IFIRST,2)).EQ.90 .OR. LUCOMP(K(I,2)).EQ.90) THEN
               MINENERGY=ENERGY
               MININVMASS=INVMASS
               I1=IFIRST
               I2=I
               PLIGHT(1)=PSUM(1)
               PLIGHT(2)=PSUM(2)
               PLIGHT(3)=PSUM(3)
               PLIGHT(4)=PSUM(4)
            ENDIF
         ENDIF
20    CONTINUE

C--       lightest singlet system is in row I1 to I2
      IF (MINENERGY.LT.DBLE(PARJ(32))) THEN

C--       add system as cluster
         N=N+1
         DO 30 I=1,4
            P(N,I)=PLIGHT(I)
30       CONTINUE
         P(N,5)=SQRT(MAX(0.D0,MININVMASS))
         K(N,1)=11
         K(N,2)=91
         K(N,3)=I1
         K(N,4)=N+1
         K(N,5)=N+2

C--       inactivate old system
         DO 40 I=I1,I2
            K(I,1)=K(I,1)+10
            K(I,4)=N
40       CONTINUE
         
C--       try to make two particles
         NTRY1=0
         NTRY2=0
50       CONTINUE
C--       take diquark end first
         IF (LUCOMP(K(I1,2)).EQ.90) THEN
            IDQ=I1
            IQ=I2
         ELSE
            IDQ=I2
            IQ=I1
         ENDIF
         KIDQ=K(IDQ,2)
         KIQ=K(IQ,2)
         KDUMMY=0
         KFB=0
         KFH1=0
         KFH2=0
         CALL LUKFDI(KIDQ,KDUMMY,KFB,KFH1)
         KDUMMY=0
         CALL LUKFDI(KIQ,-KFB,KDUMMY,KFH2)
         IF (KFH1.EQ.0 .OR. KFH2.EQ.0) THEN
            NTRY1=NTRY1+1
            IF (NTRY1.GE.100) THEN
               LST(21)=200
               RETURN
            ENDIF
            GOTO 50
         ENDIF
C--       consistency checks
         IF (LUCOMP(KFH1).EQ.0 .OR. LUCOMP(KFH2).EQ.0) THEN
            LST(21)=201
            RETURN
         ENDIF
         IF (KCHG(LUCOMP(KFH1),2)*ISIGN(1,KFH1)+
     +       KCHG(LUCOMP(KFH2),2)*ISIGN(1,KFH2) .NE.0) THEN
            LST(21)=202
            RETURN
         ENDIF
         P(N+1,5)=ULMASS(KFH1)
         P(N+2,5)=ULMASS(KFH2)
         IF (P(N,5).LE.P(N+1,5)+P(N+2,5)+PARJ(64) .AND.
     &       P(N,5).GE.ULMASS(2212)+ULMASS(111)+PARJ(64) .AND.
     &       NTRY2.LE.100) THEN
            NTRY2=NTRY2+1
            GOTO 50
         ENDIF
         IF (P(N,5).GE.P(N+1,5)+P(N+2,5)+PARJ(64)) THEN
C--       make two particles
C--       isotropic decay in cms (dcostheta*dphi)
            TOT2=MININVMASS
            M1=DBLE(P(N+1,5))
            M2=DBLE(P(N+2,5))            
            ROTARG=(TOT2-M1**2-M2**2)**2-4.D0*M1**2*M2**2
            IF (ROTARG.LT.0.D0) THEN
               LST(21)=203
               RETURN
            ENDIF
            PABS=0.5D0*SQRT(ROTARG/TOT2)
            COSTHE=-1.D0+2.D0*RLU(0)
            PTEMP=PABS*SQRT(1.D0-COSTHE**2)
            PHI=2.D0*PI*RLU(0)
            P(N+1,4)=SQRT(PABS**2+M1**2)
            P(N+1,3)=PABS*COSTHE
            P(N+1,2)=PTEMP*COS(PHI)
            P(N+1,1)=PTEMP*SIN(PHI)
            P(N+2,4)=SQRT(PABS**2+M2**2)
            P(N+2,3)=-P(N+1,3)
            P(N+2,2)=-P(N+1,2)
            P(N+2,1)=-P(N+1,1)
C--       K-vector
            K(N+1,1)=1
            K(N+1,2)=KFH1
            K(N+1,3)=N
            K(N+1,4)=0
            K(N+1,5)=0
            K(N+2,1)=1
            K(N+2,2)=KFH2
            K(N+2,3)=N
            K(N+2,4)=0
            K(N+2,5)=0
C--       boost to lab
            MSTU(33)=1
            CALL LUDBRB(N+1,N+2,0.,0.,PLIGHT(1)/PLIGHT(4),
     &               PLIGHT(2)/PLIGHT(4),PLIGHT(3)/PLIGHT(4)) 
C--       V-vector
            DO 60 I=1,5 
               V(N,I)=V(IDQ,I) 
               V(N+1,I)=V(IDQ,I) 
               V(N+2,I)=V(IQ,I) 
60          CONTINUE 
            V(N,5)=0. 
            V(N+1,5)=0. 
            V(N+2,5)=0.
            N=N+2 
         ELSE
C--       make one particle instead
70          CONTINUE
            KIDQ=K(IDQ,2)
            KIQ=K(IQ,2)
            KDUMMY=0
            KFH1=0
            CALL LUKFDI(KIDQ,KIQ,KDUMMY,KFH1)
            IF (KFH1.EQ.0) GOTO 70
C--       isospin conservation
            IF (KFH1.EQ.2214) KFH1=2212
            IF (KFH1.EQ.2114) KFH1=2112
            IF (KFH1.EQ.-2214) KFH1=-2212
            IF (KFH1.EQ.-2114) KFH1=-2112
            K(N+1,1)=1
            K(N+1,2)=KFH1
            K(N+1,3)=N
            K(N+1,4)=0
            K(N+1,5)=0
            P(N+1,5)=ULMASS(KFH1)
C--       find particle to shuffle energy & momentum to and from
            MAXENERGY=0.D0
            I1=0
            DO 80 I=1,N-1
               IF (0.LT.K(I,1) .AND. K(I,1).LT.10 .AND.
     &             LUCOMP(K(I,2)).NE.0 .AND.
     &             (K(I,2).EQ.21 .OR. ABS(K(I,2)).LT.10 .OR.
     &              ABS(K(I,2)).GT.100) ) THEN
                  INVMASS=(P(N,4)+P(I,4))**2-(P(N,1)+P(I,1))**2-
     -                    (P(N,2)+P(I,2))**2-(P(N,3)+P(I,3))**2
                  ENERGY=SQRT(MAX(0.D0,INVMASS))-P(N,5)-P(I,5)
                  IF (ENERGY.GT.MAXENERGY ) THEN
                     I1=I
                     MAXENERGY=ENERGY
                  ENDIF
               ENDIF
80          CONTINUE
C--       shuffle energy & momentum
            IF (I1.NE.0) THEN
              PCPS=DBLE(P(N,4))*DBLE(P(I1,4))-DBLE(P(N,1))*DBLE(P(I1,1))
     -            -DBLE(P(N,2))*DBLE(P(I1,2))-DBLE(P(N,3))*DBLE(P(I1,3))
              PC2=DBLE(P(N,5))**2
              PN2=DBLE(P(N+1,5))**2
              PS2=DBLE(P(I1,5))**2
              A=PC2+PS2+2.D0*PCPS
              B=PC2+PN2+2.D0*PCPS
              C=(PN2-PC2)*(4.D0*PCPS*(PCPS+PC2)-PC2*(PN2-PC2))/
     /          4.D0/(PCPS**2-PC2*PS2)
              IF (B**2-4.D0*C*A.LT.0.D0) THEN
                 LST(21)=204
                 RETURN
              ENDIF
              EPS2=(-B+SQRT(MAX(0.D0,B**2-4.D0*C*A)))/2.D0/A
              EPS1=(PN2-PC2+2.D0*EPS2*(PS2+PCPS))/2.D0/(PC2+PCPS)
              DO 90 I=1,4 
                 P(N+1,I)=(1.+EPS1)*P(N,I)-EPS2*P(I1,I) 
                 P(I1,I)=(1.+EPS2)*P(I1,I)-EPS1*P(N,I) 
                 V(N,I)=V(I1,I) 
                 V(N+1,I)=V(I1,I) 
90            CONTINUE 
              V(N,5)=0. 
              V(N+1,5)=0. 
              N=N+1
            ELSE
              LST(21)=205
              RETURN
            ENDIF
         ENDIF
      ENDIF
      
      RETURN
      
      END
