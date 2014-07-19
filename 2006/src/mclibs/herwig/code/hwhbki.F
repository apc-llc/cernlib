CDECK  ID>, HWHBKI.
*CMZ :-        -26/04/91  13.19.32  by  Federico Carminati
*-- Author :    Giovanni Abbiendi & Luca Stanco
C----------------------------------------------------------------------
      SUBROUTINE HWHBKI
C----------------------------------------------------------------------
C     gives the fourmomenta in the laboratory system for the particles
C     of the hard 2-->3 subprocess, to match with HERWIG routines of
C     jet evolution.
C----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWUECM,HWUPCM,HWUSQR,LEP,Y,Q2,SHAT,Z,PHI,AJACOB,
     & DSIGMA,ME,MP,ML,MREMIF(18),MFIN1(18),MFIN2(18),RS,SMA,W2,RSHAT,
     & PGAMMA(5),SG,MF1,MF2,EP,PP,EL,PL,E1,E2,Q1,COSBET,SINBET,COSTHE,
     & SINTHE,SINAZI,COSAZI,ROTAZI(3,3),EGAM,A,PPROT,MREMIN,PGAM,PEP(5),
     & COSPHI,SINPHI,ROT(3,3),EPROT,PROTON(5),MPART
      INTEGER IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,IPROO,I,IHAD,J,IS,ICMF
      LOGICAL CHARGD,INCLUD(18),INSIDE(18)
      EXTERNAL HWUECM,HWUPCM,HWUSQR
      COMMON /HWAREA/ LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,
     & IPROO,CHARGD,INCLUD,INSIDE
C
      IHAD=2
      IF (JDAHEP(1,IHAD).NE.0) IHAD=JDAHEP(1,IHAD)
C---Set masses
      IF (CHARGD) THEN
        MPART=ZERO
        MF1=RMASS(IDHW(NHEP+5))
        MF2=RMASS(IDHW(NHEP+6))
        MREMIN=MP
      ELSE
        IS = IFL
        IF (IFL.EQ.164) IS=IQK
        MPART=ZERO
        IF (IFL.GE.7.AND.IFL.LE.18) MPART=RMASS(IFL-6)
        MF1=MFIN1(IS)
        MF2=MFIN2(IS)
        MREMIN = MREMIF(IS)
      ENDIF
C---Calculation of kinematical variables for the generated event
C   in the center of mass frame of the incoming boson and parton
C   with parton along +z
      EGAM = HWUECM (SHAT, -Q2, MPART**2)
      PGAM = SQRT( EGAM**2 + Q2 )
      EP = RSHAT-EGAM
      PP = PGAM
      A = (W2+Q2-MP**2)/TWO
      PPROT = (A*PGAM-EGAM*SQRT(A**2+MP**2*Q2))/Q2
      IF (PPROT.LT.ZERO) CALL HWWARN('HWHBKI',101,*999)
      EPROT = SQRT(PPROT**2+MP**2)
      IF ((EPROT+PPROT).LT.(EP+PP)) CALL HWWARN('HWHBKI',102,*999)
      EL = ( PGAM / PPROT * SMA - Q2 ) / TWO
     +     / (EGAM + PGAM / PPROT * EPROT)
      IF (EL.GT.ME) THEN
        PL = SQRT ( EL**2 - ME**2 )
      ELSE
        CALL HWWARN ('HWHBKI',103,*999)
      ENDIF
      COSBET = (TWO * EPROT * EL - SMA) / (TWO * PPROT * PL)
      IF ( ABS(COSBET) .GE. ONE ) THEN
        COSBET = SIGN (ONE,COSBET)
        SINBET = ZERO
      ELSE
        SINBET = SQRT (ONE - COSBET**2)
      ENDIF
      SG = ME**2 + MPART**2 + Q2 + TWO * RSHAT * EL
      IF (SG.LE.(RSHAT+ML)**2 .OR. SG.GE.(RS-MREMIN)**2)
     +    CALL HWWARN ('HWHBKI',104,*999)
      Q1 = HWUPCM( RSHAT, MF1, MF2)
      E1 = SQRT(Q1**2+MF1**2)
      E2 = SQRT(Q1**2+MF2**2)
      IF (Q1 .GT. ZERO) THEN
        COSTHE=(TWO*EP*E1 - Z*(SHAT+Q2))/(TWO*PP*Q1)
        IF (ABS(COSTHE) .GT. ONE) THEN
          COSTHE=SIGN(ONE,COSTHE)
          SINTHE=ZERO
        ELSE
          SINTHE=SQRT(ONE-COSTHE**2)
        ENDIF
      ELSE
        COSTHE=ZERO
        SINTHE=ONE
      ENDIF
C---Initial lepton
      PHEP(1,NHEP+1)=PL*SINBET
      PHEP(2,NHEP+1)=ZERO
      PHEP(3,NHEP+1)=PL*COSBET
      PHEP(4,NHEP+1)=EL
      PHEP(5,NHEP+1)=RMASS(IDHW(1))
C---Initial Hadron
      PROTON(1)=ZERO
      PROTON(2)=ZERO
      PROTON(3)=PPROT
      PROTON(4)=EPROT
      CALL HWUMAS (PROTON)
C---Initial parton
      PHEP(1,NHEP+2)=ZERO
      PHEP(2,NHEP+2)=ZERO
      PHEP(3,NHEP+2)=PP
      PHEP(4,NHEP+2)=EP
      PHEP(5,NHEP+2)=MPART
C---HARD SUBPROCESS 2-->3 CENTRE OF MASS
      PHEP(1,NHEP+3)=PHEP(1,NHEP+1)+PHEP(1,NHEP+2)
      PHEP(2,NHEP+3)=PHEP(2,NHEP+1)+PHEP(2,NHEP+2)
      PHEP(3,NHEP+3)=PHEP(3,NHEP+1)+PHEP(3,NHEP+2)
      PHEP(4,NHEP+3)=PHEP(4,NHEP+1)+PHEP(4,NHEP+2)
      CALL HWUMAS  ( PHEP(1,NHEP+3) )
C---Virtual boson
      PGAMMA(1)=ZERO
      PGAMMA(2)=ZERO
      PGAMMA(3)=-PGAM
      PGAMMA(4)=EGAM
      PGAMMA(5)=HWUSQR(Q2)
C---Scattered lepton
      PHEP(1,NHEP+4)=PHEP(1,NHEP+1)-PGAMMA(1)
      PHEP(2,NHEP+4)=PHEP(2,NHEP+1)-PGAMMA(2)
      PHEP(3,NHEP+4)=PHEP(3,NHEP+1)-PGAMMA(3)
      PHEP(4,NHEP+4)=PHEP(4,NHEP+1)-PGAMMA(4)
      PHEP(5,NHEP+4)=RMASS(IDHW(1))
      IF (CHARGD) PHEP(5,NHEP+4)=ZERO
C---First Final parton:  quark (or J/psi) in Boson-Gluon Fusion
C---                     quark or antiquark in QCD Compton
      PHEP(1,NHEP+5)=Q1*SINTHE*COS(PHI)
      PHEP(2,NHEP+5)=Q1*SINTHE*SIN(PHI)
      PHEP(3,NHEP+5)=Q1*COSTHE
      PHEP(4,NHEP+5)=E1
      PHEP(5,NHEP+5)=MF1
C---Second Final parton: antiquark in Boson-Gluon Fusion
C---                     gluon in QCD Compton
      PHEP(1,NHEP+6)=-PHEP(1,NHEP+5)
      PHEP(2,NHEP+6)=-PHEP(2,NHEP+5)
      PHEP(3,NHEP+6)=-PHEP(3,NHEP+5)
      PHEP(4,NHEP+6)=E2
      PHEP(5,NHEP+6)=MF2
C---Boost to lepton-hadron CM frame
      PEP(1) = PHEP(1,NHEP+1)
      PEP(2) = PHEP(2,NHEP+1)
      PEP(3) = PHEP(3,NHEP+1) + PPROT
      PEP(4) = PHEP(4,NHEP+1) + EPROT
      CALL HWUMAS (PEP)
      DO I=1,6
        CALL HWULOF (PEP,PHEP(1,NHEP+I),PHEP(1,NHEP+I))
      ENDDO
      CALL HWULOF (PEP,PROTON,PROTON)
      CALL HWULOF (PEP,PGAMMA,PGAMMA)
C---Rotation around y-axis to align lepton beam with z-axis
      COSPHI = PHEP(3,NHEP+1) /
     &           SQRT( PHEP(1,NHEP+1)**2 + PHEP(3,NHEP+1)**2 )
      SINPHI = PHEP(1,NHEP+1) /
     &           SQRT( PHEP(1,NHEP+1)**2 + PHEP(3,NHEP+1)**2 )
      DO I=1,3
      DO J=1,3
        ROT(I,J)=ZERO
      ENDDO
      ENDDO
        ROT(1,1) = COSPHI
        ROT(1,3) = -SINPHI
        ROT(2,2) = ONE
        ROT(3,1) = SINPHI
        ROT(3,3) = COSPHI
      DO I=1,6
        CALL HWUROF (ROT,PHEP(1,NHEP+I),PHEP(1,NHEP+I))
      ENDDO
      CALL HWUROF (ROT,PROTON,PROTON)
      CALL HWUROF (ROT,PGAMMA,PGAMMA)
C---Boost to the LAB frame
      ICMF=3
      DO I=1,6
        CALL HWULOB (PHEP(1,ICMF),PHEP(1,NHEP+I),PHEP(1,NHEP+I))
      ENDDO
      CALL HWULOB (PHEP(1,ICMF),PROTON,PROTON)
      CALL HWULOB (PHEP(1,ICMF),PGAMMA,PGAMMA)
C---Random azimuthal rotation
      CALL HWRAZM (ONE,COSAZI,SINAZI)
      DO I=1,3
      DO J=1,3
        ROTAZI(I,J)=ZERO
      ENDDO
      ENDDO
        ROTAZI(1,1) = COSAZI
        ROTAZI(1,2) = SINAZI
        ROTAZI(2,1) = -SINAZI
        ROTAZI(2,2) = COSAZI
        ROTAZI(3,3) = ONE
      DO I=1,6
        CALL HWUROF (ROTAZI,PHEP(1,NHEP+I),PHEP(1,NHEP+I))
      ENDDO
      CALL HWUROF (ROTAZI,PROTON,PROTON)
      CALL HWUROF (ROTAZI,PGAMMA,PGAMMA)
  999 END
