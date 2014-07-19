CDECK  ID>, HWHBGF.
*CMZ :-        -26/04/91  11.11.55  by  Bryan Webber
*-- Author :    Giovanni Abbiendi & Luca Stanco
C-----------------------------------------------------------------------
      SUBROUTINE HWHBGF
C-----------------------------------------------------------------------
C     Order Alpha_s processes in charged lepton-hadron collisions
C
C       Process code IPROC has to be set in the Main Program
C       the following codes IPROC may be selected
C
C                9100 : NC  BOSON-GLUON FUSION
C                9100+IQK (IQK=1,...,6) :  produced flavour is IQK
C                9107 : produced  J/psi + gluon
C
C                9110 : NC  QCD COMPTON
C                9110+IQK (IQK=1,...,12) : struck parton is IQK
C
C                9130 : NC order alpha_s processes (9100+9110)
C
C       Select maximum and minimum generated flavour when IQK=0
C       setting IFLMIN and IFLMAX in the Main Program
C       (allowed values from 1 to 6), default are 1 and 5
C       allowing d,u,s,c,b,dbar,ubar,sbar,cbar,bbar
C
C           CHARGED CURRENT Boson-Gluon Fusion processes
C                9141 : CC  s cbar  (c sbar)
C                9142 : CC  b cbar  (c bbar)
C                9143 : CC  s tbar  (t cbar)
C                9144 : CC  b tbar  (t bbar)
C
C       other inputs : Q2MIN,Q2MAX,YBMIN,YBMAX,PTMIN,EMMIN,EMMAX
C       when IPROC=(1)9107 : as above but Q2WWMN, Q2WWMX substitute
C                            Q2MIN and Q2MAX (EPA is used); ZJMAX cut
C
C      Add 10000 to suppress soft remnant fragmentation
C
C      Mean EVWGT = cross section in nanoBarn
C
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,
     & ML,MREMIF(18),MFIN1(18),MFIN2(18),RS,SMA,W2,RSHAT,FSIGMA(18),
     & SIGSUM,PROB,PRAN,PVRT(4),X
      INTEGER IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,IPROO,LEPFIN,ID1,ID2,I,IDD
      LOGICAL CHARGD,INCLUD(18),INSIDE(18)
      EXTERNAL HWRGEN
      SAVE LEPFIN,ID1,ID2,FSIGMA,SIGSUM
      COMMON /HWAREA/ LEP,Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,
     & IPROO,CHARGD,INCLUD,INSIDE
C---Initialization
      IF (FSTWGT) THEN
C---LEP = 1 FOR LEPTONS, -1 FOR ANTILEPTONS
        LEP=ZERO
        IF (IDHW(1).GE.121.AND.IDHW(1).LE.126) THEN
          LEP=ONE
        ELSEIF (IDHW(1).GE.127.AND.IDHW(1).LE.132) THEN
          LEP=-ONE
        ENDIF
        IF (LEP.EQ.ZERO) CALL HWWARN('HWHBGF',500,*999)
        IPROO=MOD(IPROC,100)/10
        IF (IPROO.EQ.0.OR.IPROO.EQ.4) THEN
          IQK=MOD(IPROC,10)
          IFL=IQK
          IF (IQK.EQ.7) IFL=164
          CHARGD=IPROO.EQ.4
        ELSEIF (IPROO.EQ.1.OR.IPROO.EQ.2) THEN
          IQK=MOD(IPROC,100)-10
          IFL=IQK+6
          CHARGD=.FALSE.
        ELSEIF (IPROO.EQ.3) THEN
          IQK=0
          IFL=0
          CHARGD=.FALSE.
        ELSE
          CALL HWWARN('HWHBGF',501,*999)
        ENDIF
C
        LEPFIN = IDHW(1)
        IF(CHARGD) THEN
          LEPFIN = IDHW(1)+1
          IF (IQK.EQ.1) THEN
            IFLAVU=4
            IFLAVD=3
            ID1  = 3
            ID2  = 10
          ELSEIF (IQK.EQ.2) THEN
            IFLAVU=4
            IFLAVD=5
            ID1  = 5
            ID2  = 10
          ELSEIF (IQK.EQ.3) THEN
            IFLAVU=6
            IFLAVD=3
            ID1  = 3
            ID2  =12
          ELSE
            IFLAVU=6
            IFLAVD=5
            ID1  = 5
            ID2  =12
          ENDIF
          IF (LEP.EQ.-1.) THEN
            IDD=ID1
            ID1=ID2-6
            ID2=IDD+6
          ENDIF
        ENDIF
C
        IF (IQK.EQ.0) THEN
          DO I=1,18
            INCLUD(I)=.TRUE.
          ENDDO
          IMIN=1
          IMAX=18
          DO I=1,6
            IF (I.LT.IFLMIN.OR.I.GT.IFLMAX) INCLUD(I)=.FALSE.
          ENDDO
          DO I=7,18
            IF (I.LE.12) THEN
              IF (I-6.LT.IFLMIN.OR.I-6.GT.IFLMAX) INCLUD(I)=.FALSE.
            ELSE
              IF (I-12.LT.IFLMIN.OR.I-12.GT.IFLMAX) INCLUD(I)=.FALSE.
            ENDIF
          ENDDO
          IF (IPROO.EQ.0) THEN
            DO I=7,18
              INCLUD(I)=.FALSE.
            ENDDO
            IMIN=IFLMIN
            IMAX=IFLMAX
          ELSEIF (IPROO.EQ.1.OR.IPROO.EQ.2) THEN
            DO I=1,6
              INCLUD(I)=.FALSE.
            ENDDO
            IMIN=IFLMIN+6
            IMAX=IFLMAX+12
          ELSEIF (IPROO.EQ.3) THEN
            IMIN=IFLMIN
            IMAX=IFLMAX+12
          ENDIF
        ELSEIF (IQK.NE.0 .AND. (.NOT.CHARGD)) THEN
          DO I=1,18
            INCLUD(I)=.FALSE.
          ENDDO
          IF (IFL.LE.18) THEN
            INCLUD(IFL)=.TRUE.
            IMIN=IFL
            IMAX=IFL
          ELSEIF (IFL.EQ.164) THEN
            INCLUD(7)=.TRUE.
            IMIN=7
            IMAX=7
          ENDIF
        ENDIF
      ENDIF
C---End of initialization
      IF(GENEV) THEN
      IF (.NOT.CHARGD) THEN
        IF (IQK.EQ.0) THEN
          PRAN= SIGSUM * HWRGEN(0)
          PROB=ZERO
          DO 10 IFL=IMIN,IMAX
            IF (.NOT.INSIDE(IFL)) GOTO 10
            PROB=PROB+FSIGMA(IFL)
            IF (PROB.GE.PRAN) GOTO 20
  10      CONTINUE
        ENDIF
C---at this point the subprocess has been selected (IFL)
  20    CONTINUE
        IF (IFL.LE.6) THEN
C---Boson-Gluon Fusion event
          IDHW(NHEP+1)=IDHW(1)
          IDHW(NHEP+2)=13
          IDHW(NHEP+3)=15
          IDHW(NHEP+4)=LEPFIN
          IDHW(NHEP+5)=IFL
          IDHW(NHEP+6)=IFL+6
        ELSEIF (IFL.GE.7.AND.IFL.LE.18) THEN
C---QCD_Compton event
          IDHW(NHEP+1)=IDHW(1)
          IDHW(NHEP+2)=IFL-6
          IDHW(NHEP+3)=15
          IDHW(NHEP+4)=LEPFIN
          IDHW(NHEP+5)=IFL-6
          IDHW(NHEP+6)=13
        ELSEIF (IFL.EQ.164) THEN
C---gamma+gluon-->J/Psi+gluon
          IDHW(NHEP+1)=IDHW(1)
          IDHW(NHEP+2)=13
          IDHW(NHEP+3)=15
          IDHW(NHEP+4)=LEPFIN
          IDHW(NHEP+5)=164
          IDHW(NHEP+6)=13
        ELSE
          CALL HWWARN('HWHBGF',503,*999)
        ENDIF
      ELSE
C---Charged current event of specified flavours
        IDHW(NHEP+1)=IDHW(1)
        IDHW(NHEP+2)=13
        IDHW(NHEP+3)=15
        IDHW(NHEP+4)=LEPFIN
        IDHW(NHEP+5)=ID1
        IDHW(NHEP+6)=ID2
      ENDIF
C
      DO 1 I=NHEP+1,NHEP+6
    1 IDHEP(I)=IDPDG(IDHW(I))
C
C---Codes common for all processes
      ISTHEP(NHEP+1)=111
      ISTHEP(NHEP+2)=112
      ISTHEP(NHEP+3)=110
      ISTHEP(NHEP+4)=113
      ISTHEP(NHEP+5)=114
      ISTHEP(NHEP+6)=114
C
      DO I=NHEP+1,NHEP+6
        JMOHEP(1,I)=NHEP+3
        JDAHEP(1,I)=0
      ENDDO
C---Incoming lepton
      JMOHEP(2,NHEP+1)=NHEP+4
      JDAHEP(2,NHEP+1)=NHEP+4
C---Hard Process C.M.
      JMOHEP(1,NHEP+3)=NHEP+1
      JMOHEP(2,NHEP+3)=NHEP+2
      JDAHEP(1,NHEP+3)=NHEP+4
      JDAHEP(2,NHEP+3)=NHEP+6
C---Outgoing lepton
      JMOHEP(2,NHEP+4)=NHEP+1
      JDAHEP(2,NHEP+4)=NHEP+1
C
      IF (IFL.LE.6 .OR. CHARGD) THEN
C---Codes for boson-gluon fusion processes
C---  Incoming gluon
        JMOHEP(2,NHEP+2)=NHEP+6
        JDAHEP(2,NHEP+2)=NHEP+5
C---  Outgoing quark
        JMOHEP(2,NHEP+5)=NHEP+2
        JDAHEP(2,NHEP+5)=NHEP+6
C---  Outgoing antiquark
        JMOHEP(2,NHEP+6)=NHEP+5
        JDAHEP(2,NHEP+6)=NHEP+2
      ELSEIF (IFL.GE.7 .AND. IFL.LE.12) THEN
C---Codes for V+q --> q+g
C---  Incoming quark
        JMOHEP(2,NHEP+2)=NHEP+5
        JDAHEP(2,NHEP+2)=NHEP+6
C---  Outgoing quark
        JMOHEP(2,NHEP+5)=NHEP+6
        JDAHEP(2,NHEP+5)=NHEP+2
C---  Outgoing gluon
        JMOHEP(2,NHEP+6)=NHEP+2
        JDAHEP(2,NHEP+6)=NHEP+5
      ELSEIF (IFL.GE.13 .AND. IFL.LE.18) THEN
C---Codes for V+qbar --> qbar+g
C---  Incoming antiquark
        JMOHEP(2,NHEP+2)=NHEP+6
        JDAHEP(2,NHEP+2)=NHEP+5
C---  Outgoing antiquark
        JMOHEP(2,NHEP+5)=NHEP+2
        JDAHEP(2,NHEP+5)=NHEP+6
C---  Outgoing gluon
        JMOHEP(2,NHEP+6)=NHEP+5
        JDAHEP(2,NHEP+6)=NHEP+2
      ELSEIF (IFL.EQ.164) THEN
C---Codes for Gamma+gluon --> J/Psi+gluon
C---  Incoming gluon
        JMOHEP(2,NHEP+2)=NHEP+6
        JDAHEP(2,NHEP+2)=NHEP+6
C---  Outgoing J/Psi
        JMOHEP(2,NHEP+5)=NHEP+1
        JDAHEP(2,NHEP+5)=NHEP+1
C---  Outgoing gluon
        JMOHEP(2,NHEP+6)=NHEP+2
        JDAHEP(2,NHEP+6)=NHEP+2
      ENDIF
C---Computation of momenta in Laboratory frame of reference
      CALL HWHBKI
      NHEP=NHEP+6
C Decide which quark radiated and assign production vertices
      IF (IFL.LE.6) THEN
C Boson-Gluon fusion case
        IF (1-Z.LT.HWRGEN(0)) THEN
C Gluon splitting to quark
          CALL HWVZRO(4,VHEP(1,NHEP-1))
          CALL HWVDIF(4,PHEP(1,NHEP-4),PHEP(1,NHEP),PVRT)
          CALL HWUDKL(IFL,PVRT,VHEP(1,NHEP))
          CALL HWVEQU(4,VHEP(1,NHEP),VHEP(1,NHEP-4))
        ELSE
C Gluon splitting to antiquark
          CALL HWVZRO(4,VHEP(1,NHEP))
          CALL HWVDIF(4,PHEP(1,NHEP-4),PHEP(1,NHEP-1),PVRT)
          CALL HWUDKL(IFL,PVRT,VHEP(1,NHEP-1))
          CALL HWVEQU(4,VHEP(1,NHEP-1),VHEP(1,NHEP-4))
        ENDIF
      ELSEIF (IFL.GE.7.AND.IFL.LE.18) THEN
C QCD Compton case
        X=1/(1+SHAT/Q2)
        IF (1.LT.HWRGEN(0)*(1+(1-X-Z)**2+6*X*(1-X)*Z*(1-Z))) THEN
C Incoming quark radiated the gluon
          CALL HWVZRO(4,VHEP(1,NHEP-1))
          CALL HWVDIF(4,PHEP(1,NHEP-4),PHEP(1,NHEP),PVRT)
          CALL HWUDKL(IFL-6,PVRT,VHEP(1,NHEP))
          CALL HWVEQU(4,VHEP(1,NHEP),VHEP(1,NHEP-4))
        ELSE
C Outgoing quark radiated the gluon
          CALL HWVZRO(4,VHEP(1,NHEP-4))
          CALL HWVSUM(4,PHEP(1,NHEP-1),PHEP(1,NHEP),PVRT)
          CALL HWUDKL(IFL-6,PVRT,VHEP(1,NHEP))
          CALL HWVEQU(4,VHEP(1,NHEP),VHEP(1,NHEP-1))
        ENDIF
      ENDIF
C---HERWIG gets confused if lepton momentum is different from beam
C   momentum, which it can be if incoming hadron has negative virtuality
C   As a temporary fix, simply copy the momentum.
C   Momentum conservation somehow gets taken care of HWBGEN!
      call hwvequ(5,phep(1,1),phep(1,nhep-5))
      ELSE
        EVWGT=ZERO
C---generation of the 5 variables Y,Q2,SHAT,Z,PHI and Jacobian computation
C---in the largest phase space avalaible for selected processes and
C---filling of logical vector INSIDE to tag contributing ones
        CALL HWHBRN (*999)
C---calculate differential cross section corresponding to the chosen
C---variables and the weight for MC generation
        IF (IQK.EQ.0) THEN
C---many subprocesses included
          DO I=1,18
            FSIGMA(I)=ZERO
          ENDDO
          SIGSUM=ZERO
          DO I=IMIN,IMAX
            IF (INSIDE(I)) THEN
              IFL=I
              DSIGMA=ZERO
              CALL HWHBSG
              FSIGMA(I)=DSIGMA
              SIGSUM=SIGSUM+DSIGMA
            ENDIF
          ENDDO
          EVWGT=SIGSUM * AJACOB
        ELSE
C---only one subprocess included
          CALL HWHBSG
          EVWGT= DSIGMA * AJACOB
        ENDIF
        IF (EVWGT.LT.ZERO) EVWGT=ZERO
      ENDIF
  999 END
