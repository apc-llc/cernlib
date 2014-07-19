C ********************************************************************

      SUBROUTINE LYSSPA(IPU1,IPU2)
C--                                                                   --C
C--   Created:        -                                               --C
C--   Last update:    960801                                          --C

C...NEW X REDEFINITION
C...GENERATES SPACELIKE PARTON SHOWERS
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),XLP,YLP,W2LP,Q2LP,ULP
      COMMON/LUJETS/N,K(4000,5),P(4000,5),V(4000,5)
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON /LYPARA/ IPY(80),PYPAR(80),PYVAR(80)
      COMMON /LYPROC/ ISUB,KFL(3,2),X(2),SH,TH,UH,Q2,XSEC(0:40)
      COMMON /LYINT1/ XQ(2,-6:6),DSIG(-6:6,-6:6,5),FSIG(10,10,3)
      DIMENSION IFLS(4),IS(2),XS(2),ZS(2),Q2S(2),TEVS(2),ROBO(5),
     &XFS(2,-6:6),XFA(-6:6),XFB(-6:6),WTAP(-6:6),WTSF(-6:6)
      DOUBLE PRECISION DQ2(3),DSH,DSHZ,DSHR,DPLCM,DPC(3),DPD(4),DMS,
     &DMSMA,DPT2,DPB(4),DBE1(4),DBE2(4),DBEP,DGABEP,DPQ(4),DPQS(2),
     &DM2,DQ2B,DROBO(5),DBEZ,DTEMP
C-GI &DQ23,DPH(4),DM2,DQ2B,DQM2
CJR--begin
      LOGICAL SLAST
      LOGICAL SEAQUARK,SPLIT
      REAL XFT(-6:6)
      REAL XQUARK,XGLUON,XSEA,ZSPLIT,ZSOFT,ZMAX
CTEST      COMMON /SEAQTE/ XQUARK,XGLUON,XSEA,ZSPLIT,ZSOFT,ZMAX,SPLIT
      INTEGER LASTFL,SEAFL
CTEST      COMMON /FLAVOR/ LASTFL,SEAFL
      REAL LEXSEA
CJR--end
      DATA IFLA,NQ/0,0/,Z,XE0,XA/3*0./,DSHZ,DMSMA,DPT2,DSHR/4*0.D0/

C...COMMON CONSTANTS, SET UP INITIAL VALUES
      ILEP=0
      IF(IPU1.EQ.0) ILEP=1
      IF(IPU2.EQ.0) ILEP=2
      Q2E=Q2
C-GI  IF(ISET(ISUB).EQ.2.OR.ISET(ISUB).EQ.3) Q2E=Q2E/PYPAR(26)
      IF(ISUB.EQ.27) Q2E=PMAS(23,1)**2
      IF(ISUB.EQ.28) Q2E=PMAS(24,1)**2
      TMAX=ALOG(PYPAR(26)*PYPAR(27)*Q2E/PYPAR(21)**2)
      IF(ILEP.GE.1) THEN
        SH=P(25,5)**2
        IF(N.GE.27) SH=P(27,5)**2
        CALL LSCALE(-1,QMAX)
        Q2E=QMAX**2
        Q2E=MAX(PYPAR(21)**2,MIN(Q2E,(0.95/X(3-ILEP)-1.)*Q2-SH,
     &  Q2/2.+SH))
        TMAX=ALOG(Q2E/PYPAR(21)**2)
      ENDIF
CJR--begin
CJR-- varying cut-off in virtuality of incoming seaquarks
      IF (LST(35).EQ.2) THEN
         CALL LPRIKT(PARL(10),PT,PHI)
         PYPAR22=MAX(PYPAR(21)**2,PT**2)
      ELSE
         PYPAR22=PYPAR(22)
      ENDIF
CJR--end
      IF (MOD(LST(8),10).EQ.4 .OR. MOD(LST(8),10).EQ.5) THEN
         Q2E=PYPAR(22)
         TMAX=ALOG(Q2E/PYPAR(21)**2)
      ELSEIF(PYPAR(26)*Q2E.LT.MAX(PYPAR(22),2.*PYPAR(21)**2).OR.
     &TMAX.LT.0.2) THEN
         RETURN
      ENDIF
CJR--end
      IF(ILEP.EQ.0) XE0=2.*PYPAR(23)/PYVAR(1)
      B0=(33.-2.*IPY(8))/6.
      NS=N
      MSTU(2)=0
CJR--begin
      NTRY=0
  100 N=NS
      NTRY=NTRY+1
      IF (NTRY.GT.100) THEN
         LST(21)=17
         RETURN
      ENDIF
CJR--end
CJR  100 N=NS
CJR--begin
      SEAQUARK=.FALSE.
      SPLIT=.FALSE.
      SLAST=.FALSE.
CJR--end
      IF(ILEP.GE.1) THEN
        NQ=IPU2-2
        IF(ILEP.EQ.2) NQ=IPU1+2
        DPQS(1)=DBLE(P(NQ,3))
        DPQS(2)=DBLE(P(NQ,4))
        XBMIN=X(3-ILEP)*MAX(0.5,SH/Q2)
        CALL LYSTFU(IPY(43-ILEP),XBMIN,Q2,XFB)
        DO 110 IFL=-6,6
  110   XQ(3-ILEP,IFL)=XFB(IFL)
      ENDIF
      DO 120 JT=1,2
      IFLS(JT)=KFL(2,JT)
      IF(KFL(2,JT).EQ.21) IFLS(JT)=0
      IFLS(JT+2)=IFLS(JT)
      XS(JT)=X(JT)
      ZS(JT)=1.
      IF(ILEP.EQ.0) Q2S(JT)=PYPAR(26)*Q2E
      TEVS(JT)=TMAX
      DO 120 IFL=-6,6
  120 XFS(JT,IFL)=XQ(JT,IFL)
      IF(ILEP.GE.1) THEN
        Q2S(ILEP)=P(NQ,5)**2
        DQ2(ILEP)=Q2S(ILEP)
        Q2S(3-ILEP)=Q2E
      ENDIF
      DSH=SH
      IHFC=0
      IHFX=0

C...PICK UP LEG WITH HIGHEST VIRTUALITY
CJR
      NTURN=0
CJR
  130 CONTINUE
CJR
      NTURN=NTURN+1
CJR
      IF(N.GT.MSTU(4)-10) THEN
        WRITE(6,*) ' LYSSPA: no more memory in LUJETS'
        LST(21)=18
        RETURN
      ENDIF
      DO 133 I=N+1,N+8
      DO 133 J=1,5
      K(I,J)=0
  133 P(I,J)=0.
C     CALL GULIST(21,2)
      N=N+2
      JT=1
      IF((N.GT.NS+2.AND.Q2S(2).GT.Q2S(1).AND.ILEP.EQ.0).OR.ILEP.EQ.1)
     &JT=2
      JR=3-JT
      IFLB=IFLS(JT)
      XB=XS(JT)
      IF(ILEP.GE.1.AND.N.EQ.NS+2) XB=XS(JT)*MAX(SH/Q2,0.5)
      DO 140 IFL=-6,6
  140 XFB(IFL)=XFS(JT,IFL)
      Q2B=Q2S(JT)
      TEVB=TEVS(JT)
      IF(IPY(14).GE.9.AND.N.GT.NS+4) THEN
        Q2B=0.5*(1./ZS(JT)+1.)*Q2S(JT)+0.5*(1./ZS(JT)-1.)*(Q2S(3-JT)-
     &  SNGL(DSH)+SQRT((SNGL(DSH)+Q2S(1)+Q2S(2))**2+8.*Q2S(1)*Q2S(2)*
     &  ZS(JT)/(1.-ZS(JT))))
        TEVB=ALOG(PYPAR(27)*Q2B/PYPAR(21)**2)
      ENDIF
      IF(ILEP.EQ.0) THEN
        DSHR=2.*DSQRT(DSH)
        DSHZ=DSH/DBLE(ZS(JT))
      ELSEIF(ILEP.GE.1) THEN
        DSHZ=DSH
        IF(N.GT.NS+4) DSHZ=(DSH+DQ2(JR)-DQ2(JT))/ZS(JT)-DQ2(JR)+
     &  PYPAR22
        DPD(2)=DSHZ+DQ2(JR)+DBLE(PYPAR22)

        MSTJ(93)=1
        QMASS=ULMASS(IABS(IFLB))
        IF(IABS(IFLB).EQ.0) QMASS=ULMASS(21)
C...CHECK IF QUARK PAIR CREATION ONLY POSSIBILITY
        IF(DQ2(JR).LT.4.*QMASS**2) THEN
          DM2=QMASS**2
          DPC(1)=DQ2(JR)*(DBLE(PYPAR22)+DM2)**2
          DPC(2)=DPD(2)*(DPD(2)-2D0*PYPAR22)*(PYPAR22+DM2)
          DPC(3)=PYPAR22*(DPD(2)-2D0*PYPAR22)**2
          XE0=1D0-(DPC(2)-DSQRT(DPC(2)**2-4D0*DPC(1)*DPC(3)))/
     &    (2D0*DPC(1))
CJR--begin
          ZMAX=(DPC(2)-DSQRT(DPC(2)**2-4D0*DPC(1)*DPC(3)))/(2D0*DPC(1))
          XE0=XB*(1./ZMAX-1.)
CJR--end
        ELSE
          XE0=1D0-(DPD(2)-2D0*DBLE(PYPAR22))*(DPD(2)-DSQRT(DPD(2)**2-
     &    4D0*DQ2(JR)*DBLE(PYPAR22)))/(2D0*DQ2(JR)*DBLE(PYPAR22))
CJR--begin
          ZMAX=(DPD(2)-2D0*DBLE(PYPAR22))*(DPD(2)-DSQRT(DPD(2)**2-
     &    4D0*DQ2(JR)*DBLE(PYPAR22)))/(2D0*DQ2(JR)*DBLE(PYPAR22))
          XE0=XB*(1./ZMAX-1.)
CJR--end
        ENDIF
CJR--begin
CJR--          radiated parton energy cut
C        XE0=MAX(XE0,2.*PYPAR(23)/SQRT(W2LP))
CJR--end
      ENDIF
CJR  145 XE=MAX(XE0,XB*(1./(1.-PYPAR(24))-1.))
CJR--begin
      N145=0
145   CONTINUE
      N145=N145+1
      IF (N145.GT.100) THEN
CJR         WRITE(*,*) '145'
         GOTO 100
      ENDIF
      XE=MAX(XE0,XB*(1./(1.-PYPAR(24))-1.))
        ZMAX=XB/(XB+XE)
CJR--end
      IF(XB+XE.GE.0.999) THEN
        Q2B=0.
        GOTO 210
      ENDIF

C...CALCULATE ALTARELLI-PARISI AND STRUCTURE FUNCTION WEIGHTS
      DO 150 IFL=-6,6
      WTAP(IFL)=0.
  150 WTSF(IFL)=0.
      IF(IFLB.EQ.0) THEN
        WTAPQ=16.*(1.-SQRT(XB+XE))/(3.*SQRT(XB))
        DO 160 IFL=-IPY(8),IPY(8)
        IF(IFL.EQ.0) WTAP(IFL)=6.*ALOG((1.-XB)/XE)
  160   IF(IFL.NE.0) WTAP(IFL)=WTAPQ
      ELSE
        WTAP(0)=0.5*XB*(1./(XB+XE)-1.)
        WTAP(IFLB)=8.*ALOG((1.-XB)*(XB+XE)/XE)/3.
      ENDIF
  170 WTSUM=0.
      IF(IHFC.EQ.0) THEN
        DO 180 IFL=-IPY(8),IPY(8)
        WTSF(IFL)=XFB(IFL)/MAX(1E-10,XFB(IFLB))
  180   WTSUM=WTSUM+WTAP(IFL)*WTSF(IFL)
        IF(IABS(IFLB).GE.4.AND.WTSUM.GT.1E3) THEN
          IHFX=1
          DO 185 IFL=-IPY(8),IPY(8)
  185     WTSF(IFL)=WTSF(IFL)*1E3/WTSUM
          WTSUM=1E3
        ENDIF
      ENDIF

C...CHOOSE NEW T AND FLAVOUR
CJR  190 IF(IPY(14).LE.6.OR.IPY(14).GE.9) THEN
CJR--begin
      NJR=0
190   CONTINUE
      SEAQUARK=.FALSE.
      NJR=NJR+1
      IF (NJR.GT.100) THEN
CJR         WRITE(*,*) '190'
         GOTO 100
      ENDIF
CJR--end
      IF(IPY(14).LE.6.OR.IPY(14).GE.9) THEN
        TEVXP=B0/MAX(0.0001,WTSUM)
      ELSE
        TEVXP=B0/MAX(0.0001,5.*WTSUM)
      ENDIF
      TEVB=TEVB*EXP(MAX(-100.,ALOG(RLU(0))*TEVXP))
      Q2REF=PYPAR(21)**2*EXP(TEVB)/PYPAR(27)
      Q2B=Q2REF/PYPAR(27)
CJR--begin -- 
      Q2BOLD=Q2B
CAE--seaquarks up to LST(12), if the quark density != 0
      IF( LST(35).EQ.1 .OR. LST(35).EQ.2) THEN
         IF( Q2B.LT.PYPAR(22) .AND.
     &       (ABS(IFLB).LE.LST(12).AND.ABS(IFLB).GE.1)) THEN
            Q2REF=MIN(PYPAR(22),Q2S(JT))
            Q2B=MIN(PYPAR22,Q2S(JT))
            IF(ILEP.GE.1.AND.N.EQ.NS+2) THEN
               XT=X(JT)*(1.+(DSH-Q2B)/DQ2(JR))
            ELSE
               XT=XB
            ENDIF
            CALL LYSTFU(IPY(40+JT),XT,Q2REF,XFT)
            IF(XFT(IFLB).EQ.0.0.AND.XFT(-IFLB).EQ.0.0) THEN
              SEARATIO=1.0
            ELSEIF(XFT(ABS(IFLB)).EQ.0.0) THEN
              SEARATIO=1.0
            ELSE
              SEARATIO=XFT(-ABS(IFLB))/XFT(ABS(IFLB))
            ENDIF
CJR-- (protons only)
            IF (RLU(0).LT.SEARATIO) THEN
               SEAQUARK=.TRUE.
               XQUARK=XT
            ELSE
               Q2B=Q2BOLD
               SEAQUARK=.FALSE.
            ENDIF
         ENDIF
      ENDIF
CJR--end
      DQ2B=Q2B
      IF(ILEP.GE.1) THEN
        DSHZ=DSH
        IF(N.GT.NS+4) DSHZ=(DSH+DQ2(JR)-DQ2(JT))/DBLE(ZS(JT))-DQ2(JR)+
     &  DQ2B
      ENDIF
CJR      IF(Q2B.LT.PYPAR(22)) THEN
      IF(Q2B.LT.PYPAR(22).AND.(.NOT.SEAQUARK)) THEN
         Q2B=0.
      ELSE
        WTRAN=RLU(0)*WTSUM
        IFLA=-IPY(8)-1
  200   IFLA=IFLA+1
        WTRAN=WTRAN-WTAP(IFLA)*WTSF(IFLA)
        IF(IFLA.LT.IPY(8).AND.WTRAN.GT.0.) GOTO 200

CJR--begin
        IF (SEAQUARK) THEN
           SEAFL=-IFLB
           IFLA=0
CT           XE=XB*(1./(1.-0.001)-1.)
        ENDIF
CJR--end

C...CHOOSE Z VALUE AND CORRECTIVE WEIGHT
        IF(IFLB.EQ.0.AND.IFLA.EQ.0) THEN
          Z=1./(1.+((1.-XB)/XB)*(XE/(1.-XB))**RLU(0))
          WTZ=(1.-Z*(1.-Z))**2
        ELSEIF(IFLB.EQ.0) THEN
          Z=XB/(1.-RLU(0)*(1.-SQRT(XB+XE)))**2
          WTZ=0.5*(1.+(1.-Z)**2)*SQRT(Z)
        ELSEIF(IFLA.EQ.0) THEN
          Z=XB*(1.+RLU(0)*(1./(XB+XE)-1.))
          WTZ=1.-2.*Z*(1.-Z)
        ELSE
          Z=1.-(1.-XB)*(XE/((XB+XE)*(1.-XB)))**RLU(0)
          WTZ=0.5*(1.+Z**2)
        ENDIF
CJR--begin
C        IF (SEAQUARK) THEN
C           XSEA=LEXSEA(0.15*XT,Q2B)
C           XE=MIN(XE,XSEA)
C           Z=XT/(XSEA+XT)
C        ENDIF
CJR--end
C...REWEIGHT FIRST LEG BECAUSE OF MODIFIED XB OR CHECK PHASE SPACE
        IF(ILEP.GE.1.AND.N.EQ.NS+2) THEN
          XBNEW=X(JT)*(1.+(DSH-Q2B)/DQ2(JR))
          IF(XBNEW.GT.MIN(Z,0.999)) GOTO 190
          XB=XBNEW
        ENDIF
C...SUM UP SOFT GLUON EMISSION AS EFFECTIVE Z SHIFT
CJR--       should this realy always be done ??
        IF(IPY(15).GE.1) THEN
          RSOFT=6.
          IF(IFLB.NE.0) RSOFT=8./3.
          Z=Z*(TEVB/TEVS(JT))**(RSOFT*XE/((XB+XE)*B0))
          IF(Z.LE.XB) GOTO 190
CJR--begin
          ZSOFT=(TEVB/TEVS(JT))**(RSOFT*XE/((XB+XE)*B0))
          ZMAX=XB/(XB+XE)
CJR--end
        ENDIF
C...CHECK IF HEAVY FLAVOUR BELOW THRESHOLD
        IHFT=0
CIC...Skip for intrinsic charm/bottom simulation, charm quark should
CIC...not come from gluon but is non-perturbative part of proton.
        IF(LST(15).EQ.-4.OR.LST(15).EQ.-5) GOTO 205
        MSTJ(93)=1
        IF(ILEP.GE.1.AND.IABS(IFLB).GE.4.AND.(XFB(IFLB).LT.1E-10.OR.
     &    Q2B.LT.5.*ULMASS(IABS(IFLB))**2)) THEN
          IHFT=1
          IFLA=0
        ENDIF
  205   CONTINUE

C...FOR LEPTOPRODUCTION, CHECK Z AGAINST NEW LIMIT
        IF(ILEP.GE.1) THEN
          DPD(2)=DSHZ+DQ2(JR)+DQ2B
          MSTJ(93)=1
          DM2=ULMASS(IABS(IFLA-IFLB))**2
          IF(IABS(IFLA-IFLB).EQ.0) DM2=ULMASS(21)**2
          DPC(1)=DQ2(JR)*(DQ2B+DM2)**2
          DPC(2)=DPD(2)*(DPD(2)-2D0*DQ2B)*(DQ2B+DM2)
          DPC(3)=DQ2B*(DPD(2)-2D0*DQ2B)**2
          ZU=(DPC(2)-DSQRT(DPC(2)**2-4D0*DPC(1)*DPC(3)))/(2D0*DPC(1))
          IF(Z.GE.ZU) GOTO 190
        ENDIF

C...OPTION WITH EVOLUTION IN KT2=(1-Z)Q2:
        IF(IPY(14).GE.5.AND.IPY(14).LE.6.AND.N.LE.NS+4) THEN
C...CHECK THAT (Q2)LAST BRANCHING < (Q2)HARD
          IF(Q2B/(1.-Z).GT.PYPAR(26)*Q2) GOTO 190
        ELSEIF(IPY(14).GE.3.AND.IPY(14).LE.6.AND.N.GE.NS+6) THEN
C...CHECK THAT Z,Q2 COMBINATION IS KINEMATICALLY ALLOWED
          Q2MAX=0.5*(1./ZS(JT)+1.)*DQ2(JT)+0.5*(1./ZS(JT)-1.)*
     &    (DQ2(3-JT)-DSH+SQRT((DSH+DQ2(1)+DQ2(2))**2+8.*DQ2(1)*DQ2(2)*
     &    ZS(JT)/(1.-ZS(JT))))
          IF(Q2B/(1.-Z).GE.Q2MAX) GOTO 190

        ELSEIF(IPY(14).EQ.7.OR.IPY(14).EQ.8) THEN
C...OPTION WITH ALPHAS((1-Z)Q2): DEMAND KT2 > CUTOFF, REWEIGHT
          IF((1.-Z)*Q2B.LT.PYPAR22) GOTO 190
          ALPRAT=TEVB/(TEVB+ALOG(1.-Z))
          IF(ALPRAT.LT.5.*RLU(0)) GOTO 190
          IF(ALPRAT.GT.5.) WTZ=WTZ*ALPRAT/5.
        ENDIF

C...WEIGHTING WITH NEW STRUCTURE FUNCTIONS
        CALL LYSTFU(IPY(40+JT),XB,Q2REF,XFB)
        XA=XB/Z
        CALL LYSTFU(IPY(40+JT),XA,Q2REF,XFA)
        IF(IHFT.EQ.1.OR.IHFX.EQ.1) THEN
           IF(XFA(IFLA).LT.1E-10) IHFC=1
           GOTO 210
        ELSEIF(XFB(IFLB).LT.1E-20) THEN
          GOTO 100
        ENDIF
        IF(WTZ*XFA(IFLA)/XFB(IFLB).LT.RLU(0)*WTSF(IFLA)) THEN
          IF(ILEP.GE.1.AND.N.EQ.NS+2) GOTO 145
          GOTO 170
        ENDIF
CJR--begin
        IF (SEAQUARK) THEN
           SPLIT=.TRUE.
           XGLUON=XA
           XSEA=XA-XB
           ZSPLIT=Z
           SEAQUARK=.FALSE.
        ENDIF
CJR--end
      ENDIF

210   CONTINUE
      IF(N.EQ.NS+4-2*MIN(1,ILEP)) THEN
C...DEFINE TWO HARD SCATTERERS IN THEIR CM-FRAME
        DQ2(JT)=Q2B
        IF(IPY(14).GE.3.AND.IPY(14).LE.6) DQ2(JT)=Q2B/(1.-Z)
        IF(ILEP.EQ.0) THEN
          DPLCM=DSQRT((DSH+DQ2(1)+DQ2(2))**2-4.*DQ2(1)*DQ2(2))/DSHR
          DO 220 JR=1,2
          I=NS+2*JR-1
          IPO=19+2*JR
          K(I,1)=14
          K(I,2)=IFLS(JR+2)
          IF(IFLS(JR+2).EQ.0) K(I,2)=21
          K(I,3)=0
          K(I,4)=IPO
          K(I,5)=IPO
          P(I,1)=0.
          P(I,2)=0.
          P(I,3)=DPLCM*(-1)**(JR+1)
          P(I,4)=(DSH+DQ2(3-JR)-DQ2(JR))/DSHR
          P(I,5)=-SQRT(SNGL(DQ2(JR)))
          K(I+1,1)=-1
          K(I+1,2)=K(IPO+1,2)
          K(I+1,3)=I
          K(I+1,4)=0
          K(I+1,5)=0
          P(I+1,1)=0.
          P(I+1,2)=0.
          P(I+1,3)=IPO
          P(I+1,4)=IPO
          P(I+1,5)=0.
          P(IPO+1,1)=I
          P(IPO+1,2)=I
          K(IPO,4)=MOD(K(IPO,4),MSTU(5))+I*MSTU(5)
          K(IPO,5)=MOD(K(IPO,5),MSTU(5))+I*MSTU(5)
  220     CONTINUE
        ELSE
C..LEPTOPRODUCTION EVENTS: BOSON AND HADRON REST FRAME
          I1=NS+2*ILEP-1
          I2=NS-2*ILEP+5
          DO 225 ITEMP=NS+1,NS+4
          DO 225 J=1,5
          K(ITEMP,J)=0
  225     P(ITEMP,J)=0.
          DO 230 J=1,5
  230     P(I1,J)=P(NQ,J)
          K(NS+1,1)=11
          K(NS+3,1)=14
          IF(ILEP.EQ.2) THEN
            K(NS+1,1)=14
            K(NS+3,1)=11
          ENDIF
          K(NS+2,1)=-1
          K(NS+4,1)=-1
          K(NS+1,3)=0
          K(NS+2,3)=NS+1
          K(NS+3,3)=0
          K(NS+4,3)=NS+3
          K(I1,2)=KFL(2,ILEP)
          K(I2,2)=KFL(2,3-ILEP)
          DPD(1)=DSH+DQ2(1)+DQ2(2)
          DPD(3)=(3-2*ILEP)*DSQRT(DPD(1)**2-4D0*DQ2(1)*DQ2(2))
          P(I2,3)=(DPQS(2)*DPD(3)-DPQS(1)*DPD(1))/
     &    (2D0*DQ2(JR))
          P(I2,4)=(DPQS(1)*DPD(3)-DPQS(2)*DPD(1))/
     &    (2D0*DQ2(JR))
          P(I2,5)=-SQRT(SNGL(DQ2(3-ILEP)))
          P(I2+1,3)=MAX(IPU1,IPU2)
          P(I2+1,4)=MAX(IPU1,IPU2)
          K(I2,4)=K(I2,4)-MOD(K(I2,4),MSTU(5))+MAX(IPU1,IPU2)
          K(I2,5)=K(I2,5)-MOD(K(I2,5),MSTU(5))+MAX(IPU1,IPU2)
          P(26-2*ILEP,1)=I2
          P(26-2*ILEP,2)=I2
          K(25-2*ILEP,4)=MOD(K(25-2*ILEP,4),MSTU(5))+I2*MSTU(5)
          K(25-2*ILEP,5)=MOD(K(25-2*ILEP,5),MSTU(5))+I2*MSTU(5)
          N=N+2
        ENDIF

      ELSEIF(N.GT.NS+4) THEN
C...FIND MAXIMUM ALLOWED MASS OF TIMELIKE PARTON
        DQ2(3)=Q2B
        IF(IPY(14).GE.3.AND.IPY(14).LE.6) DQ2(3)=Q2B/(1.-Z)
        IF(IS(1).GE.1.AND.IS(1).LE.MSTU(4)) THEN
          DPC(1)=P(IS(1),4)
          DPC(3)=0.5*(ABS(P(IS(1),3))+ABS(P(IS(2),3)))
        ELSE
C...IS(1) not initialized
          DPC(1)=0.
          DPC(3)=0.5*(       0.      +ABS(P(IS(2),3)))
        ENDIF
        DPC(2)=P(IS(2),4)
        DPD(1)=DSH+DQ2(JR)+DQ2(JT)
        DPD(2)=DSHZ+DQ2(JR)+DQ2(3)
        DPD(3)=DSQRT(DPD(1)**2-4.*DQ2(JR)*DQ2(JT))
        DPD(4)=DSQRT(DPD(2)**2-4.*DQ2(JR)*DQ2(3))
        IKIN=0
        IF((Q2S(JR).GE.0.5*PYPAR22.AND.DPD(1)-DPD(3).GE.1D-10*DPD(1))
     &  .OR.ILEP.GE.1) IKIN=1
        IF(IKIN.EQ.0) DMSMA=(DQ2(JT)/DBLE(ZS(JT))-DQ2(3))*(DSH/
     &  (DSH+DQ2(JT))-DSH/(DSHZ+DQ2(3)))
        IF(IKIN.EQ.1) DMSMA=(DPD(1)*DPD(2)-DPD(3)*DPD(4))/(2.*
     &  DQ2(JR))-DQ2(JT)-DQ2(3)

C...GENERATE TIMELIKE PARTON SHOWER (IF REQUIRED)
        IT=N-1
        K(IT,1)=3
        K(IT,2)=IFLB-IFLS(JT+2)
        IF(IFLB-IFLS(JT+2).EQ.0) K(IT,2)=21
        MSTJ(93)=1
        P(IT,5)=ULMASS(K(IT,2))
        IF (SLAST) P(IT,5)=P(IT,5)-PARL(20)
        IF(SNGL(DMSMA).LE.P(IT,5)**2) GOTO 100
        P(IT,2)=0.
        DO 240 J=1,5
        K(IT+1,J)=0
  240   P(IT+1,J)=0.
        K(IT+1,1)=-1
        K(IT+1,2)=K(IS(JT)+1,2)
        K(IT+1,3)=IT
        IF(MOD(IPY(14),2).EQ.0) THEN
          P(IT,1)=0.
          IF(ILEP.EQ.0) P(IT,4)=(DSHZ-DSH-P(IT,5)**2)/DSHR
          IF(ILEP.GE.1) P(IT,4)=0.5*(P(IS(JT),3)*DPD(2)+
     &    DPQS(1)*(DQ2(JT)+DQ2(3)+P(IT,5)**2))/(P(IS(JT),3)*DPQS(2)-
     &    P(IS(JT),4)*DPQS(1))-DPC(JT)
          P(IT,3)=SQRT(MAX(0.,P(IT,4)**2-P(IT,5)**2))
CJR--begin
          IF (SLAST) THEN
             CALL LUSHOW(IT,0,P(IT,5))
          ELSE
             CALL LUSHOW(IT,0,SQRT(MIN(SNGL(DMSMA),PYPAR(25)*Q2)))
          ENDIF
CJR--end
          IF(N.GE.IT+2) P(IT,5)=P(IT+2,5)
          IF(N.GT.MSTU(4)-10) THEN
            WRITE(6,*) ' LYSSPA: no more memory in LUJETS'
            LST(21)=19
            RETURN
          ENDIF
          DO 243 I=N+1,N+8
          DO 243 J=1,5
          K(I,J)=0
  243     P(I,J)=0.
        ENDIF

C...RECONSTRUCT KINEMATICS OF BRANCHING: TIMELIKE PARTON SHOWER
        DMS=P(IT,5)**2
        IF(IKIN.EQ.0.AND.ILEP.EQ.0) DPT2=(DMSMA-DMS)*(DSHZ+DQ2(3))/
     &  (DSH+DQ2(JT))
        IF(IKIN.EQ.1.AND.ILEP.EQ.0) DPT2=(DMSMA-DMS)*(0.5*DPD(1)*
     &  DPD(2)+0.5*DPD(3)*DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)+DMS))/
     &  (4.*DSH*DPC(3)**2)
        IF(IKIN.EQ.1.AND.ILEP.GE.1) DPT2=(DMSMA-DMS)*(0.5*DPD(1)*
     &  DPD(2)+0.5*DPD(3)*DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)+DMS))/
     &  DPD(3)**2
        IF(DPT2.LT.0.) GOTO 100
        K(IT,3)=N+1
        IF (SLAST) K(IT,3)=2
        P(IT,1)=SQRT(SNGL(DPT2))
        IF(ILEP.EQ.0) THEN
          DPB(1)=(0.5*DPD(2)-DPC(JR)*(DSHZ+DQ2(JR)-DQ2(JT)-DMS)/
     &    DSHR)/DPC(3)-DPC(3)
          P(IT,3)=DPB(1)*(-1)**(JT+1)
          P(IT,4)=(DSHZ-DSH-DMS)/DSHR
        ELSE
          DPC(3)=DQ2(JT)+DQ2(3)+DMS
          DPB(2)=DPQS(2)*DBLE(P(IS(JT),3))-DPQS(1)*DPC(JT)
          DPB(1)=0.5D0*(DPC(JT)*DPD(2)+DPQS(2)*DPC(3))/DPB(2)-
     &    DBLE(P(IS(JT),3))
          P(IT,3)=DPB(1)
          P(IT,4)=0.5D0*(DBLE(P(IS(JT),3))*DPD(2)+
     &    DPQS(1)*DPC(3))/DPB(2)-DPC(JT)
        ENDIF
        IF(N.GE.IT+2) THEN
          MSTU(1)=IT+2
          DPB(1)=DSQRT(DPB(1)**2+DPT2)
          DPB(2)=DSQRT(DPB(1)**2+DMS)
          DPB(3)=P(IT+2,3)
          DPB(4)=DSQRT(DPB(3)**2+DMS)
          DBEZ=(DPB(4)*DPB(1)-DPB(3)*DPB(2))/(DPB(4)*DPB(2)-DPB(3)*
     &    DPB(1))
          CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,0.D0,0.D0,DBEZ)
          THE=ULANGL(P(IT,3),P(IT,1))
          CALL LUDBRB(MSTU(1),MSTU(2),THE,0.,0.D0,0.D0,0.D0)
        ENDIF

C...RECONSTRUCT KINEMATICS OF BRANCHING: SPACELIKE PARTON
        K(N+1,1)=14
        K(N+1,2)=IFLB
        IF(IFLB.EQ.0) K(N+1,2)=21
        K(N+1,3)=0
CJR--begin 
CJR--       give all radiated partons 5 as mother particle 
        K(N+1,3)=5
CJR--end
        P(N+1,1)=P(IT,1)
        P(N+1,2)=0.
        P(N+1,3)=P(IT,3)+P(IS(JT),3)
        P(N+1,4)=P(IT,4)+P(IS(JT),4)
        P(N+1,5)=-SQRT(SNGL(DQ2(3)))
        DO 250 J=1,5
        K(N+2,J)=0
  250   P(N+2,J)=0.
        K(N+2,1)=-1
        K(N+2,2)=K(IS(JT)+1,2)
        K(N+2,3)=N+1

C...DEFINE COLOUR FLOW OF BRANCHING
        K(IS(JT),1)=14
        K(IS(JT),3)=N+1
        ID1=IT
      KN1=ISIGN(500+IABS(K(N+1,2)),2*K(N+1,2)+1)
      KD1=ISIGN(500+IABS(K(ID1,2)),2*K(ID1,2)+1)
      IF(K(N+1,2).EQ.21) KN1=500
      IF(K(ID1,2).EQ.21) KD1=500
        IF((KN1.GE.501.AND.KD1.GE.501).OR.(KN1.LT.0.AND.
     &  KD1.EQ.500).OR.(KN1.EQ.500.AND.KD1.EQ.500.AND.
     &  RLU(0).GT.0.5).OR.(KN1.EQ.500.AND.KD1.LT.0))
     &  ID1=IS(JT)
        ID2=IT+IS(JT)-ID1
        P(N+2,3)=ID1
        P(N+2,4)=ID2
        P(ID1+1,1)=N+1
        P(ID1+1,2)=ID2
        P(ID2+1,1)=ID1
        P(ID2+1,2)=N+1
        K(N+1,4)=K(N+1,4)-MOD(K(N+1,4),MSTU(5))+ID1
        K(N+1,5)=K(N+1,5)-MOD(K(N+1,5),MSTU(5))+ID2
        K(ID1,4)=MOD(K(ID1,4),MSTU(5))+(N+1)*MSTU(5)
        K(ID1,5)=MOD(K(ID1,5),MSTU(5))+ID2*MSTU(5)
        K(ID2,4)=MOD(K(ID2,4),MSTU(5))+ID1*MSTU(5)
        K(ID2,5)=MOD(K(ID2,5),MSTU(5))+(N+1)*MSTU(5)
        N=N+2
C     CALL GULIST(22,2)

C...BOOST TO NEW CM-FRAME
        MSTU(1)=NS+1
        IF(ILEP.EQ.0) THEN
          CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,
     &    -DBLE(P(N-1,1)+P(IS(JR),1))/DBLE(P(N-1,4)+P(IS(JR),4)),
     &    0.D0,-DBLE(P(N-1,3)+P(IS(JR),3))/DBLE(P(N-1,4)+P(IS(JR),4)))
          IR=N-1+(JT-1)*(IS(1)-N+1)
          CALL LUDBRB(MSTU(1),MSTU(2),
     &    -ULANGL(P(IR,3),P(IR,1)),PARU(2)*RLU(0),0.D0,0.D0,0.D0)
        ELSE
C...REORIENTATE EVENT WITHOUT CHANGING THE BOSON FOUR MOMENTUM
          DO 260 J=1,4
  260     DPQ(J)=P(NQ,J)
          DBE1(4)=DPQ(4)+DBLE(P(N-1,4))
          DO 270 J=1,3,2
  270     DBE1(J)=-(DPQ(J)+DBLE(P(N-1,J)))/DBE1(4)
          DTEMP=1.D0-DBE1(1)**2-DBE1(3)**2
          IF(DTEMP.LE.0.D0) THEN
            LST(21)=20
            IF(LST(3).GE.1) WRITE(6,*) ' Warning from LYSSPA: sqrt of',
     &      DTEMP,' New event generated.'
            RETURN
          ENDIF
          DBE1(4)=1.D0/DSQRT(DTEMP)
          DBEP=DBE1(1)*DPQ(1)+DBE1(3)*DPQ(3)
          DGABEP=DBE1(4)*(DBE1(4)*DBEP/(1D0+DBE1(4))+DPQ(4))
          DO 280 J=1,3,2
  280     DPQ(J)=DPQ(J)+DGABEP*DBE1(J)
          DPQ(4)=DBE1(4)*(DPQ(4)+DBEP)
          DPC(1)=DSQRT(DPQ(1)**2+DPQ(3)**2)
          DBE2(4)=-(DPQ(4)*DPC(1)-DPQS(2)*DSQRT(DPQS(2)**2+DPC(1)**2-
     &    DPQ(4)**2))/(DPC(1)**2+DPQS(2)**2)
          THE=ULANGL(SNGL(DPQ(3)),SNGL(DPQ(1)))
          DBE2(1)=DBE2(4)*DSIN(DBLE(THE))
          DBE2(3)=DBE2(4)*DCOS(DBLE(THE))
          DBE2(4)=1D0/(1D0-DBE2(1)**2-DBE2(3)*2)

C...CONSTRUCT THE COMBINED BOOST
          DPB(1)=DBE1(4)**2*DBE2(4)/(1D0+DBE1(4))
          DPB(2)=DBE1(1)*DBE2(1)+DBE1(3)*DBE2(3)
          DPB(3)=DBE1(4)*DBE2(4)*(1D0+DPB(2))
          DO 290 JB=1,3,2
  290     DROBO(JB+2)=(DBE1(4)*DBE2(4)*DBE1(JB)+DBE2(4)*DBE2(JB)+
     &    DPB(1)*DBE1(JB)*DPB(2))/DPB(3)
          CALL LUDBRB(MSTU(1),MSTU(2),0.,0.,DROBO(3),0.D0,DROBO(5))
          IF(ILEP.EQ.1) THE=ULANGL(P(NS+1,3),P(NS+1,1))
          IF(ILEP.EQ.2) THE=PARU(1)+ULANGL(P(NS+3,3),P(NS+3,1))
          CALL LUDBRB(MSTU(1),MSTU(2),-THE,PARU(2)*RLU(0),0D0,0D0,0D0)
        ENDIF
        MSTU(1)=0
      ENDIF

C...SAVE QUANTITIES, LOOP BACK
      IS(JT)=N-1
      IF(ILEP.EQ.2.AND.N.EQ.NS+4) IS(JT)=N-3
      Q2S(JT)=Q2B
      DQ2(JT)=Q2B
      IF(IPY(14).GE.3.AND.IPY(14).LE.6) DQ2(JT)=Q2B/(1.-Z)
      DSH=DSHZ
      IF(Q2B.GE.0.5*PYPAR22) THEN
        IFLS(JT+2)=IFLS(JT)
        IFLS(JT)=IFLA
        XS(JT)=XA
        ZS(JT)=Z
        DO 300 IFL=-6,6
  300   XFS(JT,IFL)=XFA(IFL)
        TEVS(JT)=TEVB
      ELSE
        IF(JT.EQ.1) IPU1=N-1
        IF(JT.EQ.2) IPU2=N-1
      ENDIF
      IF((MAX(IABS(1-ILEP)*Q2S(1),MIN(1,2-ILEP)*Q2S(2))
     &.GE.0.5*PYPAR22.OR.N.LE.NS+2) .AND. SPLIT) SLAST=.TRUE.
      IF(MAX(IABS(1-ILEP)*Q2S(1),MIN(1,2-ILEP)*Q2S(2)).GE.0.5*PYPAR22
     &.OR.N.LE.NS+2) GOTO 130

      IF(ILEP.EQ.0) THEN
C...BOOST HARD SCATTERING PARTONS TO FRAME OF SHOWER INITIATORS
        DO 310 J=1,3
  310   DROBO(J+2)=(P(NS+1,J)+P(NS+3,J))/(P(NS+1,4)+P(NS+3,4))
        DO 320 J=1,5
  320   P(N+2,J)=P(NS+1,J)
        MSTU(1)=N+2
        MSTU(2)=N+2
        CALL LUDBRB(N+2,N+2,0.,0.,-DROBO(3),-DROBO(4),-DROBO(5))
        ROBO(2)=ULANGL(P(N+2,1),P(N+2,2))
        ROBO(1)=ULANGL(P(N+2,3),SQRT(P(N+2,1)**2+P(N+2,2)**2))
        MSTU(1)=4
        MSTU(2)=NS
        CALL LUDBRB(4,NS,ROBO(1),ROBO(2),DROBO(3),DROBO(4),DROBO(5))
        MSTU(1)=0
        MSTU(2)=0
      ENDIF

C...STORE USER INFORMATION
      K(21,1)=14
      IF(ILEP.NE.0) K(21,1)=11
      K(23,1)=14
      K(21,3)=NS+1
      K(23,3)=NS+3
      DO 330 JT=1,2
      KFL(1,JT)=IFLS(JT)
      IF(IFLS(JT).EQ.0) KFL(1,JT)=21
  330 PYVAR(30+JT)=XS(JT)

      DO 340 I=NS+1,N
      DO 340 J=1,5
  340 V(I,J)=0.
  
CJR--begin
      LASTFL=IFLA
CJR--end

      RETURN
      END
