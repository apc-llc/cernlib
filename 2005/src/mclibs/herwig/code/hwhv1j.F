CDECK  ID>, HWHV1J.
*CMZ :-        -21/04/96  14.37.45  by  Mike Seymour
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHV1J
C-----------------------------------------------------------------------
C   V + 1 JET PRODUCTION, WHERE V=W (IHPRO.LT.5) OR Z (IHPRO.GE.5).
C   USES CROSS-SECTIONS OF EHLQ FOR ANNIHILATION AND COMPTON SCATTERING
C   IHPRO=0 FOR BOTH, 1 FOR ANNIHILATION, AND 2 FOR COMPTON.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,DISFAC(2,12,2),EMV2,DISMAX,S,T,U,
     & SHAT,THAT,UHAT,Z,HWUALF,PT,EMT,GFACTR,SIGANN,SIGCOM(2),CSFAC,ET,
     & EJ,YMIN,YMAX,EMAX,CV,CA,BR,EMV,GAMV
      INTEGER HWRINT,IDINIT(2,12,2),ICOFLO(4,2),I,J,K,L,M,ID1,ID2,
     $     IDV,IDI,IDM
      EXTERNAL HWRINT
      SAVE DISFAC,SHAT,THAT,EMV,EMV2,IDV,IDI
C---IDINIT HOLDS THE INITIAL STATES FOR ANNIHILATION PROCESSES
      DATA IDINIT/1,8,2,7,3,10,4,9,5,12,6,11,1,10,2,9,3,8,4,7,5,12,6,11,
     $            1,7,2,8,3,9,4,10,5,11,6,12,1,7,2,8,3,9,4,10,5,11,6,12/
C---ICOFLO HOLDS THE COLOR FLOW FOR EACH PROCESS
C---DISFAC HOLDS THE DISTRIBUTION FUNCTION*CROSS-SECTION FOR EACH
C   POSSIBLE SUB-PROCESS.
C   INDEX1=INITIAL STATE PERMUTATION (1=AS IDINIT/QG;2=OPPOSITE/GQ),
C        2=QUARK (FOR ANNIHILATION, >6 IMPLIES CABIBBO ROTATED PAIR),
C        3=PROCESS (1=ANNIHILATION, 2=COMPTON)
      DATA ICOFLO,DISFAC/2,4,3,1,4,1,3,2,48*0./
      IF (GENEV) THEN
        DISMAX=0
        DO 110 I=1,2
        DO 110 J=1,12
        DO 110 K=1,2
 110      DISMAX=MAX(DISFAC(K,J,I),DISMAX)
 120    I=HWRINT(1,2)
        J=HWRINT(1,12)
        K=HWRINT(1,2)
        IF (HWRGEN(0)*DISMAX.GT.DISFAC(K,J,I)) GOTO 120
        IF (I.EQ.1) THEN
C---ANNIHILATION
          IDN(1)=IDINIT(K,J,IDI)
          IDN(2)=IDINIT(3-K,J,IDI)
          IDN(4)=13
        ELSE
C---COMPTON SCATTERING
          IDN(1)=J
          IDN(2)=13
          IF (IDV.EQ.200) THEN
            IDN(4)=J
          ELSE
            IF (J.EQ.5.OR.J.EQ.6.OR.J.GE.11.OR.HWRGEN(0).GT.SCABI) THEN
C---CHANGE QUARKS (1->2,2->1,3->4,4->3,...)
              IDN(4)=4*INT((J-1)/2)-J+3
            ELSE
C---CHANGE AND CABIBBO ROTATE QUARKS (1->4,2->3,3->2,...)
              IDN(4)=12*INT((J-1)/6)-J+5
            ENDIF
          ENDIF
          IF ((SQRT(EMV2)+RMASS(IDN(4)))**2.GT.SHAT) GOTO 120
          IF (K.EQ.2) THEN
C---SWAP INITIAL STATES
            IDN(3)=IDN(1)
            IDN(1)=IDN(2)
            IDN(2)=IDN(3)
          ENDIF
        ENDIF
        IF (IDV.EQ.200) THEN
          IDN(3)=200
        ELSE
C---W+ OR W-? USE CHARGE CONSERVATION TO WORK OUT
          IDN(3)=NINT(198.5-.1667*FLOAT(ICHRG(IDN(1))+ICHRG(IDN(2))))
        ENDIF
        M=K
        IF (I.EQ.2.AND.J.LE.6) M=3-K
        DO 130 L=1,4
 130      ICO(L)=ICOFLO(L,M)
        IDCMF=15
        COSTH=(SHAT+2*THAT-EMV2)/(SHAT-EMV2)
C---TRICK HWETWO INTO USING THE OFF-SHELL V MASS
        RMASS(IDN(3))=SQRT(EMV2)
        CALL HWETWO
        RMASS(IDN(3))=EMV
        RHOHEP(1,NHEP-1)=0.5
        RHOHEP(2,NHEP-1)=0.0
        RHOHEP(3,NHEP-1)=0.5
      ELSE
        EVWGT=0.
        IHPRO=MOD(IPROC,100)/10
        IF (IHPRO.LT.5) THEN
          IDV=198
          IDI=1
          IDM=10
          GAMV=GAMW
        ELSE
          IDV=200
          IDI=2
          IDM=6
          GAMV=GAMZ
          IHPRO=IHPRO-5
        ENDIF
        EMV=RMASS(IDV)
        EMV2=EMV*(EMV+GAMV*TAN(HWRUNI(0,-ONE-HALF,ONE+HALF)))
        IF (EMV2.LE.0) RETURN
        CALL HWRPOW(ET,EJ)
        PT=0.5*ET
        EMT=SQRT(PT**2+EMV2)
        EMAX=0.5*(PHEP(5,3)+EMV2/PHEP(5,3))
        IF (EMAX.LE.EMT) RETURN
        YMAX=0.5*LOG((EMAX+SQRT(EMAX**2-EMT**2))
     &              /(EMAX-SQRT(EMAX**2-EMT**2)))
        YMIN=MAX(YJMIN,-YMAX)
        YMAX=MIN(YJMAX, YMAX)
        IF (YMAX.LE.YMIN) RETURN
        Z=EXP(HWRUNI(0,YMIN,YMAX))
        S= PHEP(5,3)**2
        T=-PHEP(5,3)*EMT/Z+EMV2
        U=-PHEP(5,3)*EMT*Z+EMV2
        XXMIN=-U/(S+T-EMV2)
        IF (XXMIN.LT.0..OR.XXMIN.GT.1.) RETURN
        XLMIN=LOG(XXMIN)
        XX(1)=EXP(HWRUNI(2,XLMIN,ZERO))
        THAT =XX(1)*T+(1.-XX(1))*EMV2
        XX(2)=-THAT / (XX(1)*S+U-EMV2)
        IF (XX(2).LT.0..OR.XX(2).GT.1.) RETURN
        UHAT =XX(2)*U+(1.-XX(2))*EMV2
        SHAT =XX(1)*XX(2)*S
        EMSCA=EMT
        CALL HWSGEN(.FALSE.)
        GFACTR=GEV2NB*2.*PIFAC*ALPHEM*HWUALF(1,EMSCA)/(9.*SWEIN)
        SIGANN=GFACTR*((THAT-EMV2)**2+(UHAT-EMV2)**2)
     &               /(SHAT**2*THAT*UHAT)
        SIGCOM(1)=.375*GFACTR*(SHAT**2+UHAT**2+2*EMV2*THAT)
     &                       /(-UHAT*SHAT**3)
        SIGCOM(2)=.375*GFACTR*(SHAT**2+THAT**2+2*EMV2*UHAT)
     &                       /(-THAT*SHAT**3)
C---IF USER SPECIFIED A SUB-PROCESS, ZERO THE OTHER
        IF (IHPRO.EQ.1) THEN
          SIGCOM(1)=0.
          SIGCOM(2)=0.
        ENDIF
        IF (IHPRO.EQ.2) SIGANN=0.
        DO 210 I=1,IDM
          IF (IDV.EQ.200) THEN
            J=I
            IF(I.GT.6) J=I-6
            DISFAC(1,I,1)=SWEIN*(VFCH(J,1)**2+AFCH(J,1)**2)
          ELSE
            IF (I.LE.4) THEN
              DISFAC(1,I,1)=1-SCABI
            ELSEIF (I.GE.7) THEN
              DISFAC(1,I,1)=SCABI
            ELSE
              DISFAC(1,I,1)=1.
            ENDIF
          ENDIF
          DISFAC(2,I,1)=DISFAC(1,I,1) *
     &         SIGANN*DISF(IDINIT(1,I,IDI),2)*DISF(IDINIT(2,I,IDI),1)
          DISFAC(1,I,1)=DISFAC(1,I,1) *
     &         SIGANN*DISF(IDINIT(1,I,IDI),1)*DISF(IDINIT(2,I,IDI),2)
 210    CONTINUE
        DO 211 I=IDM+1,12
          DISFAC(1,I,1)=0
          DISFAC(2,I,1)=0
 211    CONTINUE
        DO 220 I=1,12
          IF (IDV.EQ.200) THEN
            J=I
            IF(I.GT.6) J=I-6
            DISFAC(1,I,2)=SWEIN*(VFCH(J,1)**2+AFCH(J,1)**2)
          ELSE
            DISFAC(1,I,2)=1.
          ENDIF
          DISFAC(2,I,2)=DISFAC(1,I,2)*SIGCOM(2)*DISF(I,2)*DISF(13,1)
          DISFAC(1,I,2)=DISFAC(1,I,2)*SIGCOM(1)*DISF(I,1)*DISF(13,2)
 220    CONTINUE
        DO 230 I=1,2
        DO 230 J=1,12
        DO 230 K=1,2
 230      EVWGT=EVWGT+DISFAC(K,J,I)
        CSFAC=-PT*S/(XX(1)*S+U-EMV2)*EJ
     &        *(YMAX-YMIN)*XLMIN*XX(1)
C---INCLUDE BRANCHING RATIO OF V
        CALL HWDBOZ(IDV,ID1,ID2,CV,CA,BR,0)
        EVWGT=EVWGT*CSFAC*BR
      ENDIF
 999  END
