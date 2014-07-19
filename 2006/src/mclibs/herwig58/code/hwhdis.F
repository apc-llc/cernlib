*
* $Id: hwhdis.F,v 1.1.1.1 1996/03/08 17:02:13 mclareni Exp $
*
* $Log: hwhdis.F,v $
* Revision 1.1.1.1  1996/03/08 17:02:13  mclareni
* Herwig58
*
*
*CMZ :          29/08/94  11.51.47  by  Unknown
*-- Author :
CDECK  ID>, HWHDIS.
*CMZ :-        -26/04/91  14.55.44  by  Federico Carminati
*-- Author :    Giovanni Abbiendi & Luca Stanco
C----------------------------------------------------------------------
      SUBROUTINE HWHDIS
C     DEEP INELASTIC LEPTON-HADRON SCATTERING
C     MEAN EVWGT = SIGMA IN NB
C----------------------------------------------------------------------
C     PROVIDED BY G.ABBIENDI AND L.STANCO 30/10/90
C----------------------------------------------------------------------
C     MODIFIED 5/8/92 BY MHS TO USE PHOTON AS INCOMING HADRON IN
C                            LEPTON-LEPTON COLLISIONS
C     Modified to use standardized electro-weak couplings and
C     (if ZPRIME) allow for a Z': IGK 30/9/93.
C     Modified to allow neutrino beams: IGK 16/8/94.
C----------------------------------------------------------------------
#include "herwig58/herwig58.inc"
      INTEGER I,IQK,IQKIN,IQKOUT,IDSCAT,IHAD,ILEPT
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUPCM,PRAN,PROB,SAMP,SIG,Q2,
     & XBJ,Y,W,S,MLEP,MHAD,MLSCAT,LEP,YMIN,YMAX,XXMAX,Q2JAC,XXJAC,
     & JACOBI,A1,A2,A3,B1,B2,PCM,PCMEP,PCMLW,PCMEQ,PCMLQ,COSPHI,PA,
     & EQ,PZQ,SHAT,PROP,DLEFT,DRGHT,DUP,DWN,FACT,EFACT,OMY2,YPLUS,
     & YMNUS,SIGMA,AF(7,12),SMA,Q2SUP,HWUAEM,DCHRG,DNEUT
      LOGICAL CHARGD
      SAVE MLEP,MHAD,S,SMA,PCM,MLSCAT,A1,A2,A3,B1,B2,DLEFT,DRGHT,Q2,
     & AF,XBJ,Y,YPLUS,YMNUS,OMY2,FACT,EFACT,SIGMA,IDSCAT,CHARGD,
     & ILEPT,DCHRG,DNEUT,LEP
      IQK=MOD(IPROC,10)
      IHAD=2
      IF (JDAHEP(1,IHAD).NE.0) IHAD=JDAHEP(1,IHAD)
      IF (FSTWGT.OR.IHAD.NE.2) THEN
C---INITIALISE PROCESS (MUST BE DONE EVERY TIME IF S VARIES)
C---LEPTON AND HADRON MASSES, INVARIANT MASS, MOMENTUM IN C.M. FRAME
        MLEP=PHEP(5,1)
        MHAD=PHEP(5,IHAD)
        S=PHEP(5,3)**2
        SMA=S-MLEP**2-MHAD**2
        PCM=HWUPCM(SQRT(S),MLEP,MHAD)
C---LEP = 1 FOR LEPTONS, -1 FOR ANTILEPTONS
        IF (IDHW(1).GE.121.AND.IDHW(1).LE.126) THEN
          LEP=+ONE
        ELSEIF (IDHW(1).GE.127.AND.IDHW(1).LE.132) THEN
          LEP=-ONE
        ELSE
          CALL HWWARN('HWHDIS',500,*999)
        ENDIF
        DCHRG=FLOAT(MOD(IDHW(1)  ,2))
        DNEUT=FLOAT(MOD(IDHW(1)+1,2))
        ILEPT=MOD(IDHW(1)-121,6)+11
C---DLEFT,DRIGHT = 1,0 for leptons; = 0,1 for anti-leptons
        DLEFT=MAX(+LEP,ZERO)
        DRGHT=MAX(-LEP,ZERO)
        CHARGD=MOD(IPROC,100)/10.EQ.1
C---Evaluate constant factor in cross section and
C   find and store scattered lepton identity
        IF (CHARGD) THEN
          IF ((EPOLN(3)-PPOLN(3)).EQ.1.) THEN
             WRITE(6,5)
             CALL HWWARN('HWHDIS',501,*999)
  5          FORMAT(1X,'WARNING: Cross-section is zero for the',
     &                ' specified lepton helicity')
          ENDIF
          FACT=GEV2NB*(ONE-(EPOLN(3)-PPOLN(3)))*.25D0*PIFAC
     &        *(HWUAEM(-Q2)/(SWEIN*RMASS(198)**2))**2
          IDSCAT=IDHW(1)+NINT(DCHRG-DNEUT)
        ELSE
          FACT=GEV2NB*TWO*PIFAC*HWUAEM(-Q2)**2
          IDSCAT=IDHW(1)
        ENDIF
        MLSCAT=RMASS(IDSCAT)
C---PARAMETERS USED FOR THE WEIGHT GENERATION IN NEUTRAL CURRENT
C   PROCESSES. ASSUME D(SIGMA)/D(Q**2) GOES LIKE A1+A2/Q**2+A3/Q**4
C   AND D(SIGMA)/D(X) LIKE B1+B2/X
        A1=0.5
        A2=0.5
        A3=1.
        B1=0.1
        B2=1.
      ENDIF
      IF (GENEV) THEN
C---GENERATE EVENT (KINEMATICAL VARIABLES AND STRUCTURE FUNCTION
C   ALREADY FOUND)
        PRAN=SIGMA*HWRGEN(0)
        IF (CHARGD) THEN
C---CHARGED CURRENT PROCESS
          IF (IQK.EQ.0) THEN
C---FIND FLAVOUR OF THE STRUCK QUARK (IF NOT SELECTED BY THE USER)
            PROB=ZERO
            DO 10 I=1,6
            DUP=MOD(I+1,2)
            DWN=MOD(I  ,2)
            PROB=PROB+EFACT*
     &          ((DCHRG*(DLEFT*DUP+DRGHT*DWN*OMY2)
     &           +DNEUT*(DLEFT*DWN+DRGHT*DUP*OMY2))*DISF(I  ,1)
     &          +(DCHRG*(DLEFT*DWN*OMY2+DRGHT*DUP)
     &           +DNEUT*(DLEFT*DUP*OMY2+DRGHT*DWN))*DISF(I+6,1))
            IF (PROB.GE.PRAN) GOTO 20
   10       CONTINUE
            I=6
   20       IQK=I
          ENDIF
          DUP=MOD(IQK+1,2)
          DWN=MOD(IQK  ,2)
          IQKIN=IQK
          IF ((LEP.EQ. 1.AND.MOD(IQK+IDHW(1),2).EQ.0)
     &    .OR.(LEP.EQ.-1.AND.MOD(IQK+IDHW(1),2).EQ.1)) IQKIN=IQK+6
C---FIND FLAVOUR OF THE OUTGOING QUARK
          PRAN=HWRGEN(0)
          PROB=ZERO
          IF (DUP.EQ.1.) THEN
            DO 30 I=1,3
            PROB=PROB+VCKM(IQK/2,I)
            IF (PROB.GE.PRAN) GOTO 40
   30       CONTINUE
            I=3
   40       IQKOUT=2*I-1
            IF (IQKIN.GT.6) IQKOUT=IQKOUT+6
          ELSE
            DO 50 I=1,3
            PROB=PROB+VCKM(I,(IQK+1)/2)
            IF (PROB.GE.PRAN) GOTO 60
   50       CONTINUE
            I=3
   60       IQKOUT=2*I
            IF (IQKIN.GT.6) IQKOUT=IQKOUT+6
          ENDIF
        ELSE
C---NEUTRAL CURRENT PROCESS
          IF (IQK.NE.0) THEN
            IQKIN=IQK
            PROB=EFACT*(AF(1,IQK)*YPLUS*DISF(IQK,1)+
     &              LEP*AF(3,IQK)*YMNUS*DISF(IQK,1))
            IF (PROB.LT.PRAN) IQKIN=IQK+6
          ELSE
C---FIND FLAVOUR OF THE STRUCK QUARK (IF NOT SELECTED BY THE USER)
            PROB=ZERO
            SIG=ONE
            DO 70 I=1,12
            IF (I.GT.6) SIG=-ONE
            PROB=PROB+EFACT*(AF(1,I)*YPLUS*DISF(I,1)+
     &               LEP*SIG*AF(3,I)*YMNUS*DISF(I,1))
            IF (PROB.GE.PRAN) GOTO 80
   70       CONTINUE
            I=12
   80       IQKIN=I
          ENDIF
          IQKOUT=IQKIN
        ENDIF
        IDN(1)=IDHW(1)
        IDN(2)=IQKIN
        IDN(3)=IDSCAT
        IDN(4)=IQKOUT
        ICO(1)=1
        ICO(2)=4
        ICO(3)=3
        ICO(4)=2
        XX(1)=1.
        XX(2)=XBJ
C---CHECK PHASE SPACE WITH THE SELECTED FLAVOUR. IF OUTSIDE THE
C   EVENT IS KILLED.
        PA=XBJ*(PHEP(4,IHAD)+ABS(PHEP(3,IHAD)))
        EQ=HALF*(PA+RMASS(IDN(2))**2/PA)
        PZQ=-(PA-EQ)
        SHAT=(PHEP(4,1)+EQ)**2-(PHEP(3,1)+PZQ)**2
        PCMEQ=HWUPCM(SQRT(SHAT),MLEP,RMASS(IDN(2)))
        PCMLQ=HWUPCM(SQRT(SHAT),MLSCAT,RMASS(IDN(4)))
        IF (PCMLQ.LT.0.) THEN
          CALL HWWARN('HWHDIS',101,*999)
        ELSEIF (PCMLQ.EQ.0.) THEN
          COSTH=ZERO
        ELSE
          COSTH=(TWO*SQRT(PCMEQ**2+MLEP**2)*SQRT(PCMLQ**2+MLSCAT**2)
     &         -(Q2+MLEP**2+MLSCAT**2))/(TWO*PCMEQ*PCMLQ)
        ENDIF
        IF (ABS(COSTH).GT.ONE) CALL HWWARN('HWHDIS',102,*999)
        IDCMF=15
        CALL HWETWO
      ELSE
        EVWGT=ZERO
        IF (CHARGD) THEN
C---CHOOSE X,Y (CC PROCESS)
          YMIN=MAX(YBMIN,Q2MIN/SMA)
          YMAX=MIN(YBMAX,ONE)
          IF (YMIN.GT.YMAX) GOTO 999
          Y=HWRUNI(0,YMIN,YMAX)
          XXMIN=Q2MIN/S/Y
          XXMAX=MIN(Q2MAX/SMA/Y,ONE)
          IF (XXMIN.GT.XXMAX) GOTO 999
          XBJ=HWRUNI(0,XXMIN,XXMAX)
          Q2=XBJ*Y*(S-MLEP**2-MHAD**2)
          JACOBI=(YMAX-YMIN)*(XXMAX-XXMIN)*(S-MLEP**2-MHAD**2)*XBJ
        ELSE
C---CHOOSE X,Q**2 (NC PROCESS)
          Q2SUP=MIN(Q2MAX,SMA*YBMAX)
          IF (Q2MIN.GT.Q2SUP) GOTO 999
          SAMP=(A1+A2+A3)*HWRGEN(0)
          IF (SAMP.LE.A1) THEN
            Q2=HWRUNI(0,Q2MIN,Q2SUP)
          ELSEIF (SAMP.LE.(A1+A2)) THEN
            Q2=EXP(HWRUNI(0,LOG(Q2MIN),LOG(Q2SUP)))
          ELSE
            Q2=-ONE/HWRUNI(0,-ONE/Q2MIN,-ONE/Q2SUP)
          ENDIF
          Q2JAC=(A1+A2+A3)/
     &      (A1/(Q2SUP-Q2MIN)
     &      +A2/LOG(Q2SUP/Q2MIN)/Q2
     &      +A3*Q2MIN*Q2SUP/(Q2SUP-Q2MIN)/Q2**2)
          XXMIN=Q2/SMA/YBMAX
          XXMAX=ONE
          IF (YBMIN.GT.ZERO) XXMAX=MIN(Q2/SMA/YBMIN,ONE)
          IF (XXMIN.GT.XXMAX) GOTO 999
          SAMP=(B1+B2)*HWRGEN(0)
          IF (SAMP.LE.B1) THEN
            XBJ=HWRUNI(0,XXMIN,XXMAX)
          ELSE
            XBJ=EXP(HWRUNI(0,LOG(XXMIN),LOG(XXMAX)))
          ENDIF
          XXJAC=(B1+B2)/(B1/(XXMAX-XXMIN)+B2/LOG(XXMAX/XXMIN)/XBJ)
          Y=Q2/(S-MLEP**2-MHAD**2)/XBJ
          JACOBI=Q2JAC*XXJAC
        ENDIF
C---CHECK IF THE GENERATED POINT IS INSIDE PHASE SPACE. IF NOT
C   RETURN WITH WEIGHT EQUAL TO ZERO.
        W=SQRT(MHAD**2+Q2*(ONE-XBJ)/XBJ)
        PCMEP=PCM
        PCMLW=HWUPCM(SQRT(S),MLSCAT,W)
        IF (PCMLW.LT.ZERO) THEN
          EVWGT=ZERO
          RETURN
        ELSEIF (PCMLW.EQ.ZERO) THEN
          COSPHI=ZERO
        ELSE
          COSPHI=
     &    (TWO*SQRT(PCMEP**2+MLEP**2)*SQRT(PCMLW**2+MLSCAT**2)
     &    -(Q2+MLEP**2+MLSCAT**2))/(TWO*PCMEP*PCMLW)
        ENDIF
        IF (ABS(COSPHI).GT.ONE) THEN
          EVWGT=ZERO
          RETURN
        ENDIF
C---SET SCALE EQUAL Q. EVALUATE STRUCTURE FUNCTIONS.
        EMSCA=SQRT(Q2)
        CALL HWSFUN(XBJ,EMSCA,IDHW(IHAD),NSTRU,DISF,2)
C---EVALUATE DIFFERENTIAL CROSS SECTION
        IF (CHARGD) THEN
          PROP=RMASS(198)**2/(Q2+RMASS(198)**2)
          EFACT=FACT*PROP**2/XBJ
          OMY2=(ONE-Y)**2
          SIGMA=ZERO
          DO 100 I=1,6
          DUP=MOD(I+1,2)
          DWN=MOD(I  ,2)
          IF (IQK.NE.0.AND.IQK.NE.I) GOTO 100
          SIGMA=SIGMA+EFACT*
     &        ((DCHRG*(DLEFT*DUP+DRGHT*DWN*OMY2)
     &         +DNEUT*(DLEFT*DWN+DRGHT*DUP*OMY2))*DISF(I  ,1)
     &        +(DCHRG*(DLEFT*DWN*OMY2+DRGHT*DUP)
     &         +DNEUT*(DLEFT*DUP*OMY2+DRGHT*DWN))*DISF(I+6,1))
  100     CONTINUE
        ELSE
          EFACT=FACT/XBJ/Q2**2
          YPLUS=ONE+(ONE-Y)**2
          YMNUS=ONE-(ONE-Y)**2
          DO 110 I=1,6
          CALL HWUCFF(ILEPT,I,-Q2,AF(1,I))
          AF(1,I+6)=AF(1,I)
          AF(3,I+6)=AF(3,I)
  110     CONTINUE
          SIGMA=ZERO
          DO 200 I=1,6
          IF (IQK.NE.0.AND.IQK.NE.I) GOTO 200
          SIGMA=SIGMA+EFACT*(AF(1,I)*YPLUS*(DISF(I,1)+DISF(I+6,1))+
     &                   LEP*AF(3,I)*YMNUS*(DISF(I,1)-DISF(I+6,1)))
  200     CONTINUE
        ENDIF
C---FIND WEIGHT: DIFFERENTIAL CROSS SECTION TIME THE JACOBIAN FACTOR
        EVWGT=SIGMA*JACOBI
        IF (EVWGT.LT.ZERO) EVWGT=ZERO
      ENDIF
  999 END
