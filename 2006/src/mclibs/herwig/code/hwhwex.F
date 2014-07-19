CDECK  ID>, HWHWEX.
*CMZ :-        -26/04/91  14.55.45  by  Federico Carminati
*-- Author :    Mike Seymour
C-----------------------------------------------------------------------
      SUBROUTINE HWHWEX
C-----------------------------------------------------------------------
C     TOP QUARK PRODUCTION VIA W EXCHANGE: MEAN EVWGT=TOP PROD C-S IN NB
C     C-S IS SUM OF:
C     UbarBbar, DBbar, DbarB, UB, CbarBbar, SBbar, SbarB, AND CB
C     UNLESS USER SPECIFIES OTHERWISE BY MOD(IPROC,100)=1-8 RESPECTIVELY
C---DSDCOS HOLDS THE CROSS-SECTIONS FOR THE PROCESSES LISTED ABOVE
C   (1-8) ARE WITH B FROM BEAM 1, (9-16) ARE WITH B FROM BEAM 2.
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,DSDCOS(16),EMT2,EMT,EMW2,EMW,
     & CMFMIN,TAUMIN,TAUMLN,S,T,U,ROOTS,DSMAX
      INTEGER HWRINT,IDHWEX(2,16),I
      EXTERNAL HWRGEN,HWRUNI,HWRINT
      SAVE DSDCOS,DSMAX
      EQUIVALENCE (EMW,RMASS(198)),(EMT,RMASS(6))
C---IDHWEX HOLDS THE IDs OF THE INCOMING PARTICLES FOR EACH SUB-PROCESS
      DATA IDHWEX/11,8,11,1,5,7,5,2,11,10,11,3,5,9,5,4,
     &            8,11,1,11,7,5,2,5,10,11,3,11,9,5,4,5/
      EMT2=EMT**2
      EMW2=EMW**2
      IF (GENEV) THEN
 300    IHPRO=HWRINT(1,16)
        IF (HWRGEN(0).GT.DSDCOS(IHPRO)/DSMAX) GOTO 300
        DO 10 I=1,2
          IDN(I)=IDHWEX(I,IHPRO)
          IF (IDN(I).EQ.5 .OR. IDN(I).EQ.11) THEN
C---CHANGE B QUARK INTO T QUARK
            IDN(I+2)=IDN(I)+1
          ELSEIF (HWRGEN(0).GT.SCABI) THEN
C---CHANGE QUARKS (1->2,2->1,3->4,4->3,7->8,8->7,...)
            IDN(I+2)=4*INT((IDN(I)-1)/2)-IDN(I)+3
          ELSE
C---CHANGE AND CABIBBO ROTATE QUARKS (1->4,2->3,3->2,4->1,7->10,...)
            IDN(I+2)=12*INT((IDN(I)-1)/6)-IDN(I)+5
          ENDIF
          ICO(I)=I+2
          ICO(I+2)=I
 10     CONTINUE
        IDCMF=15
        CALL HWETWO
      ELSE
        EVWGT=0.
        CMFMIN=EMT
        TAUMIN=(CMFMIN/PHEP(5,3))**2
        TAUMLN=LOG(TAUMIN)
        ROOTS=PHEP(5,3)*SQRT(EXP(HWRUNI(0,ZERO,TAUMLN)))
        XXMIN=(ROOTS/PHEP(5,3))**2
        XLMIN=LOG(XXMIN)
        COSTH=HWRUNI(0,-ONE, ONE)
        S=ROOTS**2
        T=-0.5*S*(1-COSTH)
        U=-0.5*S*(1+COSTH)
        EMSCA=SQRT(2*S*T*U/(S*S+T*T+U*U))
        DSDCOS(1)=GEV2NB*PIFAC*.125*(ALPHEM/SWEIN)**2
     &           *(S-EMT2)**2 / S / (EMW2 + 0.5*(S-EMT2)*(1-COSTH))**2
        DSDCOS(2)=DSDCOS(1) / 4
     &    * (1 + EMT2/S + 2*COSTH + (1-EMT2/S)*COSTH**2)
        DSDCOS(3)=DSDCOS(2)
        DSDCOS(4)=DSDCOS(1)
C---IF USER SPECIFIED SUB-PROCESS THEN ZERO ALL THE OTHERS
        IHPRO=MOD(IPROC,100)
        IF (IHPRO.GT.8) THEN
          CALL HWWARN('HWHWEX',1,*999)
          IHPRO=0
        ENDIF
        DO 100 I=1,8
          IF (I.LE.4) DSDCOS(I+4)=DSDCOS(I)
          IF (IHPRO.NE.0 .AND. IHPRO.NE.I) DSDCOS(I)=0
          DSDCOS(I+8)=DSDCOS(I)
 100    CONTINUE
        CALL HWSGEN(.TRUE.)
        DSMAX=0
        DO 200 I=1,16
          DSDCOS(I)=DSDCOS(I)*DISF(IDHWEX(1,I),1)*DISF(IDHWEX(2,I),2)
          EVWGT=EVWGT + 2*TAUMLN*XLMIN*DSDCOS(I)
          IF (DSDCOS(I).GT.DSMAX) DSMAX=DSDCOS(I)
 200    CONTINUE
      ENDIF
 999  END
