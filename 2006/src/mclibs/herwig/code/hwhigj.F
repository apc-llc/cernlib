CDECK  ID>, HWHIGJ
*CMZ :-        -23/08/94  13.22.29  by  Mike Seymour
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWHIGJ
C-----------------------------------------------------------------------
C     QCD Higgs plus jet production; mean EVWGT = Sigma in nb*Higgs B.R.
C     Adapted from the program of U. Baur and E.W.N. Glover
C     See: Nucl. Phys. B339 (1990) 38
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUALF,HWUAEM,EPS,RCS,EMH,EMHWT,
     & EMHTMP,BR,CV,CA,EMH2,ET,EJ,PT,EMT,EMAX,YMAX,YHINF,YHSUP,EXYH,
     & YMIN,YJINF,YJSUP,EXYJ,S,T,U,FACT,AMPQQ,AMPQG,AMPGQ,AMPGG,HCS,
     & FACTR
      INTEGER I,IDEC,ID1,ID2
      EXTERNAL HWRGEN,HWRUNI,HWUALF,HWUAEM
      SAVE HCS,IDEC
      PARAMETER (EPS=1.D-9)
      IF (GENEV) THEN
         RCS=HCS*HWRGEN(0)
      ELSE
         EVWGT=0.
C Select a Higgs mass
         CALL HWHIGM(EMH,EMHWT)
         IF (EMH.LE.0 .OR. EMH.GE.PHEP(5,3)) RETURN
C Store branching ratio for specified Higgs deacy channel
         IDEC=MOD(IPROC,100)
         BR=1.
         IF (IDEC.EQ.0) THEN
            BR=0.
            DO 10 I=1,6
  10        BR=BR+BRHIG(I)
         ELSEIF (IDEC.EQ.10) THEN
            CALL HWDBOZ(198,ID1,ID2,CV,CA,BR,1)
            CALL HWDBOZ(199,ID1,ID2,CV,CA,BR,1)
            BR=BR*BRHIG(IDEC)
         ELSEIF (IDEC.EQ.11) THEN
            CALL HWDBOZ(200,ID1,ID2,CV,CA,BR,1)
            CALL HWDBOZ(200,ID1,ID2,CV,CA,BR,1)
            BR=BR*BRHIG(IDEC)
         ELSEIF (IDEC.LE.12) THEN
            BR=BRHIG(IDEC)
         ENDIF
C Select subprocess kinematics
         EMH2=EMH**2
         CALL HWRPOW(ET,EJ)
         PT=.5*ET
         EMT=SQRT(PT**2+EMH2)
         EMAX=0.5*(PHEP(5,3)+EMH2/PHEP(5,3))
         IF (EMAX.LE.EMT) RETURN
         YMAX=LOG((EMAX+SQRT(EMAX**2-EMT**2))/EMT)
         YHINF=MAX(YJMIN,-YMAX)
         YHSUP=MIN(YJMAX, YMAX)
         IF (YHSUP.LE.YHINF) RETURN
         EXYH=EXP(HWRUNI(1,YHINF,YHSUP))
         YMIN=LOG(PT/(PHEP(5,3)-EMT/EXYH))
         YMAX=LOG((PHEP(5,3)-EMT*EXYH)/PT)
         YJINF=MAX(YJMIN,YMIN)
         YJSUP=MIN(YJMAX,YMAX)
         IF (YJSUP.LE.YJINF) RETURN
         EXYJ=EXP(HWRUNI(2,YJINF,YJSUP))
         XX(1)=(EMT*EXYH+PT*EXYJ)/PHEP(5,3)
         XX(2)=(EMT/EXYH+PT/EXYJ)/PHEP(5,3)
         S=XX(1)*XX(2)*PHEP(5,3)**2
         T=EMH2-XX(1)*EMT*PHEP(5,3)/EXYH
         U=EMH2-S-T
         COSTH=(S+2.*T-EMH2)/(S-EMH2)
C Set subprocess scale
         EMSCA=EMT
         CALL HWSGEN(.FALSE.)
         FACT=GEV2NB*PT*EJ*(YHSUP-YHINF)*(YJSUP-YJINF)*BR*EMHWT
     &       *HWUALF(1,EMSCA)**3*HWUAEM(EMH2)/(SWEIN*16*PIFAC*S**2)
         CALL HWHIGA(S,T,U,EMH2,AMPQQ,AMPQG,AMPGQ,AMPGG)
      ENDIF
      HCS=0.
      DO 30 ID1=1,13
      IF (DISF(ID1,1).LT.EPS) GOTO 30
      FACTR=FACT*DISF(ID1,1)
      IF (ID1.LT.7) THEN
C Quark first:
         ID2=ID1+6
         HCS=HCS+FACTR*DISF(ID2,2)*AMPQQ
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(13 ,201,2314,81,*99)
         ID2=13
         HCS=HCS+FACTR*DISF(ID2,2)*AMPQG
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,201,3124,82,*99)
      ELSEIF (ID1.LT.13) THEN
C Antiquark first:
         ID2=ID1-6
         HCS=HCS+FACTR*DISF(ID2,2)*AMPQQ
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(13 ,201,3124,83,*99)
         ID2=13
         HCS=HCS+FACTR*DISF(ID2,2)*AMPQG
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID1,201,2314,84,*99)
      ELSE
C Gluon first:
         DO 20 ID2=1,12
         IF (DISF(ID2,2).LT.EPS) GOTO 20
         IF (ID2.LT.7) THEN
            HCS=HCS+FACTR*DISF(ID2,2)*AMPGQ
            IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID2,201,2314,85,*99)
         ELSE
            HCS=HCS+FACTR*DISF(ID2,2)*AMPGQ
            IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID2,201,3124,86,*99)
         ENDIF
  20     CONTINUE
         HCS=HCS+FACTR*DISF(13,2)*AMPGG
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(13 ,201,2314,87,*99)
      ENDIF
  30  CONTINUE
      EVWGT=HCS
      RETURN
C Generate event
  99  IDN(1)=ID1
      IDN(2)=ID2
      IDCMF=15
C Trick HWETWO into using off-shell Higgs mass
      EMHTMP=RMASS(IDN(4))
      RMASS(IDN(4))=EMH
      CALL HWETWO
      RMASS(IDN(4))=EMHTMP
  999 END
