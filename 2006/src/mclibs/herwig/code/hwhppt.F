CDECK  ID>, HWHPPT.
*CMZ :-        -12/01/93  10.12.43  by  Bryan Webber
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWHPPT
C-----------------------------------------------------------------------
C     point-like photon/QCD di-jet production: mean EVWGT = sigma in nb
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,HWUALF,EPS,RCS,PP1,PP2,ET,EJ,
     & EXY,EXY2,FACTR,RS,S,T,U,CSTU,CTSU,HCS
      INTEGER ID1,ID2,ID3,ID4,IHAD1,IHAD2
      EXTERNAL HWRGEN,HWRUNI,HWUALF
      SAVE CSTU,CTSU,HCS
      PARAMETER (EPS=1.E-9)
      IHAD1=1
      IF (JDAHEP(1,IHAD1).NE.0) IHAD1=JDAHEP(1,IHAD1)
      IHAD2=2
      IF (JDAHEP(1,IHAD2).NE.0) IHAD2=JDAHEP(1,IHAD2)
      IF (GENEV) THEN
         RCS=HCS*HWRGEN(0)
      ELSE
         EVWGT=0.
         PP1=PHEP(4,IHAD1)+ABS(PHEP(3,IHAD1))
         PP2=PHEP(4,IHAD2)+ABS(PHEP(3,IHAD2))
         XX(1)=1.
         CALL HWRPOW(ET,EJ)
         EXY=EXP(HWRUNI(1,YJMIN,YJMAX))
         EXY2=2.*PP1/ET-EXY
         IF (EXY2.LE.EXP(YJMIN).OR.EXY2.GE.EXP(YJMAX)) RETURN
         XX(2)=PP1/(PP2*EXY*EXY2)
         IF (XX(2).LE.0..OR.XX(2).GE.1.) RETURN
         S=XX(2)*PP1*PP2
         RS=.5*SQRT(S)
         T=-PP1*0.5*ET/EXY
         U=-S-T
         COSTH=(T-U)/S
C Set EMSCA to hard process scale (Approx ET-jet)
         EMSCA=SQRT(2.*S*T*U/(S*S+T*T+U*U))
         FACTR=-GEV2NB*0.5*EJ*(YJMAX-YJMIN)*ET*PIFAC*ALPHEM
     &        *HWUALF(1,EMSCA)/(S*T)
         CALL HWSFUN(XX(2),EMSCA,IDHW(IHAD2),NSTRU,DISF(1,2),2)
         CSTU=U/T+T/U
         CTSU=-2.*CFFAC*(U/S+S/U)
      ENDIF
      HCS=0.
      ID1=59
      DO 20 ID2=1,13
      IF (DISF(ID2,2).LT.EPS) GOTO 20
      IF (ID2.LT.7) THEN
C photon+q ---> g+q
         HCS=HCS+CTSU*DISF(ID2,2)*QFCH(ID2)**2
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 13,ID2,1423,51,*99)
      ELSEIF (ID2.LT.13) THEN
C photon+qbar ---> g+qbar
         HCS=HCS+CTSU*DISF(ID2,2)*QFCH(ID2-6)**2
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 13,ID2,1342,52,*99)
      ELSE
C photon+g ---> q+qbar
         DO 10 ID3=1,6
         IF (RS.GT.RMASS(ID3)) THEN
            ID4=ID3+6
            HCS=HCS+CSTU*DISF(ID2,2)*QFCH(ID3)**2
            IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP(ID3,ID4,1423,53,*99)
         ENDIF
  10     CONTINUE
      ENDIF
  20  CONTINUE
      EVWGT=FACTR*HCS
      RETURN
C Generate event
  99  IDN(1)=ID1
      IDN(2)=ID2
      IDCMF=15
      CALL HWETWO
  999 END
