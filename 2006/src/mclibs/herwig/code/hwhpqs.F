CDECK  ID>, HWHPQS.
*CMZ :-        -27/03/95  13.27.22  by  Mike Seymour
*-- Author :    Ian Knowles
C-----------------------------------------------------------------------
      SUBROUTINE HWHPQS
C-----------------------------------------------------------------------
C     Compton scattering of point-like photon and (anti)quark
C     mean EVWGT = sigma in nb
C-----------------------------------------------------------------------
      INCLUDE 'HERWIG59.INC'
      DOUBLE PRECISION HWRGEN,HWRUNI,EPS,RCS,PP1,PP2,ET,EJ,EXY,EXY2,
     & FACTR,RS,S,T,U,CTSU,HCS
      INTEGER ID1,ID2,IHAD1,IHAD2
      EXTERNAL HWRGEN,HWRUNI
      SAVE CTSU,HCS
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
         FACTR=-GEV2NB*0.5*EJ*(YJMAX-YJMIN)*ET*PIFAC*ALPHEM**2/(S*T)
         CALL HWSFUN(XX(2),EMSCA,IDHW(IHAD2),NSTRU,DISF(1,2),2)
         CTSU=-2.*(U/S+S/U)
      ENDIF
      HCS=0.
      ID1=59
      DO 20 ID2=1,12
      IF (DISF(ID2,2).LT.EPS) GOTO 20
      IF (ID2.LT.7) THEN
C photon+q ---> photon+q
         HCS=HCS+CTSU*DISF(ID2,2)*QFCH(ID2)**4
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 59,ID2,1432,66,*99)
      ELSE
C photon+qbar ---> photon+qbar
         HCS=HCS+CTSU*DISF(ID2,2)*QFCH(ID2-6)**4
         IF (GENEV.AND.HCS.GT.RCS) CALL HWHQCP( 59,ID2,1432,67,*99)
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
