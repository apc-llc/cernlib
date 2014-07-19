*
* $Id: pyspli.F,v 1.1.1.1 1996/01/11 14:05:29 mclareni Exp $
*
* $Log: pyspli.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:29  mclareni
* Fritiof
*
*
C*********************************************************************
 
      SUBROUTINE PYSPLI(KF,KFLIN,KFLCH,KFLSP)
 
C...In case of a hadron remnant which is more complicated than just a
C...quark or a diquark, split it into two (partons or hadron + parton).
      DIMENSION KFL(3)
 
C...Preliminaries. Parton composition.
      KFA=IABS(KF)
      KFS=ISIGN(1,KF)
      KFL(1)=MOD(KFA/1000,10)
      KFL(2)=MOD(KFA/100,10)
      KFL(3)=MOD(KFA/10,10)
      IF(KFLIN.NE.21.AND.KFLIN.NE.22.AND.KFLIN.NE.23) THEN
        KFLR=KFLIN*KFS
      ELSE
        KFLR=KFLIN
      ENDIF
      KFLCH=0
 
C...Subdivide lepton.
      IF(KFA.GE.11.AND.KFA.LE.18) THEN
        IF(KFLR.EQ.KFA) THEN
          KFLSP=KFS*22
        ELSEIF(KFLR.EQ.22) THEN
          KFLSP=KFA
        ELSEIF(KFLR.EQ.-24.AND.MOD(KFA,2).EQ.1) THEN
          KFLSP=KFA+1
        ELSEIF(KFLR.EQ.24.AND.MOD(KFA,2).EQ.0) THEN
          KFLSP=KFA-1
        ELSEIF(KFLR.EQ.21) THEN
          KFLSP=KFA
          KFLCH=KFS*21
        ELSE
          KFLSP=KFA
          KFLCH=-KFLR
        ENDIF
 
C...Subdivide photon.
      ELSEIF(KFA.EQ.22) THEN
        IF(KFLR.NE.21) THEN
          KFLSP=-KFLR
        ELSE
          RAGR=0.75*RLU(0)
          KFLSP=1
          IF(RAGR.GT.0.125) KFLSP=2
          IF(RAGR.GT.0.625) KFLSP=3
          IF(RLU(0).GT.0.5) KFLSP=-KFLSP
          KFLCH=-KFLSP
        ENDIF
 
C...Subdivide meson.
      ELSEIF(KFL(1).EQ.0) THEN
        KFL(2)=KFL(2)*(-1)**KFL(2)
        KFL(3)=-KFL(3)*(-1)**IABS(KFL(2))
        IF(KFLR.EQ.KFL(2)) THEN
          KFLSP=KFL(3)
        ELSEIF(KFLR.EQ.KFL(3)) THEN
          KFLSP=KFL(2)
        ELSEIF(KFLR.EQ.21.AND.RLU(0).GT.0.5) THEN
          KFLSP=KFL(2)
          KFLCH=KFL(3)
        ELSEIF(KFLR.EQ.21) THEN
          KFLSP=KFL(3)
          KFLCH=KFL(2)
        ELSEIF(KFLR*KFL(2).GT.0) THEN
          CALL LUKFDI(-KFLR,KFL(2),KFDUMP,KFLCH)
          KFLSP=KFL(3)
        ELSE
          CALL LUKFDI(-KFLR,KFL(3),KFDUMP,KFLCH)
          KFLSP=KFL(2)
        ENDIF
 
C...Subdivide baryon.
      ELSE
        NAGR=0
        DO 100 J=1,3
  100   IF(KFLR.EQ.KFL(J)) NAGR=NAGR+1
        IF(NAGR.GE.1) THEN
          RAGR=0.00001+(NAGR-0.00002)*RLU(0)
          IAGR=0
          DO 110 J=1,3
          IF(KFLR.EQ.KFL(J)) RAGR=RAGR-1.
  110     IF(IAGR.EQ.0.AND.RAGR.LE.0.) IAGR=J
        ELSE
          IAGR=1.00001+2.99998*RLU(0)
        ENDIF
        ID1=1
        IF(IAGR.EQ.1) ID1=2
        IF(IAGR.EQ.1.AND.KFL(3).GT.KFL(2)) ID1=3
        ID2=6-IAGR-ID1
        KSP=3
        IF(MOD(KFA,10).EQ.2.AND.KFL(1).EQ.KFL(2)) THEN
          IF(IAGR.NE.3.AND.RLU(0).GT.0.25) KSP=1
        ELSEIF(MOD(KFA,10).EQ.2.AND.KFL(2).GE.KFL(3)) THEN
          IF(IAGR.NE.1.AND.RLU(0).GT.0.25) KSP=1
        ELSEIF(MOD(KFA,10).EQ.2) THEN
          IF(IAGR.EQ.1) KSP=1
          IF(IAGR.NE.1.AND.RLU(0).GT.0.75) KSP=1
        ENDIF
        KFLSP=1000*KFL(ID1)+100*KFL(ID2)+KSP
        IF(KFLR.EQ.21) THEN
          KFLCH=KFL(IAGR)
        ELSEIF(NAGR.EQ.0.AND.KFLR.GT.0) THEN
          CALL LUKFDI(-KFLR,KFL(IAGR),KFDUMP,KFLCH)
        ELSEIF(NAGR.EQ.0) THEN
          CALL LUKFDI(10000+KFLSP,-KFLR,KFDUMP,KFLCH)
          KFLSP=KFL(IAGR)
        ENDIF
      ENDIF
 
C...Add on correct sign for result.
      KFLCH=KFLCH*KFS
      KFLSP=KFLSP*KFS
 
      RETURN
      END
