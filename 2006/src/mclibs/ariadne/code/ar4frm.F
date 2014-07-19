C***********************************************************************
C $Id: ar4frm.F,v 1.2 1996/04/10 12:32:58 mclareni Exp $

      SUBROUTINE AR4FRM(ATOTSQ,A1SQ,A2SQ,ISTRAT,IRADG,ITAU)

C...ARiadne subroutine 4 FeRMion generator interface

C...General interface to a four fermion generator as specified by the
C...QCD generator working group of the LEP2 workshop

C...The following subroutine illustrates how to write a generic
C...interface between electroweak generators and QCD generators
C...for LEP 2 applications.
C...In this particular case, an electroweak generator is supposed to
C...have produced two fermions, two antifermions and an arbitrary
C...number of photons. These particles are stored in the HEPEVT
C...common block, in the order 3 = fermion, 4 = antifermion,
C...5 = fermion, 6 = antifermion, 7 onwards = photons.
C...Quarks and leptons should not appear in mixed order,
C...i.e. if 3 is a quark then 4 can not be an antilepton.
C...Incoming positron and electron are assumed stored in
C...positions 1 and 2, but the program never checks this.
C...The subroutine LU4FRM is supposed to read the configuration,
C...and call JETSET to do parton showers and fragmentation.
C...Since the colour flow need not be unique, three real numbers
C...should be given when LU4FRM is called:
C...ATOTSQ = total squared amplitude for the event, irrespective of
C...    colour flow;
C...A1SQ = squared amplitude for the configuration with 3 + 4 and
C...    5 + 6 as the two colour singlets; and
C...A2SQ = squared amplitude for the configuration with 3 + 6 and
C...    5 + 4 as the two colour singlets.
C...The choice of strategy is determined by an integer input:
C...ISTRAT = 0 : pick configurations according to A1SQ : A2SQ;
C...       = 1 : assign interference to maximize 3 + 4 and 5 + 6; or
C...       = 2 : assign interference to maximize 3 + 6 and 5 + 4.
C...Final-state QED radiation may be allowed or inhibited:
C...IRAD = 0 : no final-state photon radiation.
C...     = 1 : photon radiation inside each final fermion pair.
C...tau lepton decay may be  handled by QCD generator or not.
C...ITAU = 0 : taus are considered stable by QCD generator.
C...     = 1 : taus are allowed to decay by QCD generator.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"
#include "lujets.f"
#include "ludat1.f"

C...Call LUHEPC to convert from HEPEVT to LUJETS common.
      CALL LUHEPC(2)

C...Check that event is arranged according to conventions. Else stop.
      IF (N.LT.6) CALL ARERRM('AR4FRM',33,0)
      DO 100 I=3,6
        KFA=IABS(K(I,2))
        IF ((KFA.GE.1.AND.KFA.LE.6).OR.(KFA.GE.11.AND.KFA.LE.16)) THEN
        ELSE
          CALL ARERRM('AR4FRM',34,I)
        ENDIF
        IF (ISIGN(1,K(I,2)).NE.(-1)**(I-1)) CALL ARERRM('AR4FRM',35,I)
  100 CONTINUE
      DO 110 I=7,N
         IF (K(I,2).NE.22) CALL ARERRM('AR4FRM',36,I)
  110 CONTINUE

C....Check that E**2 = m**2 + p**2 for particles. Else stop.
      DO 120 I=1,N
        PE=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2+P(I,5)**2)
        IF (ABS(P(I,4)-PE).GT.PARA(39)*MIN(P(I,4),PE))
     $       CALL ARERRM('AR4FRM',37,I)
  120 CONTINUE

C...Check which fermion pairs are quarks and which leptons.
C...Stop if inconsistent.
      IF (IABS(K(3,2)).LT.10.AND.IABS(K(4,2)).LT.10) THEN
        IQL34=1
      ELSEIF (IABS(K(3,2)).GT.10.AND.IABS(K(4,2)).GT.10) THEN
        IQL34=2
      ELSE
        CALL ARERRM('AR4FRM',38,3)
      ENDIF
      IF (IABS(K(5,2)).LT.10.AND.IABS(K(6,2)).LT.10) THEN
        IQL56=1
      ELSEIF (IABS(K(5,2)).GT.10.AND.IABS(K(6,2)).GT.10) THEN
        IQL56=2
      ELSE
        CALL ARERRM('AR4FRM',38,5)
      ENDIF

C...Do trivial colour pairing and parton shower when none or one
C...of the fermion pairs are quarks.
      IF (IQL34.EQ.2.AND.IQL56.EQ.2) THEN
      ELSEIF (IQL34.EQ.1.AND.IQL56.EQ.2) THEN
        CALL ARGTYP(3,ITYP)
        CALL ARCOPA(3,1,ITYP)
        CALL ARCOPA(4,2,-ITYP)
        IPART=2
        CALL ARCRDI(1,1,2,1,.FALSE.)
        CALL ARCOLI(1,-1)
        IDIPS=1
        IF (IRADG.GT.0) THEN
          IDIPS=IDIPS+1
          CALL ARCRDI(2,2,1,1,.TRUE.)
        ENDIF
        IMF=3
        IML=4
        PT2LST=PARA(40)
        IF (PARA(6).GT.0.0) PT2LST=MIN(PT2LST,PARA(6))
        IPF(1)=1
        IPL(1)=IPART
        ISTRS=1
        IFLOW(1)=ITYP
        QDUMP=.FALSE.
        CALL ARCASC
      ELSEIF (IQL34.EQ.2.AND.IQL56.EQ.1) THEN
        CALL ARGTYP(5,ITYP)
        CALL ARCOPA(5,1,ITYP)
        CALL ARCOPA(6,2,-ITYP)
        IPART=2
        CALL ARCRDI(1,1,2,1,.FALSE.)
        CALL ARCOLI(1,-1)
        IDIPS=1
        IF (IRADG.GT.0) THEN
          IDIPS=IDIPS+1
          CALL ARCRDI(2,2,1,1,.TRUE.)
        ENDIF
        IMF=5
        IML=6
        PT2LST=PARA(40)
        IF (PARA(6).GT.0.0) PT2LST=MIN(PT2LST,PARA(6))
        IPF(1)=1
        IPL(1)=IPART
        ISTRS=1
        IFLOW(1)=ITYP
        QDUMP=.FALSE.
        CALL ARCASC

C....Decide colour pairing when two quark pairs.
      ELSE
        R1SQ=A1SQ
        R2SQ=A2SQ
        RDELTA=ATOTSQ-A1SQ-A2SQ
        IF (ISTRAT.EQ.1) THEN
          IF (RDELTA.GT.0.0) R1SQ=R1SQ+RDELTA
          IF (RDELTA.LT.0.0) R2SQ=MAX(0.0,R2SQ+RDELTA)
        ELSEIF (ISTRAT.EQ.2) THEN
          IF (RDELTA.GT.0.0) R2SQ=R2SQ+RDELTA
          IF (RDELTA.LT.0.0) R1SQ=MAX(0.0,R1SQ+RDELTA)
        ENDIF
        IPAIR=1
        IF (R2SQ.GT.RLU(0)*(R1SQ+R2SQ)) IPAIR=2

C...Do colour joining and parton showers when two quark pairs.
        IF (IPAIR.EQ.1) THEN
          I1=3
          I2=4
          I3=5
          I4=6
        ELSE
          I1=3
          I2=6
          I3=5
          I4=4
        ENDIF
        CALL ARGTYP(I1,ITYP)
        CALL ARCOPA(I1,1,ITYP)
        CALL ARCOPA(I2,2,-ITYP)
        IDIPS=1
        CALL ARCRDI(IDIPS,1,2,1,.FALSE.)
        CALL ARCOLI(IDIPS,-1)
        IF (IRADG.GT.0) THEN
          IDIPS=IDIPS+1
          CALL ARCRDI(IDIPS,2,1,1,.TRUE.)
        ENDIF
        IPF(1)=1
        IPL(1)=2
        IFLOW(1)=ITYP
        CALL ARGTYP(I3,ITYP)
        CALL ARCOPA(I3,3,ITYP)
        CALL ARCOPA(I4,4,-ITYP)
        IDIPS=IDIPS+1
        CALL ARCRDI(IDIPS,3,4,2,.FALSE.)
        CALL ARCOLI(IDIPS,-2)
        IF (IRADG.GT.0) THEN
          IDIPS=IDIPS+1
          CALL ARCRDI(IDIPS,4,3,1,.TRUE.)
        ENDIF
        IPF(2)=3
        IPL(2)=4
        IFLOW(2)=ITYP
        ISTRS=2
        IPART=4
        IMF=3
        IML=6
        PT2LST=PARA(40)
        IF (PARA(6).GT.0.0) PT2LST=MIN(PT2LST,PARA(6))
        ISTRS=2
        QDUMP=.FALSE.
C...Perform the cascade without crosstalk between colour singlet systems
        CALL ARCASC
C...If the cascade was limited in gluon energy, allow for cross talk
C...between the colour singlet systems and perform emissions below
C...that energy.
        IF (PARA(28).GT.0.AND.MHAR(101).EQ.2.AND.MSTA(35).EQ.2) THEN
          DO 150 ID=1,IDIPS
            IF (QEM(ID)) GOTO 150
            QDONE(ID)=.FALSE.
            ICOLI(ID)=MOD(ICOLI(ID),1000)
 150      CONTINUE
          PARA(28)=-PARA(28)
          PT2LST=PARA(40)
          CALL ARCONT
          PARA(28)=-PARA(28)
        ENDIF

      ENDIF

C...Do fragmentation and decays. Possibly except tau decay.
      IF (ITAU.EQ.0) THEN
        DO 130 I=3,6
  130   IF (IABS(K(I,2)).EQ.15) K(I,1)=11
      ENDIF
      CALL LUEXEC
      IF (ITAU.EQ.0) THEN
        DO 140 I=3,6
  140   IF (IABS(K(I,2)).EQ.15) K(I,1)=1
      ENDIF

      RETURN

C**** END OF AR4FRM ****************************************************
      END
