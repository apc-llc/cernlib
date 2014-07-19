C***********************************************************************
C $Id: ardyre.F,v 1.2 1996/04/10 12:33:10 mclareni Exp $

      REAL FUNCTION ARDYRE(IDE,BW,QRG1,QRG3)

C...ARiadne subroutine Drell-Yan REcoil treatment

C...Transfers the recoil of an emission to a Drell-Yan produced
C...particle if the emission and the particle are in the same
C...phase space region.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arint2.f"
#include "arhide.f"


      ARDYRE=-1.0

      QEXDY=((ABS(MHAR(123)).EQ.1.AND.IO.EQ.1).OR.MHAR(123).GT.1.OR.
     $     (MHAR(131).NE.0.AND.IO.EQ.1.AND.MHAR(132).EQ.1))

C...Check if there are recoil gluons
      IF (MSTA(22).EQ.0) RETURN
      IF (MSTA(22).LT.0.AND.(.NOT.QEXDY)
     $     .AND.(.NOT.QRG1).AND.(.NOT.QRG3)) RETURN

C...Locate Drell-Yan produced particle (IDY) and boost it to CMS 
C...of dipole
      IDY=MAXPAR-2
      CALL AROBO1(0.0,0.0,-DBEX,-DBEY,-DBEZ,IDY)
      CALL AROBO1(0.0,-PHI,0.0D0,0.0D0,0.0D0,IDY)
      CALL AROBO1(-THE,0.0,0.0D0,0.0D0,0.0D0,IDY)
      BZP=BP(IDY,4)+BP(IDY,3)
      BZM=BP(IDY,4)-BP(IDY,3)
      BZX=BP(IDY,1)
      BZY=BP(IDY,2)

C...Boost Gluon and check if we have overlap
      IG=IP3(IDE)
      CALL AROBO1(0.0,0.0,-DBEX,-DBEY,-DBEZ,IG)
      CALL AROBO1(0.0,-PHI,0.0D0,0.0D0,0.0D0,IG)
      CALL AROBO1(-THE,0.0,0.0D0,0.0D0,0.0D0,IG)
      BGP=BP(IG,4)+BP(IG,3)
      BGM=BP(IG,4)-BP(IG,3)

C...Check if we are inside D-Y triangle
      BFUZZ=SQRT(BZP*BZM)*PHAR(105)*LOG(RLU(0))
      IF (ABS(MSTA(22)).LT.3.AND.(.NOT.QEXDY).AND.
     $     (BGP+BFUZZ.GT.BZP.OR.BGM+BFUZZ.GT.BZM)) THEN
        CALL AROBO2(THE,PHI,DBEX,DBEY,DBEZ,IG,IDY)
        RETURN
      ENDIF
      IF (ABS(MSTA(22)).EQ.3.AND.(.NOT.QEXDY).AND.
     $     BGP+BFUZZ.GT.BZP.AND.BGM+BFUZZ.GT.BZM) THEN
        CALL AROBO2(THE,PHI,DBEX,DBEY,DBEZ,IG,IDY)
        RETURN
      ENDIF

      I1=IP1(IDE)
      IF (QRG1) I1=IP1(IDI(I1))
      I3=IP3(IDO(IG))
      IF (QRG3) I3=IP3(IDO(I3))

      CALL AROBO2(0.0,0.0,-DBEX,-DBEY,-DBEZ,I1,I3)
      CALL AROBO2(0.0,-PHI,0.0D0,0.0D0,0.0D0,I1,I3)
      CALL AROBO2(-THE,0.0,0.0D0,0.0D0,0.0D0,I1,I3)

      BSX=BZX-BP(IG,1)
      BSY=BZY-BP(IG,2)
      B1X=0.0
      B1Y=0.0
      IF (.NOT.QRG1.AND.MSTA(22).LT.0) THEN
        B1X=BP(I1,1)
        B1Y=BP(I1,2)
        BSX=BSX-B1X
        BSY=BSY-B1Y
      ENDIF
      B3X=0.0
      B3Y=0.0
      IF (.NOT.QRG3.AND.MSTA(22).LT.0) THEN
        B3X=BP(I3,1)
        B3Y=BP(I3,2)
        BSX=BSX-B3X
        BSY=BSY-B3Y
      ENDIF

      BA2=(BP(IDY,5)**2+BSX**2+BSY**2)/(BZP*BZM)
      IF (BA2.LE.0.0) GOTO 900
      BA=SQRT(BA2)
      BNZP=BA*BZP
      BNZM=BA*BZM

      BTP=BW+BZP-BGP-BNZP
      BTM=BW+BZM-BGM-BNZM
      IF (BTP.LE.0.0.OR.BTM.LE.0.0) GOTO 900
      B1T2=B1X**2+B1Y**2+BP(I1,5)**2
      B3T2=B3X**2+B3Y**2+BP(I3,5)**2
      BARG=(B1T2-B3T2+BTP*BTM)**2-4.0*BTP*BTM*B1T2
      IF (BARG.LT.0) GOTO 900
      B1P=0.5*(B1T2-B3T2+BTP*BTM+SQRT(BARG))/BTM
      IF (B1P**2.LE.B1T2) GOTO 900
      B1M=B1T2/B1P
      BARG=(B3T2-B1T2+BTP*BTM)**2-4.0*BTP*BTM*B3T2
      IF (BARG.LT.0) GOTO 900
      B3M=0.5*(B3T2-B1T2+BTP*BTM+SQRT(BARG))/BTP
      IF (B3M**2.LE.B3T2) GOTO 900
      B3P=B3T2/B3M

      IF (ABS(MSTA(22)).EQ.2) THEN
        IF (QRG1.AND.B1P.LT.BP(I1,4)+BP(I1,3)) GOTO 900
        IF (QRG3.AND.B3M.LT.BP(I3,4)-BP(I3,3)) GOTO 900
      ENDIF

      IF (QRG1) THEN
        CALL ARREMG(IP1(IDE))
        QRG1=.FALSE.
        ID=IDO(I1)
        IDE=ID
        IG=IP3(ID)
        I3=IP3(IDO(IG))
        IF (QRG3) I3=IP3(IDO(I3))
      ENDIF
      IF (QRG3) THEN
        CALL ARREMG(IP3(IDO(IG)))
        ID=IDO(I1)
        IDE=ID
        IG=IP3(ID)
        I3=IP3(IDO(IG))
        QRG3=.FALSE.
      ENDIF

      BP(I1,1)=B1X
      BP(I1,2)=B1Y
      BP(I1,3)=0.5*(B1P-B1M)
      BP(I1,4)=0.5*(B1P+B1M)

      BP(I3,1)=B3X
      BP(I3,2)=B3Y
      BP(I3,3)=0.5*(B3P-B3M)
      BP(I3,4)=0.5*(B3P+B3M)

      BP(IDY,1)=BSX
      BP(IDY,2)=BSY
      BP(IDY,3)=0.5*(BNZP-BNZM)
      BP(IDY,4)=0.5*(BNZP+BNZM)

      ARDYRE=1.0

 900  CALL AROBO4(THE,PHI,DBEX,DBEY,DBEZ,IG,I1,I3,IDY)

      RETURN

C**** END OF ARDYRE ****************************************************
      END
