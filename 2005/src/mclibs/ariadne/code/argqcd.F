C***********************************************************************
C $Id: argqcd.F,v 1.2 1996/04/10 12:33:17 mclareni Exp $

      SUBROUTINE ARGQCD(IDIN)

C...ARiadne subroutine Generate pt2 for QCD emission.

C...Generates a p_t^2 for a possible QCD emission from dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arhide.f"
#include "arint1.f"
#include "ludat1.f"


C...Copy some information from dipole record if ID < 1 these information
C...Has already been copied
C...S      = the invariant mass squared
C...W      = total energy in dipole
C...XT2MP  = maximum allowed fractional p_t^2 (x_t^2) for restricted  
C...         phase space option
C...QQ1(3) = Boolean variable 'is quark' for parton 1(3)
C...QE1(3) = true if parton 1(3) is extended
C...ALP1(3)= alpha parameter of parton 1(3)
C...XMU1(3)= mu parameter of parton 1(3)
C...SY1(3) = fractional mass of parton 1(3)

      IF (IDIN.GT.0) THEN
        ID=IDIN
        PT2IN(ID)=0.0
        S=SDIP(ID)
        IF (S.LT.4.0*PARA(3)**2) RETURN
        W=SQRT(S)
        XT2MP=PT2LST/S
        QQ1=QQ(IP1(ID))
        QQ3=QQ(IP3(ID))
        QE1=QEX(IP1(ID))
        QE3=QEX(IP3(ID))
        ALP1=XPA(IP1(ID))
        ALP3=XPA(IP3(ID))
        XMU1=XPMU(IP1(ID))
        XMU3=XPMU(IP3(ID))
        SY1=BP(IP1(ID),5)/W
        SY3=BP(IP3(ID),5)/W
        IFL1=IFL(IP1(ID))
        IFL3=IFL(IP3(ID))
        XT2GG1=-1.0
        XT2GG3=-1.0
        IF ((.NOT.QQ1).AND.(.NOT.QE1)) THEN
          XT2GG1=XT2MP
          IF (INO(IP1(ID)).NE.1) XT2GG1=PT2GG(IP1(ID))/S
        ENDIF
        IF ((.NOT.QQ3).AND.(.NOT.QE3)) THEN
          XT2GG3=XT2MP
          IF (INO(IP3(ID)).NE.1) XT2GG3=PT2GG(IP3(ID))/S
        ENDIF
        IF (PARA(19).LT.0.0) CALL ARPRGC(ID)
      ELSE
        ID=-IDIN
        PT2IN(ID)=0.0
        IF (S.LT.4.0*PARA(3)**2) RETURN
        W=SQRT(S)
      ENDIF

      IF (S.LT.4.0*PARA(3)**2) RETURN

C...XLAM = scaled lambda_QCD squared
      XLAM2=PARA(1)**2/S

C...alpha_0 for alpha_QCD = alpha_0/ln(p_t^2/lambda_QCD^2)
      XNUMFL=MAX(ARNOFL(W,MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)

C...Normal gluon emission
      CALL ARGQCG(ID)

C...q-qbar emission
      IF (MSTA(15).GT.0) CALL ARGQCQ(ID)

      RETURN

C**** END OF ARGQCD ****************************************************
      END
C***********************************************************************
C $Id: argqcd.F,v 1.2 1996/04/10 12:33:17 mclareni Exp $

      SUBROUTINE ARGQCG(ID)

C...ARiadne subroutine Generate pt2 for QCD emission.

C...Generates a p_t^2 for a possible QCD emission from dipole ID

#include "arimpl.f"
#include "ardips.f"
#include "arpart.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arint1.f"
#include "arint2.f"
#include "arhide.f"
#include "ludat1.f"
      EXTERNAL ARNDX1,ARNDX2,ARNDY1,ARNDY2,ARVET3,ARVET4
      REAL ARNDX1,ARNDX2,ARNDY1,ARNDY2,ARVET3,ARVET4


C...C = colour factors etc. in cross section
      C=6.0/(4.0*PARU(1))
      IF (QQ1.AND.QQ3) C=4.0/(3.0*PARU(1))
      SY2=0.0
      SQ2=0.0

C...Set exponents in cross section
      NXP1=3
      NXP3=3
      IF (QQ1) NXP1=2
      IF (QQ3) NXP3=2

C...Flavour of this emission 0 = gluon emission
      IFLG=0

C...Calculate mass dependent parameters
      CALL ARMADE

C...Allow extra phase space when Drell-Yan process
      QEXDY=.FALSE.
      IF (((ABS(MHAR(123)).EQ.1.AND.IO.EQ.0).OR.MHAR(123).GT.1.OR.
     $     (MHAR(131).NE.0.and.IO.EQ.0))
     $     .AND.QQ(MAXPAR-2)) THEN
        QEXDY=.TRUE.
        IDY=MAXPAR-2
        CALL ARBOCM(ID)
        CALL AROBO1(0.0,0.0,-DBEX,-DBEY,-DBEZ,IDY)
        CALL AROBO1(0.0,-PHI,0.0D0,0.0D0,0.0D0,IDY)
        CALL AROBO1(-THE,0.0,0.0D0,0.0D0,0.0D0,IDY)
        BPDY=BP(IDY,4)+BP(IDY,3)
        BMDY=BP(IDY,4)-BP(IDY,3)
        CALL AROBO1(THE,PHI,DBEX,DBEY,DBEZ,IDY)
        CALL AROBO2(0.0,0.0,DBEX,DBEY,DBEZ,IP1(ID),IP3(ID))
      ENDIF

C...Minimum x_t^2
      XT2C=MAX(PT2IN(ID),PARA(3)**2)/S
      XT2=0.0

C...Set maximum x_t^2
      IF (MSTA(11).LT.4) XT2M=MIN(XT2M,XT2MP)

      IF (XT2M.LE.XT2C) GOTO 900

C...Set additional parameters and call the veto algorith with
C...Suitable random functions
      IF (MSTA(12).GT.0) THEN
C.......Running alpha_QDC
        YINT=2.0*LOG(0.5/SQRT(XLAM2)+SQRT(0.25/XLAM2-1.0))
        CN=1.0/(YINT*C*ALPHA0)
        IF (QE1.OR.QE3) THEN
C.........Extended dipole
          CALL ARMCDI(ARNDX1,ARNDY2,ARVET4)
        ELSE
C.........Pointlike dipole
          CALL ARMCDI(ARNDX1,ARNDY1,ARVET4)
        ENDIF
      ELSE
C.......Constant alpha_QCD
        YINT=1.0
        CN=2.0/(C*PARA(2))
        IF (QE1.OR.QE3) THEN
C.........Extended dipole
          CALL ARMCDI(ARNDX2,ARNDY2,ARVET3)
        ELSE
C.........Pointlike dipole
          CALL ARMCDI(ARNDX2,ARNDY1,ARVET3)
        ENDIF
      ENDIF

C...Save the generated values of p_t^2, x1, x3, a1 and a3
      IF (XT2.GT.XT2C) THEN
        PT2IN(ID)=XT2*S
        BX1(ID)=B1
        BX3(ID)=B3
        AEX1(ID)=AE1
        AEX3(ID)=AE3
        IRAD(ID)=0
      ENDIF

 900  CONTINUE

      QEXDY=.FALSE.

      RETURN

C**** END OF ARGQCG ****************************************************
      END
C***********************************************************************
C $Id: argqcd.F,v 1.2 1996/04/10 12:33:17 mclareni Exp $

      SUBROUTINE ARGQCQ(ID)

C...ARiadne subroutine Generate pt2 for QCD emission.

C...Generates a p_t^2 for a possible QCD emission from dipole ID

#include "arimpl.f"
#include "ardips.f"
#include "ardat1.f"
#include "ardat2.f"
#include "arint1.f"
#include "ludat1.f"
      EXTERNAL ARNDX1,ARNDX3,ARNDY3,ARNDY4,ARVET5
      REAL ARNDX1,ARNDX3,ARNDY3,ARNDY4,ARVET5


C...Exit if no point-like gluons
      QG1=((.NOT.QQ1).AND.(.NOT.QE1))
      QG3=((.NOT.QQ3).AND.(.NOT.QE3))
      IF ((.NOT.QG1).AND.(.NOT.QG3)) RETURN

C...Colour factors and things in cross section. If g-g dipole
C...q-qbar splitting only calculated for one gluon but double
C...cross section
      C=1.0/(8.0*PARU(1))
      IF (QG1.AND.QG3) C=C*2.0
      SQ2=0.0

C...Parton 3 is always assumed to be split
      IF (QG1) THEN
        SY1=SY3
        QE1=QE3
        QE3=.FALSE.
        XMU1=XMU3
        ALP1=ALP3
        XMU3=0.0
        ALP3=0
      ENDIF

C...set 'minimum' XT2 to the XT2 of the gluon emission. XT2s below that
C...are not relevant

C...Loop over allowed flavours
      DO 100 IFLG=1,MSTA(15)

C...Set mass dependent parameters
        SY2=PQMAS(IFLG)/W
        SY3=SY2
        CALL ARMADE

C...Set phase space restrictions
        IF (MSTA(11).LT.2.AND.MSTA(28).GE.0) XT2M=MIN(XT2M,XT2MP)

C...Exit if not enough energy
        XT2C=MAX(PT2IN(ID),PARA(3)**2)/S
        XT2=0.0
        IF (XT2M.LE.XT2C.OR.SSY.GE.1.0) GOTO 900

C...Set additional parameters and call the veto algorith with
C...Suitable random functions
        YINT=2.0*SQRT(S)
C.......Running alpha_QCD
        IF (MSTA(12).GT.0) THEN
          CN=1.0/(YINT*C*ALPHA0)
          IF (QE1.OR.QE3) THEN
C...........extended dipole
            CALL ARMCDI(ARNDX1,ARNDY4,ARVET5)
          ELSE
C...........pointlike dipole
            CALL ARMCDI(ARNDX1,ARNDY3,ARVET5)
          ENDIF
        ELSE
C.........Constant alpha_QCD
          CN=2.0/(YINT*C*PARA(2))
          IF (QE1.OR.QE3) THEN
C...........extended dipole
            CALL ARMCDI(ARNDX3,ARNDY4,ARVET5)
          ELSE
C...........pointlike dipole
            CALL ARMCDI(ARNDX3,ARNDY3,ARVET5)
          ENDIF
        ENDIF

C...If Generated XT2 is larger than previous XT2 accept this and save
C...the generated values of p_t^2, x1, x3, a1 and a3
        IF (XT2.GT.XT2C) THEN
          PT2IN(ID)=XT2*S
          BX1(ID)=B1
          BX3(ID)=B3
          AEX1(ID)=AE1
          AEX3(ID)=AE3
          IRAD(ID)=IFLG
        ENDIF

 100  CONTINUE

C...Exit if gluon emission was chosen 
 900  IF (IRAD(ID).EQ.0) RETURN

C...Select wich gluon to split
      QFORCE=.FALSE.
      IF (MSTA(28).NE.0) THEN
        SMQQ=1.0-BX1(ID)+Y1
        IF (ABS(MSTA(28)).EQ.2)
     $       SMQQ=SMQQ-4.0*(PQMAS(ABS(IRAD(ID)))/W)**2
        IF (XT2GG1.GT.0.0.AND.(.NOT.QG3).AND.SMQQ.GT.XT2GG1) RETURN
        IF (XT2GG3.GT.0.0.AND.SMQQ.GT.XT2GG3) QFORCE=.TRUE.
      ENDIF

      IF ((.NOT.QG3).OR.(QG1.AND.(RLU(IDUM).GT.0.5.OR.QFORCE))) THEN
        IRAD(ID)=-IRAD(ID)
        B1=BX1(ID)
        BX1(ID)=BX3(ID)
        BX3(ID)=B1
        AE1=AEX1(ID)
        AEX1(ID)=AEX3(ID)
        AEX3(ID)=AE1
      ENDIF

      RETURN

C**** END OF ARGQCQ ****************************************************
      END
C***********************************************************************
C $Id: argqcd.F,v 1.2 1996/04/10 12:33:17 mclareni Exp $

      SUBROUTINE ARPRGC(ID)

C...ARiadne subroutine PRepare Gluon Check.

C...Set up variables to be used by ARCHKI to perform safety check on
C...gluon emission to prevent trouble for subsequent initial state gluon
C...splitting.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arint1.f"
#include "arhide.f"
#include "arlist.f"
#include "lujets.f"


      KQ1=0
      KQ3=0
      IF (MSTA(32).LT.2) RETURN

      IF (.NOT.(QQ(MAXPAR-3).OR.QQ(MAXPAR-4))) RETURN
      IRP=MAXPAR-3
      IF (QQ(IRP)) THEN
        IT=2
        IDIR=IRDIR(IT)
        IR=INQ(IRP)
        PMT=P(IT,4)+IDIR*P(IT,3)
        IF (IR.EQ.IP1(ID)) THEN
          KF1=K(IT,2)
          KQ1=IDO(IRP)
          BMRP1=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR1=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ELSEIF (IR.EQ.IP3(ID)) THEN
          KF3=K(IT,2)
          KQ3=IDO(IRP)
          BMRP3=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR3=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ENDIF
      ENDIF
      IRP=MAXPAR-4
      IF (QQ(IRP)) THEN
        IT=1
        IDIR=IRDIR(IT)
        IR=INQ(IRP)
        PMT=P(IT,4)+IDIR*P(IT,3)
        IF (IR.EQ.IP1(ID)) THEN
          KF1=K(IT,2)
          KQ1=IDO(IRP)
          BMRP1=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR1=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ELSEIF (IR.EQ.IP3(ID)) THEN
          KF3=K(IT,2)
          KQ3=IDO(IRP)
          BMRP3=(BP(IRP,4)+IDIR*BP(IRP,3))/PMT
          BMR3=(BP(IR,4)+IDIR*BP(IR,3))/PMT
        ENDIF
      ENDIF

      RETURN

C**** END OF ARGQCQ ****************************************************
      END
