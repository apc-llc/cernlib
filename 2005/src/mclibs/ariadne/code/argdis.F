C***********************************************************************
C $Id: argdis.F,v 1.2 1996/04/10 12:33:15 mclareni Exp $

      SUBROUTINE ARGDIS(ID)

C...ARiadne subroutine Generate first Deep Inelastic Scattering emission

C...Generates a p-t^2 for a possible emission from an original dipole in
C...a DIS scattering according to O(alpha_S) matrix elements

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "arint1.f"
#include "arhide.f"
#include "ludat1.f"
#include "leptou.f"


C...Copy some information from dipole record
C...S      = the invariant mass squared
C...W      = total energy in dipole
C...XT2MP  = maximum allowed fractional p_t^2 (x_t^2) for restricted  
C...         phase space option
C...QQ1(3) = Boolean variable 'is quark' for parton 1(3)
C...QE1(3) = true if parton 1(3) is extended
C...ALP1(3)= alpha parameter of parton 1(3)
C...XMU1(3)= mu parameter of parton 1(3)
C...SY1(3) = fractional mass of parton 1(3)
      PT2IN(ID)=0.0
      S=SDIP(ID)
      IF (S.LE.4.0*PARA(3)**2) RETURN
      W=SQRT(S)
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
      IF (PARA(19).LT.0.0) CALL ARPRGC(ID)

      IF (S.LT.4.0*PARA(3)**2) RETURN

      IF (MSTA(36).EQ.-1) XMU3=PARA(11)

C...XLAM = scaled lambda_QCD squared
      XLAM2=PARA(1)**2/S

C...alpha_0 for alpha_QCD = alpha_0/ln(p_t^2/lambda_QCD^2)
      XNUMFL=MAX(ARNOFL(W,MAX(5,MSTA(15))),3.0)
      ALPHA0=12.0*PARU(1)/(33.0-2.0*XNUMFL)

C...Set Q^2 and Y dependencies for veto algorithm
      ZSQEV=S/W2
      SQ2=XQ2/W2
      YFAC=2.0*(1.0-XY)/(1.0+(1.0-XY)**2)

C...Call veto algorithm
      CALL ARGDIG(ID)


      RETURN

C**** END OF ARGDIS ****************************************************
      END
C***********************************************************************
C $Id: argdis.F,v 1.2 1996/04/10 12:33:15 mclareni Exp $

      SUBROUTINE ARGDIG(ID)

C...ARiadne subroutine Generate first Deep Inelastic Scattering emission

C...Generates a p-t^2 for a possible emission from an original dipole in
C...a DIS scattering according to O(alpha_S) matrix elements

#include "arimpl.f"
#include "ardips.f"
#include "ardat1.f"
#include "arint1.f"
#include "arhide.f"
#include "ludat1.f"

      EXTERNAL ARNDX1,ARNDX2,ARNDY2,ARVET6,ARVET7
      REAL ARNDX1,ARNDX2,ARNDY2,ARVET6,ARVET7


C...First gluon emission
      SY2=0.0
      IFLG=0

C...Calculate mass dependent parameters
      CALL ARMADE

C...C = colour factors etc. in cross section
      C=2.0*(3.0+4.0*YFAC/27.0)/(3.0*PARU(1))
      IF (MHAR(116).LE.0) THEN
        C=2.0*(6.0+0.25*YFAC)/(3.0*PARU(1))
      ENDIF

C...Minimum x_t^2
      XT2C=MAX(PT2IN(ID),PARA(3)**2)/S
      XT2=0.0

      IF (XT2M.LE.XT2C) GOTO 900

C...Set additional parameters and call the veto algorith with
C...Suitable random functions
      IF (MSTA(12).GT.0) THEN
C.......Running alpha_QDC
        YINT=2.0*LOG(0.5/SQRT(XLAM2)+SQRT(0.25/XLAM2-1.0))
        CN=1.0/(YINT*C*ALPHA0)
        CALL ARMCDI(ARNDX1,ARNDY2,ARVET6)
      ELSE
C.......Constant alpha_QCD
        YINT=1.0
        CN=2.0/(C*PARA(2))
        CALL ARMCDI(ARNDX2,ARNDY2,ARVET7)
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


      RETURN

C**** END OF ARGDIG ****************************************************
      END
