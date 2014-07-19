C***********************************************************************
C $Id: argqed.F,v 1.2 1996/04/10 12:33:17 mclareni Exp $

      SUBROUTINE ARGQED(ID)

C...ARiadne subroutine Generate pt2 for QED emission

C...Generates a p-t^2 for a possible QED emission from dipole ID.

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "ardat1.f"
#include "arint1.f"
#include "arhide.f"
#include "ludat1.f"
      EXTERNAL ARNDX2,ARNDY1,ARVET1
      REAL ARNDX2,ARNDY1,ARVET1


C...Copy information about partons in dipole (for explanation see
C...subroutine ARGQCD
      PT2IN(ID)=0.0
      S=SDIP(ID)
      IF (S.LE.4.0*PARA(5)**2) RETURN
      IF (MSTA(20).GE.2.AND.ISTRS.GE.2) RETURN
      W=SQRT(S)
      XT2MP=PT2LST/S
      QQ1=QQ(IP1(ID))
      QQ3=QQ(IP3(ID))
      QE1=QEX(IP1(ID))
      QE3=QEX(IP3(ID))

      QEXDY=.FALSE.

      SY1=BP(IP1(ID),5)/W
      SY2=0.0
      SY3=BP(IP3(ID),5)/W
      IF (PARA(19).LT.0.0) CALL ARPRGC(ID)

      XT2C=PARA(5)**2/S
      NXP1=2
      NXP3=2

C...Set charges of emitting quarks and set constant in cross section
      IQ1=LUCHGE(IFL(IP1(ID)))
      IQ3=LUCHGE(IFL(IP3(ID)))
      FQMAX=REAL(MAX(ABS(IQ1),ABS(IQ3)))
      FQ1=REAL(IQ1)/FQMAX
      FQ3=REAL(IQ3)/FQMAX
      C=(FQMAX**2)/(9.0*PARU(1))
      IFLG=-1

C...Set mass dependent parameters
      CALL ARMADE

C...Restrict phase space if demanded
      IF (MSTA(11).EQ.0.OR.MSTA(11).EQ.2) XT2M=MIN(XT2M,XT2MP)

C...Set some further parameters and call the veto algorithm with
C...suitable random functions for constant alpha_EM.
      YINT=1.0
      CN=2.0/(C*PARA(4))
      CALL ARMCDI(ARNDX2,ARNDY1,ARVET1)

C...Save information about emission
      PT2IN(ID)=XT2*S
      BX1(ID)=B1
      BX3(ID)=B3
      AEX1(ID)=AE1
      AEX3(ID)=AE3

      RETURN

C**** END OF ARGQED ****************************************************
      END
