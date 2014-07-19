C***********************************************************************
C $Id: argpt2.F,v 1.2 1996/04/10 12:33:16 mclareni Exp $

      REAL FUNCTION ARGPT2(ID)

C...ARiadne function Generate PT2

C...Returns the p_t^2 for a possible emission from dipole ID.

#include "arimpl.f"
#include "ardat1.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arhide.f"
#include "leptou.f"


C...Set invariant mass squared in the dipole and generate a p_t^2
C...with the appropriate Monte Carlo subroutine
      IF (QEM(ID).AND.MSTA(20).GE.2.AND.ISTRS.GE.2) THEN
        PT2IN(ID)=0.0
        QDONE(ID)=.TRUE.
      ENDIF
      IF (.NOT.QDONE(ID)) THEN
        IRAD(ID)=0
        SDIP(ID)=ARMAS2(IP1(ID),IP3(ID))
        IF (QEM(ID)) THEN
          CALL ARGQED(ID)
        ELSEIF ((MSTA(33).EQ.1.AND.MSTA(1).EQ.3.AND.IO.EQ.0).OR.
     $         (MSTA(1).EQ.2.AND.IO.EQ.0
     $         .AND.XQ2.GT.0.0.AND.MHAR(120).GT.0)) THEN
          CALL ARGDIS(ID)
        ELSEIF (MSTA(33).EQ.-1.AND.MSTA(1).EQ.3.AND.IO.EQ.0) THEN
          PSAV3=PARA(3)
          IF (PARA(20).GE.0.0) THEN
            PTCUT=MAX(SQRT(PARA(20)*PARA(21)*XQ2),PARA(3))
          ELSE
            PTCUT=MAX(SQRT(-PARA(20)*PARA(21)*W2),PARA(3))
          ENDIF
          PARA(3)=PTCUT
          CALL ARGDIS(ID)
          PARA(3)=PSAV3
          IF (PT2IN(ID).LT.PTCUT**2) THEN
            PT2LST=PTCUT**2
            CALL ARGQCD(ID)
          ENDIF
        ELSE
          CALL ARGQCD(ID)
        ENDIF
        QDONE(ID)=.TRUE.
      ENDIF

      IF (QQ(MAXPAR-3).AND.MSTA(32).GE.2) THEN
        IF (PT2GG(MAXPAR-3).LT.0.0.AND.
     $       (INQ(MAXPAR-3).EQ.IP1(ID).OR.INQ(MAXPAR-3).EQ.IP3(ID)))
     $       CALL ARGING(ID,MAXPAR-3)
      ENDIF
      IF (QQ(MAXPAR-4).AND.MSTA(32).GE.2) THEN
        IF (PT2GG(MAXPAR-4).LT.0.0.AND.
     $       (INQ(MAXPAR-4).EQ.IP1(ID).OR.INQ(MAXPAR-4).EQ.IP3(ID)))
     $       CALL ARGING(ID,MAXPAR-4)
      ENDIF

      ARGPT2=PT2IN(ID)

      RETURN

C**** END OF ARGPT2 ****************************************************
      END
