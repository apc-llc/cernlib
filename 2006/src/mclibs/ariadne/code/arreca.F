C***********************************************************************
C $Id: arreca.F,v 1.2 1996/04/10 12:33:35 mclareni Exp $

      SUBROUTINE ARRECA(ID,IDS,IS1,IS3)

C...ARiadne function RECAll

C...Recalls a dipole entry stored by ARSTOR

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"


      SDIP(ID)=SDIP(IDS)
      IP1(ID)=IP1(IDS)
      IP3(ID)=IP3(IDS)
      BX1(ID)=BX1(IDS)
      BX3(ID)=BX3(IDS)
      PT2IN(ID)=PT2IN(IDS)
      AEX1(ID)=AEX1(IDS)
      AEX3(ID)=AEX3(IDS)
      QDONE(ID)=QDONE(IDS)
      QEM(ID)=QEM(IDS)
      IRAD(ID)=IRAD(IDS)
      ISTR(ID)=ISTR(IDS)
      ICOLI(ID)=ICOLI(IDS)

      I1=IP1(ID)
      I3=IP3(ID)

      DO 100 I=1,5
        BP(I1,I)=BP(IS1,I)
        BP(I3,I)=BP(IS3,I)
 100  CONTINUE
      IFL(I1)=IFL(IS1)
      IFL(I3)=IFL(IS3)
      QEX(I1)=QEX(IS1)
      QEX(I3)=QEX(IS3)
      XPA(I1)=XPA(IS1)
      XPA(I3)=XPA(IS3)
      XPMU(I1)=XPMU(IS1)
      XPMU(I3)=XPMU(IS3)
      PT2GG(I1)=PT2GG(IS1)
      PT2GG(I3)=PT2GG(IS3)
      QQ(I1)=QQ(IS1)
      QQ(I3)=QQ(IS3)
      IDI(I1)=IDI(IS1)
      IDI(I3)=IDI(IS3)
      IDO(I1)=IDO(IS1)
      IDO(I3)=IDO(IS3)
      INO(I1)=INO(IS1)
      INO(I3)=INO(IS3)

      RETURN

C**** END OF ARRECA ****************************************************
      END
