C***********************************************************************
C $Id: arputr.F,v 1.2 1996/04/10 12:33:32 mclareni Exp $

      SUBROUTINE ARPUTR(IH)

C...ARiadne subroutine PUT event Record on the stack

C...Stores the event record for later use

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arstak.f"


      IF (IH.LT.1.OR.IH.GT.MAXSTK) THEN
        CALL ARERRM('ARPUTR',31,0)
        RETURN
      ENDIF

      IPARTP(IH)=IPART
      IDIPSP(IH)=IDIPS
      PT2LSP(IH)=PT2LST
      PT2MAP(IH)=PT2MAX
      IMFP(IH)=IMF
      IMLP(IH)=IML
      IOP(IH)=IO
      QDUMPP(IH)=QDUMP
      ISTRSP(IH)=ISTRS
      DO 100 IPASS=1,2
        IF (IPASS.EQ.1) THEN
          I1P=1
          I2P=IPART
          I1D=1
          I2D=IDIPS
          I1S=1
          I2S=ISTRS
        ELSE
          I1P=MAXPAR-4
          I2P=MAXPAR
          I1D=1
          I2D=0
          I1S=1
          I2S=0
        ENDIF
        DO 110 IP=I1P,I2P
          DO 120 J=1,5
            BPP(IP,J,IH)=BP(IP,J)
 120      CONTINUE
          IFLP(IP,IH)=IFL(IP)
          QEXP(IP,IH)=QEX(IP)
          QQP(IP,IH)=QQ(IP)
          IDIP(IP,IH)=IDI(IP)
          IDOP(IP,IH)=IDO(IP)
          INOP(IP,IH)=INO(IP)
          INQP(IP,IH)=INQ(IP)
          XPMUP(IP,IH)=XPMU(IP)
          XPAP(IP,IH)=XPA(IP)
          PT2GGP(IP,IH)=PT2GG(IP)
 110    CONTINUE
        DO 130 ID=I1D,I2D
          BX1P(ID,IH)=BX1(ID)
          BX3P(ID,IH)=BX3(ID)
          PT2INP(ID,IH)=PT2IN(ID)
          SDIPP(ID,IH)=SDIP(ID)
          IP1P(ID,IH)=IP1(ID)
          IP3P(ID,IH)=IP3(ID)
          AEX1P(ID,IH)=AEX1(ID)
          AEX3P(ID,IH)=AEX3(ID)
          QDONEP(ID,IH)=QDONE(ID)
          QEMP(ID,IH)=QEM(ID)
          IRADP(ID,IH)=IRAD(ID)
          ISTRP(ID,IH)=ISTR(ID)
          ICOLIP(ID,IH)=ICOLI(ID)
 130    CONTINUE
        DO 140 IS=I1S,I2S
          IPFP(IS,IH)=IPF(IS)
          IPLP(IS,IH)=IPL(IS)
          IFLOWP(IS,IH)=IFLOW(IS)
 140    CONTINUE
 100  CONTINUE

      RETURN

C**** END OF ARPUPO ****************************************************
      END
C***********************************************************************
C $Id: arputr.F,v 1.2 1996/04/10 12:33:32 mclareni Exp $

      SUBROUTINE ARGETR(IH)

C...ARiadne subroutine GET event Record from stack

C...Restores an event record from the stack

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arstak.f"


      IF (IH.LT.1.OR.IH.GT.MAXSTK) THEN
        CALL ARERRM('ARGETR',31,0)
        RETURN
      ENDIF

      IPART=IPARTP(IH)
      IDIPS=IDIPSP(IH)
      PT2LST=PT2LSP(IH)
      PT2MAX=PT2MAP(IH)
      IMF=IMFP(IH)
      IML=IMLP(IH)
      IO=IOP(IH)
      QDUMP=QDUMPP(IH)
      ISTRS=ISTRSP(IH)
      DO 100 IPASS=1,2
        IF (IPASS.EQ.1) THEN
          I1P=1
          I2P=IPART
          I1D=1
          I2D=IDIPS
          I1S=1
          I2S=ISTRS
        ELSE
          I1P=MAXPAR-4
          I2P=MAXPAR
          I1D=1
          I2D=0
          I1S=1
          I2S=0
        ENDIF
        DO 110 IP=I1P,I2P
          DO 120 J=1,5
            BP(IP,J)=BPP(IP,J,IH)
 120      CONTINUE
          IFL(IP)=IFLP(IP,IH)
          QEX(IP)=QEXP(IP,IH)
          QQ(IP)=QQP(IP,IH)
          IDI(IP)=IDIP(IP,IH)
          IDO(IP)=IDOP(IP,IH)
          INO(IP)=INOP(IP,IH)
          INQ(IP)=INQP(IP,IH)
          XPMU(IP)=XPMUP(IP,IH)
          XPA(IP)=XPAP(IP,IH)
          PT2GG(IP)=PT2GGP(IP,IH)
 110    CONTINUE
        DO 130 ID=I1D,I2D
          BX1(ID)=BX1P(ID,IH)
          BX3(ID)=BX3P(ID,IH)
          PT2IN(ID)=PT2INP(ID,IH)
          SDIP(ID)=SDIPP(ID,IH)
          IP1(ID)=IP1P(ID,IH)
          IP3(ID)=IP3P(ID,IH)
          AEX1(ID)=AEX1P(ID,IH)
          AEX3(ID)=AEX3P(ID,IH)
          QDONE(ID)=QDONEP(ID,IH)
          QEM(ID)=QEMP(ID,IH)
          IRAD(ID)=IRADP(ID,IH)
          ISTR(ID)=ISTRP(ID,IH)
          ICOLI(ID)=ICOLIP(ID,IH)
 130    CONTINUE
        DO 140 IS=I1S,I2S
          IPF(IS)=IPFP(IS,IH)
          IPL(IS)=IPLP(IS,IH)
          IFLOW(IS)=IFLOWP(IS,IH)
 140    CONTINUE
 100  CONTINUE

      RETURN

C**** END OF ARGETR ****************************************************
      END
