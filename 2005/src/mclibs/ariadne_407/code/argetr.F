*
* $Id: argetr.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
*
* $Log: argetr.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:07  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: argetr.F,v 1.1.1.1 1996/03/08 16:51:07 mclareni Exp $
 
      SUBROUTINE ARGETR(IH)
 
C...ARiadne subroutine GET event Record from stack
 
C...Restores an event record from the stack
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),QEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),INQ(MAXPAR),
     $                XPMU(MAXPAR),XPA(MAXPAR),PT2GG(MAXPAR),IPART
      SAVE /ARPART/
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      PARAMETER (MAXSTK=2)
      COMMON /ARSTAK/ BPP(MAXPAR,5,MAXSTK),IFLP(MAXPAR,MAXSTK),
     $                QEXP(MAXPAR,MAXSTK),QQP(MAXPAR,MAXSTK),
     $                IDIP(MAXPAR,MAXSTK),IDOP(MAXPAR,MAXSTK),
     $                INOP(MAXPAR,MAXSTK),INQP(MAXPAR,MAXSTK),
     $                XPMUP(MAXPAR,MAXSTK),XPAP(MAXPAR,MAXSTK),
     $                PT2GGP(MAXPAR,MAXSTK),IPARTP(MAXSTK),
     $                BX1P(MAXDIP,MAXSTK),BX3P(MAXDIP,MAXSTK),
     $                PT2INP(MAXDIP,MAXSTK),SDIPP(MAXDIP,MAXSTK),
     $                IP1P(MAXDIP,MAXSTK),IP3P(MAXDIP,MAXSTK),
     $                AEX1P(MAXDIP,MAXSTK),AEX3P(MAXDIP,MAXSTK),
     $                QDONEP(MAXDIP,MAXSTK),QEMP(MAXDIP,MAXSTK),
     $                IRADP(MAXDIP,MAXSTK),ISTRP(MAXDIP,MAXSTK),
     $                ICOLIP(MAXDIP,MAXSTK),IDIPSP(MAXSTK),
     $                IPFP(MAXSTR,MAXSTK),IPLP(MAXSTR,MAXSTK),
     $                IFLOWP(MAXSTR,MAXSTK),PT2LSP(MAXSTK),
     $                PT2MAP(MAXSTK),IMFP(MAXSTK),IMLP(MAXSTK),
     $                IOP(MAXSTK),QDUMPP(MAXSTK),ISTRSP(MAXSTK)
      SAVE /ARSTAK/
 
 
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
