*
* $Id: arputr.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: arputr.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arputr.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARPUTR(IH)
 
C...ARiadne subroutine PUT event Record on the stack
 
C...Stores the event record for later use
 
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
