*
* $Id: arstor.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
*
* $Log: arstor.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:04  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arstor.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
 
      SUBROUTINE ARSTOR(ID,IDS,IS1,IS3)
 
C...ARiadne subroutine STORe
 
C...Stores a dipole entry for later use
 
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
 
 
      IDS=MAXDIP
      SDIP(IDS)=SDIP(ID)
      IP1(IDS)=IP1(ID)
      IP3(IDS)=IP3(ID)
      BX1(IDS)=BX1(ID)
      BX3(IDS)=BX3(ID)
      PT2IN(IDS)=PT2IN(ID)
      AEX1(IDS)=AEX1(ID)
      AEX3(IDS)=AEX3(ID)
      QDONE(IDS)=QDONE(ID)
      QEM(IDS)=QEM(ID)
      IRAD(IDS)=IRAD(ID)
      ISTR(IDS)=ISTR(ID)
      ICOLI(IDS)=ICOLI(ID)
 
      I1=IP1(ID)
      I3=IP3(ID)
      IS1=MAXPAR-1
      IS3=MAXPAR
      DO 100 I=1,5
        BP(IS1,I)=BP(I1,I)
        BP(IS3,I)=BP(I3,I)
 100  CONTINUE
      IFL(IS1)=IFL(I1)
      IFL(IS3)=IFL(I3)
      QEX(IS1)=QEX(I1)
      QEX(IS3)=QEX(I3)
      XPA(IS1)=XPA(I1)
      XPA(IS3)=XPA(I3)
      XPMU(IS1)=XPMU(I1)
      XPMU(IS3)=XPMU(I3)
      PT2GG(IS1)=PT2GG(I1)
      PT2GG(IS3)=PT2GG(I3)
      QQ(IS1)=QQ(I1)
      QQ(IS3)=QQ(I3)
      IDI(IS1)=IDI(I1)
      IDI(IS3)=IDI(I3)
      IDO(IS1)=IDO(I1)
      IDO(IS3)=IDO(I3)
      INO(IS1)=INO(I1)
      INO(IS3)=INO(I3)
 
      RETURN
 
C**** END OF ARSTOR ****************************************************
      END
