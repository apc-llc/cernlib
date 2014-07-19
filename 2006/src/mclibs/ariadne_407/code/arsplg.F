*
* $Id: arsplg.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
*
* $Log: arsplg.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:04  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arsplg.F,v 1.1.1.1 1996/03/08 16:51:04 mclareni Exp $
 
      SUBROUTINE ARSPLG(IG,IFLAV)
 
C...ARiadne subroutine SPLit Gluon
 
C...Splits a gluon entry into a q and a q-bar entry
 
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
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
      INXT(I)=IDO(IP3(I))
 
 
C...Allocate space for new parton and new string if there is room
      IPART=IPART+1
      ISTRS=ISTRS+1
 
      IF (IPART.GE.MAXPAR-10) CALL ARERRM('ARSPLG',6,0)
      IF (ISTRS.GT.MAXSTR) CALL ARERRM('ARSPLG',8,0)
 
C...Set new pointers
      IDP=IDI(IG)
      IDN=IDO(IG)
      IDO(IG)=0
      IDI(IPART)=0
      IDO(IPART)=IDN
 
      IP1(IDN)=IPART
 
      IS=ISTR(IDP)
 
C...If closed gluonic string, no new string is created. The colour flow
C...which was previously undefined is set randomly
      IF (IFLOW(IS).EQ.2) THEN
        ISTRS=ISTRS-1
        IFLOW(IS)=1
        IPF(IS)=IPART
        IPL(IS)=IG
        IF (RLU(IDUM).GT.0.5) IFLOW(IS)=-1
        IFL(IPART)=IFLAV*IFLOW(IS)
        IFL(IG)=-IFL(IPART)
 
C...If new string is created set pointers for its partons
      ELSE
        IFLOW(ISTRS)=IFLOW(IS)
        IPF(ISTRS)=IPART
        IPL(ISTRS)=IPL(IS)
        IPL(IS)=IG
        IFL(IPART)=IFLAV*IFLOW(IS)
        IFL(IG)=-IFL(IPART)
        IDNI=IDN
 100    ISTR(IDNI)=ISTRS
        IF (.NOT.QQ(IP3(IDNI))) THEN
          IDNI=INXT(IDNI)
          GOTO 100
        ENDIF
      ENDIF
 
C...Reset momenta for created quarks and flag affected dipoles
      DO 200 I=1,4
        BP(IG,I)=0.0
        BP(IPART,I)=0.0
 200  CONTINUE
      BP(IG,5)=PQMAS(IFLAV)
      BP(IPART,5)=PQMAS(IFLAV)
      QEX(IG)=.FALSE.
      QEX(IPART)=.FALSE.
      XPMU(IG)=0.0
      XPMU(IPART)=0.0
      XPA(IG)=0.0
      XPA(IPART)=0.0
      QQ(IG)=.TRUE.
      QQ(IPART)=.TRUE.
      QDONE(IDP)=.FALSE.
      QDONE(IDN)=.FALSE.
      INO(IG)=SIGN(1000*ABS(INO(IG))+IO,INO(IG))
      INO(IPART)=INO(IG)
      INQ(IPART)=IG
      INQ(IG)=IPART
 
      RETURN
 
C**** END OF ARSPLG ****************************************************
      END
