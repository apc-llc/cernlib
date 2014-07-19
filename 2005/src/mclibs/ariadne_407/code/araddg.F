*
* $Id: araddg.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: araddg.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: araddg.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARADDG(ID,ICLOSE)
 
C...ARiadne subroutine ADD Gluon
 
C...Adds a gluon entry between the partons in dipole ID thus creating
C...a new dipole
 
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
 
      INXT(I)=IDO(IP3(I))
      IPRV(I)=IDI(IP1(I))
 
 
C...Allocate new gluon and new dipole at postitons IPART+1 and IDIPS+1
C...if there is space left.
      IPART=IPART+1
      IDIPS=IDIPS+1
      IF (IPART.GE.MAXPAR-10) CALL ARERRM('ARADDG',6,0)
      IF (IDIPS.GE.MAXDIP-10) CALL ARERRM('ARADDG',7,0)
 
C...Set properties of new gluon
      DO 100 I=1,5
        BP(IPART,I)=0.0
 100  CONTINUE
      IFL(IPART)=21
      QEX(IPART)=.FALSE.
      XPMU(IPART)=0.0
      XPA(IPART)=0.0
      QQ(IPART)=.FALSE.
      INQ(IPART)=0
      IDI(IPART)=ID
      IDO(IPART)=IDIPS
      PT2GG(IPART)=PT2IN(ID)
 
C...Set properties of new dipole
      IP1(IDIPS)=IPART
      IP3(IDIPS)=IP3(ID)
      QDONE(IDIPS)=.FALSE.
      QEM(IDIPS)=.FALSE.
      ISTR(IDIPS)=ISTR(ID)
      PT2IN(IDIPS)=PT2IN(ID)
 
C...Fix pointers for old dipole
      IP3(ID)=IPART
      IDI(IP3(IDIPS))=IDIPS
      IF (IPRV(ID).NE.0) QDONE(IPRV(ID))=.FALSE.
      QDONE(ID)=.FALSE.
      IF (INXT(IDIPS).NE.0) QDONE(INXT(IDIPS))=.FALSE.
 
      IS=ISTR(ID)
      IF (IFLOW(IS).EQ.2) THEN
        IPF(IS)=IP3(ID)
        IPL(IS)=IP1(ID)
      ENDIF
 
C...Fix colour assignment
      ISCOL=ICOLI(ID)/1000
      IF (ICLOSE.EQ.1) THEN
        ICOLI(IDIPS)=ICOLI(ID)
        CALL ARCOLI(ID,-ISCOL)
      ELSE
        CALL ARCOLI(IDIPS,-ISCOL)
      ENDIF
 
      RETURN
 
C**** END OF ARADDG ****************************************************
      END
