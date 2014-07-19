*
* $Id: arjoqq.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arjoqq.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arjoqq.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARJOQQ(IQ1,IQ2)
 
C...ARiadne subroutine JOin QQbar pair
 
C...Join quark and anti-quark entry IQ1 and IQ2 into a gluon entry
C...located in MIN(IQ1,IQ2). Disregard flavour consistency
 
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
 
 
      IG=MIN(IQ1,IQ2)
      IREM=MAX(IQ1,IQ2)
 
C...First remove any EM-dipoles
      IF (IDI(IREM).GT.0) THEN
        IF (QEM(IDI(IREM))) CALL ARREMD(IDI(IREM))
      ENDIF
      IF (IDO(IREM).GT.0) THEN
        IF (QEM(IDO(IREM))) CALL ARREMD(IDO(IREM))
      ENDIF
      IF (IDI(IG).GT.0) THEN
        IF (QEM(IDI(IG))) CALL ARREMD(IDI(IG))
      ENDIF
      IF (IDO(IG).GT.0) THEN
        IF (QEM(IDO(IG))) CALL ARREMD(IDO(IG))
      ENDIF
 
C...Check that quarks not directly connected and then join them
      IF (IDI(IG).GT.0) THEN
        IF (IDO(IG).GT.0) RETURN
        IF (IDI(IREM).GT.0) CALL AREVST(ISTR(IDI(IREM)))
        CALL ARJOST(ISTR(IDI(IG)),ISTR(IDO(IREM)),IG,IREM)
        IP1(IDO(IREM))=IG
        IDO(IG)=IDO(IREM)
        IDO(IREM)=0
      ELSE
        IF (IDO(IG).LE.0) RETURN
        IF (IDO(IREM).GT.0) CALL AREVST(ISTR(IDO(IREM)))
        CALL ARJOST(ISTR(IDO(IG)),ISTR(IDI(IREM)),IG,IREM)
        IP3(IDI(IREM))=IG
        IDI(IG)=IDI(IREM)
        IDI(IREM)=0
      ENDIF
 
C...Remove quark entry
      CALL ARREMP(IREM)
      QQ(IG)=.FALSE.
      IFL(IG)=21
      PT2GG(IG)=0.0
      INQ(IG)=0
 
      IF (ICOLI(IDI(IG)).EQ.ICOLI(IDO(IG))) THEN
        ISCOL=ICOLI(IDI(IG))/1000
        ICOLI(IDI(IG))=0
        ICOLI(IDO(IG))=0
        CALL ARCOLI(IDI(IG),-ISCOL)
        CALL ARCOLI(IDO(IG),-ISCOL)
      ENDIF
 
      RETURN
 
C**** END OF ARJOQQ ****************************************************
      END
