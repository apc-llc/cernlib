*
* $Id: aremi3.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: aremi3.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: aremi3.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE AREMI3(ID)
 
C...ARiadne subroutine EMIT version 3
 
C...Administers the an emission from dipole ID
 
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
      COMMON /ARLIST/ B1SAVE(2),B3SAVE(2),IPTOT(MAXPAR),NPTOT,
     $     IPSTQ(MAXPAR),NPSTQ,IPREM(MAXPAR),NPREM,IRDIR(2),
     $     YIQQ(2),PT2IQQ(2),PT2SAV(2),IRASAV(2),A1SAVE(2),A3SAVE(2)
 
      SAVE /ARLIST/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARHIDE/ PHAR(400),MHAR(400)
      SAVE /ARHIDE/
 
 
C...Save the event record in case we want to throw
      NFIRST=IPART+1
      IPP1=IP1(ID)
      IPP3=IP3(ID)
      JRAD=IRAD(ID)
      CALL ARPUTR(1)
 
C...IF Initial state gluon splitting, go ahead.
      IF (IO.EQ.1) MHAR(121)=2
      IF (IRAD(ID).GT.10000) THEN
        IF (IO.EQ.1) MHAR(121)=3
        CALL ARADIG(ID)
        GOTO 900
      ENDIF
 
C...Reset initial state gluon splitting generation
      PT2GG(MAXPAR-3)=-1.0
      PT2GG(MAXPAR-4)=-1.0
      NPTOT=0
 
C...If FSR photon emission go a head
      IF (QEM(ID)) THEN
        IF (IO.EQ.1) MHAR(121)=1
        CALL ARRADP(ID)
        GOTO 900
 
C...If q-qbar splitting go a head
      ELSEIF (IRAD(ID).NE.0) THEN
        IF (IO.EQ.1) MHAR(121)=4
        CALL ARRADQ(ID)
        GOTO 900
 
C...If gluon emission from point-like dipole or if no p_t-ordered
C...recoil gluon, go a head
      ELSEIF (((.NOT.QEX(IP1(ID))).AND.(.NOT.QEX(IP3(ID))))
     $             .OR.MSTA(18).EQ.0) THEN
        CALL ARRADG(ID,0,SNR,PT21,PT23)
        GOTO 900
      ENDIF
 
C...If p_t-ordered recoil gluon, first save initial configuration
C...Then perform trial emission
 
      CALL ARRADG(ID,0,SNR,PT21,PT23)
 
C...If no recoil gluon was produces keep trial emission
      IF (SNR.LE.1.0) GOTO 900
 
C...If two recoil gluons, tag the smallest one for p_t-ordering
      IF (AEX1(ID).LT.1.0.AND.AEX3(ID).LT.1.0) THEN
        IF ((MSTA(17).GE.2.AND.PT21.GE.PT23).OR.
     $       (MSTA(17).LT.2.AND.BX3(ID).GE.BX1(ID))) THEN
          IGR=3
          PT2RG=PT23
        ELSE
          IGR=1
          PT2RG=PT21
        ENDIF
 
C...If only one recoil gluon, tag it for p_t-ordering
      ELSEIF (AEX1(ID).LT.1.0.AND.AEX3(ID).GE.1.0) THEN
        IGR=1
        PT2RG=PT21
      ELSEIF (AEX1(ID).GE.1.0.AND.AEX3(ID).LT.1.0) THEN
        IGR=3
        PT2RG=PT23
      ENDIF
 
      QTHR2=(ARTHRW(ID,JRAD,IPP1,IPP3,NFIRST,IPART).LT.0)
 
      CALL ARPUTR(2)
      CALL ARGETR(1)
      IS1=IP1(ID)
      IS3=IP3(ID)
      CALL ARRADG(ID,IGR,SNREF,PT21,PT23)
 
      IF (IGR.EQ.1) THEN
        IDT=IDO(IS1)
      ELSE
        IDT=IDI(IS3)
      ENDIF
 
      QTHR3=(ARTHRW(ID,JRAD,IPP1,IPP3,NFIRST,IPART).LT.0)
      IF (QTHR3.AND.QTHR2) THEN
        CALL ARGETR(1)
        IO=IO-1
        QDONE(ID)=.FALSE.
      ELSEIF (QTHR2.AND.(.NOT.QTHR3)) THEN
        CALL ARGETR(2)
      ELSEIF ((.NOT.QTHR2).AND.(.NOT.QTHR3)) THEN
        IF (ARGPT2(IDT).LE.PT2RG) CALL ARGETR(2)
      ENDIF
      RETURN
 
 900  IF (ARTHRW(ID,JRAD,IPP1,IPP3,NFIRST,IPART).LT.0) THEN
        CALL ARGETR(1)
        IO=IO-1
        QDONE(ID)=.FALSE.
      ENDIF
 
      RETURN
 
C**** END OF AREMI3 ****************************************************
      END
