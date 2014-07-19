*
* $Id: aremi2.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: aremi2.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: aremi2.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE AREMI2(ID)
 
C...ARiadne subroutine EMIT version 2
 
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
 
      DIMENSION IPEM1(MAXPAR),IPEM2(MAXPAR),IDEM1(MAXDIP),IDEM2(MAXDIP)
 
      INXT(I)=IDO(IP3(I))
 
C...IF Initial state gluon splitting, go ahead.
      IF (IO.EQ.1) MHAR(121)=2
      IF (IRAD(ID).GT.10000) THEN
        IF (IO.EQ.1) MHAR(121)=3
        CALL ARADIG(ID)
        GOTO 990
      ENDIF
 
C...Reset initial state gluon splitting generation
      PT2GG(MAXPAR-3)=-1.0
      PT2GG(MAXPAR-4)=-1.0
      NPTOT=0
 
C...If FSR photon emission go a head
      IF (QEM(ID)) THEN
        IF (IO.EQ.1) MHAR(121)=1
        CALL ARRADP(ID)
        RETURN
 
C...If q-qbar splitting go a head
      ELSEIF (IRAD(ID).NE.0) THEN
        IF (IO.EQ.1) MHAR(121)=4
        CALL ARRADQ(ID)
        RETURN
 
C...If gluon emission from point-like dipole or if no p_t-ordered
C...recoil gluon, go a head
      ELSEIF (((.NOT.QEX(IP1(ID))).AND.(.NOT.QEX(IP3(ID))))
     $             .OR.MSTA(18).EQ.0) THEN
        CALL ARRADG(ID,0,SNR,PT21,PT23)
        GOTO 990
      ENDIF
 
C...If p_t-ordered recoil gluon, first save initial configuration
C...Then perform trial emission
 
      NPEM1=0
      NDEM1=0
      CALL ARPADO(IP1(ID),NPEM1,IPEM1)
      CALL ARPADO(IP3(ID),NPEM1,IPEM1)
      CALL ARPADO(ID,NDEM1,IDEM1)
      CALL ARCODI(ID,IDS,IS1,IS3)
      NPEM2=0
      NDEM2=0
      CALL ARPADO(IS1,NPEM2,IPEM2)
      CALL ARPADO(IS3,NPEM2,IPEM2)
      CALL ARPADO(IDS,NDEM2,IDEM2)
      NPSAV=IPART
      NDSAV=IDIPS
      IPP1=IP1(ID)
      IPP3=IP3(ID)
      CALL ARRADG(ID,0,SNR,PT21,PT23)
      DO 100 I=NPSAV+1,IPART
        CALL ARPADO(I,NPEM1,IPEM1)
 100  CONTINUE
      DO 110 I=NDSAV+1,IDIPS
        CALL ARPADO(I,NDEM1,IDEM1)
 110  CONTINUE
 
C...If no recoil gluon was produces keep trial emission
      IF (SNR.LE.1.0) GOTO 910
 
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
 
      NPSAV=IPART
      NDSAV=IDIPS
      CALL ARRADG(IDS,IGR,SNREF,PT21,PT23)
      DO 120 I=NPSAV+1,IPART
        CALL ARPADO(I,NPEM2,IPEM2)
 120  CONTINUE
      DO 130 I=NDSAV+1,IDIPS
        CALL ARPADO(I,NDEM2,IDEM2)
 130  CONTINUE
 
      IF (IGR.EQ.1) THEN
        IDT=IDO(IS1)
      ELSE
        IDT=IDI(IS3)
      ENDIF
 
      NPTOT=0
      DO 200 I=1,IPART
        DO 210 J=1,NPEM1
          IF (IPEM1(J).EQ.I) GOTO 200
 210    CONTINUE
        CALL ARPADD(I,NPTOT,IPTOT)
 200  CONTINUE
      IF (QQ(MAXPAR-3)) THEN
        IF (INQ(MAXPAR-3).EQ.IPP1) INQ(MAXPAR-3)=IS1
        IF (INQ(MAXPAR-3).EQ.IPP3) INQ(MAXPAR-3)=IS3
        IF (IDI(MAXPAR-3).EQ.IPP1) IDI(MAXPAR-3)=IS1
        IF (IDI(MAXPAR-3).EQ.IPP3) IDI(MAXPAR-3)=IS3
      ENDIF
      IF (QQ(MAXPAR-4)) THEN
        IF (INQ(MAXPAR-4).EQ.IPP1) INQ(MAXPAR-4)=IS1
        IF (INQ(MAXPAR-4).EQ.IPP3) INQ(MAXPAR-4)=IS3
        IF (IDI(MAXPAR-4).EQ.IPP1) IDI(MAXPAR-4)=IS1
        IF (IDI(MAXPAR-4).EQ.IPP3) IDI(MAXPAR-4)=IS3
      ENDIF
 
      IF (ARGPT2(IDT).GT.PT2RG) GOTO 900
 
      IF (QQ(MAXPAR-3)) THEN
        IF (INQ(MAXPAR-3).EQ.IS1) INQ(MAXPAR-3)=IPP1
        IF (INQ(MAXPAR-3).EQ.IS3) INQ(MAXPAR-3)=IPP3
        IF (IDI(MAXPAR-3).EQ.IS1) IDI(MAXPAR-3)=IPP1
        IF (IDI(MAXPAR-3).EQ.IS3) IDI(MAXPAR-3)=IPP3
      ENDIF
      IF (QQ(MAXPAR-4)) THEN
        IF (INQ(MAXPAR-4).EQ.IS1) INQ(MAXPAR-4)=IPP1
        IF (INQ(MAXPAR-4).EQ.IS3) INQ(MAXPAR-4)=IPP3
        IF (IDI(MAXPAR-4).EQ.IS1) IDI(MAXPAR-4)=IPP1
        IF (IDI(MAXPAR-4).EQ.IS3) IDI(MAXPAR-4)=IPP3
      ENDIF
 910  DO 300 I=NPEM2,1,-1
        CALL ARREMP(IPEM2(I))
 300  CONTINUE
      DO 310 I=NDEM2,1,-1
        CALL ARREMD(IDEM2(I))
 310  CONTINUE
      PT2GG(MAXPAR-3)=-1.0
      PT2GG(MAXPAR-4)=-1.0
      NPTOT=0
 
      GOTO 990
 
 900  IF (IDI(IPP1).GT.0) IP3(IDI(IPP1))=IS1
      IF (IDO(IPP3).GT.0) IP1(IDO(IPP3))=IS3
      DO 320 IS=1,ISTRS
        IF (IPF(IS).EQ.IPP1) IPF(IS)=IS1
        IF (IPF(IS).EQ.IPP3) IPF(IS)=IS3
        IF (IPL(IS).EQ.IPP1) IPL(IS)=IS1
        IF (IPL(IS).EQ.IPP3) IPL(IS)=IS3
 320  CONTINUE
      DO 330 I=NPEM1,1,-1
        CALL ARREMP(IPEM1(I))
 330  CONTINUE
      DO 340 I=NDEM1,1,-1
        CALL ARREMD(IDEM1(I))
 340  CONTINUE
      DO 350 I=1,IPART
        IPTOT(I)=I
 350  CONTINUE
      NPTOT=IPART
 
 990  CONTINUE
 
      RETURN
 
C**** END OF AREMI2 ****************************************************
      END
