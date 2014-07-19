C***********************************************************************
C $Id: aremit.F,v 1.2 1996/04/10 12:33:12 mclareni Exp $

      SUBROUTINE AREMIT(ID)

C...ARiadne subroutine EMIT

C...Administers the an emission from dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"
#include "arhide.f"
#include "arint1.f"

      INXT(I)=IDO(IP3(I))

      IF (MHAR(101).EQ.2) THEN
        CALL AREMI3(ID)
        RETURN
      ELSEIF (MSTA(32).GT.1.OR.MHAR(101).EQ.1) THEN
        CALL AREMI2(ID)
        RETURN
      ENDIF

C...If FSR photon emission go a head
      IF (QEM(ID)) THEN
        CALL ARRADP(ID)
        RETURN

C...If q-qbar splitting go a head
      ELSEIF (IRAD(ID).NE.0) THEN
        CALL ARRADQ(ID)
        RETURN

C...If gluon emission from point-like dipole or if no p_t-ordered
C...recoil gluon, go a head
      ELSEIF (((.NOT.QEX(IP1(ID))).AND.(.NOT.QEX(IP3(ID))))
     $             .OR.MSTA(18).EQ.0) THEN
        CALL ARRADG(ID,0,SNR,PT21,PT23)
        RETURN
      ENDIF

C...If p_t-ordered recoil gluon, first save initial configuration
C...Then perform trial emission
      CALL ARSTOR(ID,IDS,IS1,IS3)
      CALL ARRADG(ID,0,SNR,PT21,PT23)

C...If no recoil gluon was produces keep trial emission
      IF (SNR.LE.1.0) RETURN

C...If two recoil gluons, tag the smallest one for p_t-ordering
      IF (AEX1(ID).LT.1.0.AND.AEX3(ID).LT.1.0) THEN
        INEWD=3
        IF ((MSTA(17).GE.2.AND.PT21.GE.PT23).OR.
     $       (MSTA(17).LT.2.AND.BX3(ID).GE.BX1(ID))) THEN
          IGR=3
        ELSE
          IGR=1
        ENDIF

C...If only one recoil gluon, tag it for p_t-ordering
      ELSEIF (AEX1(ID).LT.1.0.AND.AEX3(ID).GE.1.0) THEN
        IGR=1
        INEWD=2
      ELSEIF (AEX1(ID).GE.1.0.AND.AEX3(ID).LT.1.0) THEN
        IGR=3
        INEWD=2
      ENDIF

      IDT=MAXDIP-1

C...Calculate the p_t^2 of a possibly earlier emission in place
C...of the recoil gluon. If this p_t^2 is lower than that of the
C...recoil gluon it could not have been emitted earlier and hence
C...the recoil gluon from the trial emission is kept.
      IF (IGR.EQ.1) THEN
        QABOVE=((MSTA(18).GT.2.OR.PT21.GT.PARA(3)**2).AND.
     $       (MSTA(18).GT.1.OR.PT21.GT.XPMU(IS1)**2))
        S=SNR
        XT2MP=PT2IN(IDS)/SNR
        QQ1=QQ(IS1)
        QQ3=.FALSE.
        QE1=QEX(IS1)
        QE3=.FALSE.
        XT2GG3=XT2MP
        XT2GG1=-1.0
        IF ((.NOT.QQ1).AND.(.NOT.QE1)) THEN
          XT2GG1=XT2MP
          IF(INO(IS1).NE.0) XT2GG1=PT2GG(IS1)
        ENDIF
        ALP1=XPA(IS1)
        ALP3=0.0
        XMU1=XPMU(IS1)
        XMU3=0.0
        SY1=BP(IS1,5)/SQRT(SNR)
        SY3=0.0
        IFL1=IFL(IS1)
        IFL3=21
        CALL ARGQCD(-IDT)
        IF (PT2IN(IDT).LT.PT21.AND.QABOVE) RETURN
      ELSE
        QABOVE=((MSTA(18).GT.2.OR.PT23.GT.PARA(3)**2).AND.
     $       (MSTA(18).GT.1.OR.PT23.GT.XPMU(IS3)**2))
        S=SNR
        IF (INO(IS3).NE.0) XT2GG3=PT2GG(IS3)
        QQ1=.FALSE.
        QQ3=QQ(IS3)
        QE1=.FALSE.
        QE3=QEX(IS3)
        XT2GG1=XT2MP
        XT2GG3=-1.0
        IF ((.NOT.QQ3).AND.(.NOT.QE3)) THEN
          XT2GG3=XT2MP
          IF(INO(IS3).NE.0) XT2GG3=PT2GG(IS3)
        ENDIF
        ALP1=0.0
        ALP3=XPA(IS3)
        XMU1=0.0
        XMU3=XPMU(IS3)
        SY1=0.0
        SY3=BP(IS3,5)/SQRT(SNR)
        IFL1=21
        IFL3=IFL(IS3)
        CALL ARGQCD(-IDT)
        IF (PT2IN(IDT).LT.PT23.AND.QABOVE) RETURN
      ENDIF

C...A gluon can be emittes in place of the recoil gluon at an earlier 
C...time. Recall the initial configuration and redo the emission without
C...recoil gluon
      CALL ARRECA(ID,IDS,IS1,IS3)

      IDIPS=IDIPS-INEWD
      IPART=IPART-INEWD
      CALL ARRADG(ID,IGR,SNREF,PT21,PT23)

C...Set p_t^2 for the emission in place of the recoil gluon
      IDS=ID
      IF (IGR.EQ.3) THEN
        IDS=INXT(ID)
        IF (INEWD.EQ.3.AND.PT21.GT.0.0) IDS=INXT(IDS)
      ENDIF

      CALL ARSTOR(IDS,IDSS,ISS1,ISS3)
      IP1(IDSS)=ISS1
      IP3(IDSS)=ISS3
      CALL ARBOCM(IDSS)

      QDONE(IDS)=.TRUE.
      SDIP(IDS)=ARMAS2(ISS1,ISS3)
      BX1(IDS)=BX1(IDT)
      BX3(IDS)=BX3(IDT)
      AEX1(IDS)=AEX1(IDT)
      AEX3(IDS)=AEX3(IDT)
      IRAD(IDS)=IRAD(IDT)
      PT2IN(IDS)=PT2IN(IDT)

      CALL ARCHKI(IDS,IOK)
      IF (IOK.EQ.0.AND.PT2IN(IDS).GT.PARA(3)**2) THEN
        QDONE(IDS)=.FALSE.
      ENDIF

      RETURN

C**** END OF AREMIT ****************************************************
      END
C***********************************************************************
C $Id: aremit.F,v 1.2 1996/04/10 12:33:12 mclareni Exp $

      SUBROUTINE AREMI2(ID)

C...ARiadne subroutine EMIT version 2

C...Administers the an emission from dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arlist.f"
#include "ardat1.f"
#include "arhide.f"

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
C***********************************************************************
C $Id: aremit.F,v 1.2 1996/04/10 12:33:12 mclareni Exp $

      SUBROUTINE AREMI3(ID)

C...ARiadne subroutine EMIT version 3

C...Administers the an emission from dipole ID

#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "arstrs.f"
#include "arlist.f"
#include "ardat1.f"
#include "arhide.f"


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
