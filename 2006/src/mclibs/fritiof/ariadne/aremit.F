*
* $Id: aremit.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: aremit.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: aremit.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE AREMIT(ID)
 
C...ARiadne subroutine EMIT
 
C...Administers the an emission from dipole ID
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),IDIPS
      SAVE /ARDIPS/
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      INXT(I)=IDO(IP3(I))
 
 
C...If FSR photon emission go a head
      IF(QEM(ID)) THEN
        CALL ARRADP(ID)
        RETURN
 
C...If q-qbar splitting go a head
      ELSEIF(IRAD(ID).NE.0) THEN
        CALL ARRADQ(ID)
        RETURN
 
C...If gluon emission from point-like dipole or if no p_t-ordered
C...recoil gluon, go a head
      ELSEIF((IEX(IP1(ID)).EQ.0.AND.IEX(IP3(ID)).EQ.0)
     $             .OR.MSTA(18).EQ.0) THEN
        CALL ARRADG(ID,0,SNR,PT21,PT23)
        RETURN
      ENDIF
 
C...If p_t-ordered recoil gluon, first save initial configuration
C...Then perform trial emission
      CALL ARSTOR(ID,IDS,IS1,IS3)
      CALL ARRADG(ID,0,SNR,PT21,PT23)
 
C...If no recoil gluon was produces keep trial emission
      IF(SNR.LE.1.0) RETURN
 
C...If two recoil gluons, tag the smallest one for p_t-ordering
      IF(AEX1(ID).LT.1.0.AND.AEX3(ID).LT.1.0) THEN
        INEWD=3
        IF(PT23.LT.PT21) THEN
          IGR=3
          IDR=INXT(INXT(ID))
        ELSE
          IGR=1
          IDR=ID
        ENDIF
 
C...If only one recoil gluon, tag it for p_t-ordering
      ELSEIF(AEX1(ID).LT.1.0.AND.AEX3(ID).GE.1.0) THEN
        IGR=1
        IDR=ID
        INEWD=2
      ELSEIF(AEX1(ID).GE.1.0.AND.AEX3(ID).LT.1.0) THEN
        IGR=3
        IDR=INXT(ID)
        INEWD=2
      ENDIF
 
      IDT=MAXDIP-1
 
C...Calculate the p_t^2 of a possibly earlier emission in place
C...of the recoil gluon. If this p_t^2 is lower than that of the
C...recoil gluon it could not have been emitted earlier and hence
C...the recoil gluon from the trial emission is kept.
      IF(IGR.EQ.1) THEN
        SY1=BP(IS1,5)/SQRT(SNR)
        CALL ARGQTE(IDT,SNR,PT2IN(IDS)/SNR,QQ(IS1),.FALSE.,
     $              IEX(IS1),0,SY1,0.0)
        IF(PT2IN(IDT).LT.PT21.AND.PT21.GT.PARA(3)**2
     $       .AND.PT21.GT.PARA(10+IEX(IS1))**2) RETURN
      ELSE
        SY3=BP(IS3,5)/SQRT(SNR)
        CALL ARGQTE(IDT,SNR,PT2IN(IDS)/SNR,.FALSE.,QQ(IS3),
     $              0,IEX(IS3),0.0,SY3)
        IF(PT2IN(IDT).LT.PT23.AND.PT23.GT.PARA(3)**2
     $       .AND.PT23.GT.PARA(10+IEX(IS3))**2) RETURN
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
      IF(IGR.EQ.3) THEN
        IDS=INXT(ID)
        IF(INEWD.EQ.3) IDS=INXT(IDS)
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
      IF(IOK.EQ.0.AND.PT2IN(IDS).GT.PARA(3)**2) THEN
        QDONE(IDS)=.FALSE.
      ENDIF
 
      RETURN
 
C**** END OF AREMIT ****************************************************
      END
