*
* $Id: arswap.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
*
* $Log: arswap.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:06  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arswap.F,v 1.1.1.1 1996/03/08 16:51:06 mclareni Exp $
 
      SUBROUTINE ARSWAP(ID1,ID2)
 
C...ARiadne subroutine SWAP partons
 
C...Get the colour neighbour ICON and anti-colour neighbour ICBN of
C...parton I and the respective connecting dipoles IDCON and IDCBN.
 
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
 
      QDONE(ID1)=.FALSE.
      QDONE(ID2)=.FALSE.
      QDUMP=.FALSE.
      PT2GG(MAXPAR-3)=-1.0
      PT2GG(MAXPAR-4)=-1.0
      NPTOT=0
      I1=IP1(ID1)
      I2=IP1(ID2)
 
      IS1=ISTR(ID1)
      IS2=ISTR(ID2)
      IF (IFLOW(IS1)*IFLOW(IS2).LT.0) THEN
        IF (IFLOW(IS2).LT.0) THEN
          CALL AREVST(IS2)
        ELSE
          CALL AREVST(IS1)
        ENDIF
      ENDIF
 
      IP1(ID1)=I2
      IP1(ID2)=I1
      IDO(I1)=ID2
      IDO(I2)=ID1
      SDIP(ID1)=ARMAS2(I2,IP3(ID1))
      SDIP(ID2)=ARMAS2(I1,IP3(ID2))
 
      IF (IS1.NE.IS2) THEN
        IF (IFLOW(IS1).NE.2.AND.IFLOW(IS2).NE.2) THEN
          IL=IPL(IS1)
          IPL(IS1)=IPL(IS2)
          IPL(IS2)=IL
          I=IPF(IS1)
 100      ISTR(IDO(I))=IS1
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS1)) GOTO 100
          I=IPF(IS2)
 110      ISTR(IDO(I))=IS2
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS2)) GOTO 110
          CALL ARCHFL
        ELSEIF(IFLOW(IS1).EQ.2.AND.IFLOW(IS2).NE.2) THEN
          I=IPF(IS2)
 120      ISTR(IDO(I))=IS2
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS2)) GOTO 120
          CALL ARREMS(IS1)
          CALL ARCHFL
        ELSEIF(IFLOW(IS1).NE.2.AND.IFLOW(IS2).EQ.2) THEN
          I=IPF(IS1)
 130      ISTR(IDO(I))=IS1
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS1)) GOTO 130
          CALL ARREMS(IS2)
          CALL ARCHFL
        ELSE
          IPF(IS1)=I1
          IPL(IS1)=IP1(IDI(I1))
          ISTR(IDI(I1))=IS1
          I=IPF(IS1)
 140      ISTR(IDO(I))=IS1
          I=IP3(IDO(I))
          IF (I.NE.IPL(IS1)) GOTO 140
          CALL ARREMS(IS2)
          CALL ARCHFL
        ENDIF
        RETURN
      ENDIF
 
      I=IPF(IS1)
      IR1=I1
      IR2=I2
 200  IF (I.EQ.IR1) IR1=0
      IF (I.EQ.IR2) IR2=0
      I=IP3(IDO(I))
      IF (I.NE.IPL(IS1).AND.I.NE.IPF(IS1)) GOTO 200
      IF (IFLOW(IS1).EQ.2.AND.I.EQ.IPF(IS1)) IPL(IS1)=IP1(IDI(I))
      IF (I.EQ.IR1) IR1=0
      IF (I.EQ.IR2) IR2=0
 
      IF (MAX(IR1,IR2).EQ.0) THEN
        CALL ARCHFL
        RETURN
      ENDIF
 
      IF (ISTRS+1.GT.MAXSTR) CALL ARERRM('ARSWAP',8,0)
 
      ISTRS=ISTRS+1
      IS1=ISTRS
      IPF(IS1)=MAX(IR1,IR2)
      I=IPF(IS1)
      IPL(IS1)=IP1(IDI(I))
      ISTR(IDI(I))=IS1
      IFLOW(IS1)=2
 210  ISTR(IDO(I))=IS1
      I=IP3(IDO(I))
      IF (I.NE.IPL(IS1)) GOTO 210
 
      CALL ARCHFL
      RETURN
 
C**** END OF ARSWAP ****************************************************
      END
