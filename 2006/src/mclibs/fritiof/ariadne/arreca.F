*
* $Id: arreca.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
*
* $Log: arreca.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:18  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arreca.F,v 1.1.1.1 1996/01/11 14:05:18 mclareni Exp $
 
      SUBROUTINE ARRECA(ID,IDS,IS1,IS3)
 
C...ARiadne function RECAll
 
C...Recalls a dipole entry stored by ARSTOR
 
 
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
 
 
 
      SDIP(ID)=SDIP(IDS)
      IP1(ID)=IP1(IDS)
      IP3(ID)=IP3(IDS)
      BX1(ID)=BX1(IDS)
      BX3(ID)=BX3(IDS)
      PT2IN(ID)=PT2IN(IDS)
      AEX1(ID)=AEX1(IDS)
      AEX3(ID)=AEX3(IDS)
      QDONE(ID)=QDONE(IDS)
      QEM(ID)=QEM(IDS)
      IRAD(ID)=IRAD(IDS)
      ISTR(ID)=ISTR(IDS)
 
      I1=IP1(ID)
      I3=IP3(ID)
 
      DO 100 I=1,5
        BP(I1,I)=BP(IS1,I)
        BP(I3,I)=BP(IS3,I)
 100  CONTINUE
      IFL(I1)=IFL(IS1)
      IFL(I3)=IFL(IS3)
      IEX(I1)=IEX(IS1)
      IEX(I3)=IEX(IS3)
      QQ(I1)=QQ(IS1)
      QQ(I3)=QQ(IS3)
      IDI(I1)=IDI(IS1)
      IDI(I3)=IDI(IS3)
      IDO(I1)=IDO(IS1)
      IDO(I3)=IDO(IS3)
      INO(I1)=INO(IS1)
      INO(I3)=INO(IS3)
 
      RETURN
 
C**** END OF ARRECA ****************************************************
      END
