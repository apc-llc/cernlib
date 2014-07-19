*
* $Id: araddg.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
*
* $Log: araddg.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:16  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: araddg.F,v 1.1.1.1 1996/01/11 14:05:16 mclareni Exp $
 
      SUBROUTINE ARADDG(ID)
 
C...ARiadne subroutine ADD Gluon
 
C...Adds a gluon entry between the partons in dipole ID thus creating
C...a new dipole
 
 
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
 
      INXT(I)=IDO(IP3(I))
      IPRV(I)=IDI(IP1(I))
 
 
C...Allocate new gluon and new dipole at postitons IPART+1 and IDIPS+1
C...if there is space left.
      IPART=IPART+1
      IDIPS=IDIPS+1
      IF(IPART.GE.MAXPAR-1) CALL ARERRM('ARADDG',6,0)
      IF(IDIPS.GE.MAXDIP-1) CALL ARERRM('ARADDG',7,0)
 
C...Set properties of new gluon
      DO 100 I=1,5
        BP(IPART,I)=0.0
 100  CONTINUE
      IFL(IPART)=21
      IEX(IPART)=0
      QQ(IPART)=.FALSE.
      IDI(IPART)=ID
      IDO(IPART)=IDIPS
 
C...Set properties of new dipole
      IP1(IDIPS)=IPART
      IP3(IDIPS)=IP3(ID)
      QDONE(IDIPS)=.FALSE.
      QEM(IDIPS)=.FALSE.
      ISTR(IDIPS)=ISTR(ID)
 
C...Fix pointers for old dipole
      IP3(ID)=IPART
      IDI(IP3(IDIPS))=IDIPS
      IF(IPRV(ID).NE.0) QDONE(IPRV(ID))=.FALSE.
      QDONE(ID)=.FALSE.
      IF(INXT(IDIPS).NE.0) QDONE(INXT(IDIPS))=.FALSE.
 
      RETURN
 
C**** END OF ARADDG ****************************************************
      END
