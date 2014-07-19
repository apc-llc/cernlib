*
* $Id: arexec.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arexec.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arexec.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE AREXEC
 
C...ARiadne subroutine EXECute ariadne
 
C...The Main driver routine in Ariadne.
 
 
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
 
      COMMON /ARDAT2/ PQMAS(10)
      SAVE /ARDAT2/
 
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
 
      COMMON /ARINT2/ DBEX,DBEY,DBEZ,PHI,THE
      SAVE /ARINT2/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),X,Y,W2,XQ2,U
      SAVE /LEPTOU/
 
      COMMON /PYPARS/ MSTP(200),PARP(200),MSTI(200),PARI(200)
      SAVE /PYPARS/
 
      COMMON /PYSUBS/ MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      SAVE /PYSUBS/
 
      COMMON /PYINT1/ MINT(400),VINT(400)
      SAVE /PYINT1/
 
C...Step counter
      MSTA(4)=MSTA(4)+1
 
C...Reset error log
      MSTA(13)=0
 
C...Error if ARINIT has not been called
      IF(MSTA(2).EQ.0) CALL ARERRM('AREXEC',12,0)
 
C...If ariadne mode just pass event through to ARPARS
      IF(MSTA(1).EQ.0) THEN
 
      CALL ARPARS(1,N)
 
C...If JETSET mode should work by just passing event on to ARPARS
      ELSEIF(MSTA(1).EQ.1) THEN
        CALL ARPARS(1,N)
 
C...If PYTHIA mode tag extended partons etc.
      ELSEIF(MSTA(1).EQ.2) THEN
 
        ISUB=MINT(1)
        IF(ISUB.NE.11.AND.ISUB.NE.12.AND.ISUB.NE.13.AND.
     $     ISUB.NE.28.AND.ISUB.NE.53.AND.ISUB.NE.68)
     $       CALL ARERRM('AREXEC',20,0)
 
        IFIRST=1
        ILAST=N
 
        DO 100 I=IFIRST,ILAST
          IF(K(I,1).GT.2) GOTO 100
          CALL ARGTYP(I,ITYP)
          IF(ITYP.EQ.0) GOTO 100
          IF(K(I,3).EQ.1.OR.K(I,3).EQ.2) THEN
            K(I,4)=1
          ELSE
            K(I,4)=0
          ENDIF
 100    CONTINUE
 
        CALL ARPARS(IFIRST,ILAST)
 
C...If LEPTO mode tag extended partons
      ELSEIF(MSTA(1).EQ.3) THEN
        IF(LST(24).EQ.1) THEN
 
C...Boost to hadronic cm to avoid precision problems
          DEL=DBLE(P(5,4))+DBLE(P(6,4))
          DBXL=(DBLE(P(5,1))+DBLE(P(6,1)))/DEL
          DBYL=(DBLE(P(5,2))+DBLE(P(6,2)))/DEL
          DBZL=(DBLE(P(5,3))+DBLE(P(6,3)))/DEL
          CALL LUDBRB(5,N,0.0,0.0,-DBXL,-DBYL,-DBZL)
 
          IF(MSTA(30).LT.2) THEN
            K(5,4)=0
          ELSE
            K(5,4)=3
            PARA(13)=SQRT(XQ2)
          ENDIF
          IF(MSTA(30).EQ.0) THEN
            K(6,4)=1
          ELSE
            K(6,4)=2
            PARA(12)=PARA(11)/(1.0-X)
          ENDIF
          CALL ARPARS(5,6)
          CALL LUDBRB(5,N,0.0,0.0,DBXL,DBYL,DBZL)
        ELSEIF(LST(24).EQ.3) THEN
 
C...Boost to hadronic cm to avoid precision problems
          DEL=DBLE(P(5,4))+DBLE(P(6,4))+DBLE(P(7,4))+DBLE(P(8,4))
          DBXL=(DBLE(P(5,1))+DBLE(P(6,1))+
     $         DBLE(P(7,1))+DBLE(P(8,1)))/DEL
          DBYL=(DBLE(P(5,2))+DBLE(P(6,2))+
     $         DBLE(P(7,2))+DBLE(P(8,2)))/DEL
          DBZL=(DBLE(P(5,3))+DBLE(P(6,3))+
     $         DBLE(P(7,3))+DBLE(P(8,4)))/DEL
          CALL LUDBRB(5,N,0.0,0.0,-DBXL,-DBYL,-DBZL)
 
          IF(MSTA(30).LT.2) THEN
            K(5,4)=0
          ELSE
            K(5,4)=3
            PARA(13)=SQRT(XQ2)
          ENDIF
          IF(MSTA(30).EQ.0) THEN
            K(6,4)=1
          ELSE
            K(6,4)=2
            PARA(12)=PARA(11)/(1.0-X)
          ENDIF
          CALL ARPARS(5,6)
          IF(MSTA(30).LT.2) THEN
            K(7,4)=0
          ELSE
            K(7,4)=3
            PARA(13)=SQRT(XQ2)
          ENDIF
          IF(MSTA(30).EQ.0) THEN
            K(8,4)=1
          ELSE
            K(8,4)=2
            PARA(12)=PARA(11)/(1.0-X)
          ENDIF
          CALL ARPARS(7,8)
          CALL LUDBRB(5,N,0.0,0.0,DBXL,DBYL,DBZL)
        ENDIF
      ENDIF
 
C...Perform fragmentation if requested
      IF(MSTA(5).EQ.1) CALL LUEXEC
 
      RETURN
 
C**** END OF AREXEC ****************************************************
      END
