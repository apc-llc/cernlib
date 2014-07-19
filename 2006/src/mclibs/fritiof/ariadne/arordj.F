*
* $Id: arordj.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arordj.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arordj.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARORDJ
 
C...ARiadne subroutine ORDer Jets
 
C...Orders jets in falling energy
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARPART/ BP(MAXPAR,5),IFL(MAXPAR),IEX(MAXPAR),QQ(MAXPAR),
     $                IDI(MAXPAR),IDO(MAXPAR),INO(MAXPAR),IPART
      SAVE /ARPART/
 
      COMMON /ARJETX/ N,K(300,5),P(300,5),V(300,5)
      SAVE /ARJETX/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
 
 
C...Error if no space left in /ARPART/
      IF(MSTU(3).GT.MAXPAR) CALL ARERRM('ARORDJ',10,0)
 
C...Copy jets into /ARPART/ and link them with pointers
      IPF=1
      NJET=0
      DO 100 I=1,MSTU(3)
        IF(K(N+I,5).LT.0) GOTO 100
        NJET=NJET+1
        IDO(NJET)=NJET+1
        IDI(NJET)=NJET-1
        DO 110 J=1,5
          BP(NJET,J)=P(N+I,J)
 110    CONTINUE
 100  CONTINUE
      IDI(1)=0
      IDO(NJET)=0
 
C...Copy back jets to /LUJETS/ in falling order in energy
      MSTU(3)=NJET
      DO 200 I=1,MSTU(3)
        EMAX=0.0
        IM=0
        IP=IPF
 210    IF(BP(IP,4).GT.EMAX) THEN
          EMAX=BP(IP,4)
          IM=IP
        ENDIF
        IF(IDO(IP).NE.0) THEN
          IP=IDO(IP)
          GOTO 210
        ENDIF
 
        DO 220 J=1,5
          P(N+I,J)=BP(IM,J)
 220    CONTINUE
 
        IF(IM.EQ.IPF) THEN
          IPF=IDO(IM)
        ELSE
          IDO(IDI(IM))=IDO(IM)
          IF(IDO(IM).NE.0) IDI(IDO(IM))=IDI(IM)
        ENDIF
 
 200  CONTINUE
 
      RETURN
 
C**** END OF ARORDJ ****************************************************
      END
