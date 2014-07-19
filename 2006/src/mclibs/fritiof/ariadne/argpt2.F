*
* $Id: argpt2.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: argpt2.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: argpt2.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      REAL FUNCTION ARGPT2(ID)
 
C...ARiadne function Generate PT2
 
C...Returns the p_t^2 for a possible emission from dipole ID.
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
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
 
 
C...Set invariant mass squared in the dipole and generate a p_t^2
C...with the appropriate Monte Carlo subroutine
      IF(QEM(ID).AND.MSTA(20).GE.2.AND.ISTRS.GE.2) THEN
        PT2IN(ID)=0.0
        QDONE(ID)=.TRUE.
      ENDIF
      IF(.NOT.QDONE(ID)) THEN
        SDIP(ID)=ARMAS2(IP1(ID),IP3(ID))
        IF(QEM(ID)) THEN
          CALL ARGQED(ID)
        ELSE
          CALL ARGQCD(ID)
        ENDIF
        QDONE(ID)=.TRUE.
      ENDIF
 
      ARGPT2=PT2IN(ID)
 
      RETURN
 
C**** END OF ARGPT2 ****************************************************
      END
