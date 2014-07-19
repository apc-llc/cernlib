*
* $Id: armipt.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: armipt.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: armipt.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      REAL FUNCTION ARMIPT(IF,IL)
 
C...ARiadne function determine MInimum PT2
 
C...Determines the minimum p_t^2 of any gluon between positions
C...IF and IL.
 
 
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
 
      INXT(IP)=IP3(IDO(IP))
      IPRV(IP)=IP1(IDI(IP))
 
 
      ARMIPT=PARA(40)
      DO 100 I=IF,IL
        IF(.NOT.QQ(I)) ARMIPT=MIN(ARMIPT,ARIPT2(IPRV(I),I,INXT(I)))
 100  CONTINUE
 
      RETURN
 
C**** END OF ARMIPT ****************************************************
      END
