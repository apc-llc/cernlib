*
* $Id: armipt.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
*
* $Log: armipt.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:02  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: armipt.F,v 1.1.1.1 1996/03/08 16:51:02 mclareni Exp $
 
      REAL FUNCTION ARMIPT(IFST,ILST)
 
C...ARiadne function determine MInimum PT2
 
C...Determines the minimum p_t^2 of any gluon between positions
C...IF and IL.
 
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
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      INXT(IP)=IP3(IDO(IP))
      IPRV(IP)=IP1(IDI(IP))
 
 
      ARMIPT=PARA(40)
      DO 100 I=IFST,ILST
        IF (.NOT.QQ(I)) THEN
          IF (INXT(I).NE.IPRV(I))
     $         ARMIPT=MIN(ARMIPT,ARIPT2(IPRV(I),I,INXT(I)))
        ENDIF
 100  CONTINUE
 
      RETURN
 
C**** END OF ARMIPT ****************************************************
      END
