*
* $Id: artpt2.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: artpt2.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: artpt2.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      REAL FUNCTION ARTPT2(ID,SN,BX1IN,BX3IN,Y1IN,Y2IN,Y3IN)
 
C...Ariadne function True PT2
 
C...Calculates the (minimum) true p_t^2 of a gluon given x1 and x3 of
C...an emission
 
 
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
 
 
      ARTPT2=-1.0
 
      IF (ID.EQ.0) THEN
        S=SN
        B1=BX1IN
        B3=BX3IN
        Y1=Y1IN
        Y2=Y2IN
        Y3=Y3IN
      ELSE
        S=SDIP(ID)
        B1=BX1(ID)
        B3=BX3(ID)
        Y1=(BP(IP1(ID),5)**2)/SDIP(ID)
        Y2=0.0
        Y3=(BP(IP3(ID),5)**2)/SDIP(ID)
      ENDIF
 
 
      BZ12=0.25*B1**2-Y1
      BZ22=0.25*(2.0-B1-B3)**2-Y2
      BZ32=0.25*B3**2-Y3
      BCOS=2.0*BZ12*BZ22+2.0*BZ12*BZ32+2.0*BZ22*BZ32
     $     -BZ12**2-BZ22**2-BZ32**2
      IF (BCOS.LE.0.0) RETURN
      ARTPT2=0.25*S*BCOS/MAX(BZ12,BZ32)
 
      RETURN
 
C**** END OF ARTPT2 ****************************************************
      END
