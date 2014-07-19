*
* $Id: arcrdi.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arcrdi.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arcrdi.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARCRDI(ID,IPA1,IPA3,IS,QED)
 
C...ARiadne subroutine CReate DIpole
 
C...Creates a dipole from partons IPA1 and IPA3
 
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
 
 
      IDO(IPA1)=ID
      IDI(IPA3)=ID
      IP1(ID)=IPA1
      IP3(ID)=IPA3
      ISTR(ID)=IS
      QDONE(ID)=.FALSE.
      QEM(ID)=QED
      ICOLI(ID)=0
 
      RETURN
 
C**** END OF ARCRDI ****************************************************
      END
