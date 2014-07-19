*
* $Id: arrems.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arrems.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arrems.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARREMS(IREM)
 
C...ARiadne subroutine REMove String
 
C...Remove String entry from /ARSTRS/ and purge event record
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARDIPS/ BX1(MAXDIP),BX3(MAXDIP),PT2IN(MAXDIP),
     $                SDIP(MAXDIP),IP1(MAXDIP),IP3(MAXDIP),
     $                AEX1(MAXDIP),AEX3(MAXDIP),QDONE(MAXDIP),
     $                QEM(MAXDIP),IRAD(MAXDIP),ISTR(MAXDIP),
     $                ICOLI(MAXDIP),IDIPS
      SAVE /ARDIPS/
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
 
C...Purge the dipole vector
      DO 100 IS=IREM+1,ISTRS
        I=IS-1
        IFLOW(I)=IFLOW(IS)
        IPF(I)=IPF(IS)
        IPL(I)=IPL(IS)
 100  CONTINUE
 
      ISTRS=ISTRS-1
 
      DO 110 ID=1,IDIPS
        IF (ISTR(ID).EQ.IREM) ISTR(ID)=0
        IF (ISTR(ID).GT.IREM) ISTR(ID)=ISTR(ID)-1
 110  CONTINUE
 
      RETURN
 
C**** END OF ARREMS ****************************************************
      END
