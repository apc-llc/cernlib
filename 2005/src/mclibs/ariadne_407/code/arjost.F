*
* $Id: arjost.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
*
* $Log: arjost.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:00  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arjost.F,v 1.1.1.1 1996/03/08 16:51:00 mclareni Exp $
 
      SUBROUTINE ARJOST(IS1,IS2,IPA1,IPA2)
 
C...ARiadne subroutine JOin two STring entries
 
C...Join the string entries IS1 and IS2 in the ends IP1 and IP2
 
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
 
 
      IS=IS1
      ISREM=IS2
      IP=IPA1
      IPREM=IPA2
 
C...If the strings are the same we make a purely gluonic string
      IF (IS.EQ.ISREM) THEN
        IFLOW(IS)=2
        IPF(IS)=IP
        IPL(IS)=IP
        RETURN
      ENDIF
 
      IF (IP.EQ.IPF(IS)) THEN
        IF (IPREM.EQ.IPF(ISREM)) CALL ARERRM('ARJOST',5,0)
        IPF(IS)=IPF(ISREM)
      ELSE
        IF (IP.NE.IPL(IS).OR.IPREM.EQ.IPL(ISREM))
     $       CALL ARERRM('ARJOST',5,0)
        IPL(IS)=IPL(ISREM)
      ENDIF
 
      DO 100 ID=1,IDIPS
        IF (ISTR(ID).EQ.ISREM) ISTR(ID)=IS
 100  CONTINUE
 
      DO 110 IS=ISREM,ISTRS-1
        IPF(IS)=IPF(IS+1)
        IPL(IS)=IPL(IS+1)
        IFLOW(IS)=IFLOW(IS+1)
 110  CONTINUE
 
      ISTRS=ISTRS-1
 
      DO 120 ID=1,IDIPS
        IF (ISTR(ID).GT.ISREM) ISTR(ID)=ISTR(ID)-1
 120  CONTINUE
 
      RETURN
 
C**** END OF ARJOST ****************************************************
      END
