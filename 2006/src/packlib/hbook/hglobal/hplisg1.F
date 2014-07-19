*
* $Id: hplisg1.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hplisg1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.20/09 16/09/93  11.02.44  by  Rene Brun
*-- Author :    Rene Brun   27/07/93
      SUBROUTINE HPLISG1(IB,LB,ID1,JTAB)
*.==========>
*.           Auxiliary for HPLISG
*..=========> ( R.Brun)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcntpar.inc"
      DIMENSION IB(1),LB(1)
*.___________________________________________
#include "hbook/jbyt.inc"
      NH=IB(JTAB-1)
      DO 10 I=1,NH
         IF(IB(JTAB+I).EQ.ID1)GO TO 20
  10  CONTINUE
      GO TO 99
*
  20  CONTINUE
C     ID=ID1
      JCID=LB(JTAB-I)
      NTOT=IB(JCID+KNTOT)
      I123=JBYT(IB(JCID+KBITS),1,3)
      I230=JBYT(IB(JCID+KBITS),2,2)
      I1  =JBIT(I123,1)
      I4  =JBIT(IB(JCID+KBITS),4)
*
*
      IF(I1.NE.0)THEN
         NWTITL=IB(JCID-1)-KTIT1+1
         ITIT1=JCID+KTIT1
      ELSEIF(I230.NE.0)THEN
         NWTITL=IB(JCID-1)-KTIT2+1
         ITIT1=JCID+KTIT2
      ELSEIF(I4.NE.0)THEN
         IF (IB(JCID-2) .EQ. 2) THEN
            ITIT1=JCID+IB(JCID+9)
            NWTITL=IB(JCID+8)
         ELSE
            ITIT1=JCID+IB(JCID+ZITIT1)
            NWTITL=IB(JCID+ZNWTIT)
         ENDIF
      ELSE
         GO TO 99
      ENDIF
      WRITE (LOUT,1001) ID1,(IB(KK),KK=ITIT1,ITIT1+NWTITL-1)
1001  FORMAT(1X,I12,2X,15A4)
*
  99  RETURN
      END
#endif
