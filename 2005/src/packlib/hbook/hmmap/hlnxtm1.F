*
* $Id: hlnxtm1.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hlnxtm1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/06 06/12/93  16.49.08  by  Rene Brun
*-- Author :    Rene Brun   06/12/93
      SUBROUTINE HLNXTM1(IB,LB,ID1,JCID,IDH,CHTYPE,CHTITL,CHOPT)
*.==========>
*.           Auxiliary for HLNXTM
*..=========> ( R.Brun)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcntpar.inc"
      CHARACTER*(*) CHTYPE,CHTITL,CHOPT
      DIMENSION IB(1),LB(1)
*.___________________________________________
#include "hbook/jbyt.inc"
      I123=JBYT(IB(JCID+KBITS),1,3)
      I230=JBYT(IB(JCID+KBITS),2,2)
      I1  =JBIT(I123,1)
      I4  =JBIT(IB(JCID+KBITS),4)
*
*
      IF(I1.NE.0)THEN
         NWTITL=IB(JCID-1)-KTIT1+1
         IF(INDEX(CHOPT,'1').NE.0) THEN
            IDH=ID1
            CHTYPE='1'
            CHTITL=' '
            CALL UHTOC(IB(JCID+KTIT1),4,CHTITL,NWTITL*4)
            RETURN
         ENDIF
      ELSEIF(I230.NE.0)THEN
         NWTITL=IB(JCID-1)-KTIT2+1
         IF(INDEX(CHOPT,'2').NE.0) THEN
            IDH=ID1
            CHTYPE='2'
            CHTITL=' '
            CALL UHTOC(IB(JCID+KTIT2),4,CHTITL,NWTITL*4)
            RETURN
         ENDIF
      ELSEIF(I4.NE.0)THEN
         IF (IB(JCID-2) .EQ. 2) THEN
            ITIT1=JCID+IB(JCID+9)
            NWTITL=IB(JCID+8)
         ELSE
            ITIT1=JCID+IB(JCID+ZITIT1)
            NWTITL=IB(JCID+ZNWTIT)
         ENDIF
         IF(INDEX(CHOPT,'N').NE.0) THEN
            IDH=ID1
            CHTYPE='N'
            CHTITL=' '
            CALL UHTOC(IB(ITIT1),4,CHTITL,NWTITL*4)
            RETURN
         ENDIF
      ELSE
         IF(INDEX(CHOPT,'?').NE.0) THEN
            IDH=-1
            CHTYPE='?'
            CHTITL='??? '
            RETURN
         ENDIF
         GO TO 99
      ENDIF
*
  99  RETURN
      END
