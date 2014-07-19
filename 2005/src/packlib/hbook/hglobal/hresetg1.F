*
* $Id: hresetg1.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hresetg1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.19/00 13/04/93  08.48.23  by  Rene Brun
*-- Author :
      SUBROUTINE HRESETG1(IB,LB,ID1,JTAB)
*.==========>
*.           Auxiliary for HRESETG
*.           Reset histogram ID1 in a global section
*..=========> ( R.Brun)
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
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
      ID=ID1
      JCID=LB(JTAB-I)
      NTOT=IB(JCID+KNTOT)
      I123=JBYT(IB(JCID+KBITS),1,3)
      I230=JBYT(IB(JCID+KBITS),2,2)
      I1  =JBIT(I123,1)
      IF(I123.EQ.0)THEN
         CALL HBUG('Object is not an histogram','HRESEG',ID1)
         GO TO 99
      ENDIF
*
*             Reset contents
*
*======>     Array
*
      IF(I123.EQ.0)THEN
         CALL VZERO(IB(JCID+1),IB(JCID-1))
         GO TO 90
      ENDIF
*
*======>     1-DIM histogram
*
      IF(I1.NE.0)THEN
         CALL HREZ0G(IB,LB,LB(JCID-1))
         GO TO 90
      ENDIF
*
*=====>     2-DIM histogram
*
      IF(I230.NE.0)THEN
         JSCAT=LB(JCID-1)
         CALL VZERO(IB(JSCAT+KNOENT),IB(JSCAT-1)-1)
*
*           PROX
*
         CALL HREZ0G(IB,LB,LB(JCID-2))
*
*           PROY
*
         CALL HREZ0G(IB,LB,LB(JCID-3))
*
*           SLIX
*
         JSLIX=LB(JCID-4)
         IF(JSLIX.NE.0)THEN
            DO 40 I=1,IB(JSLIX-2)
               CALL HREZ0G(IB,LB,LB(JSLIX-I))
  40        CONTINUE
         ENDIF
*
*           SLIY
*
         JSLIY=LB(JCID-5)
         IF(JSLIY.NE.0)THEN
            DO 50 I=1,IB(JSLIY-2)
               CALL HREZ0G(IB,LB,LB(JSLIY-I))
  50        CONTINUE
         ENDIF
*
*           BANX
*
         JBANX=LB(JCID-6)
  60     IF(LBANX.NE.0)THEN
            CALL HREZ0G(IB,LB,LB(JBANX-1))
            JBANX=LB(JBANX)
            IF(JBANX.NE.0)GO TO 60
         ENDIF
*
*           BANY
*
         JBANY=LB(JCID-7)
  70     IF(LBANY.NE.0)THEN
            CALL HREZ0G(IB,LB,LB(JBANY-1))
            JBANY=LB(JBANY)
            IF(JBANY.NE.0)GO TO 70
         ENDIF
      ENDIF
*
  90  CONTINUE
*
  99  RETURN
      END
#endif
