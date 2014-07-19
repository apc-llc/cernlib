*
* $Id: hresetm1.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hresetm1.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 17/08/94  12.45.54  by  Rene Brun
*-- Author :    Rene Brun   27/07/93
      SUBROUTINE HRESETM1(IB,LB,ID1,JTAB,KOF)
*.==========>
*.           Auxiliary for HRESETM
*.           Reset histogram ID1 in a global section
*..=========> ( R.Brun, W.Bruckner)
#include "hbook/hcbook.inc"
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
      JCID=LB(JTAB-I)-KOF
      NTOT=IB(JCID+KNTOT)
      I123=JBYT(IB(JCID+KBITS),1,3)
      I230=JBYT(IB(JCID+KBITS),2,2)
      I1  =JBIT(I123,1)
      IF(I123.EQ.0)THEN
         CALL HBUG('Object is not histogram','HRESEM',ID1)
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
         CALL HREZ0M(IB,LB,LB(JCID-1),KOF)
         GO TO 90
      ENDIF
*
*=====>     2-DIM histogram
*
      IF(I230.NE.0)THEN
         JSCAT=LB(JCID-1)-KOF
         CALL VZERO(IB(JSCAT+KNOENT),IB(JSCAT-1)-1)
*
*           PROX
*
        IF(LB(JCID-2).NE.0) THEN
         CALL HREZ0M(IB,LB,LB(JCID-2),KOF)
        ENDIF
*
*           PROY
*
        IF(LB(JCID-3).NE.0)THEN
         CALL HREZ0M(IB,LB,LB(JCID-3),KOF)
        ENDIF
*
*           SLIX
*
         JSLIX=LB(JCID-4)
         IF(JSLIX.NE.0)THEN
            DO 40 I=1,IB(JSLIX-2-KOF)
               CALL HREZ0M(IB,LB,LB(JSLIX-KOF-I),KOF)
  40        CONTINUE
         ENDIF
*
*           SLIY
*
         JSLIY=LB(JCID-5)
         IF(JSLIY.NE.0)THEN
            DO 50 I=1,IB(JSLIY-KOF-2)
               CALL HREZ0M(IB,LB,LB(JSLIY-KOF-I),KOF)
  50        CONTINUE
         ENDIF
*
*           BANX
*
         JBANX=LB(JCID-6)
  60     IF(LBANX.NE.0)THEN
            CALL HREZ0M(IB,LB,LB(JBANX-KOF-1),KOF)
            JBANX=LB(JBANX-KOF)
            IF(JBANX.NE.0)GO TO 60
         ENDIF
*
*           BANY
*
         JBANY=LB(JCID-7)
  70     IF(LBANY.NE.0)THEN
            CALL HREZ0M(IB,LB,LB(JBANY-KOF-1),KOF)
            JBANY=LB(JBANY-KOF)
            IF(JBANY.NE.0)GO TO 70
         ENDIF
      ENDIF
*
  90  CONTINUE
*
  99  RETURN
      END
