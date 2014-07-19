*
* $Id: hrzinm.F,v 1.1.1.1 1996/01/16 17:08:10 mclareni Exp $
*
* $Log: hrzinm.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:10  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/06 08/12/93  13.18.27  by  Rene Brun
*-- Author :    Rene Brun   06/12/93
      SUBROUTINE HRZINM(IPAWD,IDH,JCID)
*.==========>
*.           To return pointer JCID for IDH in shared memory IPAWD
*..=========> ( R.Brun)
#include "hbook/hcdire.inc"
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
*.___________________________________________
      NW=IPAWD(1)
      KOF=IPAWD(2)
      ILAST=IPAWD(NW)
      JR1=ILAST-KOF
      JCID=0
*
*          Search levels down
*
      IF(NLPAT.GT.1)THEN
         DO 50 IL=2,NLPAT
            CALL UCTOH(CHPAT(IL),IHDIR,4,16)
            JR1=IPAWD(JR1-1)-KOF
  30        IF(JR1.EQ.0)GO TO 99
            DO 40 I=1,4
               IF(IHDIR(I).NE.IPAWD(JR1+I+8))THEN
                  JR1=IPAWD(JR1)-KOF
                  GO TO 30
               ENDIF
  40        CONTINUE
  50     CONTINUE
      ENDIF
*
      JCDIR = JR1
      JTAB  = IPAWD(JCDIR-3)-KOF
*
      NTOT =IPAWD(JTAB+7)
      DO 60 I=1,NTOT
         IF(IPAWD(JTAB-I).EQ.0)GO TO 60
         ID1=IPAWD(JTAB+I+8)
         IF(ID1.EQ.IDH)THEN
            JCID=IPAWD(JTAB-I)-KOF
            GO TO 99
         ENDIF
  60  CONTINUE
*
  99  RETURN
      END
