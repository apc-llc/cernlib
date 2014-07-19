*
* $Id: hlnxtg.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hlnxtg.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.21/06 06/12/93  18.28.53  by  Rene Brun
*-- Author :    Rene Brun   06/12/93
      SUBROUTINE HLNXTG(IPAWD,IDH,CHTYPE,CHTITL,CHOPT)
*.==========>
*.           To return next object from global section IPAWD
*..=========> ( R.Brun)
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHTYPE,CHTITL,CHOPT
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
      SAVE NEXT
*.___________________________________________
      JR1=IPAWD(11)
*
*          Search levels down
*
      IF(NLPAT.GT.1)THEN
         DO 50 IL=2,NLPAT
            CALL UCTOH(CHPAT(IL),IHDIR,4,16)
            JR1=IPAWD(JR1+9)
  30        IF(JR1.EQ.0)GO TO 99
            DO 40 I=1,4
               IF(IHDIR(I).NE.IPAWD(JR1+I+18))THEN
                  JR1=IPAWD(JR1+10)
                  GO TO 30
               ENDIF
  40        CONTINUE
  50     CONTINUE
      ENDIF
*
      JCDIR = JR1
      JTAB  = IPAWD(JCDIR+7)
*
      NTOT =IPAWD(JTAB+17)
      IF(IDH.EQ.0)NEXT=0
  60  NEXT=NEXT+1
      IF(NEXT.GT.NTOT)THEN
         IDH=0
         GO TO 99
      ENDIF
      JCID=IPAWD(JTAB-NEXT+10)
      IF(JCID.EQ.0)GO TO 60
      ID1=IPAWD(JTAB+NEXT+18)
      CALL HLNXTG1(IPAWD(19),IPAWD(11),ID1,JCID,IDH,CHTYPE,CHTITL,CHOPT)
      IF(IDH.EQ.0)GO TO 60
*
  99  RETURN
      END
#endif
