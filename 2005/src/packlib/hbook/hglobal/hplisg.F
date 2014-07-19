*
* $Id: hplisg.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hplisg.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.20/09 16/09/93  09.10.57  by  Rene Brun
*-- Author :
      SUBROUTINE HPLISG(IPAWD)
*.==========>
*.           To list histograms in global section IPAWD
*..=========> ( R.Brun)
#include "hbook/hcdire.inc"
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
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
      DO 60 I=1,NTOT
         IF(IPAWD(JTAB-I+10).EQ.0)GO TO 60
         ID1=IPAWD(JTAB+I+18)
         CALL HPLISG1(IPAWD(19),IPAWD(11),ID1,JTAB)
  60  CONTINUE
*
  99  RETURN
      END
#endif
