*
* $Id: hcopym.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcopym.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/00 23/10/93  16.53.51  by  Rene Brun
*-- Author :
      SUBROUTINE HCOPYM(ID,IPAWD,IOFSET)
*.==========>
*.           To copy histogram ID from IPAWD area to /PAWC/
*.           If ID=0 copy all histograms
*.           The copied histogram is stored as ID+IOFSET
*..=========> ( R.Brun)
#include "hbook/hcdire.inc"
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
*.___________________________________________
      JR1=IPAWD(11)
      KOF=0
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
      IF(ID.NE.0)THEN
         CALL HCOPYN(IPAWD(19),IPAWD(11),ID,IOFSET,JTAB,KOF)
      ELSE
         NTOT =IPAWD(JTAB+17)
         DO 60 I=1,NTOT
            IF(IPAWD(JTAB-I+10).EQ.0)GO TO 60
            ID1=IPAWD(JTAB+I+18)
            CALL HCOPYN(IPAWD(19),IPAWD(11),ID1,IOFSET,JTAB,KOF)
  60     CONTINUE
      ENDIF
*
  99  RETURN
      END
