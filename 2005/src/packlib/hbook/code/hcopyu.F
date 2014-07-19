*
* $Id: hcopyu.F,v 1.1.1.1 1996/01/16 17:07:34 mclareni Exp $
*
* $Log: hcopyu.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:34  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/00 23/10/93  16.53.51  by  Rene Brun
*-- Author :    Rene Brun   29/03/91
      SUBROUTINE HCOPYU(ID,IPAWD,IOFSET)
*.==========>
*.           To copy histogram ID from IPAWD area to /PAWC/
*.           If ID=0 copy all histograms
*.           The copied histogram is stored as ID+IOFSET
*..=========> ( R.Brun)
#include "hbook/hcdire.inc"
      DIMENSION IHDIR(4)
      DIMENSION IPAWD(100)
*.___________________________________________
      NW=IPAWD(1)
      KOF=IPAWD(2)
      ILAST=IPAWD(NW)
      JR1=ILAST-KOF
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
      JTAB  = IPAWD(JCDIR-3) -KOF
*
      IF(ID.NE.0)THEN
         CALL HCOPYN(IPAWD(9),IPAWD(1),ID,IOFSET,JTAB,KOF)
      ELSE
         NTOT =IPAWD(JTAB-1+8)
         DO 60 I=1,NTOT
            IF(IPAWD(JTAB-I).EQ.0)GO TO 60
            ID1=IPAWD(JTAB+I+8)
            CALL HCOPYN(IPAWD(9),IPAWD(1),ID1,IOFSET,JTAB,KOF)
  60     CONTINUE
      ENDIF
*
  99  RETURN
      END
 
