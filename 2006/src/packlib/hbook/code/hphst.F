*
* $Id: hphst.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hphst.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.51  by  Rene Brun
*-- Author :
      SUBROUTINE HPHST(ID1)
*.==========>
*.           PRINT 1-DIM VERTICAL,SCATTER AND TABLES
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      ID=ID1
      IDLAST=0
*
      IF(ID1.NE.0)THEN
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.GT.0)THEN
            LCID=LQ(LTAB-IDPOS)
            GO TO 30
         ELSE
            CALL HBUG('Unknown histogram','HPHST',ID1)
            GO TO 99
         ENDIF
      ENDIF
*
      LCID=LIDS
  20  IF(LCID.EQ.0)GO TO 99
      IF(ID1.NE.0)GO TO 99
      ID=IQ(LCID-5)
*
  30  IF(JBIT(IQ(LCID+KBITS),1).EQ.0)GO TO 60
      CALL HPHIST(ID,'    ',0)
      GO TO 90
*
  60  IF(JBIT(IQ(LCID+KBITS),2).NE.0)THEN
         CALL HPSCAT(ID)
      ELSEIF(JBIT(IQ(LCID+KBITS),3).NE.0)THEN
         CALL HPTAB(ID)
      ELSE
         GO TO 95
      ENDIF
      IF(JBIT(IQ(LCID+KBITS),8).NE.0.AND.IQ(LSCAT+KNOENT).NE.0)THEN
         CALL HPHIST(ID,'    ',0)
      ENDIF
*
  90  IF(IPONCE.NE.0)CALL SBYT(0,IQ(LCID+KBITS),1,3)
  95  LCID=LQ(LCID)
      GO TO 20
*
  99  RETURN
      END
