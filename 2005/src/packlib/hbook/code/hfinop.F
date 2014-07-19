*
* $Id: hfinop.F,v 1.1.1.1 1996/01/16 17:07:37 mclareni Exp $
*
* $Log: hfinop.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 09/08/89  14.43.53  by  Rene Brun
*-- Author :
      SUBROUTINE HFINOP(IDD,LABEL,IFWW,NBB,IFX,IFY,ICASE,NUM)
*.==========>
*.           LOCATE THE POSITION OF OPTION (ICASE,NUM )
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcprin.inc"
      CHARACTER*(*) LABEL
*.___________________________________________
      IFX=0
      IFY=0
      IFW=0
      NB=0
      IC=IUCOMP(ICASE,IDENT,8)
*
      IRET=3
      CALL HLOOP(IDD,LABEL,IRET)
      IF(IRET.EQ.0)GO TO 99
*
      I1=JBIT(IQ(LCID+KBITS),1)
      ILINK=1
      IF(IC.GT.2)ILINK=IC-1
      NBX=0
      NBY=0
*
  30  LNEXT=LQ(LCID-ILINK)
      IF(LNEXT.EQ.0)GO TO 99
*
      GO TO(40,40,70,80,90,100,110,99,99),ILINK
*
  40  IFX=LCID+KNCX
  45  IFW=LNEXT
      NB=IQ(LNEXT+KNBIT)
      IF(I1.EQ.0.AND.ILINK.EQ.1)IFY=LCID+KNCY
      GO TO 99
  70  IFX=LCID+KNCY
      GO TO 45
*
  80  IFX=LCID+KNCX
  85  NSL=IQ(LNEXT-2)
      IF(NUM.LT.1.OR.NUM.GT.NSL)GO TO 99
      LNEXT=LQ(LNEXT-NUM)
      GO TO 45
  90  IFX=LCID+KNCY
      GO TO 85
*
 100  NBX=NBX+1
      IF(NUM.NE.NBX)THEN
         LNEXT=LQ(LNEXT)
         IF(LNEXT.EQ.0)GO TO 99
         GO TO 100
      ELSE
         LNEXT=LQ(LNEXT-1)
         GO TO 40
      ENDIF
*
 110  NBY=NBY+1
      IF(NUM.NE.NBY)THEN
         LNEXT=LQ(LNEXT)
         IF(LNEXT.EQ.0)GO TO 99
         GO TO 110
      ELSE
         LNEXT=LQ(LNEXT-1)
         IFX=LCID+KNCY
         GO TO 45
      ENDIF
*
  99  IFWW=IFW
      NBB=NB
      END
