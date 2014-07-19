*
* $Id: hloop.F,v 1.1.1.1 1996/01/16 17:07:42 mclareni Exp $
*
* $Log: hloop.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 24/03/89  10.46.48  by  Rene Brun
*-- Author :
      SUBROUTINE HLOOP(ID1,CHROUT,IRET)
*.==========>
*.           IRET=2  CONTINUATION OF IRET=3
*.               =3  RETURN CONTROL TO ROUTINE CHROUT ONCE ENOUGH
*.                   INFORMATION IS OBTAINED
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHROUT
*.___________________________________________
      IDLAST=0
      IF(IRET.EQ.2)GO TO 50
      LCID=LIDS
*
      IF(ID1.NE.0)THEN
         ID=ID1
         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         IF(IDPOS.GT.0)THEN
            LCID=LQ(LTAB-IDPOS)
            GO TO 99
         ELSE
            LCID=0
            CALL HBUG('Unknown histogram',CHROUT,ID1)
            GO TO 60
         ENDIF
      ENDIF
*
  20  IF(LCID.EQ.0)GO TO 60
      ID=IQ(LCID-5)
      GO TO 99
*
  50  IF(ID1.NE.0)GO TO 60
      IF(IRET.EQ.2)IRET=3
      LCID=LQ(LCID)
      GO TO 20
*
  60  IRET=0
  99  RETURN
      END
