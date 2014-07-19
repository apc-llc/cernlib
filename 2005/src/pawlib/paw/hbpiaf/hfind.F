*
* $Id: hfind.F,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
*
* $Log: hfind.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/02 14/05/93  14.50.00  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE HFIND(IDD,CHROUT)
*.==========>
*.           Find IDD location and compute local pointers
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
      CHARACTER*(*) CHROUT
*.___________________________________________
      IF(LFIX.NE.0)GO TO 99
      ID=IDD
      IDLAST=0
***      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      CALL HIDPOS(ID,IDPOS)
      IF(IDPOS.LE.0)THEN
         LCID=0
         CALL HBUG('Unknown histogram',CHROUT,IDD)
         GO TO 99
      ENDIF
*
      LCID=LQ(LTAB-IDPOS)
      LCONT=LQ(LCID-1)
      LSCAT=LCONT
      NB=IQ(LCONT+KNBIT)
      LPRX=LCID+KNCX
      IF(JBYT(IQ(LCID+KBITS),2,2).NE.0)THEN
         LPRY=LCID+KNCY
      ELSE
         LPRY=0
      ENDIF
  99  RETURN
      END
