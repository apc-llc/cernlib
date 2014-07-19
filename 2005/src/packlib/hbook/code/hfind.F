*
* $Id: hfind.F,v 1.2 2001/09/24 14:25:12 couet Exp $
*
* $Log: hfind.F,v $
* Revision 1.2  2001/09/24 14:25:12  couet
* - The number of entries of a non existing histogram was not 0.
*
* Revision 1.1.1.1  1996/01/16 17:07:37  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 14/01/93  17.06.36  by  Rene Brun
*-- Author :
      SUBROUTINE HFIND(IDD,CHROUT)
*.==========>
*.           Find IDD location and compute local pointers
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
      COMMON /QUEST/ IQUEST(100)
      CHARACTER*(*) CHROUT
*.___________________________________________
      IF(LFIX.NE.0)GO TO 99
      IQUEST(1)=0
      ID=IDD
      IDLAST=0
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)THEN
         LCID=0
         CALL HBUG('Unknown histogram',CHROUT,IDD)
         IQUEST(1)=1
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
