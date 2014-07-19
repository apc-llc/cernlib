*
* $Id: hfill.F,v 1.1.1.1 1996/03/01 11:39:11 mclareni Exp $
*
* $Log: hfill.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:11  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 22/08/93  11.12.38  by  Fons Rademakers
*-- Author :
      SUBROUTINE HFILL(ID1,X,Y,W)
*.==========>
*.           GENERAL FILLING ROUTINE
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
      IF(ID1.NE.IDLAST)THEN
         ID=ID1
***         IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
         CALL HIDPOS(ID,IDPOS)
         IF(IDPOS.LE.0)RETURN
         LCID=LQ(LTAB-IDPOS)
         I1=JBIT(IQ(LCID+KBITS),1)
         I5=JBIT(IQ(LCID+KBITS),5)
         I6=JBIT(IQ(LCID+KBITS),6)
         I7=JBIT(IQ(LCID+KBITS),7)
         I8=JBIT(IQ(LCID+KBITS),8)
         LCONT=LQ(LCID-1)
         LPRX=LCID+KNCX
         IDLAST=ID1
      ENDIF
      CALL SBIT0(IQ(LCID),6)
*
      IF(I1.NE.0)THEN
*
*             1-DIM HISTOGRAM OR PROFILE
*
         IF(I8.EQ.0)THEN
            CALL HF1(ID1,X,W)
         ELSE
            CALL HFILPF(ID1,X,Y,W)
         ENDIF
      ELSE
*
*             2-DIM HISTOGRAM
*
         CALL HF2 (ID1,X,Y,W)
         CALL HFIDP(ID1,X,Y,W)
         LCONT=LQ(LCID-1)
      ENDIF
      END
