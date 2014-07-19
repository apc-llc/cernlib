*
* $Id: pahmov.F,v 1.1.1.1 1996/03/01 11:38:40 mclareni Exp $
*
* $Log: pahmov.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  1.08/00 04/12/89  17.47.40  by  Rene Brun
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAHMOV(ISTAT)
*.==========>
*.
*.        To update contents of one histogram using graphics input
*.
*..=========> (R.Brun)
      DIMENSION X(4),Y(4)
*._____________________________
*
      CALL IGLOC2(1,NTLOC,XLOC1,YLOC1,XLOC2,YLOC2,ISTAT,' ')
      IF(ISTAT.EQ.0)GO TO 99
      CALL HPLCHA(NTLOC,XLOC1,YLOC1,IDH,XLOCH1,YLOCH1,ICHAN,ICY)
      IF(IDH.EQ.0)GO TO 99
      IF(ICHAN.LE.0)GO TO 99
      CALL HPLCHA(NTLOC,XLOC2,YLOC2,IDH,XLOCH2,YLOCH2,ICX,ICY)
*
*          Redraw new channel
*
      CALL HFIND(IDH,'PAHMOV')
      CALL HIX(IDH,ICHAN,X(1))
      CALL HIX(IDH,ICHAN+1,X(4))
      X(2)=X(1)
      X(3)=X(4)
      Y(1)=HI(IDH,ICHAN)
      IF(YLOC2.LT.YLOCH2)THEN
         Y(1)=ALOG10(Y(1))
      ENDIF
      Y(4)=Y(1)
      Y(2)=YLOC2
      Y(3)=YLOC2
      CALL ISELNT(NTLOC)
      CALL IPL(4,X,Y)
*
*           Replace channel contents
*
      CALL HREP1(IDH,ICHAN,YLOCH2)
*
  99  END
