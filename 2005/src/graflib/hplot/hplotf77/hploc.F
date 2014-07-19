*
* $Id: hploc.F,v 1.1.1.1 1996/01/19 10:50:10 mclareni Exp $
*
* $Log: hploc.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:10  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/12 09/05/94  18.27.23  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE HPLOC(NTPRI,NTLOC,XLOC,YLOC,IDH,ICX,ICY,ISTAT)
*.==========>
*.
*.   Picking of an histogram point and returns its coordinates and
*. the number of transformation. The location is limited to 100
*. windows maximum. If NTPRI.LT.0 then automatic selection of NTLOC.
*. If NTPRI.GE.0 then transformation NTPRI is prioritary
*.
*..=========> (N. Somon)
#include "hplot/hpl2.inc"
*._____________________________
*
      LCDNR = 21
      IF(ISTAT.NE.0)LCDNR = 11
      IF(NTPRI.GE.0)THEN
         IF(NTPRI.NE.1)CALL ISELNT(1)
         CALL ISELNT(NTPRI)
      ELSE
         CALL ISELNT(1)
         DO 10 I=10,NTHIST,10
            CALL ISELNT(I)
  10     CONTINUE
      ENDIF
      CALL IRQLC(1,LCDNR,ISTAT,NTLOC,X,Y)
      IF(ISTAT.NE.0)THEN
         XLOC = X
         YLOC = Y
         CALL HPLCHA(NTLOC,X,Y,IDH,XLOC,YLOC,ICX,ICY)
      ENDIF
*
      END

