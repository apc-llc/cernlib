*
* $Id: fpsavev.F,v 1.3 1996/05/31 16:24:37 couet Exp $
*
* $Log: fpsavev.F,v $
* Revision 1.3  1996/05/31 16:24:37  couet
* - Cleaned up useless commented lines.
*
* Revision 1.2  1996/05/31 16:06:57  couet
* - pilot.h was missing
*
* Revision 1.1.1.1  1996/03/01 11:39:10  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/00 09/04/95  22.20.53  by  Gregory Kozlovsky
*-- Author :    Gregory Kozlovsky   09/04/95
      SUBROUTINE FPSAVEV(CHVNAME,N,VECT,IERFLG)
      CHARACTER*(*) CHVNAME
      INTEGER N,IERFLG
      DOUBLE PRECISION VECT(*)
*
*.==========> Author: G. Kozlovsky, 1994
*.
*.  Save a double precision array as a KUIP vector.
*.  If vector with the given name does not exist, it is created.
*.  If vector is not of real type IERFLG is set to 1.
*.  If vector exists and is too short, IERFLG is set to 2.
*..=========>
*
#include "hbook/hcbook.inc"
#include "paw/quest.inc"
      INTEGER LENGTH(3)
*.___________________________________________
*
      IERFLG = 0
      LENGTH(1) = N
      LENGTH(2) = 0
      LENGTH(3) = 0
      CALL CLTOU(CHVNAME)

      CALL KUVECT(CHVNAME,ILO,IHI)

      IF (ILO .EQ. 0) THEN
          CALL KUVCRE(CHVNAME,'R',LENGTH,ILO,IHI)
      ELSE IF (IQUEST(14) .NE. 1) THEN
          IERFLG = 1
      ELSE IF (IHI-ILO+1 .LT. N) THEN
          IERFLG = 2
      ENDIF

      IF (IERFLG .EQ. 0) THEN
          DO 100 I=1,N
              Q(ILO+I-1) = VECT(I)
100       CONTINUE
      ENDIF
      END
