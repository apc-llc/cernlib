*
* $Id: igaxi0.F,v 1.1.1.1 1996/02/14 13:10:32 mclareni Exp $
*
* $Log: igaxi0.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:32  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/04 09/10/92  13.46.16  by  O.Couet
*-- Author :
      SUBROUTINE IGAXI0(A1,A2,NAA,BL,BH,NB,BWID)
*.==========>
*.
*.  Get reasonable values for tick marks & ensure they are
*. not plotted beyond allowed limits
*.
*. _Input parameters:
*.
*.  REAL A1,A2 : Old WMIN,WMAX .
*.  REAL BL,BH : New WMIN,WMAX .
*.  INTEGER NAA : Old NDIV .
*.  INTEGER NB : New NDIV .
*.
*..=========>
      CALL IGAXI3(A1,A2,NAA,BL,BH,NB,BWID)
      ATEST=BWID*0.0001
      IF(ABS(BL-A1).GE.ATEST)THEN
         BL=BL+BWID
         NB=NB-1
      ENDIF
      IF(ABS(BH-A2).GE.ATEST)THEN
         BH=BH-BWID
         NB=NB-1
      ENDIF
*
      END
