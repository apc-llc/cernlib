*
* $Id: hprlow.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprlow.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.32.11  by  Rene Brun
*-- Author :
      SUBROUTINE HPRLOW(ID1,KOPT)
*.==========>
*.           NO PRINTING OF LOW-EDGE IF(IOPT=2HNO)
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PLOW')
      ELSE
         CALL HIDOPT(ID1,'NPLO')
      ENDIF
      CALL HOBSOL('HPRLOW','NPLO','PLOW')
      END
