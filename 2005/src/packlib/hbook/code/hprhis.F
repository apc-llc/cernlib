*
* $Id: hprhis.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprhis.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.32.00  by  Rene Brun
*-- Author :
      SUBROUTINE HPRHIS(ID1,KOPT)
*.==========>
*.           SUPPRESS PRINTING OF HISTOGRAM IF(IOPT=2HNO)
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PHIS')
      ELSE
         CALL HIDOPT(ID1,'NPHI')
      ENDIF
      CALL HOBSOL('HPRHIS','NPHI','PHIS')
      END
