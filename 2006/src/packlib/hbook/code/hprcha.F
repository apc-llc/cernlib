*
* $Id: hprcha.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprcha.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.30.57  by  Rene Brun
*-- Author :
      SUBROUTINE HPRCHA(ID1,KOPT)
*.==========>
*.           CHANNEL NUMBER IS NOT PRINTED IF (IOPT=2HNO)
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PCHA')
      ELSE
         CALL HIDOPT(ID1,'NPCH')
      ENDIF
      CALL HOBSOL('HPRCHA','NPCH','PCHA')
      END
