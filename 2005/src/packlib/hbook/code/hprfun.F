*
* $Id: hprfun.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprfun.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.31.49  by  Rene Brun
*-- Author :
      SUBROUTINE HPRFUN(ID1,KOPT)
*.==========>
*.           VALUES OF FUNCTION ARE PRINTED IF(IOPT.NE.2HNO)
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PFUN')
      ELSE
         CALL HIDOPT(ID1,'NPFU')
      ENDIF
      CALL HOBSOL('HPRFUN','NPFU','PFUN')
      END
