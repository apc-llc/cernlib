*
* $Id: hprerr.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprerr.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.31.35  by  Rene Brun
*-- Author :
      SUBROUTINE HPRERR(ID1,KOPT)
*.==========>
*.           VALUES OF ERRORS ARE PRINTED IF(IOPT=3HYES)
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PERR')
      ELSE
         CALL HIDOPT(ID1,'NPER')
      ENDIF
      CALL HOBSOL('HPRERR','NPER','PERR')
      END
