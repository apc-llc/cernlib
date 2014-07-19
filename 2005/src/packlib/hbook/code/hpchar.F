*
* $Id: hpchar.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpchar.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 07/03/89  11.05.37  by  Rene Brun
*-- Author :
      SUBROUTINE HPCHAR(LABEL,ICHAR)
*.==========>
*.           To change character type for printing histos
*.           default characters are
*.
*.           BLACK   1HX
*.           STAR    1H*
*.           FUNC    1H*
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      DIMENSION LABEL(1),ICHAR(1)
*.___________________________________________
      CALL UCTOH(LABEL,ITEST,1,1)
      IF(ITEST.EQ.IDG(12))THEN
         CALL UCTOH(ICHAR,ICBLAC,1,1)
      ELSEIF(ITEST.EQ.IDG(29))THEN
         CALL UCTOH(ICHAR,ICSTAR,1,1)
      ELSEIF(ITEST.EQ.IDG(16))THEN
         CALL UCTOH(ICHAR,ICFUNC,1,1)
      ENDIF
      END
