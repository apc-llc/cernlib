*
* $Id: hprsta.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprsta.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 04/12/92  10.32.32  by  Rene Brun
*-- Author :
      SUBROUTINE HPRSTA(ID1,KOPT)
*.==========>
*.           IF(IOPT='NO  ')NO PRINTING OF STATISTICS
*..=========> ( R.Brun )
#include "hbook/hcprin.inc"
      CHARACTER*(*) KOPT
*.___________________________________________
      CALL UCTOH(KOPT,IOPT,4,4)
      IF(IOPT.NE.INO)THEN
         CALL HIDOPT(ID1,'PSTA')
      ELSE
         CALL HIDOPT(ID1,'NPST')
      ENDIF
      CALL HOBSOL('HPRSTA','NPST','PSTA')
      END
