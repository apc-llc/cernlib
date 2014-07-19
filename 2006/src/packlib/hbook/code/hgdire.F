*
* $Id: hgdire.F,v 1.1.1.1 1996/01/16 17:07:38 mclareni Exp $
*
* $Log: hgdire.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:38  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 26/04/89  11.56.46  by  Federico Carminati
*-- Author : Federico Carminati
      SUBROUTINE HGDIRE(LUN,CHDIR)
*.==========>
*.           Gets the Directory name for a given Logical unit
*..=========> ( F.Carminati )
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHDIR
*
*.___________________________________________
*
      DO 10 I=2,NCHTOP
         IF(ICHTOP(I).EQ.LUN)THEN
            CHDIR=CHTOP(I)
            GO TO 999
         ENDIF
  10  CONTINUE
*
      CALL HBUG('Unit not used by HBOOK','HGDIRE',LUN)
      CHDIR=' '
*
 999  END
