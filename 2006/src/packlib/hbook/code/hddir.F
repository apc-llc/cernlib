*
* $Id: hddir.F,v 1.1.1.1 1996/01/16 17:07:35 mclareni Exp $
*
* $Log: hddir.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:35  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/03 04/01/93  12.21.06  by  Rene Brun
*-- Author :
      SUBROUTINE HDDIR(CHDIR)
*.==========>
*.            Delete sub-directory CHDIR from /PAWC/
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHDIR
      CHARACTER*64 CHWOLD
*.___________________________________________
*
      CALL HCDIR(CHWOLD,'R')
      IF(CHWOLD(3:6).NE.'PAWC')THEN
*
*           Directory is in a RZ file
*
         CALL RZDELT(CHDIR)
      ELSE
*
*           Directory is in //PAWC
*
         CALL HCDIR(CHDIR,' ')
         IF(IQUEST(1).NE.0)GO TO 99
         CALL MZDROP(IHDIV,LCDIR,' ')
      ENDIF
      CALL HCDIR(CHWOLD,' ')
*
  99  RETURN
      END
