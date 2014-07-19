*
* $Id: hwork.F,v 1.2 1997/03/14 17:04:24 mclareni Exp $
*
* $Log: hwork.F,v $
* Revision 1.2  1997/03/14 17:04:24  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:28:10  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.54  by  Rene Brun
*-- Author :
      SUBROUTINE HWORK(N,IAD,CHROUT)
*.==========>
*.           Reserve N words in IHWORK division
*.           IF(IAD=0)  NOT ENOUGH SPACE
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
      CHARACTER*(*) CHROUT
*.___________________________________________
      IF(NZLEFT(IHWORK,0).GT.N+100)THEN
         IAD=100
      ELSE
         IAD=0
         IERR=1
         CALL HBUG('Not enough space in memory',CHROUT,ID)
      ENDIF
  99  RETURN
      END
