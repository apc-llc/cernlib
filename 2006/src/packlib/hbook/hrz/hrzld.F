*
* $Id: hrzld.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrzld.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.55  by  Rene Brun
*-- Author :
      SUBROUTINE HRZLD(CHDIR,CHOPT)
*.==========>
*.        to list the Current Directory of a RZ file
*..=========> ( R.Brun )
      CHARACTER*(*)CHDIR,CHOPT
#include "hbook/hcdire.inc"
*.___________________________________________
*
      IF(ICHTOP(ICDIR).GT.1000)THEN
#if defined(CERNLIB_CZ)
         CALL HBUG('Function not implemented for remote files',
     +           'HRZLD',0)
#endif
#if !defined(CERNLIB_CZ)
         CALL HBUG('CZ option not active','HRZLD',0)
#endif
         RETURN
      ENDIF
*
      CALL RZLDIR(CHDIR,CHOPT)
*
      END
