*
* $Id: hrzcd.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrzcd.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 28/01/94  15.21.52  by  Fons Rademakers
*-- Author :
      SUBROUTINE HRZCD(CHDIR,CHOPT)
*.==========>
*.        to change the Current Directory of a RZ file
*..=========> ( R.Brun )
#include "hbook/hcdire.inc"
#if defined(CERNLIB_CZ)
#include "hbook/czsock.inc"
#endif
      CHARACTER*(*)CHDIR,CHOPT
*.___________________________________________
*
      IF(ICHTOP(ICDIR).GT.1000)THEN
#if defined(CERNLIB_CZ)
*
         ISKIN =MOD(ICHTOP(ICDIR),10000)
         ISKOUT=(ICHTOP(ICDIR)-ISKIN)/10000
#endif
#if !defined(CERNLIB_CZ)
         CALL HBUG('CZ option not active','HRZCD',0)
#endif
         RETURN
      ENDIF
*
      CALL RZCDIR(CHDIR,CHOPT)
*
      END
