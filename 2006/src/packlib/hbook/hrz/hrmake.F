*
* $Id: hrmake.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrmake.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 08/10/92  18.35.45  by  Fons Rademakers
*-- Author :
      SUBROUTINE HRMAKE(LUN,CHDIR,NREC,CHOPT)
*.==========>
*.           Create a HBOOK/RZ file
*..=========> ( R.Brun )
      CHARACTER*(*) CHDIR,CHOPT
      CHARACTER*8   TAGS(2)
*.___________________________________________
*
      TAGS(1) = 'HBOOK-ID'
      TAGS(2) = 'VARIABLE'
      CALL RZMAKE(LUN,CHDIR,2,'II',TAGS,NREC,CHOPT)
*
      END
