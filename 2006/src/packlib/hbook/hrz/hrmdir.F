*
* $Id: hrmdir.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrmdir.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/04 20/01/93  08.08.58  by  Rene Brun
*-- Author :
      SUBROUTINE HRMDIR(CHDIR,CHOPT)
*.==========>
*.           Create an HBOOK/RZ subdirectory
*..=========> ( R.Brun )
      CHARACTER*(*) CHDIR
      CHARACTER*8   TAGS(2)
#include "hbook/hcdire.inc"
*.___________________________________________
*
      TAGS(1) = 'HBOOK-ID'
      TAGS(2) = 'VARIABLE'
      NWK=ICHTYP(ICDIR)
      CALL RZMDIR(CHDIR,NWK,'II',TAGS)
*
      END
