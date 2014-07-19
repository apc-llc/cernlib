*
* $Id: ifinit.F,v 1.2 1996/03/07 17:38:10 couet Exp $
*
* $Log: ifinit.F,v $
* Revision 1.2  1996/03/07 17:38:10  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.33.00  by  O.Couet
*-- Author :
      SUBROUTINE IFINIT
#include "higz/hifc1.inc"
#include "higz/hifc6.inc"
#if defined(CERNLIB_VAX)
      CALL IFIOI
#endif
      ISFL=1
      ISCR=0
      IPOUT=0
      CALL IFVWIN(0.,131071.,0.,100160.)
      CALL IFSWIN(0,131071,0,100160)
      END
