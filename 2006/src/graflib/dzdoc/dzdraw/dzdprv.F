*
* $Id: dzdprv.F,v 1.2 2000/03/31 14:09:51 mclareni Exp $
*
* $Log: dzdprv.F,v $
* Revision 1.2  2000/03/31 14:09:51  mclareni
* Increase the minor version to 05
*
* Revision 1.1.1.1  1996/03/04 16:13:06  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDPRV
*     print first 28 characters of TITLE
      WRITE(*,100)
100   FORMAT(10H Starting ,
     + ' DZDOC   6.00/05   Q101'                 
     +)
      END
