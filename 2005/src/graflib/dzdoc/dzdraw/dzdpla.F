*
* $Id: dzdpla.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdpla.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDPLA(IVAL,COUT)
      CHARACTER*(*) COUT
      CHARACTER*9 CTEMP
      COUT=' '
      WRITE(CTEMP,'(I9)')IVAL
      IFNB=INDEXC(CTEMP,' ')
      NC=9-IFNB+1
      COUT(1:NC)=CTEMP(IFNB:9)
      END
 
 
***********************************************************************
