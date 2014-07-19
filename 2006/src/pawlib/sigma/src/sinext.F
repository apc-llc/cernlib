*
* $Id: sinext.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: sinext.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SINEXT (N)
C
C
C    ....................................................
C
C
C         PURPOSE
C            RETURN NEXT PART OF PROGRAM ENTRY STRING
C
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CALL SITRAC (' NEXT    ')
C
      I=IPOINT+1
      IPOINT=IPOINT+1
C
      N=IPROGR(I)
C
      IF(SITRAK(20).EQ.1)PRINT *,' NEXT = ',N
C
 999  END
