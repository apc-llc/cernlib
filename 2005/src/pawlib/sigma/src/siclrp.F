*
* $Id: siclrp.F,v 1.1.1.1 1995/12/12 14:36:15 mclareni Exp $
*
* $Log: siclrp.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:15  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.17  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SICLRP
C
C    ....................................................
C         PURPOSE
C            CLEAR PROGRAM AREA AND POINTERS
C
C         USAGE
C            CALL SICLRP
C
C         COMM. BLOCKS USED
C            COM1
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAC
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
      CALL SITRAC(' SICLRP  ')
C
      IPOINT=1
      DO 1 I=1,260
      IPROGR(I)=777777
 1    CONTINUE
C
C
 999  END
