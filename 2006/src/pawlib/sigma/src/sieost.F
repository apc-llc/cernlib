*
* $Id: sieost.F,v 1.1.1.1 1995/12/12 14:36:16 mclareni Exp $
*
* $Log: sieost.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:16  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIEOST
C
C    ....................................................
C
C         PURPOSE
C            HANDLING  END - OF - STATEMENT
C
C         USAGE
C            CALL SIEOST
C
C         COMM. BLOCKS USED
C            COM1
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAC
C            SINERR
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
#include "sigma/sigc.inc"
C
      CALL SITRAC(' SIEOST     ')
C
******************************************
      CALL KUVDEL('$*')
******************************************
      IF(IPOINT.LT.259)RETURN
       PRINT *,IPOINT
      CALL SINERR(20)
C
 999  END
