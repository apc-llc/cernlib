*
* $Id: sibool.F,v 1.1.1.1 1995/12/12 14:36:15 mclareni Exp $
*
* $Log: sibool.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:15  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.17  by  Carlo E. Vandoni
*-- Author :
      FUNCTION SIBOOL(A)
C
C    ....................................................
C         PURPOSE
C            MAKE A BOOLEAN
C
C         USAGE
C            R=SIBOOL(A)
C
C         COMM. BLOCKS USED
C            COM1
C            COMVAR USED: N ONE
C
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAC
C            SIROUN
C            SINERR
C    ......................................................
C
       SIBOOL=SIROUN(A)
       IF((SIBOOL.EQ.0.0).OR.(SIBOOL.EQ.1.0)) GOTO 999
       SIBOOL=0.
       CALL SINERR(10)
C
 999  END
