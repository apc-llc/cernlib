*
* $Id: sitrax.F,v 1.1.1.1 1995/12/12 14:36:20 mclareni Exp $
*
* $Log: sitrax.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:20  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.20  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SITRAX(C)
C
C    ....................................................
C
C
C         PURPOSE
C            COMPILER ROUTINE TRACING
C
C         DESCRIPTION OF PARAMETERS
C            C  - ROUTINE NAME FOR PRINTOUT
C
C         COMM. BLOCKS USED
C            COM1
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            ITRAK
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
#include "sigma/sigc.inc"
C
      CHARACTER*(*) C
C

      IF(SITRAK(10).EQ.0) RETURN
      PRINT *,C

C
 999  END
