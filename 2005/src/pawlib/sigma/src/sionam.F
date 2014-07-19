*
* $Id: sionam.F,v 1.1.1.1 1995/12/12 14:36:19 mclareni Exp $
*
* $Log: sionam.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:19  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.10/00 26/10/93  12.10.16  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIONAM(N,NH)
C
C    ....................................................
C
C
C         PURPOSE
C            OUTPUT NAME INTO THE COMPILED STRING
C
C         USAGE
C            CALL SIONAM(IOUTS(IC+1),IOUTS(IC+2))
C
C         DESCRIPTION OF PARAMETERS
C            N  - FIRST PART OF NAME TO BE OUTPUT
C            NH - SECOND PART OF NAME TO BE OUTPUT
C
C
C
C         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C            SITRAX
C            OUTPUT
C            SEE
C            SINERR
C
C...  PAW VERSION  ...  MAY 1988
C
C    ......................................................
C
C
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
C
C
      DIMENSION NAM(2)
      EQUIVALENCE (NAM(1),INAM)
C
C
      CALL SITRAX(' SIONAM   ')
      NAM(1)=N
      NAM(2)=NH
C---  OUTPUT CODE 007 FOR SIVARI
      CALL SIOUTP(7)
C---  OUTPUT DUMMY (CDC - NO OF BITS NOW NO OF CHAR IS 8 FIXED)
      CALL SIOUTP(8)
C---  OUTPUT FIRST PART OF NAME (2ND ARG DUMMY - NO OF BITS AT CDC)
      CALL SIOUTP(N)
C---  OUTPUT SECOND PART OF NAME (2ND ARG DUMMY - NO OF BITS AT CDC)
      CALL SIOUTP(NH)
 999  END
