*
* $Id: sindlf.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: sindlf.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SINDLF
C
C
C
C---                       DUMMY ROUTINE
C
C
      CALL SITRAC(' SINDLF  ')
C
      CALL SINEXT(N)
      PRINT *, ' WE ENTERED SINDLFT AND N =',N


C                PROBLEM:
C                           NEXT IS 4 (KLASS CODE SIFAM0) INDEX N-DIM
C                CALLING SEQUENCE ---  SILSTK(LL,CL)
C                                      WITH
C                                           DIMENSION LL(6)
C                                           CHARACTER CL*8
 999  END
