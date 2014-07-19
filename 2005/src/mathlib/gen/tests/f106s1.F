*
* $Id: f106s1.F,v 1.1.1.1 1996/04/01 15:01:27 mclareni Exp $
*
* $Log: f106s1.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:27  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE F106S1(N,Z)
#include "iorc.inc"
C     Subroutine to print an nxn matrix Z
      REAL Z(N,N)
      DO 1000 I=1,N
         WRITE(LOUT,'(8F10.6)') (Z(I,J),J=1,N)
1000  CONTINUE
      RETURN
      END

