*
* $Id: casetw.F,v 1.1.1.1 1996/02/26 17:16:48 mclareni Exp $
*
* $Log: casetw.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:48  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.02  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CASETW(A,N,V)
      IF(N.LE.0)RETURN
C+SELF,IF=APOLLO.
C      CALL VEC_$INIT(A,N,V)
C+SELF,IF=IBM,VAX.
      CALL VFILL(A,N,V)
C+SELF.
      END
