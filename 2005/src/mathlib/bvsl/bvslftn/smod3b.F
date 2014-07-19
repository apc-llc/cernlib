*
* $Id: smod3b.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: smod3b.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE SMOD3B (N,A1,A2,A3,OUT,LV)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      DIMENSION  A1(N), A2(N), A3(N), OUT(N)
      LOGICAL    LV(N)
      DO 1 I = 1,N
        IF (LV(I)) OUT(I)=SQRT(A1(I)**2+A2(I)**2+A3(I)**2)
    1 CONTINUE
      RETURN
      END
