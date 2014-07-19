*
* $Id: rjctb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: rjctb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE RJCTB (RAN,XIN,FREJ,XOUT,LV,N,NWOUT,ISW)
C
C  ************************************************************
C  *                                                          *
C  * Author: M.Roethlisberger/IBM                             *
C  *                                                          *
C  * Date:  6 Nov 1990,  V1.01,  Creation of the module       *
C  *                                                          *
C  ************************************************************
C
      LOGICAL    LV (*)
      DIMENSION  RAN(*), XIN(*), FREJ(*), XOUT(*)
 
      K = 0
 
      IF (ISW.EQ.1) THEN
        DO 1 I = 1,N
          IF (FREJ(I).GT.RAN(I)) THEN
            XOUT(I) = XIN(I)
            LV(I) = .FALSE.
          ELSE
            LV(I) = .TRUE.
            K = K + 1
          ENDIF
    1   CONTINUE
        ISW = 2
 
      ELSE
 
      J = 0
        DO 2 I = 1,N
          IF (LV(I)) THEN
            J=J+1
            IF (FREJ(J).GT.RAN(J)) THEN
              XOUT(I) = XIN(J)
              LV  (I) = .FALSE.
            ELSE
              K = K + 1
            ENDIF
          ENDIF
    2   CONTINUE
 
      ENDIF
      NWOUT = K
      RETURN
      END
