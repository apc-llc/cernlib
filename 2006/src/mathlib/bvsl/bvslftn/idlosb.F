*
* $Id: idlosb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: idlosb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.25  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE IDLOSB (N,IY,IX,LV,NB,LO,JS)
C
C.    *****************************************************************
C.    *                                                               *
C.    *      FORTRAN version of BVSL subroutine.                      *
C.    *      See BVSL description for purpose and usages.             *
C.    *      The Pseudo-BIT must be dimensioned to N, that            *
C.    *      is the only constraint to use both BVSL and P_BVSL       *
C.    *                                                               *
C.    *      Author: M.Roethlisberger/IBM                             *
C.    *                                                               *
C.    *      DATE  : 20-05-90, VERSION 1.01, CREATION                 *
C.    *                                                               *
C.    *****************************************************************
C
      LOGICAL    LV  (N)
      DIMENSION  IY  (N), IX (N)
      CHARACTER*2    LO
 
      IF (LO.EQ.'EQ') THEN
        NB = 0
        DO 110 I=1,N
          LV(I) = IY(I)-IX(I).EQ.JS
          IF (LV(I)) NB = NB + 1
 110    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'LT') THEN
        NB = 0
        DO 120 I=1,N
          LV(I) = IY(I)-IX(I).LT.JS
          IF (LV(I)) NB = NB + 1
 120    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'LE') THEN
        NB = 0
        DO 130 I=1,N
          LV(I) = IY(I)-IX(I).LE.JS
          IF (LV(I)) NB = NB + 1
 130    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'GT') THEN
        NB = 0
        DO 140 I=1,N
          LV(I) = IY(I)-IX(I).GT.JS
          IF (LV(I)) NB = NB + 1
 140    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'GE') THEN
        NB = 0
        DO 150 I=1,N
          LV(I) = IY(I)-IX(I).GE.JS
          IF (LV(I)) NB = NB + 1
 150    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'NE') THEN
        NB = 0
        DO 160 I=1,N
          LV(I) = IY(I)-IX(I).NE.JS
          IF (LV(I)) NB = NB + 1
 160    CONTINUE
      ENDIF
 
 999  RETURN
      END
