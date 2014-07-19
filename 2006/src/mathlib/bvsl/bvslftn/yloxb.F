*
* $Id: yloxb.F,v 1.1.1.1 1996/03/21 17:19:56 mclareni Exp $
*
* $Log: yloxb.F,v $
* Revision 1.1.1.1  1996/03/21 17:19:56  mclareni
* Bvsl
*
*
#include "pilot.h"
*CMZ :          06/11/90  14.36.24  by  Michel Roethlisberger/IBM
*-- Author :
      SUBROUTINE YLOXB (N,Y,X,LV,NB,LO)
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
C.    *      Date  : 20-03-90, Version 1.01, Creation                 *
C.    *      Date  : 14-05-90, Version 2.01, YGESB,YGTSB,YEQSB,       *
C.    *                                      YLESB,YLTSB,YneSB becomes*
C.    *                                      YLOSB.                   *
C.    *****************************************************************
C
      LOGICAL    LV  (N)
      DIMENSION  Y   (N)
      DIMENSION  X   (N)
      CHARACTER*2    LO
 
      IF (LO.EQ.'EQ') THEN
        NB = 0
        DO 110 I=1,N
          LV(I) = Y(I).EQ.X(I)
          IF (LV(I)) NB = NB + 1
 110    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'LT') THEN
        NB = 0
        DO 120 I=1,N
          LV(I) = Y(I).LT.X(I)
          IF (LV(I)) NB = NB + 1
 120    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'LE') THEN
        NB = 0
        DO 130 I=1,N
          LV(I) = Y(I).LE.X(I)
          IF (LV(I)) NB = NB + 1
 130    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'GT') THEN
        NB = 0
        DO 140 I=1,N
          LV(I) = Y(I).GT.X(I)
          IF (LV(I)) NB = NB + 1
 140    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'GE') THEN
        NB = 0
        DO 150 I=1,N
          LV(I) = Y(I).GE.X(I)
          IF (LV(I)) NB = NB + 1
 150    CONTINUE
        GOTO 999
      ENDIF
 
      IF (LO.EQ.'NE') THEN
        NB = 0
        DO 160 I=1,N
          LV(I) = Y(I).NE.X(I)
          IF (LV(I)) NB = NB + 1
 160    CONTINUE
      ENDIF
 
 999  RETURN
      END
