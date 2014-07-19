*
* $Id: sortch.F,v 1.1.1.1 1996/03/07 15:18:06 mclareni Exp $
*
* $Log: sortch.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:06  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE SORTCH (CHTB,JSORT,NELEM)
*
      CHARACTER*(*) CHTB(NELEM)
      CHARACTER*255 WORK
      DIMENSION     JSORT(NELEM)
*
      NC = MIN(LEN (CHTB(1)),255)
      DO 10 I=1,NELEM
   10 JSORT(I) = I
 
      N = NELEM
      DO 50 I1=2,N
         I3 = I1
         I33 = JSORT(I3)
         WORK(1:NC) = CHTB(I33)(1:NC)
   20    I2 = I3/2
*        IF (I2)                                  50,50,30
         IF (I2.LE.0)                             GOTO 50
   30    I22 = JSORT(I2)
         IF(LLE(WORK(1:NC),CHTB(I22)(1:NC)))      GOTO 50
   40    JSORT (I3) = I22
         I3 = I2
                                                  GOTO 20
   50 JSORT (I3) = I33
   60 I3 = JSORT (N)
      JSORT (N) = JSORT (1)
      WORK(1:NC) = CHTB(I3)(1:NC)
      N = N-1
*     IF (N-1)                                    140,140,70
      IF (N-1.LE.0)                               GOTO 140
   70 I1 = 1
   80 I2 = I1 + I1
      IF (I2.LE.N) I22= JSORT(I2)
*     IF (I2-N)                                   90,110,130
      IF (I2.EQ.N) THEN
         GOTO 110
      ELSEIF(I2.GT.N) THEN
         GOTO 130
      ENDIF
   90 I222 = JSORT (I2+1)
      IF (LGE(CHTB(I22)(1:NC),CHTB(I222)(1:NC)))  GOTO 110
  100 I2 = I2+1
      I22 = I222
  110 IF (LGE(WORK(1:NC),CHTB(I22)(1:NC)))        GOTO 130
  120 JSORT(I1) = I22
      I1 = I2
                                                  GOTO 80
  130 JSORT (I1) = I3
                                                  GOTO 60
  140 JSORT (1) = I3
      RETURN
      END
 
