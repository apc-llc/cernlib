*
* $Id: polint.F,v 1.1.1.1 1996/02/15 17:48:35 mclareni Exp $
*
* $Log: polint.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:35  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      SUBROUTINE POLINT(F,ARG,MMMM,Z,SUM)
C         NEW VERSION OF E100 POLINT WRITTEN BY F.JAMES     DEC. 1976
C         LIMITED TO ORDER 19  (20 POINTS)  BY DIMENSION OF COF
C         BUT SUCH INTERPOLATION IS ALREADY UNSTABLE FOR ORDER 10
      DIMENSION F(20), ARG(20),COF(20)
      LOGICAL MFLAG, RFLAG
      IF(MMMM .LT. 2) GOTO 1900
      MM = MIN0(MMMM, 20)
      M = MM - 1
      DO 1780 I= 1, MM
 1780    COF(I) = F(I)
      DO 1800 I= 1, M
      DO 1790 J= I, M
         JNDEX = MM - J
         INDEX = JNDEX + I
         COF(INDEX) = (COF(INDEX)-COF(INDEX-1))/(ARG(INDEX)-ARG(JNDEX))
 1790 CONTINUE
 1800 CONTINUE
      SUM = COF(MM)
      DO 1810 I= 1, M
         INDEX = MM - I
         SUM = (Z-ARG(INDEX))*SUM + COF(INDEX)
 1810 CONTINUE
      RETURN
 1900 CALL KERMTR('E100.1',LGFILE,MFLAG,RFLAG)
      IF(MFLAG) THEN
         IF(LGFILE .EQ. 0) THEN
            WRITE(*,2000) MMMM
         ELSE
            WRITE(LGFILE,2000) MMMM
         ENDIF
      ENDIF
      IF(.NOT. RFLAG) CALL ABEND
      RETURN
 2000 FORMAT( 7X, 'SUBROUTINE POLINT ... K =', I6,
     +        ' IS LESS THAN 2')
      END
