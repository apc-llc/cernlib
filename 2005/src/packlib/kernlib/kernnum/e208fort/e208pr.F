*
* $Id: e208pr.F,v 1.1.1.1 1996/02/15 17:48:36 mclareni Exp $
*
* $Log: e208pr.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:36  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE E208PR(N,M,IDIM)
          LOGICAL MFLAG, RFLAG
          K  =  0
          IF(M .GT. IDIM) K  =  3
          IF(M .LT. 1)    K  =  2
          IF(N .LT. M)    K  =  1
          IF(K .NE. 0)  CALL KERMTR('E208.1',LGFILE,MFLAG,RFLAG)
          IF(K .EQ. 0)  CALL KERMTR('E208.2',LGFILE,MFLAG,RFLAG)
          IF(MFLAG)  THEN
             IF(LGFILE .EQ. 0)  THEN
                IF(K .EQ. 0)  WRITE(*,2000) N,M
                IF(K .EQ. 1)  WRITE(*,2001) M,N
                IF(K .EQ. 2)  WRITE(*,2002) M
                IF(K .EQ. 3)  WRITE(*,2003) M,IDIM
             ELSE
                IF(K .EQ. 0)  WRITE(LGFILE,2000) N,M
                IF(K .EQ. 1)  WRITE(LGFILE,2001) M,N
                IF(K .EQ. 2)  WRITE(LGFILE,2002) M
                IF(K .EQ. 3)  WRITE(LGFILE,2003) M,IDIM
             ENDIF
          ENDIF
          IF(.NOT. RFLAG) CALL ABEND
  500     M  =  0
          RETURN
2000      FORMAT( 7X, 34HSUBROUTINE LSQ ... NORMAL EQUATION,
     +            25H MATRIX IS SINGULAR.  N =, I5, 5X,
     +            5H  M =, I5, 1H. )
2001      FORMAT( 7X, 22HSUBROUTINE LSQ ... M =, I5,
     +            13H  EXCEEDS N =, I5, 1H. )
2002      FORMAT( 7X, 22HSUBROUTINE LSQ ... M =, I5,
     +            17H  IS LESS THAN 1. )
2003      FORMAT( 7X, 22HSUBROUTINE LSQ ... M =, I5,
     +            9H  EXCEEDS, I3, 1H. )
      END
