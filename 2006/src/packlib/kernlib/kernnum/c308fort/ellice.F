*
* $Id: ellice.F,v 1.1.1.1 1996/02/15 17:49:07 mclareni Exp $
*
* $Log: ellice.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:07  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION ELLICE(RX)
      REAL RX,SX
      CHARACTER*6 ENAME
      LOGICAL MFLAG,RFLAG
#if defined(CERNLIB_NUMHIPRE)
      REAL DELLIE,X,ETA,ZERO,ONE,A(8),B(8),PA,PB,D
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION DELLIE,X,ETA,ZERO,ONE,A(8),B(8),PA,PB,D
      DOUBLE PRECISION DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/
      DATA A
     1/7.09809 64089 987D-4, 7.33561 64974 290D-3,
     2 1.53771 02528 552D-2, 1.30341 46073 731D-2,
     3 1.25105 92410 845D-2, 2.18762 20647 186D-2,
     4 5.68056 57874 695D-2, 4.43147 18112 156D-1/
      DATA B
     1/1.64272 10797 048D-4, 3.48386 79435 896D-3,
     2 1.55251 29948 041D-2, 3.03027 47728 413D-2,
     3 4.23828 07456 948D-2, 5.85828 39536 559D-2,
     4 9.37499 20249 680D-2, 2.49999 99993 618D-1/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      ENAME='ELLICE'
      X=RX
#if defined(CERNLIB_NUMLOPRE)
      GOTO 9
      ENTRY DELLIE(DX)
      ENAME='DELLIE'
      X=DX
#endif
 
    9 U=ABS(X)
      IF(U .LT. ONE) THEN
       ETA=ONE-X**2
       PA=A(1)
       PB=B(1)
       DO 1 I = 2,8
       PA=PA*ETA+A(I)
    1  PB=PB*ETA+B(I)
       PB=ONE+(PA-LOG(ETA)*PB)*ETA
       IF(ENAME .EQ. 'ELLICE')  THEN
          ELLICE  =  ROUND(PB)
       ELSE
          DELLIE  =  PB
       ENDIF
      ELSE IF(U .EQ. ONE) THEN
       IF(ENAME .EQ. 'ELLICE')  THEN
          ELLICE  =  ONE
       ELSE
          DELLIE  =  ONE
       ENDIF
      ELSE
       CALL KERMTR('C308.1',LGFILE,MFLAG,RFLAG)
       IF(MFLAG) THEN
        SX=X
        IF(LGFILE .EQ. 0) THEN
         WRITE(*,100) ENAME,SX
        ELSE
         WRITE(LGFILE,100) ENAME,SX
        ENDIF
       ENDIF
       IF(.NOT.RFLAG) CALL ABEND
       IF(ENAME .EQ. 'ELLICE')  THEN
          ELLICE  =  ZERO
       ELSE
          DELLIE  =  ZERO
       ENDIF
      ENDIF
      RETURN
 
 
  100 FORMAT(7X,A6,' ... ILLEGAL ARGUMENT = ',E16.6)
      END
