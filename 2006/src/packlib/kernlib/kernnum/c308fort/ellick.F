*
* $Id: ellick.F,v 1.1.1.1 1996/02/15 17:49:07 mclareni Exp $
*
* $Log: ellick.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:07  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION ELLICK(RX)
      REAL RX,SX
      CHARACTER*6 ENAME
      LOGICAL MFLAG,RFLAG
#if defined(CERNLIB_NUMHIPRE)
      REAL DELLIK,X,ETA,ZERO,ONE,HALF,C,A(8),B(8),PA,PB,D
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION DELLIK,X,ETA,ZERO,ONE,HALF,C,A(8),B(8),PA,PB,D
      DOUBLE PRECISION DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/, HALF /0.5D0/
      DATA C /1.38629 43611 199D0/
      DATA A
     1/6.49984 43329 390D-4, 6.69055 09906 898D-3,
     2 1.38556 01247 157D-2, 1.12089 18554 644D-2,
     3 9.65875 79861 753D-3, 1.49789 88178 705D-2,
     4 3.08855 73486 753D-2, 9.65735 90797 589D-2/
      DATA B
     1/1.50491 81783 602D-4, 3.18313 09927 863D-3,
     2 1.41053 80776 158D-2, 2.71898 61116 788D-2,
     3 3.70683 98934 155D-2, 4.88180 58565 404D-2,
     4 7.03124 26464 627D-2, 1.24999 99994 118D-1/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      ENAME='ELLICK'
      X=RX
#if defined(CERNLIB_NUMLOPRE)
      GOTO 9
      ENTRY DELLIK(DX)
      ENAME='DELLIK'
      X=DX
#endif
 
    9 IF(ABS(X) .LT. ONE) THEN
       ETA=ONE-X**2
       PA=A(1)
       PB=B(1)
       DO 1 I = 2,8
       PA=PA*ETA+A(I)
    1  PB=PB*ETA+B(I)
       PB=C+PA*ETA-LOG(ETA)*(HALF+PB*ETA)
       IF(ENAME .EQ. 'ELLICK')  THEN
          ELLICK  =  ROUND(PB)
       ELSE
          DELLIK  =  PB
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
       IF(ENAME .EQ. 'ELLICK')  THEN
          ELLICK  =  ZERO
       ELSE
          DELLIK  =  ZERO
       ENDIF
      ENDIF
      RETURN
 
  100 FORMAT(7X,A6,' ... ILLEGAL ARGUMENT = ',E16.6)
      END
