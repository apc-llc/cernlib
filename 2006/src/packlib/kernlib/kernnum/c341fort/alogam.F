*
* $Id: alogam.F,v 1.1.1.1 1996/02/15 17:49:10 mclareni Exp $
*
* $Log: alogam.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:10  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
      REAL FUNCTION ALOGAM(RX)
      REAL RX,SX
      CHARACTER*6 ENAME
      LOGICAL MFLAG,RFLAG
#if defined(CERNLIB_NUMHIPRE)
      REAL P1(7),Q1(7),P2(7),Q2(7),P3(7),Q3(7),C(5),XL(5)
      REAL DLOGAM,X,Y,ZERO,ONE,TWO,HALF,AP,AQ,D
#endif
#if defined(CERNLIB_NUMLOPRE)
      DOUBLE PRECISION P1(7),Q1(7),P2(7),Q2(7),P3(7),Q3(7),C(5),XL(5)
      DOUBLE PRECISION DLOGAM,X,Y,ZERO,ONE,TWO,HALF,AP,AQ,D
      DOUBLE PRECISION DX
#endif
 
      DATA ZERO /0.0D0/, ONE /1.0D0/, TWO /2.0D0/, HALF /0.5D0/
      DATA XL /0.0D0,0.5D0,1.5D0,4.0D0,12.0D0/
      DATA P1
     1/+3.84287 36567 460D+0, +5.27068 93753 010D+1,
     2 +5.55840 45723 515D+1, -2.15135 13573 726D+2,
     3 -2.45872 61722 292D+2, -5.75008 93603 041D+1,
     4 -2.33590 98949 513D+0/
      DATA Q1
     1/+1.00000 00000 000D+0, +3.37330 47907 071D+1,
     2 +1.93877 84034 377D+2, +3.08829 54973 424D+2,
     3 +1.50068 39064 891D+2, +2.01068 51344 334D+1,
     4 +4.57174 20282 503D-1/
      DATA P2
     1/+4.87402 01396 839D+0, +2.48845 25168 574D+2,
     2 +2.17973 66058 896D+3, +3.79751 24011 525D+3,
     3 -1.97780 70769 842D+3, -3.69298 34005 591D+3,
     4 -5.60177 73537 804D+2/
      DATA Q2
     1/+1.00000 00000 000D+0, +9.50999 17418 209D+1,
     2 +1.56120 45277 929D+3, +7.23400 87928 948D+3,
     3 +1.04595 76594 059D+4, +4.16994 15153 200D+3,
     4 +2.76785 83623 804D+2/
      DATA P3
     1/-6.88062 40094 594D+3, -4.30699 69819 571D+5,
     2 -4.75045 94653 440D+6, -2.94234 45930 322D+6,
     3 +3.63218 04931 543D+7, -3.35677 82814 546D+6,
     4 -2.48043 69488 286D+7/
      DATA Q3
     1/+1.00000 00000 000D+0, -1.42168 29839 651D+3,
     2 -1.55528 90280 854D+5, -3.41525 17108 011D+6,
     3 -2.09696 23255 804D+7, -3.45441 75093 344D+7,
     4 -9.16055 82863 713D+6/
      DATA C
     1/ 1.12249 21356 561D-1,  7.95916 92961 204D-2,
     1 -1.70877 94611 020D-3,  9.18938 53320 467D-1,
     2  1.34699 05627 879D+0/
#if defined(CERNLIB_NUMHIPRE)
      ROUND(D)  =  D
#endif
#if defined(CERNLIB_NUMLOPRE)
      ROUND(D)  =  SNGL(D+(D-DBLE(SNGL(D))))
#endif
 
      ENAME='ALOGAM'
      X=RX
#if defined(CERNLIB_NUMLOPRE)
      GOTO 9
      ENTRY DLOGAM(DX)
      ENAME='DLOGAM'
      X=DX
#endif
 
    9 IF(X .LE. XL(1)) THEN
       CALL KERMTR('C341.1',LGFILE,MFLAG,RFLAG)
       IF(MFLAG) THEN
        SX=X
        IF(LGFILE .EQ. 0) THEN
         WRITE(*,100) ENAME,SX
        ELSE
         WRITE(LGFILE,100) ENAME,SX
        ENDIF
       ENDIF
       IF(.NOT.RFLAG) CALL ABEND
       IF(ENAME .EQ. 'ALOGAM')  THEN
          ALOGAM=ZERO
       ELSE
          DLOGAM=ZERO
       ENDIF
       RETURN
      ENDIF
      IF(X .LE. XL(2)) THEN
       Y=X+ONE
       AP=P1(1)
       AQ=Q1(1)
       DO 2 I = 2,7
          AP=P1(I)+Y*AP
    2     AQ=Q1(I)+Y*AQ
       Y=-LOG(X)+X*AP/AQ
      ELSEIF(X .LE. XL(3)) THEN
       AP=P1(1)
       AQ=Q1(1)
       DO 3 I = 2,7
          AP=P1(I)+X*AP
    3     AQ=Q1(I)+X*AQ
       Y=(X-ONE)*AP/AQ
      ELSEIF(X .LE. XL(4)) THEN
       AP=P2(1)
       AQ=Q2(1)
       DO 4 I = 2,7
          AP=P2(I)+X*AP
    4     AQ=Q2(I)+X*AQ
       Y=(X-TWO)*AP/AQ
      ELSEIF(X .LE. XL(5)) THEN
       AP=P3(1)
       AQ=Q3(1)
       DO 5 I = 2,7
          AP=P3(I)+X*AP
    5     AQ=Q3(I)+X*AQ
       Y=AP/AQ
      ELSE
       Y=ONE/X**2
       Y=(X-HALF)*LOG(X)-X+C(4)+(C(1)+Y*(C(2)+Y*C(3)))/
     1                                        ((C(5)+Y)*X)
      ENDIF
      IF(ENAME .EQ. 'ALOGAM')  THEN
         ALOGAM=ROUND(Y)
      ELSE
         DLOGAM=Y
      ENDIF
      RETURN
 
  100 FORMAT(7X,A6,' ... NON-POSITIVE ARGUMENT X = ',E16.6)
      END
