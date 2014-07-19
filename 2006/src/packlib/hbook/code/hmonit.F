*
* $Id: hmonit.F,v 1.1.1.1 1996/01/16 17:07:43 mclareni Exp $
*
* $Log: hmonit.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:43  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 05/03/89  09.22.02  by  Rene Brun
*-- Author :
      SUBROUTINE HMONIT (S,M,NN3,IT,GT,AKAPPA,ALAMBD,
     +A,SIGMA,R,PL,PL0)
*.==========>
*.           ITERATION PROGRESS OUTPUT
*..=========> ( I.Ivanchenko )
      DIMENSION A(1),SIGMA(1),R(1),PL(1),PL0(1)
#include "hbook/hcfit2.inc"
#include "hbook/hcunit.inc"
#include "hbook/hcflag.inc"
      SAVE NM
      DATA NM/0/
*.___________________________________________
      IF (IT) 11,3,1
   1  IF (NN3) 4,4,2
   2  IF (NM) 3,4,4
   3  IF (ENDFLG) 4,12,4
*
*             PRINTER CARRIAGE CONTROL
*
   4  I1=0
      IF(M.GT.20)I1=1
      S2=S+S
      WRITE(LOUT,19) I1,NN3,S2,GT,AKAPPA,ALAMBD
      DO 10 I=1,M
         IF (PL0(I).LE.0.)GO TO 9
         IF (PL(I)) 8,7,6
   6     CONTINUE
         WRITE(LOUT,20) I,A(I),SIGMA(I),R(I)
         GO TO 10
   7     WRITE(LOUT,21) I,A(I),SIGMA(I),R(I)
         GO TO 10
   8     IF (PL(I).GE.-1.) GO TO 7
         WRITE(LOUT,22) I,A(I)
         GO TO 10
   9     WRITE(LOUT,23) I,A(I)
  10  CONTINUE
  11  NM=-IT
  12  NM=NM+1
      IF (ENDFLG) 13,14,14
  13  I=-ENDFLG
      GO TO (15,16,17,18), I
  14  RETURN
  15  CALL HBUG('Error 723','HFIT**',ID)
      GO TO 14
  16  CALL HBUG('724','HFIT**',ID)
      GO TO 14
  17  CALL HBUG('725','HFIT**',ID)
      GO TO 14
  18  WRITE(LERR,27)
      GO TO 14
*
  19  FORMAT (I1,'ITERATION NO.',I3,',2S=',E12.5,', EC = ',E12.5,',KAP',
     1'PA= ',E12.5,', LAMBDA=',E12.5//7X,'PARAMETER',6X,'PARAMETER',9X
     2,'STANDARD',8X,'CORRELATION',/,9X,'NUMBER',9X,'VALUE',11X,'DEV',
     3'IATION',9X,'FACTOR'/)
  20  FORMAT (' ',8X,I3,4X,3(5X,E12.5))
  21  FORMAT (' ',8X,I3,4X,3(5X,E12.5),' PARAMETER ON BOUNDARY')
  22  FORMAT (' ',8X,I3,9X,E12.5,5X,'INFINITE ERROR ESTIMATED')
  23  FORMAT (' ',8X,I3,9X,E12.5,5X,'THIS PARAMETER FIXED')
  27  FORMAT ('0','MINIMISATION TERMINATED AS NEGATIVE OR ZERO Y ENCOU',
     1'NTERED AS LOGARITHMIC ARGUEMENT',/'1')
      END
