*
* $Id: arprda.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
*
* $Log: arprda.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:05  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arprda.F,v 1.1.1.1 1996/03/08 16:51:05 mclareni Exp $
 
      SUBROUTINE ARPRDA
 
C...ARiadne subroutine PRint DAta
 
C...Prints out parameters and switches used in Ariadne.
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
 
      WRITE(MSTA(7),*)
      WRITE(MSTA(7),1000)
      DO 100 I=1,20
        WRITE(MSTA(7),1010) I,MSTA(I),MSTA(I+20),PARA(I),PARA(I+20)
 100  CONTINUE
      WRITE(MSTA(7),*)
 
 1000 FORMAT(10X,'Parameters and switches used by Ariadne:',/,/,
     $     '         I   MSTA(I) MSTA(I+20)   PARA(I) PARA(I+20)',/)
 1010 FORMAT(2I10,I11,3X,2G11.3)
 
      RETURN
 
C**** END OF ARPRDA ****************************************************
      END
