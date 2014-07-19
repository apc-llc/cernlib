*
* $Id: dumrzlx.F,v 1.1.1.1 1996/02/15 17:51:33 mclareni Exp $
*
* $Log: dumrzlx.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:33  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE DUMRZLX (A,B,X,CA)
 
C CERN PROGLIB# N202    DUMRZLX         .VERSION KERNCDC  1.13  820111
 
C-    PROGRAM TAKEN FROM CERN PROGLIB  15-OCT-77
 
      DIMENSION A(8),B(8),X(8),CA(8)
 
      PRINT 2,A(1),B(1),X(1)
      DO 100 I=1,7
      IF(I.EQ.2) PRINT 1,CA(1),I,A(I+1),I,B(I+1),I,CA(I+1),I,X(I+1)
      IF(I.NE.2) PRINT 3,      I,A(I+1),I,B(I+1),I,CA(I+1),I,X(I+1)
100   CONTINUE
      PRINT 4
      RETURN
 
    1 FORMAT (20H DUMRZL CALLED FROM ,O6,5X,1HA,I1,2X,O6,4X,1HB,I1,2X,
     + O6,4X,3HC(A,I1,4H)=  ,O20,4X,1HX,I1,2X,O20)
    2 FORMAT (18H0REGISTER CONTENTS,13X,4HA0  ,O6,4X,4HB0  ,O6,36X,
     + 4HX0  ,O20)
    3 FORMAT (31X,1HA,I1,2X,O6,4X,1HB,I1,2X,O6,4X,3HC(A,I1,4H)=  ,O20,
     + 4X,1HX,I1,2X,O20)
    4 FORMAT (1X)
      END
