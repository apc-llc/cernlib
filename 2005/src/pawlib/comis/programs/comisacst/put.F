*
* $Id: put.F,v 1.1.1.1 1996/02/26 17:16:38 mclareni Exp $
*
* $Log: put.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:38  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.06  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE PUT(ARR,N,I,NAME)
      CHARACTER *(*) ARR(N),NAME
      DO 1 K=1,I
        IF(ARR(K).EQ.NAME)RETURN
   1  CONTINUE
      I=I+1
      IF(I.GT.N)THEN
        PRINT 11,N
 11     FORMAT(1X,'NAMES GREATE THEN',I6)
        STOP
      ELSE
        ARR(I)=NAME
      ENDIF
      END
