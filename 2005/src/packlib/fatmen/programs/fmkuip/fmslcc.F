*
* $Id: fmslcc.F,v 1.1.1.1 1996/03/07 15:17:44 mclareni Exp $
*
* $Log: fmslcc.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:44  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSLCC
#include "fatmen/fatlcc.inc"
 
      IF(NKLCFA.EQ.0) THEN
         PRINT 9001
      ELSE
         PRINT 9002,NKLCFA
         DO 10 I=1,NKLCFA
            PRINT 9003,NLCCFA(I),CHLOCF(I)(1:LENOCC(CHLOCF(I)))
 10      CONTINUE
      ENDIF
9001  FORMAT(' FMSLCC. no location codes defined')
9002  FORMAT(' FMSLCC. ',I2,' locations are defined'/
     +       ' Location code       Location')
9003  FORMAT(4X,I10,7X,A)
      END
