*
* $Id: dzdsco.F,v 1.1.1.1 1996/03/04 16:12:56 mclareni Exp $
*
* $Log: dzdsco.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:56  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSCO(CHOPT,IFLOPT)
      INTEGER IFLOPT(26)
      CHARACTER*(*) CHOPT
      CHARACTER*26 CALPHA
      PARAMETER (CALPHA='ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      DO 10 I=1,26
   10 IFLOPT(I) = 0
      DO 20 I=1,LEN(CHOPT)
      II =  INDEX(CALPHA,CHOPT(I:I))
      IF(II.GT.0)IFLOPT(II)=II
   20 CONTINUE
      END
*************************************************************
