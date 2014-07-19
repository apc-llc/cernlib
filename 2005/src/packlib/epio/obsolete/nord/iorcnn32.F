*
* $Id: iorcnn32.F,v 1.1.1.1 1996/03/08 15:21:58 mclareni Exp $
*
* $Log: iorcnn32.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:58  mclareni
* Epio
*
*
#include "epio/pilot.h"
#if defined(CERNLIB_ND50)||defined(CERNLIB_ND500)
      FUNCTION IORCNV(NERR)
C     *********************
C   CONVERTS FROM NORD FILE ERROR TO EP PACKET FORMAT (IBM)
C   NERR   = NORD FILE ERROR
C   IORCNV = EP FILE ERROR
      INTEGER*4 IORCNV,NERR
      DIMENSION NFE(15),NEP(15)
      DATA NFE/3,102,107,115,116,117,118,121,122,123,124,125,
     +           130,132,133/
      DATA NEP/2,  4,  1,  3,  3,  3,  1,  1,  1,  1,  1,  1,
     +             1,  1,  1/
      NER1=NERR
      DO 10 I=1,15
   10 IF(NER1.EQ.NFE(I)) GOTO 20
      IORCNV=1
77777 RETURN
   20 IORCNV=NEP(I)
      GOTO 77777
      END
#endif
