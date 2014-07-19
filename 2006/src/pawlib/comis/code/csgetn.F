*
* $Id: csgetn.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csgetn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.58  by  Rene Brun
*-- Author : V.Berezhnoi
      CHARACTER*1 FUNCTION CSGETN(STR,I,N,NUM)
      CHARACTER*(*) STR,CH*1, CSNOTB*1, DIGIT*10
      SAVE DIGIT
      DATA DIGIT/'0123456789'/
 1    CH=CSNOTB(STR,I,N)
      IF(CH.EQ.' ')GO TO 2
      K=INDEX(DIGIT,CH)
      IF(K.EQ.0)GO TO 2
      NUM=NUM*10+K-1
      GO TO 1
 2    CSGETN=CH
      END
