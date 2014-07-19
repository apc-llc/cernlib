*
* $Id: kskpb.F,v 1.1.1.1 1996/02/26 17:16:40 mclareni Exp $
*
* $Log: kskpb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:40  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.05  by  Rene Brun
*-- Author : V.Berezhnoi
      FUNCTION KSKPB(J,N)
*
*   FIND THE NEAREST BLANK OR TAB,
*   THEN SKIP BLANKS AND TABS
*
    1 IF(N.LE.0) RETURN
      K=MIKCHA(J)
      IF(K.EQ.32) GO TO 2
      IF(K.EQ.9) GO TO 2
      J=J+1
      N=N-1
      GO TO 1
    2 KSKPB=MKBLAN(J,N)
      RETURN
      END
