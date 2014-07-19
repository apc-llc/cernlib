*
* $Id: izghnp.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izghnp.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.17/00 05/02/93  11.31.22  by  O.Couet
*-- Author :
      INTEGER FUNCTION IZGHNP(LP)
*.===========>
*.
*.   This function Gives the Higher Normalisation
*. transformation Priority in the picture LP .
*.
*. _Input parameters:
*.
*.  INTEGER LP : Picture link .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IZGHNP=1
      IF(LP.LE.0)RETURN
      LN=LQ(LP-1)
      DO 10 I=1,IQ(LP+8)
         IF(LN.EQ.0)RETURN
         IF(IQ(LN+3).GT.IZGHNP)IZGHNP=IQ(LN+3)
         LN=LQ(LN)
 10   CONTINUE
*
      END
#endif
