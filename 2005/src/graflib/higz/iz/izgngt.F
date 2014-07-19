*
* $Id: izgngt.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izgngt.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.02  by  O.Couet
*-- Author :
      INTEGER FUNCTION IZGNGT(LP,NGT)
*.===========>
*.
*.   This function Gives the link to the first normalisation transformation
*. which priority is Greater Than NGT in the picture LP .
*.
*. _Input parameters:
*.
*.  INTEGER LP : Picture link .
*.  INTEGER NGT : Lower value of priority .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IZGNGT=0
      LN=LQ(LP-1)
      IGP=IZGHNP(LP)
      DO 10 I=1,IQ(LP+8)
         IF(LN.EQ.0)GOTO 99
         IP=IQ(LN+3)
         IF(IP.GT.NGT)THEN
            IF(IP.LE.IGP)THEN
               IGP=IP
               IZGNGT=LN
            ENDIF
         ENDIF
         LN=LQ(LN)
 10   CONTINUE
*
 99   END
#endif
