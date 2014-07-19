*
* $Id: izgntp.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izgntp.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.11/00 27/09/90  15.56.02  by  O.Couet
*-- Author :
      FUNCTION IZGNTP(NT)
*.===========>
*.
*.   This routine searchs in the current picture if the Normalisation
*. Transformation bank NT exits .
*.
*. If IZGNTP=0 : There is no transformation bank NT in the current picture .
*. If IZGNTP>0 : The transformation bank NT exists in the current picture
*.               and IZGNTP gives the link NT bank value .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalisation transformation number .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IZGNTP=0
      IF(LPICT.LE.0)GOTO 99
      LN=LHNT0
      DO 10 I=1,INBNT
         IF(LN.EQ.0)GOTO 99
         IF(IQ(LN+1).EQ.NT)THEN
            IZGNTP=LN
            GOTO 99
         ENDIF
         LN=LQ(LN)
 10   CONTINUE
*
  99  END
#endif
