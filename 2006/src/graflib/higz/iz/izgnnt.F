*
* $Id: izgnnt.F,v 1.1.1.1 1996/02/14 13:11:10 mclareni Exp $
*
* $Log: izgnnt.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:10  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)
*CMZ :  1.23/00 10/04/95  10.38.45  by  O.Couet
*-- Author : O.Couet
      FUNCTION IZGNNT(NT)
*.===========>
*.
*.   This function returns in the current picture the Normalisation
*. Transformation stored after NT. This is useful to loop over all
*. the transformations in a picture. If -1 is return the transformation
*. NT has not be found, or the last NT has been found.
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalisation transformation number .
*.
*..==========> (O.Couet)
#include "higz/hipaw.inc"
*.______________________________________
*
      IZGNNT = -1
      IF (LPICT.LE.0) GOTO 999
      LN = LHNT0
      DO 10 I=1,INBNT
         IF (LN.EQ.0) THEN
            GOTO 999
         ELSE
            IF (IQ(LN+1).EQ.NT) THEN
               LN = LQ(LN)
               IF (LN.EQ.0) THEN
                  GOTO 999
               ELSE
                  IZGNNT = IQ(LN+1)
                  GOTO 999
               ENDIF
            ENDIF
            LN = LQ(LN)
         ENDIF
   10 CONTINUE
*
  999 END
#endif
