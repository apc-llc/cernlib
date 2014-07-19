*
* $Id: iggfin.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: iggfin.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/00 06/07/92  18.33.16  by  O.Couet
*-- Author :    O.Couet   24/10/90
      SUBROUTINE IGGFIN(IFINO)
*.===========>
*.
*.   Get a Free INdex in the color table for stacked legos.
*.
*..==========> (O.Couet)
#include "higz/hihid.inc"
#include "higz/hilut.inc"
      SAVE IFIN
      DATA IFIN /7/
*.______________________________________
*
      IFINO1=IFIN
   10 IFINO1=IFINO1+1
      IF(IFINO1.GT.NBCLUT)THEN
         IFINO1=7
         GOTO 10
      ENDIF
      IF(IFINO1.EQ.ISTCOT)GOTO 10
      IF(IFINO1.EQ.ISTCOB)GOTO 10
      DO 20 I=1,NIDS+1
         IF(IFINO1.EQ.ISTCOL(I))GOTO 10
         IF(IFINO1.EQ.ISTCO2(I))GOTO 10
   20 CONTINUE
      IFIN=IFINO1
      IFINO=IFINO1
*
      END
