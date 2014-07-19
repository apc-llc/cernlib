*
* $Id: igifai.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: igifai.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/05 27/02/89  14.13.56  by  O.Couet
*-- Author : O.Couet
      INTEGER FUNCTION IGIFAI(CHVALI)
*.===========>
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHVALI
*.______________________________________
*
      IGIFAI=0
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Solid')IGIFAI=1
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Hollow')IGIFAI=0
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Pattern')IGIFAI=2
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Hatch')IGIFAI=3
*
      END
