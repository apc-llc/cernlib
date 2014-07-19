*
* $Id: igicol.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: igicol.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.11/00 05/11/90  17.05.58  by  O.Couet
*-- Author : O.Couet
      INTEGER FUNCTION IGICOL(CHVALI)
*.===========>
*.
*.   Convert color name in color index
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHVALI
*.______________________________________
*
      IGICOL=0
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Red')IGICOL=2
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Green')IGICOL=3
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Blue')IGICOL=4
#if !defined(CERNLIB_DECGKS)
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Black')IGICOL=0
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'White')IGICOL=1
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Yellow')IGICOL=5
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Magenta')IGICOL=6
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Cyan')IGICOL=7
#endif
#if defined(CERNLIB_DECGKS)
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'White')IGICOL=0
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Black')IGICOL=1
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Magenta')IGICOL=6
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Cyan')IGICOL=5
      IF(CHVALI(1:LENOCC(CHVALI)).EQ.'Yellow')IGICOL=7
#endif
*
      END
