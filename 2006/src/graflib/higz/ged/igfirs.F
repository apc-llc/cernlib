*
* $Id: igfirs.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: igfirs.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.06/05 27/02/89  14.13.05  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGFIRS(CHDEF,STRING)
*.===========>
*.
*.   This routine puts the STRING in the first place in CHDEF.
*.
*..==========> (O.Couet)
      CHARACTER*(*) CHDEF,STRING
      CHARACTER*80 CHINT,CHINT2
*.______________________________________
*
      ILEN1=LENOCC(CHDEF)
      ILEN2=LENOCC(STRING)
      I1=INDEX(CHDEF,STRING(1:ILEN2))
      CHINT=' '
      CHINT2=' '
      IF(I1.EQ.1)RETURN
      CHINT=CHDEF(I1:ILEN1)
      II2=LENOCC(CHINT)
      CHINT2=CHDEF(1:I1-2)
      I2=LENOCC(CHINT2)
      CHDEF=CHINT(1:II2)//','//CHINT2(1:I2)
*
      END
