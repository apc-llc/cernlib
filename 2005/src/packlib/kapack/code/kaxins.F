*
* $Id: kaxins.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxins.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXINS(IZ,LOC,KEY,IDATA,LDATA)
*
*.....INSERT A RECORD IN A BLOCK
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
*
      INTEGER IDATA(*), IZ(*), KEY(*)
*
*-----------------------------------------------------------------------
*
*.....CREATE A GAP FOR THE NEW RECORD, (UNLESS IT GOES AT THE END)
      IF ( LOC .NE. IZ(7) ) CALL UCOPY2(IZ(LOC),IZ(LOC+LREC),IZ(7)-LOC)
      IZ(7) = IZ(7) + LREC
*
*.....INSERT THE NEW RECORD
      IZ(LOC) = LREC
      CALL UCOPY(KEY,IZ(LOC+NRCW),KEY(1))
      IF ( LDATA .NE. 0 )  CALL UCOPY(IDATA,IZ(LOC+NRCW+KEY(1)),LDATA)
*
      END
