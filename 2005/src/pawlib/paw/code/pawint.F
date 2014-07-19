*
* $Id: pawint.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawint.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/06 27/11/93  09.55.51  by  Rene Brun
*-- Author :    Rene Brun   11/11/93
      SUBROUTINE PAWINT(NWPAW)
*
*     Main initialisation routine for PAW
*
      EXTERNAL      PAEXIT
      CHARACTER*80  BATCHF,PAWLOGF
*
      CALL PAWINT1('PAW',BATCHF,PAWLOGF)
*
      CALL PAWINT2(NWPAW,' ',PAEXIT,IWTYP)
*
      CALL PAWINT3
*
      CALL PAWINT4(PAWLOGF)
*
      END
