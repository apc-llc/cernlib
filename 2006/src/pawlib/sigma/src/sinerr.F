*
* $Id: sinerr.F,v 1.1.1.1 1995/12/12 14:36:18 mclareni Exp $
*
* $Log: sinerr.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:18  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.19  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SINERR(LIERR)
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/sicipa.inc"
C
      IERRNO=LIERR
      IQUEST(1)=-LIERR
      CALL SIERRO(LIERR)
      PRINT *, ' **** THE  OFFENDING STATEMENT IS'
      LE=LENOCC(CSTA1)
      PRINT *, CSTA1(1:LE)
      IPOINT = 1
      ISTAPO=0
      ICMPI=1
      ITYPE=0
      ISTRI=0
      MODE=1
******************************************
      CALL KUVDEL('$*')
******************************************
 999  END
