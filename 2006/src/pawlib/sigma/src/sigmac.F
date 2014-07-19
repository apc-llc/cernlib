*
* $Id: sigmac.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigmac.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGMAC(CHCOMP,ITABCP,LCOMP)
#include "sigma/sigc.inc"
#include "sigma/sitab.inc"
#include "sigma/sicst4.inc"
#include "sigma/sicipa.inc"
      COMMON/PAWSIG/CHCOM
      DIMENSION DYNA(4096)
      DIMENSION ITABCP(*)
      CHARACTER CHCOMP*(*)
      CHARACTER*500 CHCOM
C
      IPOINT=1
      LINE=1
      IPASW=-1
      CHCOM=CHCOMP
      CALL SICMPL
      LCOMP=IPOINT
      CALL UCOPY(IPROGR,ITABCP,LCOMP)
 999  END
