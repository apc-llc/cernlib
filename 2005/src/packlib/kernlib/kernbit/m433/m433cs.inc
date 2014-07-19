*
* $Id: m433cs.inc,v 1.1.1.1 1996/02/15 17:47:49 mclareni Exp $
*
* $Log: m433cs.inc,v $
* Revision 1.1.1.1  1996/02/15 17:47:49  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNBIT_M433CS_INC
#define CERNLIB_KERNBIT_M433CS_INC
*
*
* m433cs.inc
*
C
      COMMON/M433CS/ICHSEP,CHSEP
      CHARACTER*1 CHSEP
C
      IF(ICHSEP.NE.1) THEN
        ICHSEP=1
        CHSEP=' '
      ENDIF
C

#endif
