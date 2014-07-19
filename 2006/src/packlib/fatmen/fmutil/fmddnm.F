*
* $Id: fmddnm.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmddnm.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMDDNM(LUN,CHDD,IRC)
 
      CHARACTER*(*) CHDD
 
      IRC  = 0
      CHDD = ' '
 
#if defined(CERNLIB_UNIX)
      CHDD = 'fort.'
      IF(LUN.LT.10) THEN
         WRITE(CHDD(6:6),'(I1)') LUN
      ELSE
         WRITE(CHDD(6:6),'(I1)') LUN
      ENDIF
 
#endif
#if defined(CERNLIB_VAXVMS)
      CHDD = 'FOR000'
      WRITE(CHDD(4:6),'(I3.3)') LUN
 
#endif
#if defined(CERNLIB_IBM)
      CHDD = 'FT00F001'
      WRITE(CHDD(4:5),'(I2.2)') LUN
#endif
 
      END
