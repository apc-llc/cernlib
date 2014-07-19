*
* $Id: fml2dd.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fml2dd.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FML2DD(LUN,DDNAME,IRC)
*
*     Convert a FORTRAN logical unit to the corresponding DDNAME
*
      CHARACTER*8 DDNAME
 
      IRC = 0
#if defined(CERNLIB_IBMVM)
      DDNAME = 'FT00F001'
      WRITE(DDNAME(3:4),9001) LUN
9001  FORMAT(I2)
#endif
#if defined(CERNLIB_VAXVMS)
      DDNAME = 'FOR00N'
      WRITE(DDNAME(4:6),9002) LUN
9002  FORMAT(I3)
#endif
#if defined(CERNLIB_UNIX)
      DDNAME = 'fort.   '
      IF(LUN.LT.10) THEN
        WRITE(DDNAME(6:6),'(I1)') LUN
        ELSE
        WRITE(DDNAME(6:7),'(I2)') LUN
        ENDIF
#endif
*
*     Now replace blanks by zeroes
*
      CALL CTRANS(' ','0',DDNAME,1,LENOCC(DDNAME))
      END
 
