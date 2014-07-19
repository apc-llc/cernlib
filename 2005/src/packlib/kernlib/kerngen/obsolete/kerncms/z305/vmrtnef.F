*
* $Id: vmrtnef.F,v 1.1.1.1 1996/02/15 17:51:55 mclareni Exp $
*
* $Log: vmrtnef.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:55  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_QCMS4)
C
C CERN PROGLIB#    Z305 VMRTNE          .VERSION KERNCMS  1.05  910308
C ORIG. 08/03/91, Miguel Marquina
C FORTRAN version of VMRTNE in CMS4 (using Rexx Local Function DOCONS)
C
      SUBROUTINE VMRTNE(PW,LENG)
      CHARACTER*(*) PW
      CALL VMCMS('EXEC VMRTNE',IRC)
      CALL VMRTRM(PW,LENG)
      RETURN
      END
#endif
