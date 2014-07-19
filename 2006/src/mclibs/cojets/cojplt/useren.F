*
* $Id: useren.F,v 1.1.1.1 1996/01/11 14:15:06 mclareni Exp $
*
* $Log: useren.F,v $
* Revision 1.1.1.1  1996/01/11 14:15:06  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE USEREN
C     *****************
C
C          HANDLES INFORMATION FROM FINAL RECORD
C
C          COJETS COMMON BLOCKS
#include "cojets/final.inc"
#include "cojets/itapes.inc"
C
C          WRITE HERE INFORMATION FROM FINAL RECORD
C-----------------------------------------------------------------------
C-
      WRITE(ITLIS,6) NEVTAP,SIGF
6     FORMAT(/////1X,'*****EVENTS READ FROM TAPE:',I10
     1//1X,'*****CROSS SECTION        :',E12.3,' MILLIBARN'/////)
C-
C-----------------------------------------------------------------------
C
      RETURN
      END
