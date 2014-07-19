*
* $Id: imfuwk.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: imfuwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE IMFUWK(ITY)
#include "higz/hiatt.inc"
#include "higz/hikern.inc"
#include "higz/himeta.inc"
      DIMENSION PAR(2)
      IF (ITY .EQ. ISURF) THEN
         DY=RDWYMA-RDWYMI
         DX=RDWXMA-RDWXMI
*
         IF(DY.GT.DX)THEN
            PAR(1)=(RDVXMA)/DX
         ELSE
            PAR(1)=(RDVYMA)/DY
         ENDIF
         PAR(2) = PAR(1)/SCMETA
         SCMETA = PAR(1)
         PAR(1) = PAR(2)
         CALL GOLWRI(SC,PAR,' ')
      END IF
      END
#endif
