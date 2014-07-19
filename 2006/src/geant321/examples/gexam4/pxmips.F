*
* $Id: pxmips.F,v 1.2 1996/02/01 15:00:37 ravndal Exp $
*
* $Log: pxmips.F,v $
* Revision 1.2  1996/02/01 15:00:37  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:15  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE PXMIPS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *     Print MIPS                                                 *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcflag.inc"
#include "urmips.inc"
C
C              Compute average number of MIPS
C
      IF(NTXMIP.GT.0)THEN
         TXM=TXMIP/NTXMIP
         TXMRMS=SQRT(MAX(TXMIP2/NTXMIP - TXM*TXM,0.))
         PRINT 1000,IEVENT,XMIP,TXM,TXMRMS
 1000    FORMAT(' =====> IEVENT=',I5,' MIPS =',F7.1,
     +          '  TOTAL MIPS =',F7.1,' +-',F7.1)
      ENDIF
C
      END
