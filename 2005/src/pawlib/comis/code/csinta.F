*
* $Id: csinta.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: csinta.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.50  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSINTA(IFP1,IPC1,IPC2)
***--------------------------------------------
*       recurtion entry to interpreter
***--------------------------------------------
#if !defined(CERNLIB_IBM)
      INTEGER CSINTX
#endif
#if defined(CERNLIB_IBM)
      INTEGER CSINTZ
#endif
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
      INTEGER IFPARR(LRECU)
      EQUIVALENCE (IFP,IFPARR(1))
      IRECU=IRECU+LRECU
**      CALL CCOPYA(IFP,MRECU(IRECU),LRECU)
      DO 11 I=1,LRECU
 11     MRECU(IRECU-1+I)=IFPARR(I)
      IF(IRECU.GE.MXRECU)GO TO 708
*                        ----------
      IFP=0
      CALL CCOPYA(IA(IFP1+1),IAP,3)
      IPC=IPC1
      IPARCE=IPC2
      IJMPT=0
#if !defined(CERNLIB_IBM)
 999  CSINTA=CSINTX(IPARCE,IJMPT)
#endif
#if defined(CERNLIB_IBM)
 999  CSINTA=CSINTZ(IPARCE,IJMPT)
#endif
  998 CONTINUE
**      CALL CCOPYA(MRECU(IRECU),IFP,LRECU)
      DO 12 I=1,LRECU
  12    IFPARR(I)=MRECU(IRECU-1+I)
      IRECU=IRECU-LRECU
      RETURN
  708 CALL CSIERR(-8)
      GO TO 998
      END
