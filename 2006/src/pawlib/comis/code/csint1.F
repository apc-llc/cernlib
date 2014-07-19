*
* $Id: csint1.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: csint1.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.50  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSINT1(IPCB)
***--------------------------------------------
*      calls the  interpreter
***--------------------------------------------
      INTEGER CSINTX
#include "comis/cspar.inc"
#include "comis/cspnts.inc"
      INTEGER IFPARR(LRECU)
      EQUIVALENCE (IFP,IFPARR(1))
      INTEGER IMNPNM(2)
       DATA IMNPNM/4H*MAI, 4HN*  /
      IRECU=IRECU+LRECU
**      CALL CCOPYA(IFP,MRECU(IRECU),LRECU)
      DO 11 I=1,LRECU
 11     MRECU(IRECU-1+I)=IFPARR(I)
      IF(IRECU.GE.MXRECU)GO TO 708
*                        ----------
      IBASE=IPCB
      IPC=IPCB
      IFP=0
      IAP=0
      IJMPT=1
      IPARCE=0
**      CALL CCOPYA(IMNPNM(1),NAMEST(1),2)
      NAMEST(1)=IMNPNM(1)
      NAMEST(2)=IMNPNM(2)
      CSINT1=CSINTX(IPARCE,IJMPT)
    1 CONTINUE
**      CALL CCOPYA(MRECU(IRECU),IFP,LRECU)
      DO 12 I=1,LRECU
  12    IFPARR(I)=MRECU(IRECU-1+I)
      IRECU=IRECU-LRECU
      RETURN
  708 CALL CSIERR(-8)
      GO TO 1
      END
