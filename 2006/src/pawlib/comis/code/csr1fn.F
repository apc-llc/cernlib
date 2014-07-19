*
* $Id: csr1fn.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: csr1fn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 04/10/93  17.31.25  by  Vladimir Berezhnoi
*-- Author :
      FUNCTION CSR1FN(JSUB,P1)
***--------------------------------------------
*     fast  calls the  interpreter
***--------------------------------------------
#include "comis/cspar.inc"
#include "comis/cstabps.inc"
#include "comis/csfres.inc"
#include "comis/comis.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      INTEGER CSINTX, IMNPNM(2), IPCB
      PARAMETER (KLK=45, KCALLS=87, KEXIT=113)
      DATA IMNPNM/4H*MAI, 4HN*  /, IPCB/0/
*
      IF(IPCB.EQ.0)THEN
        IPCB=MHLOC(8)
        IQ(IPCB)=KLK
        IQ(IPCB+2)=KCALLS
        IQ(IPCB+4)=1
        IQ(IPCB+5)=102
        IQ(IPCB+6)=KEXIT
      ENDIF
      IF(IQ(JSUB+KSIFCS).GT.0)THEN
*       call cs-routine
        IPC=IQ(JSUB+KSADGP)
        IA(1)=LOCF(P1)
        IBASE=IPC
        IAP=0
        IFP=2
        ITA=10
* truck with numst=-1
        IA(10)=-1
      ELSE
*       call f77-routine
        NAMEST(1)=IMNPNM(1)
        NAMEST(2)=IMNPNM(2)
        IQ(IPCB+1)=LOCF(P1)
        IQ(IPCB+3)=JSUB
        IBASE=IPCB
        IPC  =IPCB
        IFP=0
        IAP=0
      ENDIF
      IJMPT=1
      INT1=CSINTX(0,IJMPT)
      CSR1FN=RCSRES
      RETURN
      END
