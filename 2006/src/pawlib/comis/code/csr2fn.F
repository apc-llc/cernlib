*
* $Id: csr2fn.F,v 1.1.1.1 1996/02/26 17:16:28 mclareni Exp $
*
* $Log: csr2fn.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:28  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 04/10/93  17.32.40  by  Vladimir Berezhnoi
*-- Author :
      FUNCTION CSR2FN(JSUB,P1,P2)
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
        IPCB=MHLOC(12)
        IQ(IPCB)=KLK
        IQ(IPCB+2)=KLK
        IQ(IPCB+4)=KCALLS
        IQ(IPCB+6)=2
        IQ(IPCB+7)=102
        IQ(IPCB+8)=102
        IQ(IPCB+9)=KEXIT
      ENDIF
      IF(IQ(JSUB+KSIFCS).GT.0)THEN
*       call cs-routine
        IA(1)=LOCF(P1)
        IA(2)=LOCF(P2)
        IPC=IQ(JSUB+KSADGP)
        IBASE=IPC
        IAP=0
        IFP=3
        ITA=11
*           numst=-1
        IA(11)=-1
      ELSE
*       call f77-routine
        NAMEST(1)=IMNPNM(1)
        NAMEST(2)=IMNPNM(2)
        IQ(IPCB+1)=LOCF(P1)
        IQ(IPCB+3)=LOCF(P2)
        IQ(IPCB+5)=JSUB
        IBASE=IPCB
        IPC  =IPCB
        IFP=0
        IAP=0
      ENDIF
      IJMPT=1
      INT1=CSINTX(0,IJMPT)
      CSR2FN=RCSRES
      END
