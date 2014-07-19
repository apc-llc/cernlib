*
* $Id: csspar.F,v 1.1.1.1 1996/02/26 17:16:26 mclareni Exp $
*
* $Log: csspar.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:26  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/23 24/10/93  12.20.24  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSSPAR(N,NAME,ITGP)
***---------------------------------
      INTEGER CSLTGP,CSITGP
         CHARACTER *(*) NAME
         PARAMETER (KSETPC=85)
#include "comis/cswpar.inc"
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csrec.inc"
#include "comis/cstab.inc"
*un+SEQ,CSPNTS.
#include "comis/csdpvs.inc"
*un         DATA KBLN/4H    /
      ITGP=0
      CALL CSXPAR(N)
      IF(IREP.GT.0.AND.IBP.EQ.8)THEN
        IREP=0
        IF(IPCB.EQ.0)RETURN
        CALL CSCHID(NAME)
        IP=CSLTGP(IPVSP)
        IREP=-1
        IF(IP.GT.0)RETURN
        IREP=1
        IP=MHLOC(6)
        ITYPGP=1
        IFCS=4
        IADGP=IP+4
        IQ(IP)=0
        IQ(IP+1)=0
        IQ(IP+2)=0
        IQ(IP+3)=0
        IQ(IADGP)=KSETPC
        IQ(IADGP+1)=IPCB
        ITGP=CSITGP(IPVSP)
        NN=IQ(IBASEP-1)
        IF(NN.EQ.0)THEN
          NN=-100
        ELSEIF(NN.GT.0)THEN
          NN=-NN
        ENDIF
        IQ(IBASEP-1)=NN
      ENDIF
      END
