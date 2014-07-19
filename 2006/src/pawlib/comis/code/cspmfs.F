*
* $Id: cspmfs.F,v 1.1.1.1 1996/02/26 17:16:16 mclareni Exp $
*
* $Log: cspmfs.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:16  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/14 12/01/95  16.39.07  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSPMFS(IBCODE)
***--------------------------------
*      this routine calls comis translater
***--------------------------------
      INTEGER CSLTGP,CSITGP
       COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
  1   CALL CSSVPT
      CALL CCOPYS(JSMAIN,JID,8)
      NCIDEN=6
      NWIDEN=2
      CALL CSTRAN(IPCB,IPCE,KPRO,LSTCL)
      IF(KPRO.LE.0)THEN
        CALL CSRSPT(0)
        CALL CSRMCLL(LSTCL)
        CALL CSTRER(IGSST,IREJ)
        IF(IREJ.EQ.0)THEN
          KPRO=0
          KEYRD=0
**IF(Q AND FILE)THEN SKIP UNTIL 'END'
          CALL CSSKUE
          CALL CSLFRE(JPMB)
          JPMC=0
          GO TO 2
        ENDIF
        KEYRD=-1
        JPMC=JPMB
        CALL CSRD(JGSST,NGSST)
        IF (NGSST .LT. 0) GO TO 2
        GO TO 1
      ELSE
        IGP=CSLTGP(IPVS)
        IF(IGP.GT.0)  CALL CSDPRO(IGP)
        CALL CSDTAB(IPLL,IPLL,0)
        IPLL=0
        LCODES=KOD(IPCB+1)
        LDAT=KOD(IPCB+2)
        N=LCODES+LDAT+4
        IP=MHLOC(N)
        IADGP=IP+4
        CALL CCOPYA(KOD(IPCB),KD(IADGP),LCODES)
        ITYPGP=ITYPGI
        IFCS=KPRO
        CALL CSTLOG(JPMB)
        IF(ISTPM.NE.0)THEN
          IQ(IP)=JPMB
          JPMB=0
        ELSE
          IQ(IP)=0
          CALL CSLFRE(JPMB)
        ENDIF
        JPMC=0
        IQ(IP+1)=0
        IQ(IP+2)=0
        IQ(IP+3)=ISHGI
        IF(IGP.GT.0)THEN
          CALL CSRTGP(IGP)
        ELSE
          IGP=CSITGP(IPVS)
        ENDIF
        CALL CSPDLS(IGP)
        CALL CSINCCL(IGP,LSTCL)
        IPTD=IADGP+LCODES
        CALL VZERO(KD(IPTD),LDAT)
        IF(IEXTGB.NE.0)CALL CSEXGB
        IF(LDATA.NE.0)CALL CSPDAT(IPTD)
        CALL CSPTLI(KPRO,IP,IPTD,IGP)
      ENDIF
      IBCODE=IADGP
  2   CSPMFS=KPRO
      END
