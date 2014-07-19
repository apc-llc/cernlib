*
* $Id: cscpar.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: cscpar.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.37.00  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSCPAR(N)
***-----------------------
      INTEGER CSINTA
#include "comis/cswpar.inc"
         PARAMETER (KON3=2**16 ,KDLEN=2, NBYTPW=4)
      CALL CSXPAR(N)
      IF(IREP.LE.0)RETURN
      IF(LORN.EQ.1)THEN
        IF(IBP.EQ.1.OR.IBP.EQ.4.OR.IBP.EQ.5)THEN
          IF(IPCB.GT.0)THEN
            IREP=CSINTA(IFPP,IPCB,IPCE)
            IF(IREP.LE.0)RETURN
            JRESP=IREP
          ENDIF
        ENDIF
       ENDIF
       IF(IBP.EQ.0)THEN
         CALL COPYAJ(JRESP,LOCF(IVP),1)
         CALL CCOPYA(IVP,RVP,1)
       ELSEIF(IBP.LT.7)THEN
         IF(ITP.EQ.1)THEN
           IVP=MIWORD(JRESP)
           RVP=IVP
         ELSEIF(ITP.EQ.2)THEN
           RVP=CRWORD(JRESP)
           IVP=RVP
         ELSEIF(ITP.EQ.3)THEN
           NCHVP=MOD(MIWORD(JRESP),KON3)
           JCHVP=MIWORD(JRESP+1)
         ELSEIF(ITP.EQ.4)THEN
           IVP=MIWORD(JRESP)
           IF(IVP.NE.0)IVP=1
         ELSE IF(ITP.EQ.5)THEN
           CALL COPYAJ(JRESP,LOCF(DVP),KDLEN)
         ELSEIF(ITP.EQ.6)THEN
           NCHVP=MIWORD(JRESP-1)*NBYTPW
           JCHVP=JRESP*NBYTPW
         ENDIF
       ELSEIF(IBP.EQ.7.OR.IBP.EQ.9)THEN
         IREP=0
         RETURN
       ELSEIF(IBP.EQ.8)THEN
         IF(IPCB.GT.0)THEN
           IREP=CSINTA(IFPP,IPCB,IPCE)
         ELSE
           IREP=-1
         ENDIF
       ENDIF
      END
