*
* $Id: decays.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: decays.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      SUBROUTINE DECAYS(NTEIL0,NTEIL1)
C.----------------------------------------------------------------------
C.
C.    SUBROUTINE PERFORMS THE DECAY OF THE PRIMARY FRAGMENTATION
C.    PRODUCTS STORED IN THE COMMON /RESULT/. IT LOOPS FROM  NTEIL0
C.    TO NTEIL1 IN /RESULT/. DECAY PRODUCTS ARE ADDED TO /RESULT/.
C.    LAST UPDATE: 05/04/89
C.
C.----------------------------------------------------------------------
#include "eurodec/eudopt.inc"
#include "eurodec/result.inc"
#include "eurodec/ptable.inc"
#include "eurodec/convrt.inc"
#include "eurodec/mixing.inc"
      DO 30 IPAR=NTEIL0,NTEIL1
C--
C--   FULL CASCADE DECAY OF EACH PARTICLE...
        IP1=IPAR
        IP2=IPAR
   10   NTOLD=NTEIL
        DO 20 IP=IP1,IP2
          ID=INDEXT(IP)
          IPPAR=ICONV(IABS(ID))
          IPDEC=IDP(IPPAR)
C--
C--   CALCULATE REAL LIFETIME
          REALLT(IP)=-LOG(EURRAN(IP))*PLT(IPPAR)
          PROMIX(IP)=0.
C--
C--   IS PARTICLE STABLE OR LONG LIVING?
          IF ((IDCAYT(IP).NE.0).OR.(IPDEC.LE.0).OR.(REALLT(IP).GT.
     &    PLIFET)) GOTO 20
          CALL PARDEC(IP,IPPAR,IPDEC)
   20   CONTINUE
        IF (NTEIL.EQ.NTOLD) GOTO 30
        IP1=NTOLD+1
        IP2=NTEIL
        GOTO 10
   30 CONTINUE
      RETURN
      END
