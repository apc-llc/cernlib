*
* $Id: guout.F,v 1.2 1996/02/01 14:54:50 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/01 14:54:50  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
*
C.    ******************************************************************
C.    *                                                                *
C.    *       User routine called at the end of each event.            *
C.    *                                                                *
C.    ******************************************************************
C.
C.
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "cdhscom.inc"
      SAVE ID101,ID102,ID103,ID104
      DATA ID101,ID102,ID103,ID104/4*0/
C.
C.    ------------------------------------------------------------------
C.
      ENEP = 0.0
      ENEP2 = 0.0
      NACT = 0
      DO 10 J=1,16
        ESTOT=VSUM(ESINT(1,J),8)
        CALL HFF1(104,ID104,ESTOT,1.0)
        ENEP=ENEP+ESTOT
        ENEP2=ENEP2+ESTOT**2
        IF(ESTOT.NE.0.0) NACT=NACT+1
  10  CONTINUE
      IF(NACT.GT.0) THEN
        EMEAN = ENEP / FLOAT(NACT)
        ERMS  = ENEP2 / FLOAT(NACT) - EMEAN**2
        ERMS  = SQRT(ERMS)
      ENDIF
*
      ETOT = 0.0
      DO 30 J=1,16
         ENEP=0.
         DO 20 K=1,8
            ENEPK=ESINT(K,J)/EMUON
            IF(ENEPK.GT.TRESHO)ENEP=ENEP+ENEPK
  20     CONTINUE
         IF(J.EQ.1)ENEP1=ENEP
         IF(ENEP.GT.TRESHO) THEN
            ETOT = ETOT+ENEP
            CALL HFF1(102, ID102,FLOAT(J), ENEP)
            IF(ENEP1.GT.TRIGGE) CALL HFF1(103, ID103,FLOAT(J), ENEP)
         ENDIF
  30  CONTINUE
      ETOT = ETOT * 2.5
      CALL HFF1(101, ID101,ETOT, 1.0)
*
      END
