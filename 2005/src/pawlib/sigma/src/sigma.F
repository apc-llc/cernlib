*
* $Id: sigma.F,v 1.1.1.1 1995/12/12 14:36:17 mclareni Exp $
*
* $Log: sigma.F,v $
* Revision 1.1.1.1  1995/12/12 14:36:17  mclareni
* Imported sources
*
*
#include "sigma/pilot.h"
*CMZ :  1.09/04 26/10/93  09.49.18  by  Carlo E. Vandoni
*-- Author :
      SUBROUTINE SIGMA(CSTAT)
#include "sigma/sigc.inc"
#include "sigma/pawc.inc"
#include "sigma/sicst4.inc"
C
#include "sigma/sicipa.inc"
      CHARACTER *(*) CSTAT
C     ITRAK =  1   FOR INAME PRINTOUT
C     ITRAK =  2
C     ITRAK =  3
C     ITRAK =  4   FOR COMPILER TRACING: COMPILE,DIS,GETAREA,RELAREA)
C     ITRAK =  5   SICMPR,SIINPS,OUTPUT
C     ITRAK =  6   SICOMA
C     ITRAK =  7   SICOMA, MAINLO
C     ITRAK = 10   SITRAX
C     ITRAK = 12   COMPIL,OUTPUT
C     ITRAK = 20   GENERAL TRACING
C     ITRAK = 30
C     ITRAK = 33   SIASSI
C     ITRAK = 34   SINDXI
C     ITRAK = 36   NUMCOD
C     ITRAK = 37   DROP,ORDER,TRACOP
C     ITRAK = 38   DROP,ORDER,TRACOP,SUMN
C
      IPASW=1
      IERRNO=0
      CSTA1=CSTAT
      IADDR=1
C---
C---  COMPILER PREPARATION
 1    CONTINUE
      CALL SICOMA
      IF(IQUEST(1).NE.0) GO TO 10
      CALL SINTRP
C
      IF(IERRNO.NE.0)THEN
         IQUEST(1)=-IERRNO
         IERRNO=0
         GOTO 20
      ENDIF
10    RETURN
20    CONTINUE
         CALL SIINIT
         IADDR=1
         LINE=1
         GOTO 1
      END
