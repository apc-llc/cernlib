*
* $Id: uxnewp.F,v 1.1.1.1 1996/02/28 16:23:40 mclareni Exp $
*
* $Log: uxnewp.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE UXNEWIP(COMND,PNAME,LVNEW,IPNEW,LVPAR,IPARM)
*     -------------------------------------------*****-*****-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*      -- arguments --
      CHARACTER*(*) COMND , PNAME
      INTEGER       LVNEW , LVPAR
      INTEGER       IPNEW , IPARM(2)
*
*      -- local variables --
      INTEGER   LOLD , IOLD
      INTEGER          INEW
*
      LOLD=LVPAR
      IOLD=IPARM(LOLD)
      IF(LVNEW.NE.0) THEN
*     -- use LVNEW#0 to choose & take value from IPARM(LVNEW) --
*     but if LVNEW=-1, force the changing of default value with IPNEW !
        LVPAR=IABS(LVNEW)
        IF(LVNEW.EQ.-1) THEN
          INEW=IPNEW
          IPARM(1)=INEW
        ELSE
          IF(LVPAR.NE.1) LVPAR=2
          INEW=IPARM(LVPAR)
        ENDIF
      ELSE
*     -- use IPNEW as a new value --
        LVPAR=2
        IF(IPNEW.EQ.IPARM(1)) LVPAR=1
        INEW=IPNEW
        IF(LVPAR.EQ.2) IPARM(LVPAR)=INEW
      ENDIF
      CALL UXIPARM(COMND,PNAME,LVPAR,IPARM(LVPAR),LOLD,IOLD,IPARM(1))
      END
*
*
      SUBROUTINE UXNEWRP(COMND,PNAME,LVNEW,RPNEW,LVPAR,RPARM)
*     -------------------------------------------*****-*****-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*      -- arguments --
      CHARACTER*(*) COMND , PNAME
      INTEGER       LVNEW , LVPAR
      REAL          RPNEW , RPARM(2)
*
*      -- local variables --
      INTEGER   LOLD
      REAL      ROLD , RNEW
*
      LOLD=LVPAR
      ROLD=RPARM(LOLD)
      IF(LVNEW.NE.0) THEN
*     -- use LVNEW#0 to choose & take value from RPARM(LVNEW) --
*     but if LVNEW=-1, force the changing of default value with RPNEW !
        LVPAR=IABS(LVNEW)
        IF(LVNEW.EQ.-1) THEN
          RNEW=RPNEW
          RPARM(1)=RNEW
        ELSE
          IF(LVPAR.NE.1) LVPAR=2
          RNEW=RPARM(LVPAR)
        ENDIF
      ELSE
*     -- use RPNEW as a new value --
        LVPAR=2
        IF(RPNEW.EQ.RPARM(1)) LVPAR=1
        RNEW=RPNEW
        IF(LVPAR.EQ.2) RPARM(LVPAR)=RNEW
      ENDIF
      CALL UXRPARM(COMND,PNAME,LVPAR,RPARM(LVPAR),LOLD,ROLD,RPARM(1))
      END
*
*
      SUBROUTINE UXNEWCP(COMND,PNAME,LVNEW,CPNEW,LVPAR,CPARM)
*     -------------------------------------------*****-*****-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
*      -- arguments --
      CHARACTER*(*) COMND , PNAME
      INTEGER       LVNEW , LVPAR
      CHARACTER*(*) CPNEW , CPARM(2)
*
*      -- local variables --
      INTEGER   LOLD
      CHARACTER*10     COLD , CNEW
*
      LOLD=LVPAR
      COLD=CPARM(LOLD)
      IF(LVNEW.NE.0) THEN
*     -- use LVNEW#0 to choose & take value from CPARM(LVNEW) --
*     but if LVNEW=-1, force the changing of default value with CPNEW !
        LVPAR=IABS(LVNEW)
        IF(LVNEW.EQ.-1) THEN
          CNEW=CPNEW
          CPARM(1)=CNEW
        ELSE
          IF(LVPAR.NE.1) LVPAR=2
          CNEW=CPARM(LVPAR)
        ENDIF
      ELSE
*     -- use CPNEW as a new value --
        LVPAR=2
        IF(CPNEW.EQ.CPARM(1)) LVPAR=1
        CNEW=CPNEW
        IF(LVPAR.EQ.2) CPARM(LVPAR)=CNEW
      ENDIF
      CALL UXCPARM(COMND,PNAME,LVPAR,CPARM(LVPAR),LOLD,COLD,CPARM(1))
      END
