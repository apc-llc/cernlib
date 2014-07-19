*
* $Id: xzlcat.F,v 1.1.1.1 1996/03/08 15:44:30 mclareni Exp $
*
* $Log: xzlcat.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:30  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_IBMMVS)
      SUBROUTINE XZLCAT(LEVEL,DSN,MAXDSN,FILLED,FLAG)
*                              --- ------ ------ ----
*
*     LEVEL - MVS catalogue level to be listed
*     DSN   - character array of length MAXDSN filled up to FILLED
*     FLAG  - 1 = more calls needed for full listing
*           - 0 = finished
*                                         (c) J.H.Peters, DESY, 8.3.1991
************************************************************************
 
      CHARACTER*(*) LEVEL,DSN(MAXDSN)
      INTEGER       MAXDSN,FILLED,FLAG
 
      CHARACTER     OUTLINE*133,INLINE*80
      INTEGER       RETC,OUTLGTH
 
      FLAG = 1
      INLINE  = ' LISTCAT  LEVEL('//LEVEL//') NONVSAM'
      FILLED = 0
 
   1  CALL FORTIDC(INLINE,OUTLINE,OUTLGTH,RETC)
      IF (RETC.EQ.-9999) THEN
         FLAG = 0
         GOTO 99
      ENDIF
      IF (OUTLINE(1:8).EQ.'0NONVSAM') THEN
          FILLED = FILLED + 1
          IF (FILLED.LE.MAXDSN) THEN
              DSN(FILLED) = OUTLINE(18:OUTLGTH)
              IF (FILLED.EQ.MAXDSN) RETURN
          ENDIF
      ENDIF
      GOTO 1
 
  99  RETURN
      END
#endif
