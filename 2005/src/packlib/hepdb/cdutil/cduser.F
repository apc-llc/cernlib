*
* $Id: cduser.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cduser.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if !defined(CERNLIB_UNIX)
      SUBROUTINE CDUSER(CHUSER,IRC)
      CHARACTER*(*) CHUSER
#if defined(CERNLIB_IBMVM)
      CHARACTER*80 CHLINE
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_HEPVM))
      DIMENSION SPACE(32)
      LOGICAL BATCH,JOB$VM
      CHARACTER*8 INFO(10)
      EQUIVALENCE (INFO(1),SPACE(1))
      EQUIVALENCE (TIME,SPACE(21))
      EQUIVALENCE (TNORM,SPACE(23))
      EQUIVALENCE (MACH,SPACE(25))
C
      BATCH  = JOB$VM(SPACE)
      CHUSER = INFO(1)
      IRC    = 0
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_VMBATCH))&&(!defined(CERNLIB_HEPVM))
      IRC    = 0
      CALL VMCMS('IDENTIFY (STACK LIFO',IRC)
      CALL VMRTRM(CHLINE,LLINE)
      CHUSER = CHLINE(1:LLINE)
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_VMBATCH))
      IRC    = 0
*
*     Are we in batch?
*
      CALL VMCMS('IDENTIFY (STACK LIFO',IRC)
      CALL VMRTRM(CHLINE,LLINE)
      IF(CHLINE(1:5).EQ.'VMBAT') THEN
         CALL VMCMS('VMBATCH SERVICE STACK $USER',IRC)
         IF(IRC.EQ.0) THEN
            CALL VMRTRM(CHLINE,LLINE)
            CHUSER = CHLINE(1:LLINE)
         ELSE
            IF(IDEBFA.GE.-3) PRINT *,'CDUSER. cannot determine ',
     +         'username. Using *UNKNOWN'
            CHUSER = '*UNKNOWN'
            IRC    = 1
         ENDIF
      ELSE
         USER = CHLINE(1:LLINE)
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      INCLUDE '($JPIDEF)'
      INTEGER SYS$GETJPIW
      STRUCTURE /ITMLST/
      UNION
      MAP
      INTEGER*2 BUFFER_LENGTH
      INTEGER*2 ITEM_CODE
      INTEGER*4 BUFFER_ADDRESS
      INTEGER*4 RETURN_LENGTH_ADDRESS
      ENDMAP
      MAP
      INTEGER*4 END_LIST /0/
      ENDMAP
      END UNION
      END STRUCTURE
 
      RECORD /ITMLST/ JPI_LIST(2)
 
      JPI_LIST(1).BUFFER_LENGTH           = LEN(CHUSER)
      JPI_LIST(1).ITEM_CODE               = JPI$_USERNAME
      JPI_LIST(1).BUFFER_ADDRESS          = %LOC(CHUSER)
      JPI_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LENU)
 
      JPI_LIST(2).END_LIST                = 0
 
      ISTAT = SYS$GETJPIW(,,,JPI_LIST,,,)
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_NEWLIB))
      CHARACTER*16   NAMES
      CHARACTER*80   VARIABLE,VALUE
      CALL JOBNAM(NAMES)
      CHUSER(1:8) = NAMES
      IRC         = 0
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      LOGICAL        INTRAC
      IF(INTRAC(DUMMY)) THEN
*-- interactive mode
         NCH = 8
         IRC = ISPLNK('VCOPY','(ZUSER)',NCH,CHUSER,'MOVE')
         IF(IRC   .NE.0) THEN
            CHUSER = 'unknown'
         ENDIF
      ELSE
*-- batch mode
         CHUSER  = 'MVSBATCH'
      ENDIF
#endif
      RETURN
      END
#endif
