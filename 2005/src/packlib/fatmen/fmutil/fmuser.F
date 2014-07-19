*
* $Id: fmuser.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fmuser.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if !defined(CERNLIB_UNIX)
      INTEGER FUNCTION FMUSER(USER)
      CHARACTER*8 USER
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
      USER   = INFO(1)
      FMUSER = 0
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_VMBATCH))&&(!defined(CERNLIB_HEPVM))
      FMUSER = 0
      CALL VMCMS('IDENTIFY (STACK LIFO',IRC)
      CALL VMRTRM(CHLINE,LLINE)
      USER = CHLINE(1:LLINE)
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_VMBATCH))
      FMUSER = 0
*
*     Are we in batch?
*
      CALL VMCMS('IDENTIFY (STACK LIFO',IRC)
      CALL VMRTRM(CHLINE,LLINE)
      IF(CHLINE(1:5).EQ.'VMBAT') THEN
         CALL VMCMS('VMBATCH SERVICE STACK $USER',IRC)
         IF(IRC.EQ.0) THEN
            CALL VMRTRM(CHLINE,LLINE)
            USER = CHLINE(1:LLINE)
         ELSE
            IF(IDEBFA.GE.-3) PRINT *,'FMUSER. cannot determine ',
     +         'username. Using *UNKNOWN'
            USER = '*UNKNOWN'
            FMUSER = 1
         ENDIF
      ELSE
         USER = CHLINE(1:LLINE)
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*12 CHUSER
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
 
      JPI_LIST(1).BUFFER_LENGTH           = 12
      JPI_LIST(1).ITEM_CODE               = JPI$_USERNAME
      JPI_LIST(1).BUFFER_ADDRESS          = %LOC(CHUSER)
      JPI_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LENU)
 
      JPI_LIST(2).END_LIST                = 0
 
      ISTAT = SYS$GETJPIW(,,,JPI_LIST,,,)
      IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      LUSER = MIN(8,LENOCC(USER))
      USER  = CHUSER(1:LUSER)
      IF (ISTAT.NE.0) THEN
         FMUSER=1
      ENDIF
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_NEWLIB))
      CHARACTER*16   NAMES
      CHARACTER*80   VARIABLE,VALUE
      CALL JOBNAM(NAMES)
      USER(1:8) = NAMES
      FMUSER    = 0
#endif
#if (defined(CERNLIB_IBMMVS))&&(!defined(CERNLIB_NEWLIB))
      LOGICAL        INTRAC
      IF(INTRAC(DUMMY)) THEN
*-- interactive mode
         NCH = 8
         FMUSER = ISPLNK('VCOPY','(ZUSER)',NCH,USER,'MOVE')
         IF(FMUSER.NE.0) THEN
            USER = 'unknown'
         ENDIF
      ELSE
*-- batch mode
         FMUSER = FMJOBN(USER)
         USER(7:8) = '  '
      ENDIF
#endif
      RETURN
      END
#endif
