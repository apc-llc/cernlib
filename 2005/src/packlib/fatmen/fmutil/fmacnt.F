*
* $Id: fmacnt.F,v 1.1.1.1 1996/03/07 15:18:16 mclareni Exp $
*
* $Log: fmacnt.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:16  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMACNT(HACNT)
#if defined(CERNLIB_VAXVMS)
      IMPLICIT INTEGER (S)
#endif
      CHARACTER*8 HACNT,CHACNT
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
#if defined(CERNLIB_IBMVM)
      CHARACTER*80 CHLINE
#endif
#if (defined(CERNLIB_IBMVM))&&(!defined(CERNLIB_HEPVM))
      CALL VMCMS('QUERY ACCOUNT (STACK LIFO',IND)
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_HEPVM))
      CALL VMCMS('EXECIO 1 CP (LIFO STRING EXTRACT ACCOUNT . ',IND)
#endif
#if defined(CERNLIB_IBMVM)
      IF(IND.EQ.0) THEN
         CALL VMRTRM(CHLINE,LLINE)
         READ (CHLINE,'(8X,A8)') HACNT
      ELSE
         IF(IDEBFA.GE.-3) PRINT *,'FMACNT. cannot determine ',
     +      'account. Set to *UNKNOWN'
         HACNT = '*UNKNOWN'
         FMACNT=1
         RETURN
      ENDIF
#endif
#if (defined(CERNLIB_IBMVM))&&(defined(CERNLIB_VMBATCH))
*
*     Are we in batch?
*
      CALL VMCMS('IDENTIFY (STACK LIFO',IRC)
      CALL VMRTRM(CHLINE,LLINE)
      IF(CHLINE(1:5).EQ.'VMBAT') THEN
         CALL VMCMS('VMBATCH SERVICE STACK ACCOUNT',IRC)
         IF(IRC.EQ.0) THEN
            CALL VMRTRM(CHLINE,LLINE)
            HACNT = CHLINE(1:LLINE)
         ELSE
            IF(IDEBFA.GE.-3) PRINT *,'FMACNT. cannot determine ',
     +         'account. Set to *UNKNOWN'
            HACNT = '*UNKNOWN'
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)
 
      CHARACTER*9 ACCOUNT
      CHARACTER*12 USER
 
 
      INCLUDE '($UAIDEF)'
      INCLUDE '($JPIDEF)'
      INCLUDE '($SSDEF)'
 
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
       RECORD /ITMLST/ UAI_LIST(2)
 
 
      JPI_LIST(1).BUFFER_LENGTH           = 12
      JPI_LIST(1).ITEM_CODE               = JPI$_USERNAME
      JPI_LIST(1).BUFFER_ADDRESS          = %LOC(USER)
      JPI_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LENU)
 
      JPI_LIST(2).END_LIST                = 0
 
      STATUS = SYS$GETJPIW(,,,JPI_LIST,,,)
      IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
 
      UAI_LIST(1).BUFFER_LENGTH           = 9
      UAI_LIST(1).ITEM_CODE               = UAI$_ACCOUNT
      UAI_LIST(1).BUFFER_ADDRESS          = %LOC(ACCOUNT)
      UAI_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LENA)
 
      UAI_LIST(2).END_LIST                = 0
 
      STATUS = SYS$GETUAI(,,USER(1:LENU),UAI_LIST,,,)
      IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
 
      READ(ACCOUNT,'(A8)') HACNT
 
#endif
#if (defined(CERNLIB_IBMMVS))&&(defined(CERNLIB_NEWLIB))
*     Routine from DESYLIB
      CALL ACCTNR(HACNT)
#endif
#if defined(CERNLIB_UNIX)
      FMACNT = FMACTC(HACNT)
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Use value of environmental variable FMACNT if set
*
      CALL GETENVF('FMACNT',CHACNT)
      IF(IS(1).GT.0) HACNT = CHACNT
C
#endif
      IF(IDEBFA.GE.3) PRINT *,'FMACNT. account set to ',
     +   HACNT
      FMACNT=0
      RETURN
      END
