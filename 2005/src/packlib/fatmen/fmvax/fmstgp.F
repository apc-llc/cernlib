*
* $Id: fmstgp.F,v 1.1.1.1 1996/03/07 15:18:21 mclareni Exp $
*
* $Log: fmstgp.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:21  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMSTGP(CHGRP,IRC)
*CMZ :          25/09/91  15.47.18  by  Jamie Shiers
*-- Author :    Jamie Shiers   25/09/91
*
*     Get the name of the staging group for the current process
*
      IMPLICIT INTEGER (S)
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
 
      RECORD /ITMLST/ LNM_LIST(2)
 
      INCLUDE '($LNMDEF)'
      INCLUDE '($SSDEF)'
      INCLUDE '($JPIDEF)'
      INCLUDE '($RMSDEF)'
 
      CHARACTER*(*) CHGRP
      CHARACTER*133 UIC
      CHARACTER*133 TOPDIR
      CHARACTER*20  GROUP
      CHARACTER*255 STGDSK,CHTEMP,CHMESS
      INTEGER       FAFNDF
#include "fatmen/slate.inc"
#include "fatmen/fatbug.inc"
      IRC = 0
*
*     Check if this group is enabled to use STAGE
*
      LNM_LIST(1).BUFFER_LENGTH           = 255
      LNM_LIST(1).ITEM_CODE               = LNM$_STRING
      LNM_LIST(1).BUFFER_ADDRESS          = %LOC(STGDSK)
      LNM_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LENSTG)
 
      LNM_LIST(2).END_LIST                = 0
 
      STATUS = SYS$TRNLNM(,'LNM$SYSTEM_TABLE','DISK$STAGE',,LNM_LIST)
 
      IF (STATUS .EQ. SS$_NOLOGNAM) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMSTGP. no staging disk found'
         IRC = 1
         RETURN
      ENDIF
*
*     Translate again if necessary
*
      IF (INDEX(STGDSK(1:LENSTG),':').EQ.0) THEN
*
*     Use undocumented (obsolete) LIB$ routine to perform
*     translation if concealed
*
         CHTEMP = STGDSK(1:LENSTG)
         LTEMP  = LENSTG
         STATUS = LIB$SYS_TRNLOG(CHTEMP(1:LTEMP),LENSTG,STGDSK,,,)
 
      ENDIF
*
*     Is DISK$STAGE a search list?
*
      CALL FMGTEL('DISK$STAGE',CHTEMP,'LNM$SYSTEM',1,ILIST)
 
      IF(ILIST.EQ.0.AND.IS(1).GT.0.AND.IDEBFA.GE.1)
     +   PRINT *,'FMSTGP. DISK$STAGE is a search list'
*
*     Take staging group from STAGE_GROUP, if defined
*
      STATUS = LIB$SYS_TRNLOG('STAGE_GROUP',LGROUP,GROUP,,,)
 
      IF(STATUS.EQ.SS$_NOTRAN) THEN
 
         STATUS = LIB$GETJPI(JPI$_UIC,,,,UIC,LENUIC)
         IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
 
         LCOMMA = INDEX(UIC,',')
         IF(LCOMMA.EQ.0) LCOMMA = LENUIC
 
         GROUP = UIC(2:LCOMMA-1)
         LGROUP = LCOMMA-2
*
*     Fix for ALEPH, DELPHI, L3 and OPAL
*
         IF ((INDEX(GROUP,'ALEPH_') .NE. 0) .OR. (INDEX(GROUP,'DELPHI_'
     +   ) .NE. 0) .OR. (INDEX(GROUP,'L3_') .NE. 0) .OR. (INDEX(GROUP,
     +   'OPAL_') .NE. 0)) THEN
 
            LGROUP = INDEX(GROUP,'_') - 1
 
         ENDIF
*
*     Fix for DELPHI after VXCERN reconfiguration of 15/01/94
*
         IF(GROUP(1:LGROUP).EQ.'PUBXX') THEN
            GROUP  = 'DELPHI'
            LGROUP = 6
         ENDIF
 
      ENDIF
 
      IF(INDEX(STGDSK(1:LENSTG),'.]').EQ.0) THEN
 
         CHGRP = STGDSK(1:LENSTG) // '[' //
     +               GROUP(1:LGROUP)//']'
         TOPDIR   = STGDSK(1:LENSTG) // '[000000]' //
     +               GROUP(1:LGROUP)//'.DIR'
      ELSE
 
         CHGRP = STGDSK(1:LENSTG-1) //
     +               GROUP(1:LGROUP)//']'
         TOPDIR   = STGDSK(1:LENSTG-2) // ']' //
     +               GROUP(1:LGROUP)//'.DIR'
      ENDIF
 
      LENTOP = LENOCC(TOPDIR)
      LENGRP = LENOCC(CHGRP)
 
      IF(IDEBFA.GE.1) PRINT *,'FMSTGP. group is ',GROUP(1:LGROUP),
     +   ' DISK$STAGE points to ',STGDSK(1:LENSTG)
      IF(IDEBFA.GE.1) PRINT *,'FMSTGP. top directory is ',
     +  TOPDIR(1:LENTOP),' staging directory is ',CHGRP(1:LENGRP)
 
*     STATUS = LIB$FIND_FILE(TOPDIR(1:LENTOP),CHTEMP,ICONT)
      STATUS = FAFNDF(TOPDIR(1:LENTOP),CHTEMP,ICONT)
      ISTAT  = LIB$FIND_FILE_END(ICONT)
 
      IF (STATUS .NE. RMS$_SUC) THEN
         IF(IDEBFA.GE.-3) THEN
            PRINT *,'FMSTGP. staging directory not ',
     +              'found for group ',GROUP(1:LGROUP)
            IC = SYS$GETMSG(%VAL(STATUS),LMESS,CHMESS,,)
            PRINT *,'FMSTGP. error from LIB$FIND_FILE = ',
     +              CHMESS(1:LMESS)
         ENDIF
         IRC = 2
         RETURN
      ENDIF
 
      CHGRP = 'DISK$STAGE:[' // GROUP(1:LGROUP) // ']'
 
      END
