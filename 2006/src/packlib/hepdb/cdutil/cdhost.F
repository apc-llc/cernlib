*
* $Id: cdhost.F,v 1.1.1.1 1996/02/28 16:24:43 mclareni Exp $
*
* $Log: cdhost.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:43  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDHOST(CHHOST,IRC)
*
*     Return host name of current machine
*
      CHARACTER*(*) CHHOST
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
      CHARACTER*8 CHTEMP
#include "hepdb/slate.inc"
#endif
#if defined(CERNLIB_IBMVM)
      CHARACTER*80 CHLINE
 
      CHHOST = '??????'
 
      CALL VMCMS('QUERY USERID (STACK LIFO',IRC)
      IF(IRC.NE.0) RETURN
      CALL VMRTRM(CHLINE,LLINE)
      ISTART = INDEXB(CHLINE(1:LLINE),' ') + 1
      CHHOST = CHLINE(ISTART:LLINE)
 
#endif
#if defined(CERNLIB_VAXVMS)
 
      CHARACTER*15 CHNODE
 
      INTEGER SYS$GETSYIW
 
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
 
      INCLUDE '($SYIDEF)'
      INCLUDE '($SSDEF)'
 
      LNM_LIST(1).BUFFER_LENGTH           = LEN(CHNODE)
      LNM_LIST(1).ITEM_CODE               = SYI$_NODENAME
      LNM_LIST(1).BUFFER_ADDRESS          = %LOC(CHNODE)
      LNM_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LNODE)
 
      LNM_LIST(2).END_LIST                = 0
 
      IRC    = 0
      CHHOST = '??????'
 
      ISTAT=SYS$GETSYIW(,,,LNM_LIST,,,)
      IF(ISTAT.NE.SS$_NORMAL) THEN
         IRC = 42
         RETURN
      ENDIF
 
      LHOST  = MIN(LEN(CHHOST),LNODE)
 
      CHHOST = CHNODE(1:LHOST)
 
#endif
#if defined(CERNLIB_UNIX)
 
      CALL CDHSTC(CHHOST)
 
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_UNIX)
*
*     Use value of environmental variable CDHOST if set
*
      CALL GETENVF('CDHOST',CHTEMP)
      IF(IS(1).NE.0) CHHOST = CHTEMP(1:IS(1))
#endif
      END
