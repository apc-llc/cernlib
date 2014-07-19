*
* $Id: fmgtel.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmgtel.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE FMGTEL(LOGNAM,EQUNAM,TABNAM,IELEM,IRC)
      IMPLICIT INTEGER (S)
 
#include "fatmen/fatbug.inc"
#include "fatmen/slate.inc"
      CHARACTER*(*) LOGNAM,EQUNAM,TABNAM
 
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
 
       RECORD /ITMLST/ LNM_LIST(3)
 
      INCLUDE '($LNMDEF)'
      INCLUDE '($SSDEF)'
 
      CHARACTER*80 MESS
 
*
      IRC = 0
 
      LBUF = 255
      LNM_LIST(1).BUFFER_LENGTH           = 4
      LNM_LIST(1).ITEM_CODE               = LNM$_INDEX
      LNM_LIST(1).BUFFER_ADDRESS          = %LOC(IELEM)
      LNM_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LELEM)
 
      LNM_LIST(2).BUFFER_LENGTH           = LBUF
      LNM_LIST(2).ITEM_CODE               = LNM$_STRING
      LNM_LIST(2).BUFFER_ADDRESS          = %LOC(EQUNAM)
      LNM_LIST(2).RETURN_LENGTH_ADDRESS   = %LOC(LNAM)
 
      LNM_LIST(3).END_LIST                = 0
 
      LENLOG = LENOCC(LOGNAM)
      LENTAB = LENOCC(TABNAM)
      IF(IDEBFA.GE.3) PRINT *,'FMGTEL. attempting translation of ',
     +   LOGNAM(1:LENLOG),' from table ',TABNAM(1:LENTAB)
 
      STATUS = SYS$TRNLNM(,TABNAM(1:LENTAB)
     +                    ,LOGNAM(1:LENLOG),,LNM_LIST)
      IF (.NOT. STATUS) THEN
         IC = SYS$GETMSG(%VAL(STATUS),LNAM,EQUNAM,,)
         IRC = 1
      ENDIF
 
      IS(1) = LNAM
 
      END
 
#endif
