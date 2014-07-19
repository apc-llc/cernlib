*
* $Id: cdgtlg.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdgtlg.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB_VAXVMS)
      SUBROUTINE CDGTLG(LOGNAM,EQUNAM,TABNAM,IRC)
      IMPLICIT INTEGER (S)
#include "hepdb/slate.inc"
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
 
      RECORD /ITMLST/ LNM_LIST(2)
 
      INCLUDE '($LNMDEF)'
      INCLUDE '($SSDEF)'
*
      IRC = 0
 
      LBUF = 255
      LNM_LIST(1).BUFFER_LENGTH           = LBUF
      LNM_LIST(1).ITEM_CODE               = LNM$_STRING
      LNM_LIST(1).BUFFER_ADDRESS          = %LOC(EQUNAM)
      LNM_LIST(1).RETURN_LENGTH_ADDRESS   = %LOC(LNAM)
 
      LNM_LIST(2).END_LIST                = 0
 
      LENLOG = LENOCC(LOGNAM)
      LENTAB = LENOCC(TABNAM)
 
      STATUS = SYS$TRNLNM(,TABNAM(1:LENTAB)
     +                    ,LOGNAM(1:LENLOG),,LNM_LIST)
      IF (.NOT. STATUS) THEN
         IRC   = 1
         IS(1) = 0
      ELSE
         IS(1) = LNAM
      ENDIF
 
      END
 
#endif
