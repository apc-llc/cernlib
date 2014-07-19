*
* $Id: fmvusr.F,v 1.1.1.1 1996/03/07 15:18:22 mclareni Exp $
*
* $Log: fmvusr.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:22  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      INTEGER FUNCTION FMVUSR(USER)
      CHARACTER*(*) USER
      CHARACTER*12  CHUSER
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
      LUSER = MIN(12,LENOCC(USER))
      USER  = CHUSER(1:LUSER)
      IF (ISTAT) THEN
         FMVUSR = 0
      ELSE
         FMVUSR=1
      ENDIF
 
      END
