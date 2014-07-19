*
* $Id: userid.F,v 1.1.1.1 1996/02/15 17:47:53 mclareni Exp $
*
* $Log: userid.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:53  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*15 FUNCTION USERID()
C
C CERN PROGLIB#    Z313 USERID          .VERSION KERNBIT  x.xx  911216
C ORIG. 16/12/91, Miguel Marquina
C Extraction of User Identifier
C
      IMPLICIT NONE

      INCLUDE '($JPIDEF)'
      INTEGER  LIB$GETJPI, ISTAT,ULEN

      CHARACTER*15 STRING
      ISTAT = LIB$GETJPI(JPI$_USERNAME,,,, STRING, ULEN)
C
      USERID=STRING( 1: ULEN)
      RETURN
      END
