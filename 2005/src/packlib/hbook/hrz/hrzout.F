*
* $Id: hrzout.F,v 1.2 1996/03/08 13:03:30 julian Exp $
*
* $Log: hrzout.F,v $
* Revision 1.2  1996/03/08 13:03:30  julian
* Set iquest(1) to zero before call to rz. Avoids redundant error messages.
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/02 23/02/95  09.47.27  by  Julian Bunn
*-- Author :
      SUBROUTINE HRZOUT(IXDIV,LBANK,KEYS,ICYCLE,CHOPT)
*.==========>
*.        to write a record to a RZ file r
*..=========> ( R.Brun )
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
      COMMON/QUEST/IQUEST(100)
#if defined(CERNLIB_CZ)
      DIMENSION IQSAVE(100)
#endif
      CHARACTER*(*) CHOPT
      INTEGER       KEYS(2)
*.___________________________________________
*
      IF(ICHTOP(ICDIR).GT.1000)THEN
#if defined(CERNLIB_CZ)
*
*          Send message to the host
*
         WRITE(CHMAIL,1000)KEYS(1),KEYS(2),ICYCLE,CHOPT
 1000    FORMAT('CZOUT ',2I10,I10,A)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF (ISTAT .NE. 0) THEN
            IQUEST(1) = ISTAT
            RETURN
         ENDIF
*
*          Write the ZEBRA data structure to the host
*
         NUH=0
         CALL FZOUT(998,IXDIV,LBANK,1,' ',0,NUH,IQSAVE)
         IF(IQUEST(1).NE.0)GO TO 99
*
*          Read the IQUEST vector
*
         NUH=100
         CALL FZIN(999,IXDIV,0,1,' ',NUH,IQSAVE)
         CALL UCOPY(IQSAVE,IQUEST,100)
#endif
#if !defined(CERNLIB_CZ)
         CALL HBUG('CZ option not active','HRZOUT',0)
#endif
         RETURN
      ENDIF
*
      IQUEST(1) = 0
      CALL RZOUT(IXDIV,LBANK,KEYS,ICYCLE,CHOPT)
      IF(IQUEST(1).NE.0) THEN
         CALL HBUG('An error has occured whilst writing data',
     &             'HRZOUT',IQUEST(1))
      ENDIF
*
  99  RETURN
      END
