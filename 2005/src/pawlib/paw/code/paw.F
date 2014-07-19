*
* $Id: paw.F,v 1.2 1999/05/20 07:37:16 couet Exp $
*
* $Log: paw.F,v $
* Revision 1.2  1999/05/20 07:37:16  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/19 22/11/95  19.25.13  by  Julian Bunn
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAW(NWPAW,IWTYP)
*
* Standard Paw setup routine.
*
* To get Paw++ (Motif version) call PAWPP.
*
      EXTERNAL      PAEXIT
      CHARACTER*80  BATCHF,PAWLOGF
*
      CALL PAWINT1('PAW',BATCHF,PAWLOGF)
*
      CALL PAWINT2(NWPAW,' ',PAEXIT,IWTYP)
*
      CALL PAWINT3
*
      CALL KUEXEC('SET/PROMPT ''PAW >''')
*
      CALL PAWINT4(PAWLOGF)
*
      IF (BATCHF .NE. ' ') THEN
         CALL KUEXEC('EXEC '//BATCHF)
         CALL PAEXIT
         STOP
      ENDIF
*
      END
