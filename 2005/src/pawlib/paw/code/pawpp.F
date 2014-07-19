*
* $Id: pawpp.F,v 1.2 1999/05/20 07:37:18 couet Exp $
*
* $Log: pawpp.F,v $
* Revision 1.2  1999/05/20 07:37:18  couet
* - The flag CERNLIB_TKTCL is now removed. It was not used and just made
*   the code confusing.
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/19 21/11/95  18.25.00  by  Unknown
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWPP(NWPAW, IWTYP)
*
*  Paw++ setup routine.
*
*  The final workstation type IWTYP is returned and should be used later
*  to decide to call either KUWHAM (IWTYP=999) or KUWHAG.
*
#include "hbook/hcminpu.inc"
      CHARACTER*80  BATCHF, PAWLOGF
      EXTERNAL      PAEXIT
*
      CALL PAWINT1('PAW',BATCHF,PAWLOGF)
*
      CALL PAWINT2(NWPAW,'+',PAEXIT,IWTYP)
*
      CALL PAWINTM('Paw++')
*
      CALL PAWINT3
*
      IF (IWTYP .EQ. 999) THEN
         CALL KUEXEC('SET/PROMPT ''PAW++ [] ''')
         CALL KUEXEC('OPT ZFL1')
#if !defined(CERNLIB_VMS)
         CALL KUEXEC('HOST_EDITOR ''xterm -e vi &''')
#endif
#if defined(CERNLIB_VMS)
         CALL KUEXEC('HOST_EDITOR ''edit/tpu/disp=decw''')
#endif
      ELSE
         CALL KUEXEC('SET/PROMPT ''PAW [] ''')
      ENDIF
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
