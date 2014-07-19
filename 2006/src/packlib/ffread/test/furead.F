*
* $Id: furead.F,v 1.1.1.1 1996/03/08 11:50:41 mclareni Exp $
*
* $Log: furead.F,v $
* Revision 1.1.1.1  1996/03/08 11:50:41  mclareni
* Ffread
*
*
#include "ffread/pilot.h"
      SUBROUTINE FUREAD (IBFSIZ)
 
      COMMON / FFCHAR / STRING
      CHARACTER*132 STRING
      EXTERNAL FUSTAR
C
C----------------- Beginning of executable statements -------------------------
C
      DATA ITIME /0/
#if defined(CERNLIB_SAVE)
      SAVE ITIME
#endif
      ITIME = ITIME + 1
      WRITE(*,*) 'User reading routine called'
      IF(ITIME.EQ.1) THEN
        WRITE(STRING(1:IBFSIZ),'(''*INTARRAY 7=-77'')')
      ELSE IF(ITIME.EQ.2) THEN
        CALL FFUSET(FUSTAR)
        WRITE(STRING(1:IBFSIZ),'(''REALARRAY 7=-77.'')')
      ELSE IF(ITIME.EQ.3) THEN
        WRITE(STRING(1:IBFSIZ),'(''*LOGARRAY 7=OFF'')')
      ELSE IF(ITIME.EQ.4) THEN
        WRITE(STRING(1:IBFSIZ),'(''END'')')
      END IF
      END
