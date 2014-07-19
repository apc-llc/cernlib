*
* $Id:
*
* $Log:
*
#include "comis/pilot.h"
*-- Author : O.Couet
      SUBROUTINE CSINC1(CHIN)
*
* This routine initialize the string INCNAM with an include file name
* like: comisPID.inc. This file name is used when the directive
*
* #include ?
*
* is used in a uwfunc generated function. With PID inside the name, the
* file is unique which allows to run several PAW sessions in the same
* directory without names conflict. Previoulsy the name used was fixed
* to "comis.inc".
*
      CHARACTER*(*) CHIN
#include "comis/csinc.inc"
      CHARACTER*8 CPID
* Retrieve the process ID (IPID) and convert it in a string (CPID)
      CALL GETPIDF(IPID)
      WRITE(CPID,'(I8)') IPID
* Supress the blank in CPID
      I=1
  10  IF (CPID(I:I).EQ.' ') THEN
         I=I+1
         IF (I.GT.8) THEN
            INCNAM='comis.inc'
            GOTO 999
         ENDIF
         GOTO 10
      ENDIF
* Build the include file name
      INCNAM='comis'
      INCNAM(6:)=CPID(I:)
      INCNAM(LENOCC(INCNAM)+1:) = '.inc'
* Return the include file name in CHIN
 999  CHIN = INCNAM
      END
