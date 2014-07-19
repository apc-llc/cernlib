*
* $Id: szinqr.F,v 1.1.1.1 1996/03/08 15:44:27 mclareni Exp $
*
* $Log: szinqr.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:27  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SZINQR(LUN,FNAME,IEXIST,LRECL,IRC)
      CHARACTER*(*) FNAME
      LOGICAL EXIST
*
*     Check if file exists
*
      LF = LENOCC(FNAME)
      IEXIST = 0
      IRC    = 0
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',FNAME,1,LF)
#endif
#if defined(CERNLIB_IBM)
      INQUIRE(FILE='/'//FNAME(1:LF),EXIST=EXIST)
#endif
#if !defined(CERNLIB_IBM)
      INQUIRE(FILE=FNAME(1:LF),EXIST=EXIST)
#endif
      IF(EXIST) THEN
         IEXIST = 1
      ELSE
         IRC    = 4
      ENDIF
 
      END
*
 
