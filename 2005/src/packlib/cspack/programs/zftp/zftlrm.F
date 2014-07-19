*
* $Id: zftlrm.F,v 1.1.1.1 1996/03/08 15:44:20 mclareni Exp $
*
* $Log: zftlrm.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZFTLRM
*
      LOGICAL LOPEN,IEXIST
      CHARACTER*80 PATH
*
      CALL KUGETC(PATH,LPATH)
      IF(LPATH.EQ.0) RETURN
#if defined(CERNLIB_UNIX)
      CALL CUTOL(PATH)
#endif
*
*     Check if file exists
*
#if defined(CERNLIB_IBMVM)
      CALL CTRANS('.',' ',PATH,1,LPATH)
      INQUIRE(FILE='/'//PATH(1:LPATH),EXIST=IEXIST)
#endif
#if !defined(CERNLIB_IBMVM)
      INQUIRE(FILE=PATH(1:LPATH),EXIST=IEXIST)
#endif
      IF(.NOT.IEXIST) THEN
         PRINT *,'File ',PATH(1:LPATH),' not found'
         GOTO 99
      ENDIF
*
*     Find a unit number which is free
*
      DO 10 I=1,99
      INQUIRE(I,OPENED=LOPEN)
      IF(.NOT.LOPEN) GOTO 20
10    CONTINUE
      IRC = 1
      RETURN
#if defined(CERNLIB_IBMVM)
20    OPEN(I,FILE='/'//PATH(1:LPATH),STATUS='OLD',FORM='UNFORMATTED',
#endif
#if !defined(CERNLIB_IBMVM)
20    OPEN(I,FILE=PATH(1:LPATH),STATUS='OLD',FORM='UNFORMATTED',
#endif
     +ERR=99)
      CLOSE(I,STATUS='DELETE',ERR=99)
      IRC = 0
      RETURN
99    IRC = 1
      END
 
 
