*
* $Id: putgeti.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: putgeti.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.01/02 01/03/93  15.19.19  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE PUTGET(PSTR, LPSTR, ANSW, MAXANS, NANSW)
*
************************************************************************
*                                                                      *
* PUTGET                                                               *
*                                                                      *
* Write prompt string PSTR on standard output and wait for an answer.  *
* On return ANSW contains the answer and NANSW the length of the       *
* answer. If NANSW .LT. 0 then there has been an error.                *
* Read typed answer from the same line                                 *
*                                                                      *
#if defined(CERNLIB_NEWLIB)
*                                                                      *
* Read answer from NEWLIB buffer if present rather than from terminal  *
* If the NEWLIB buffer contains an '~' , a shell command is created of *
* the remaining part                                                   *
*                                                                      *
* Calls Assembler routines $PUTGET and $PUT                            *
*                                                                      *
#endif
*                                                                      *
* INPUT:                                                               *
*    PSTR     (C)         prompt string                                *
*    LPSTR    (I)         length of prompt string                      *
*    MAXANS   (I)         max length of answer                         *
*                                                                      *
* OUTPUT:                                                              *
*    ANSW     (C)         answer                                       *
*    NANSW    (I)         length of answer, if .LT. 0 then error       *
*                                                                      *
************************************************************************
*                                                                      *
      CHARACTER*(*) PSTR, ANSW
      INTEGER LPSTR, MAXANS, NANSW
#if defined(CERNLIB_NEWLIB)
      CHARACTER*80 VARIABLE, RESULT
      INTEGER IRET
      VARIABLE='CMZ_ATT'
      CALL VARGET(VARIABLE,RESULT,IRET)
      IF (IRET. EQ. 0) THEN
        IF (RESULT(1:1) .EQ. '1') THEN
          RESULT=' '
*--       clear attention variable
          VARIABLE='CMZ_ATT'
          RESULT=' '
          CALL VARPUT(VARIABLE,RESULT,IRET)
*--       stop session due to attention
          PRINT *,'Program stopped due to attention interrupt'
          ANSW='EXIT'
          GOTO 999
        ENDIF
      ENDIF
      VARIABLE='CMZ_CMD'
      CALL VARGET(VARIABLE,RESULT,IRET)
      IF (IRET. EQ. 0) THEN
        ANSW=RESULT
        IF (ANSW .NE. ' ') THEN
          RESULT=' '
          NANSW=LENOCC(ANSW)
          IL=LENOCC(PSTR(1:LPSTR)//' '//ANSW(1:NANSW))
          CALL $PUT(PSTR(1:LPSTR)//' '//ANSW(1:NANSW),IL,IRET)
*--       position of next NEWLIB command
          I=INDEX(ANSW,'~')
          IF (I.GT.0) THEN
            RESULT='SHELL '//ANSW(I+1:)
            ANSW=ANSW(1:I-1)
          ENDIF
*--       clear/set input variables
          VARIABLE='PAW_CMD'
          CALL VARPUT(VARIABLE,RESULT,IRET)
          VARIABLE='CMZ_CMD'
          CALL VARPUT(VARIABLE,RESULT,IRET)
          GOTO 999
        ENDIF
      ENDIF
#endif
#if defined(CERNLIB_IBMMVS)
      CALL $PUTGET ( PSTR, LPSTR, ANSW, MAXANS, NANSW )
#endif
#if defined(CERNLIB_NEWLIB)
999   RESULT=ANSW
      CALL CLTOU(RESULT)
*--   is it a CL command, route to NEWLIB
      IF (RESULT(1:3) .EQ. 'CL ')
     +   ANSW='N '//RESULT
*--   remove erroneous prefix
      IF (RESULT(1:4) .EQ. 'CMZ ' .OR. RESULT(1:4) .EQ. 'PAW ')
     +   ANSW=RESULT(5:)
      NANSW=LENOCC(ANSW)
#endif
      RETURN
      END
#endif
