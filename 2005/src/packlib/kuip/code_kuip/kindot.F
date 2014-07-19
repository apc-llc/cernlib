*
* $Id: kindot.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kindot.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/08 15/01/93  13.02.08  by  Alfred Nathaniel
*-- Author :
      FUNCTION KINDOT(STRING)
*
********************************************************************************
*
* Like Fortran intrinsic function INDEX, with second parameter assumed '.'
* and, in case of VMS, searching the dot only outside square brackets [ ]
*
* The dot search is terminated when a blank is found (that means
* that the macro name is always the first parameter of STRING).
*
*     Input :
*
      CHARACTER*(*) STRING
*
********************************************************************************
*
      I=INDEX(STRING,'.')
#if defined(CERNLIB_VMS)
      L=LEN(STRING)
10    IF (I.LT.L) THEN
        I1=INDEX(STRING(I+1:),'.')
        IF (I1.GT.0) THEN
          I=I1+I
          GO TO 10
        ENDIF
      ENDIF
      IBRA=INDEX(STRING,']')
      IF (I.LT.IBRA) I=0
      IBRA=INDEX(STRING,'}')
      IF (I.LT.IBRA) I=0
#endif
      IBLA=INDEX(STRING,' ')
      IF (IBLA.GT.0.AND.I.GE.IBLA) THEN
        KINDOT=0
      ELSE
        IF (I.EQ.1) THEN
          II=INDEX(STRING(2:),'.')
          IF (II.GT.0) THEN
            KINDOT=II+1
          ELSE
            KINDOT=0
          ENDIF
        ELSE IF (I.GT.1.AND.STRING(I-1:I-1).EQ.'/') THEN
          II=INDEX(STRING(I+1:),'.')
          IF (II.GT.0) THEN
            KINDOT=II+I
          ELSE
            KINDOT=0
          ENDIF
        ELSE
          KINDOT=I
        ENDIF
      ENDIF
      END



