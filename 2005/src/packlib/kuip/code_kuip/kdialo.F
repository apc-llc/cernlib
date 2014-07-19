*
* $Id: kdialo.F,v 1.1.1.1 1996/03/08 15:32:53 mclareni Exp $
*
* $Log: kdialo.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:53  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_IBMMVS)
*CMZ :  1.61/01 22/06/89  17.34.26  by
*-- Author :
      SUBROUTINE KDIALO ( PROMPT, CHREAD, NCHREA )

*...KDIALO PROMPT/READ ROUTINE (DIALOG)                                 03/02/88
*.
*.  CALLING SEQUENCE =
*.        CALL KDIALO ( PROMPT, CHREAD, NCHREA )
*.                  PROMT     CHARACTER STRING TO BE PRINTED     (INPUT)
*.                  CHREAD    CHARACTER STRING TO BE READ        (OUTPUT)
*.                  NCHREA    NUMBER OF CHARACTERS READ          (OUTPUT)
*.

      CHARACTER*(*)  PROMPT, CHREAD
      LOGICAL        INTRAC

      IF ( INTRAC(DUMMY) )              THEN
*--        INTERACTIVE MODE
           NCHWRI = LENOCC ( PROMPT )
           MAXREA = 80
           CALL PUTGET ( PROMPT, NCHWRI, CHREAD, MAXREA, NCHREA )
           IF ( NCHREA .LT. 0 )    PRINT *, ' KDIALO ERROR: ', NCHREA
      ELSE
*--        BATCH MODE
           PRINT *, PROMPT
           READ  8000, CHREAD
 8000      FORMAT ( A80 )
           PRINT *, CHREAD
           NCHREA = LENOCC ( CHREAD )
      ENDIF

      RETURN
      END

#endif
