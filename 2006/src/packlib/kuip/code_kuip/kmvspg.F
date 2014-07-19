*
* $Id: kmvspg.F,v 1.1.1.1 1996/03/08 15:32:55 mclareni Exp $
*
* $Log: kmvspg.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
#if defined(CERNLIB_IBMMVS)
*CMZ :  2.01/00 25/02/93  10.22.36  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KMVSPG(FILNAM,LUNO)
************************************************************************
*                                                                      *
* Called by KUPAD to viewing a file in read-only mode on MVS           *
*                                                                      *
************************************************************************
*
      CHARACTER*(*) FILNAM
*
#if !defined(CERNLIB_NEWLIB)
      CALL KMVSED(FILNAM,ISTAT)
*
#endif
#if defined(CERNLIB_NEWLIB)
      CHARACTER*44   CNAME
      CHARACTER*20   PREFIX
      CHARACTER*80   MVSCOM
      INTEGER        SPACE(2)
*
*
*     Test for enough memory for shell
      CALL $SPACEM(SPACE(1),SPACE(2))
      IF (SPACE(2) .LT. 200 000) THEN
         PRINT *, '***ERROR*** Not enough storage available for SHELL'
     +,           '. Command rejected'
         RETURN
      ENDIF
*--       don't add prefix if the first character of file name is a dot

      LENGTH=LENOCC(FILNAM)
      IF ( FILNAM(1:1) .EQ. '.' )         THEN
          CNAME  = FILNAM(2:LENGTH)
          LENGTH = LENGTH - 1
      ELSE
          CALL KPREFI ( PREFIX, NCHPRE )
          CNAME  = PREFIX(1:NCHPRE)//FILNAM
          LENGTH = LENGTH + NCHPRE
      ENDIF

      MVSCOM ='LIST '''//CNAME(1:LENGTH)//''''

      CALL NEWLIB(MVSCOM, 1)
*
#endif
*
999   END

#endif
