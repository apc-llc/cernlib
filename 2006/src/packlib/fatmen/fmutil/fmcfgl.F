*
* $Id: fmcfgl.F,v 1.1.1.1 1996/03/07 15:18:19 mclareni Exp $
*
* $Log: fmcfgl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:19  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if (defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS))&&(!defined(CERNLIB_NOCIO))
       SUBROUTINE FMCFGL(LUNPTR,LINE,LLINE,CHOPT,IRC)
*CMZ :          14/11/91  15.21.53  by  Jamie Shiers
*-- Author :    Jamie Shiers   14/11/91
*
*      Get a line from a Unix file using C I/O
*
      CHARACTER*(*) LINE,CHOPT
      CHARACTER*80  BUFF
      CHARACTER*1   NL
 
#include "fatmen/fatbug.inc"
 
      SAVE NL,BUFF,ISTART,INL,JSTART,JEND,JCONT,NREAD,NENTRY
 
#include "fatmen/fatoptd.inc"
 
      DATA NENTRY/0/,JCONT/0/
 
      CALL FMOPTC(CHOPT,ALFNUM,IOPT)
 
      IF(NENTRY.EQ.0) THEN
         NL = CHAR(10)
         NENTRY = 1
      ELSE
         IF(IOPTF.NE.0) THEN
            JCONT = 0
            RETURN
         ENDIF
      ENDIF
 
      IRC    = 0
      IF(JCONT.NE.0) GOTO 20
 
      JSTART = 1
      JEND   = 0
10    CONTINUE
      ISTART = 1
      CALL CIGET(LUNPTR,BUFF,80,NREAD,IRC)
      IF(IRC.LT.0) RETURN
      IF(IDEBFA.GE.4) PRINT *,'FMCFGL. read ',BUFF(1:NREAD)
*
*     Look for a \n character
*
20    CONTINUE
      INL = INDEX(BUFF(ISTART:NREAD),NL)
      IF(INL.NE.0) THEN
         JEND = JEND + INL - 1
         INL = INL + ISTART -1
 
         LINE(JSTART:) = BUFF(ISTART:INL-1)
 
         LLINE  = JEND
         JSTART = 1
         JEND   = 0
         JCONT  = 1
         ISTART = INL + 1
         RETURN
 
      ELSE
         LINE(JSTART:) = BUFF(ISTART:NREAD)
         JSTART = JSTART + NREAD - ISTART + 1
         JEND   = JSTART - 1
      ENDIF
 
      JCONT = 0
      GOTO 10
 
      END
#endif
