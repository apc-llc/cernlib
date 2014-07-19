*
* $Id: fmcfgl.F,v 1.1.1.1 1996/03/07 15:18:25 mclareni Exp $
*
* $Log: fmcfgl.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:25  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
#if defined(CERNLIB_UNIX)||defined(CERNLIB_VAXVMS)
       SUBROUTINE FMCFGL(LUNPTR,CHLINE,LLINE,CHOPT,IRC)
*CMZ :          14/11/91  15.21.53  by  Jamie Shiers
*-- Author :    Jamie Shiers   14/11/91
*
*      Get a line from a Unix file using C I/O
*
*      IOPTF: flush buffer from previous operation
*
       CHARACTER*(*) CHLINE,CHOPT
       CHARACTER*80  CHBUFF
       CHARACTER*1   NL
#if defined(CERNLIB_CRAY)
       PARAMETER     (IQCHAW=8)
#endif
#if !defined(CERNLIB_CRAY)
       PARAMETER     (IQCHAW=4)
#endif
#include "fatmen/fatoptd.inc"
       DATA          NENTRY/0/,ISTART/0/
       SAVE          NENTRY,NL,ISTART,LBUFF,NBUFF
 
       CALL FMOPTC(CHOPT,ALFNUM,IOPT)
 
       IRC = 0
 
       IF(NENTRY.EQ.0) THEN
          NENTRY = 1
          NL     = CHAR(10)
          NBUFF  = LEN(CHBUFF)
          LBUFF  = NBUFF/IQCHAW
       ENDIF
 
       IF(IOPTF.NE.0) THEN
          ISTART = 0
          CHBUFF = ' '
          RETURN
       ENDIF
*
*      Get logical line
*
       LLINE = 0
       IF(ISTART.EQ.0) THEN
          NWTAK  = LBUFF
          CALL CFGET(LUNPTR,0,LBUFF,NWTAK,CHBUFF,ISTAT)
 
          IF(ISTAT.NE.0.AND.ISTAT.NE.-1) RETURN
          JBUFF  = NWTAK*IQCHAW
          ISTART = 1
       ENDIF
*
*      Look for newline character
*
       IEND = INDEX(CHBUFF(ISTART:JBUFF),NL)
       IF(IEND.NE.0) THEN
          LLINE  = IEND - 1
          CHLINE = CHBUFF(ISTART:ISTART+IEND-1)
          ISTART = ISTART + IEND
          RETURN
       ELSE
*
*      Move first part of line
*
10     CONTINUE
          CHLINE = CHBUFF(ISTART:JBUFF)
          ISTART = JBUFF + ISTART - 1
          LLINE  = ISTART
*
*      Get next buffer
*
          NWTAK  = LBUFF
          CALL CFGET(LUNPTR,0,LBUFF,NWTAK,CHBUFF,ISTAT)
          IF(ISTAT.NE.0.AND.ISTAT.NE.-1) RETURN
          JBUFF  = NWTAK*IQCHAW
*
*      Look for end of last line
*
          IEND = INDEX(CHBUFF(1:JBUFF),NL)
*
*      Is first character a newline?
*
          IF(IEND.GT.1) THEN
             CHLINE(ISTART+1:ISTART+IEND-1) = CHBUFF(1:IEND-1)
             LLINE  = ISTART + IEND - 1
             ISTART = IEND + 1
          ELSE
             CHLINE(ISTART+1:ISTART+JBUFF-1) = CHBUFF(1:JBUFF)
             GOTO 10
          ENDIF
 
       ENDIF
 
       END
#endif
