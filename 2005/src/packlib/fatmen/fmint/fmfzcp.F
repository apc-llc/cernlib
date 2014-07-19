*
* $Id: fmfzcp.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmfzcp.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFZCP(LUNI,LUNO,CHOPT,IRC)
*CMZ :          21/01/91  15.22.25  by  Jamie Shiers
*-- Author :    Jamie Shiers   21/01/91
*     Routine to copy a complete FZ file from LUNI to LUNO
*     Files have already been opened, e.g. by FMOPEN.
*     Upon completion, FMCLOS should be called to close input
*     and output streams.
*
      DIMENSION   IOCR(100)
      DIMENSION   IUHEAD(400)
      CHARACTER*1 CHFZ
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
*
*     Perform the copy
*
   10 CONTINUE
      NUH = 400
      CALL FZIN(LUNI,IXDIV,LSUP,JBIAS,' ',NUH,IUHEAD)
      IF(IQUEST(1).LT.0) GOTO 30
      IF(IQUEST(1).GE.4) GOTO 20
      IF(IQUEST(1).EQ.0) IEVENT = IQUEST(11)
*
*     start of run
*
      IF(IQUEST(1).EQ.1) THEN
         IF(IOPTR.NE.0) GOTO 10
         CALL FZRUN(LUNO,IQUEST(11),NUH,IUHEAD)
         IF(IQUEST(1).NE.0) GOTO 30
         GOTO 10
*
*     end of run
*
      ELSEIF(IQUEST(1).EQ.2) THEN
         IF(IOPTR.NE.0) GOTO 10
         CALL FZRUN(LUNO,-1,NUH,IUHEAD)
         IF(IQUEST(1).NE.0) GOTO 30
         GOTO 10
*
*     ZEBRA eof
*
      ELSEIF(IQUEST(1).EQ.3) THEN
      ENDIF
 
      CALL UCOPY(IQUEST(21),IOCR,MIN(IQUEST(20),100))
      CHFZ = 'L'
      IF(IQUEST(14).EQ.0) CHFZ = 'Z'
      CALL FZOUT(LUNO,IXDIV,LSUP,IEVENT,CHFZ,IOCR,NUH,IUHEAD)
      IF(IQUEST(1).NE.0) GOTO 30
      GOTO 10
   20 CONTINUE
      RETURN
   30 IRC = IQUEST(1)
 
      END
