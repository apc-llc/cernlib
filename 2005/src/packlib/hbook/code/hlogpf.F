*
* $Id: hlogpf.F,v 1.2 1996/03/15 10:40:08 dinofm Exp $
*
* $Log: hlogpf.F,v $
* Revision 1.2  1996/03/15 10:40:08  dinofm
* The SUBROUTINE hlogpf.F has been modified so that the 'Logfile' coming from
* the PIAF master is checked line by line before being printed on stdout.
* This allows to suppress output of lines containing information that are
* not relevant to the PAW user (such as PIAF server version)
*
* Revision 1.1.1.1  1996/01/16 17:07:42  mclareni
* First import
*
*
#include "hbook/pilot.h"
#if defined(CERNLIB_CZ)
*CMZ :  4.20/05 11/08/93  19.26.58  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   07/04/93
      SUBROUTINE HLOGPF(CHHEAD,ISTAT)
      CHARACTER*(*) CHHEAD
*
* receive log file from Piaf server and print it
*
#include "hbook/hcbook.inc"
#include "hbook/czbuff.inc"
#include "hbook/czcbuf.inc"
*
      COMMON/QUEST/IQUEST(100)
      DIMENSION IQSAVE(100)
      CHARACTER CHMAIL*80
 
      IF(CHHEAD.NE.' ') THEN
         CHMAIL=CHHEAD
      ELSE
*
* get header from PSPLOG first
*
         CALL CZGETA(CHMAIL,ISTAT)
      ENDIF
 
*--- no log file
      IF(CHMAIL(7:).NE.' ') GOTO 99
 
 1    CONTINUE
*
* get record length
*
      N=6
      IF(IZCBUF.EQ.0)GO TO 999
      CALL JUMPST(IZCBUF)
      CALL JUMPX2(N,ISTAT)
      IF(ISTAT.NE.0) GOTO 999
      READ(CHBUF,'(I6)') LRECL
      IF(LRECL.LT.0) THEN
         NBYTE=-LRECL-1
      ELSE
         NBYTE=LRECL
      ENDIF
      IF(NBYTE.EQ.0) THEN
         CHBUF(:1)=' '
         L=1
      ELSE
         CALL JUMPX2(NBYTE,ISTAT)
         IF(ISTAT.NE.0) GOTO 999
         L=NBYTE
      ENDIF
*-- If the PQRY keyword is found in the Log Line then there is some useful
*-- info that must be retrieved instead of being printed.
      CALL GREPPQRY(CHBUF(:L),IRET)
      IF (IRET.EQ.0) THEN
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAX)
         PRINT '(1X,A)', CHBUF(:L)
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_VAX))
         PRINT '(A)', CHBUF(:L)
#endif
      ENDIF

*
* last line has LRECL<0
*
      IF(LRECL.GE.0) GOTO 1
 
 99   CONTINUE
*
* receive IQUEST vector
*
      NUH=100
      CALL FZIN(999,IHDIV,0,1,' ',NUH,IQSAVE(1))
      ISTAT=IQUEST(1)
      CALL UCOPY(IQSAVE(1),IQUEST(1),100)
 
 999  CONTINUE
      END
#endif
