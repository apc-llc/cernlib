*
* $Id: cssinp.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: cssinp.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if !defined(CERNLIB_PAW)
*CMZ :  1.18/09 02/09/94  15.17.16  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
         SUBROUTINE CSSINP(RECXX,PR,N)
C
C    MAKE N < 0 FOR EOF !!
***---------------------------------
*      reads line from terminal
***---------------------------------
#include "comis/cslun.inc"
*un+SEQ,CSPAR.
*un+SEQ,CSPNTS.
         CHARACTER *(*) RECXX,PR
#if defined(CERNLIB_MSDOS)
C&SELF, DEFINSTK.
      PARAMETER (MAXHIS=65)
      CHARACTER*80 CHSTKB(MAXHIS)
      INTEGER IPTRST,MAXSTK
      DATA MAXSTK, IPTRST /1,0/
C&SELF.
#endif
      IF(LUNINP.EQ.5)THEN
#if defined(CERNLIB_IBMMVS)
      CALL KDIALO ( PR, RECXX, NN)
#endif
#if !defined(CERNLIB_IBMMVS)
        WRITE(6,5000) PR
#endif
#if (!defined(CERNLIB_IBM))&&(!defined(CERNLIB_AIX370))
 2000   FORMAT(A)
 5000   FORMAT( ' ',A, $ )
#endif
#if (defined(CERNLIB_IBM)||defined(CERNLIB_AIX370))&&(!defined(CERNLIB_IBMMVS))
 2000   FORMAT(A80)
 5000   FORMAT( ' ',A )
#endif
#if (!defined(CERNLIB_UNIX))&&(!defined(CERNLIB_IBMMVS))
        READ(LUNINP,2000,END=5) RECXX
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CRAY))
        IFLAG=-1
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_IPSC))&&(!defined(CERNLIB_MSDOS))
        READ(LUNINP,2000,END=5,ERR=998) RECXX
#endif
#if defined(CERNLIB_MSDOS)
*
***   READ (LUNINP,2000,END=5,ERR=998) RECXX
*
        CALL PUTGET(RECXX,LL,CHSTKB,MAXSTK,IPTRST)
        IF (LL .EQ. -1) GO TO 5
        IF (LL .LT.  0) GO TO 998
        WRITE(6,*)
C&SELF, PUSHLINE.
        IF (LL .GT. 0) THEN
          IPTRST = IPTRST + 1
          IF (MAXSTK .LT. MAXHIS) MAXSTK = MAXSTK+1
          IF (IPTRST .GT. MAXSTK) IPTRST=1
          CHSTKB(IPTRST)=RECXX
        END IF
#endif
#if defined(CERNLIB_IPSC)
        READ(LUNINP,2000,END=5,IOSTAT=IOS) RECXX
        IF(IOS.NE.0)GO TO 998
#endif
      ELSE
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
        RECXX=' '
        READ(LUNINP,NUM=NBYT,END=5)RECXX
#endif
#if !defined(CERNLIB_IBM)||defined(CERNLIB_IBMMVS)
        READ(LUNINP,77,END=5)RECXX
 77     FORMAT(A)
#endif
      ENDIF
      N=MCLINE(RECXX)
#if defined(CERNLIB_IBMMVS)
      IF ( N .GT. NN ) N = NN
#endif
      RETURN
#if (!defined(CERNLIB_UNIX))&&(!defined(CERNLIB_IBMMVS))
 5    IF(LUNINP.EQ.5)REWIND(LUNINP)
#endif
#if defined(CERNLIB_UNIX)
 5    IF(LUNINP.NE.5)THEN
*         REWIND(LUNINP,ERR=998)
         N=-1
         RETURN
      ENDIF
#endif
      N=-1
      RETURN
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_CRAY))
 998  PRINT *,' Error during read from system input'
      N=0
      RETURN
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_CRAY))
 998  PRINT *,' Error during read from system input'
C??     IF(IFLAG.EQ.1) CALL LONGJMP(ISTACK, 1)
      N=0
      RETURN
#endif
      END
#endif
