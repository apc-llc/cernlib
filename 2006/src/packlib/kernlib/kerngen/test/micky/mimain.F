*
* $Id: mimain.F,v 1.2 1996/04/02 23:19:34 thakulin Exp $
*
* $Log: mimain.F,v $
* Revision 1.2  1996/04/02 23:19:34  thakulin
* Support for EPC Fortran:  remove char-int equivalences and use F90
* transfer facility.
*
* Revision 1.1.1.1  1996/02/15 17:54:54  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      PROGRAM MICKY

#include "kerngent/mkch.inc"
#include "kerngent/mkcde.inc"
      COMMON /SLATY/ ISLY(4)
      COMMON /SLATE/ ISLATE(40)
      COMMON /SLATX/ ISLX(4)
      DIMENSION    IDATEV(2),      ITIMEV(2)
#if !defined(CERNLIB_F90) && !defined(CERNLIB_QFEPC)
      CHARACTER    CHDAT*8,        CHTIM*8
      EQUIVALENCE (CHDAT,IDATEV), (CHTIM,ITIMEV)
#endif

      IFLGU = 0
      LUNGU = 0

C-        Intialize
      CALL MINIT

C-        Timing

C-        Execution

C-        Check of the checking routines
      CALL CHKVER

C-        Very basic things first
      CALL  TUZERO
      CALL  TBYT
      CALL  TMBYT
      CALL  TUCOPY
      CALL  TUBLOW
      CALL  TUCTOH

C-        Patch PM432
      CALL  TLENOC
      CALL  TICNTH

C-        Patch PUTIL
      CALL  TCOCOP
      CALL  TIUCHA
      CALL  TIUCOM
      CALL  TIUSAM
      CALL  TUOPT
      CALL  TUSWOP

C-        Patch PCHAR
      CALL  TBABY
      CALL  TPKCH

C-        Patch NPBYT
      CALL  TVCBYT

C-        Patch PDIV
      CALL  TMAXMU
      CALL  TSCALE
      CALL  TSORTN
C!    CALL  TSORTX
      CALL  TTRIGO

C-        Patch PMATRIX
C!    CALL  TMXEQU
      CALL  TMXM
      CALL  TMXTRP
      CALL  TMXUTY

C-        Patch PVECT
      CALL  TCROSS
      CALL  TLOR
      CALL  TROT

C-        Patch PVPACK
      CALL  TVEC

C-        Patch PTLPAC
      CALL  TTLPAC

C-        Patch PTRPAC
      CALL TTRCHO
      CALL TTRINV
      CALL TTRLA

C-        Patch PM434
      CALL  TVXINV

      CALL PRTEST
      NFAIL = 0

C----         Exercise some Unix system calls

#if !defined(CERNLIB_QMIBM)
      CALL TCHDIR
#endif

C----         Exercise the date/time routines

   81 CALL DATIME (IDATEV(1),IDATEV(2))

      WRITE (ITB,9081) IDATEV, (ISLATE(J),J=1,6)
 9081 FORMAT (1X/' DATIME  ',I8,2X,I4,'    SLATE: ',6I4)

      CALL DATIMH (IDATEV,ITIMEV)

#if defined(CERNLIB_F90) || defined(CERNLIB_QFEPC)
      WRITE (ITB,9084) transfer(IDATEV,'abcdefgh')
                       transfer(ITIMEV,'abcdefgh')
#else
      WRITE (ITB,9084) CHDAT, CHTIM
#endif
 9084 FORMAT (' DATIMH  ',A,1X,A)

      IF (NFAILT.EQ.0)  THEN
          WRITE (ITB,9088)
        ELSE
          WRITE (ITB,9089) NFAILT
          CALL EXITRC (1)
        ENDIF

 9088 FORMAT (/' All tests passed.')
 9089 FORMAT (/' !!! ',I6,' tests failed in all !!!')
      END
