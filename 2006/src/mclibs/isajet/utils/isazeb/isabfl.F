*
* $Id: isabfl.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: isabfl.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISABFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     book and fill ISAB (ISAJET begin-of-run) bank
C-
C-   ENTRY ISA_STOP (program stop, done generating)
C-
C-   ENTRY ISA_RUNNO(IRUN) provide a run number
C-   Input:
C-   IRUN = run number
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "idrun.inc"
#include "itapes.inc"
#include "keys.inc"
#include "jetlim.inc"
#include "primar.inc"
#include "zebcom.inc"
      INTEGER K,IOISAB,IRN,LOUT,NREAC,IUH
      CHARACTER*4 TTL
      LOGICAL FIRST
      SAVE FIRST,TTL
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C
C
      DATA IRN/1/
      IF(ITCOM.NE.0) READ(ITCOM,1) TTL
    1 FORMAT(A4)
C
C  set flag for reaction
      DO 6 K=1,6
        IF(KEYS(K)) NREAC=K
    6 CONTINUE
C
C  create Zebra bank ISAB
      IF(FIRST) CALL MZFORM('ISAB','3I,-F',IOISAB)
      FIRST=.FALSE.
      LISAB=0
      CALL MZBOOK(IXMAIN,LISAB,LISAB,1,
     $            'ISAB',2,2,22,IOISAB,-1)
      IQ(LISAB+1)=IDVER
      IQ(LISAB+2)=NREAC
      IQ(LISAB+3)=NEVENT
      Q(LISAB+4)=ECM
      CALL UCOPY(PTMIN,Q(LISAB+5),6)
      CALL UCOPY(THMIN,Q(LISAB+11),6)
      CALL UCOPY(PHIMIN,Q(LISAB+17),6)
      IF(TTL.NE.'SAME'.AND.ITCOM.NE.0) CALL ISCMFL(ITCOM)
  999 RETURN
C
      ENTRY ISA_STOP
      IF(TTL.EQ.'STOP') THEN
        WRITE(ITLIS,2) TTL
    2   FORMAT(//'   ',A4,' in ISAJET command file, JOB STOPPED')
        STOP
      ENDIF
      RETURN
      END
