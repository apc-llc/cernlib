*
* $Id: forcsl.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: forcsl.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE FORCSL(HFLAVS,LEPT,NTIMES)
C     *************************************
C-- FORCES SEMILEPTONIC DECAY OF PARTICLES CARRYING HEAVY FLAVOR
C-- HFLAVS INTO LEPTON LEPT, FOR A NUMBER OF SUCH PARTICLES
C-- IN THE EVENT EQUAL TO NTIMES
C-- HFLAVS = 'CH','BT','TP'
C-- LEPT   = 'E  ','MU ','TAU'
C-- NTIMES=1,2
C-- CREATED: 88/06/14
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/bopar.inc"
#include "cojets/data1.inc"
#include "cojets/data3.inc"
#include "cojets/evtype.inc"
#include "cojets/fstate.inc"
#include "cojets/itapes.inc"
#include "cojets/nflav.inc"
      CHARACTER*2 HFLAVS
      CHARACTER*3 LEPT
      CHARACTER*2 CHAQ(6)
      DATA CHAQ/3*' ','CH','BT','TP'/
      DATA ICALL/0/
C
C-- CHECK NO. OF CALLS
      IFL=0
      IF(ICALL.GT.0) THEN
        WRITE(ITLIS,59)
59      FORMAT(/' ***FORCSL USED MORE THAN ONCE'
     *  ,' -- JOB WILL BE ABORTED')
        IFL=1
        GO TO 100
      ENDIF
      ICALL=1
C
C-- FLAVOR
      DO 90 L=4,6
      IF(HFLAVS.NE.CHAQ(L)) GO TO 90
      KFORSL=L
      GO TO 89
90    CONTINUE
      WRITE(ITLIS,92)
92    FORMAT(/' ***BAD 1ST ENTRY FOR FORCSL (ALLOWED INPUTS ARE '
     *,14H'CH','BT','TP',' -- JOB WILL BE ABORTED')
      IFL=1
89    CONTINUE
      IF(IEVTYP.EQ.1.AND.KFORSL.NE.KFRFLA) THEN
        WRITE(ITLIS,60)
60      FORMAT(/' ***WITH TWOJET, FORCSL CAN BE USED ONLY WITH HEAVY'
     *  ,' FLAVOR FORCING OF THE SAME TYPE AS IN THE FORCSL CALL'
     *  /' --- JOB WILL BE ABORTED')
        IFL=1
      ENDIF
        IF(IEVTYP.EQ.2.AND.KFORSL+6.NE.IDECBO) THEN
          WRITE(ITLIS,91)
91        FORMAT(/' ***WITH Z DRELLYAN, FORCSL CAN BE USED ONLY WITH'
     *    ,' A FLAVOR COMPATIBLE WITH THE Z DECAY CHANNEL FORCED'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(IEVTYP.EQ.3.AND.((KFORSL.EQ.4.AND.IDECBO.NE.5).OR.
     *  (KFORSL.GE.5.AND.IDECBO.NE.6))) THEN
          WRITE(ITLIS,93)
93        FORMAT(/' ***WITH W DRELLYAN, FORCSL CAN BE USED ONLY WITH'
     *    ,' A FLAVOR COMPATIBLE WITH THE W DECAY CHANNEL FORCED'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(IEVTYP.EQ.4) THEN
          WRITE(ITLIS,94)
94        FORMAT(/' ***FORCSL CANNOT BE USED WITH GM DRELLYAN'
     *    ,' (WHAT FOR ?)'
     *    /' --- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
C
C-- LEPTON
        IF(LEPT.EQ.'E  ') THEN
          LFORSL=12
        ELSEIF(LEPT.EQ.'MU ') THEN
          LFORSL=14
        ELSEIF(LEPT.EQ.'TAU') THEN
          LFORSL=16
        ELSE
          WRITE(ITLIS,61)
61        FORMAT(/' ***BAD 2ND ENTRY FOR FORCSL'
     *    ,' -- JOB WILL BE ABORTED')
       IFL=1
        ENDIF
C
C-- NO. OF TIMES
        IF(NTIMES.GT.2) THEN
          WRITE(ITLIS,76)
76        FORMAT(/' ***NO. OF FORCING TIMES .GT.2'
     *    ,' -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(IEVTYP.EQ.3.AND.NTIMES.GT.1) THEN
          WRITE(ITLIS,95)
95        FORMAT(/' ***WITH W DRELLYAN, NO. OF FORCING TIMES'
     *    ,' CAN BE ONLY 1 -- JOB WILL BE ABORTED')
          IFL=1
        ENDIF
        IF(NTIMES.EQ.0) NTIMES=1
        NFORSL=NTIMES
100   CONTINUE
C
C-- ABNORMAL EXIT
300   IF(IFL.EQ.1) THEN
        WRITE(ITLIS,301)
301      FORMAT(/' ***JOB ABORTED')
        STOP
      ENDIF
      RETURN
      END
