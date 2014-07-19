*
* $Id: faopen.F,v 1.1.1.1 1996/03/07 15:18:05 mclareni Exp $
*
* $Log: faopen.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:05  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FAOPEN (TYPE, LUN, CHOPT, NAME, LRECL, MEXTN)
*
************************************************************************
*
*        SUBR. UTOPEN (TYPE, LUN, CHOPT, NAME, LRECL, MEXTN)
*
*   Decodes the File name and does the Fortran OPEN
*
*     TYPE  IOPA type, e.g. SAVG,GETX...
*     LUN   logical unit number
*     CHOPT Character Option for RZOPEN
*     NAME  File name
*     LRECL Record length
*     MEXTN Number of records in file (relevant for mode R on IBM)
*           (default 5000)
*
*
************************************************************************
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatsys.inc"
*
      DIMENSION       IOPT(8)
      CHARACTER       LFNAM*255
      CHARACTER       TYPE*(*), CHOPT*(*), NAME*(*), STATE*8
      CHARACTER       FORMT*12
      EQUIVALENCE     (IOPTA, IOPT(1)), (IOPTF, IOPT(2))
     +              , (IOPTO, IOPT(3)), (IOPTR, IOPT(4))
     +              , (IOPTX, IOPT(5)), (IOPTP, IOPT(6))
     +              , (IOPTN, IOPT(7)), (IOPTZ, IOPT(8))
*
#if defined(CERNLIB_IBMVM)
      CHARACTER       FILE*96
#endif
      LOGICAL         OPEN
      LDEF = LENOCC(DEFAULT)
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.NE.' ') THEN
         IF(LDEF.GT.0) THEN
            LFNAM = DEFAULT(1:LDEF) // '/' // NAME(1:LENOCC(NAME))
         ELSE
            LFNAM = NAME(1:LENOCC(NAME))
         ENDIF
 
         LCH = LENOCC(LFNAM)
         NAME = LFNAM
 
      ELSE
#endif
#if defined(CERNLIB_VAXVMS)
*
*     File name is in default directory of server
*
         LFNAM = DEFAULT(1:LDEF) // NAME(1:LENOCC(NAME))
         LCH = LENOCC(LFNAM)
         NAME = LFNAM
#endif
#if defined(CERNLIB_UNIX)
*
*     File name is in default directory of server
*
         IF(LDEF.GT.0) THEN
            LFNAM = DEFAULT(1:LDEF) // '/' // NAME(1:LENOCC(NAME))
         ELSE
            LFNAM = NAME(1:LENOCC(NAME))
         ENDIF
 
         LCH = LENOCC(LFNAM)
         NAME = LFNAM
#endif
#if defined(CERNLIB_IBMVM)
*
*     Allow RZ file to be on any disk
*
         LCH = LENOCC(NAME)
         CALL CTRANS('.',' ',NAME,1,LCH)
         IF(INDEX(CHOPT,'N').NE.0) THEN
            LFNAM = NAME(1:LCH) // ' A6'
            LCH = LCH + 3
         ELSEIF(INDEX(NAME(1:LCH),' ').EQ.INDEXB(NAME(1:LCH),' '))
     +   THEN
            LFNAM = NAME(1:LCH)//' *'
            LCH = LCH + 2
         ENDIF
#endif
#if defined(CERNLIB_IBMMVS)
         LFNAM = '.'//DEFAULT(1:LDEF)//'.'//NAME
         LCH = LENOCC(LFNAM)
#endif
#if defined(CERNLIB_CSPACK)
      ENDIF
#endif
      IF(INDEX(CHOPT,'N').NE.0) THEN
         LRECL = 1024
      ELSE
         LRECL = 0
      ENDIF
 
#if defined(CERNLIB_CSPACK)
      IF(FATNOD.EQ.' ') THEN
#endif
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9001) LFNAM(1:LCH),CHOPT,LRECL
 9001 FORMAT(' FAOPEN. calling RZOPEN for file ',A,' CHOPT ',A,
     +       ' LRECL ',I6)
*
*     Check that file is not already open. Close if this is the case
*
         INQUIRE(LUN,OPENED=OPEN)
         IF(OPEN) THEN
            IF(IDEBFA.GE.1) WRITE(LPRTFA,9002) LUN
 9002 FORMAT(' FAOPEN. file already open on ',I3,
     +       ' - closing before call to RZOPEN')
            CLOSE(LUN)
         ENDIF
 
         CALL RZOPEN(LUN,TOPDIR(1:LENOCC(TOPDIR)), LFNAM(1:LCH),CHOPT,
     +   LRECL,ISTAT)
         IF((ISTAT.NE.0).OR.(IDEBFA.GE.2)) WRITE(LPRTFA,9003) LRECL,
     +   ISTAT
 9003 FORMAT(' FAOPEN. return from RZOPEN with LRECL = ',I6,
     +       ' ISTAT = ',I6)
#if defined(CERNLIB_CSPACK)
      ELSE
         LFATND = LENOCC(FATNOD)
         IF(IDEBFA.GE.2) WRITE(LPRTFA,9004) LFNAM(1:LCH),CHOPT,LRECL,
     +   FATNOD(1:LFATND)
 9004 FORMAT(' FAOPEN. calling XZRZOP for file ',A,' CHOPT ',A,
     +       ' LRECL ',I6,' node ',A)
         CALL XZRZOP(LUN,FATNOD(1:LFATND),LFNAM(1:LCH),
     +      CHOPT,LRECL,ISTAT)
         IF((ISTAT.NE.0).OR.(IDEBFA.GE.2)) WRITE(LPRTFA,9005) LRECL,
     +   ISTAT
 9005 FORMAT(' FAOPEN. return from XZRZOP with LRECL = ',I6,
     +       ' ISTAT = ',I6)
      ENDIF
#endif
      IFMODX = IQUEST(12)
      IF(ISTAT.NE.0) THEN
         PRINT *,'FAOPEN. stopping due to fatal error opening RZ file'
         STOP 16
      ENDIF
*
*     Exchange mode flag
*
#if !defined(CERNLIB_UNIX)
      IFMODX = IQUEST(12)
#endif
#if defined(CERNLIB_UNIX)
      IFMODX = 1
#endif
      END
 
