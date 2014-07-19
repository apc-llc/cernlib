*
* $Id: zsout.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsout.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSOUT(CHMAIL)
*
*         to write a record to a RZ file remotely
*
#include "cspack/czunit.inc"
#include "cspack/hcbook.inc"
      COMMON/QUEST/IQUEST(100)
      COMMON/QSAVE/IQSAVE(100)
      CHARACTER*(*) CHMAIL
      CHARACTER*8 CHOPT
      DIMENSION KEYS(1)
      DIMENSION IUHEAD(9),IOCR(9)
*
*          Decode message from host
*
      READ(CHMAIL,1000)KEYS,ICYCLE,CHOPT
 1000 FORMAT(6X,I10,I10,A)
*
*          Transfer Data structure from client
*
      LCID=0
      NUH =0
      CALL FZIN(LUNFZI,IHDIV,LCID,1,' ',NUH,IUHEAD)
      IF(IQUEST(1).NE.0)GO TO 90
*
*          Write ZEBRA structure to RZ file
*
#if defined(CERNLIB_OS9)
      CALL VZERO(IQUEST,100)
#endif
#if !defined(CERNLIB_OS9)
      CALL RZOUT(IHDIV,LCID,KEYS,ICYCLE,CHOPT)
      CALL RZSAVE
      CALL HDELET(0)
#endif
*
*          Send the IQUEST vector
*
  90  CALL UCOPY(IQUEST,IQSAVE,100)
      IOCR(1) = 2
      CALL FZOUT(LUNFZO,0,0,1,'Z',IOCR,100,IQSAVE)
*
  99  END
 
