*
* $Id: zsrzin.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zsrzin.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSRZIN(CHMAIL)
*
*         to read a record from a RZ file remotely
*
#include "cspack/czdir.inc"
#include "cspack/czunit.inc"
#include "cspack/hcbook.inc"
      COMMON/QUEST/IQUEST(100)
      COMMON/QSAVE/IQSAVE(100)
      CHARACTER*8 CHOPT
      CHARACTER*(*) CHMAIL
      INTEGER KEYS(2)
      DIMENSION IOCR(9)
*
*
*          Decode message from host
*
      READ(CHMAIL,1000)KEYS,ICYCLE,CHOPT
 1000 FORMAT(6X,2I10,I10,A)
*
      LCID=0
#if defined(CERNLIB_OS9)
      IF(LUNRZ(IDIR).LE.0)THEN
         CALL ZSGET(KEYS(1),CHOPT)
         GO TO 99
      ENDIF
#endif
#if defined(CERNLIB_VAXVMS)||defined(CERNLIB_MMAP)
      IF(LUNRZ(IDIR).LE.0)THEN
         IQUEST(1)=0
         CALL HRIN(KEYS(1),1,0)
         GO TO 10
      ENDIF
#endif
*
*          Read ZEBRA structure from RZ file
*
      CALL RZIN(IHDIV,LCID,1,KEYS,ICYCLE,CHOPT)
*
*          Send the IQUEST vector
*
  10  CALL UCOPY(IQUEST,IQSAVE,100)
*
*          Transfer Data structure to client
*
      IF(LCID.NE.0)THEN
         IOCR(1) = 2
         CALL FZOUT(LUNFZO,IHDIV,LCID,1,' ',IOCR,100,IQSAVE)
         CALL MZDROP(IHDIV,LCID,' ')
         LCID=0
         CALL HDELET(0)
      ELSE
         IOCR(1) = 2
         CALL FZOUT(LUNFZO,0,0,1,'Z',IOCR,100,IQSAVE)
      ENDIF
*
  99  END
