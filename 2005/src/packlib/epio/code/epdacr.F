*
* $Id: epdacr.F,v 1.1.1.1 1996/03/08 15:21:43 mclareni Exp $
*
* $Log: epdacr.F,v $
* Revision 1.1.1.1  1996/03/08 15:21:43  mclareni
* Epio
*
*
#include "epio/pilot.h"
      SUBROUTINE EPDACR(LUNIT,IBLOCK,IRCOFF,IERR)
C.
C. prepares a unit for random access reading
C.
C. input :
C.
C. LUNIT  logical unit
C. IBLOCK no. of the block to be read next (READ(LUN,REC=IBLOCK)...)
C. IRCOFF record off-set of first logical record in block (see
C.        remark below)
C.
C. output
C. IERR     ERROR FLAG. SEE SEPARATE LIST.
C.
C. Remark:
C. The file has to be scanned in sequential mode beforehand,
C. by calling EPREAD with MODE=20 (obligatory).
C. After each such call the user keeps control words 11 (block
C. number) and 31 (record off-set) for those records from
C. where he wants to start.
C. The unit must be opened for random access by the user.He may
C. then read any number of records following the one given
C. in the call to EPDACR.
C. The example should be consulted.
C.
#include "epio/epiocom.inc"
 
      IF(LASTUT.NE.LUNIT)  THEN
         CALL EPUNIT(LUNIT,IERR)
         IF(IERR.NE.0)  GOTO 999
      ENDIF
*--- set random access mode
      LIST(ISTART+32)=1
*--- set flag for reading next buffer to "true"
      LIST(ISTART+14)=0
*--- set pointer position flag to "start of header"
      LIST(ISTART+22)=0
*--- set record off-set
      LIST(ISTART+23)=IRCOFF
*--- set block number of last block "read"
      LIST(ISTART+11)=IBLOCK-1
  999 END
