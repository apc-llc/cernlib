*
* $Id: ffsizeapo.F,v 1.1.1.1 1996/03/06 10:47:16 mclareni Exp $
*
* $Log: ffsizeapo.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:16  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
#if defined(CERNLIB_QMAPO)
      SUBROUTINE FFSIZE (LUN,NWREC,JREC,ISTAT)

C-    Get the size of a disk file connected to Fortran unit LUN

%include '/sys/ins/base.ins.ftn'
%include '/sys/ins/ios.ins.ftn'

      INTEGER*2    IDSTRM

      INQUIRE (LUN, STRID=IDFOR, ERR=99)

      IDSTRM = IDFOR

C--       byte inquiry

      NBYTES = IOS_$INQ_BYTE_POS (IDSTRM,IOS_$EOF,ISTAT)
      IF (ISTAT.NE.0)              GO TO 41

      NWORDS = NBYTES / 4
      JREC   = NWORDS / NWREC
      RETURN

C--       record inquiry

   41 NBYTES = IOS_$INQ_REC_POS (IDSTRM,IOS_$EOF,ISTAT)
      IF (ISTAT.NE.0)              RETURN
      JREC   = NBYTES
      RETURN

   99 ISTAT = -1
      RETURN
      END
#endif
