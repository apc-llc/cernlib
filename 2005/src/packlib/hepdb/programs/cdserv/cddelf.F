*
* $Id: cddelf.F,v 1.2 1996/03/12 13:07:58 cernlib Exp $
*
* $Log: cddelf.F,v $
* Revision 1.2  1996/03/12 13:07:58  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:36  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDDELF(CHFILE,IRC)
      CHARACTER*(*) CHFILE
      CHARACTER*255 CHNAME
      INTEGER       SYSTEMF
#include "hepdb/cduscm.inc"
 
      IRC    = 0
      LNAME  = LENOCC(CHFILE)
      CHNAME = CHFILE(1:LNAME)
      IF(IDEBCD.GE.1) WRITE(LPRTCD,9001) CHNAME(1:LNAME)
9001  FORMAT(' CDDELF. deleting ',A)
 
#if defined(CERNLIB_UNIX)
      IRC = SYSTEMF('rm -f '//CHNAME(1:LNAME))
#endif
#if defined(CERNLIB_VAXVMS)
      ICODE = LIB$DELETE_FILE(CHNAME(1:LNAME))
      IF(.NOT.ICODE) IRC = 42
#endif
#if defined(CERNLIB_IBMVM)
      CALL VMCMS('ERASE '//CHNAME(1:LNAME),IRC)
#endif
 
      END
