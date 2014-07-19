*
* $Id: zslist.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zslist.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSLIST(IGOFF,CHMAIL)
*
*         to list contents of a remote directory (module or file)
*
#include "cspack/czdir.inc"
      CHARACTER*(*) CHMAIL
*
      if(nchrz.le.0)then
         CALL CZPUTA('1 Please connect file or module first',ISTAT)
         go to 99
      endif
*
      IF(CHMAIL(1:2).EQ.'//')THEN
         DO 10 I=1,NCHRZ
            CALL CZPUTA('2 //'//CHRZ(I)(1:12)//FLNAME(I),ISTAT)
  10     CONTINUE
         CALL CZPUTA('0',ISTAT)
         GO TO 99
      ENDIF
#if defined(CERNLIB_OS9)
      IF(LUNRZ(IDIR).LE.0) CALL ZSLMOD
#endif
*
#if defined(CERNLIB_MMAP)
      IF(LUNRZ(IDIR).LT.0)THEN
         CALL ZSLISTM(IGOFF)
         CALL CZPUTA('0',ISTAT)
      ENDIF
#endif
*
      IF(LUNRZ(IDIR).GT.0)THEN
         CALL ZSLDIR
         CALL CZPUTA('0',ISTAT)
      ENDIF
*
  99  END
 
