*
* $Id: zscdir.F,v 1.2 1996/04/11 14:54:20 cernlib Exp $
*
* $Log: zscdir.F,v $
* Revision 1.2  1996/04/11 14:54:20  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE ZSCDIR(CHMAIL)
*
*         Change Current Directory of a remote file or module
*
#if defined(CERNLIB_OS9)
      TEMPLATE PAWM
      COMMON/PAWM/NM(10000)
      INTEGER F_LINK
#include "cspack/hos9c.inc"
#endif
#include "cspack/czdir.inc"
#include "zebra/rzdir.inc"
#include "zebra/quest.inc"
C
      CHARACTER*64 CHP
      CHARACTER*32 CHDIR
      CHARACTER*(*) CHMAIL
*
      IF(NCHRZ.EQ.0)THEN
         CALL CZPUTA('1 Please connect file or module first',ISTAT)
         GO TO 99
      ENDIF
*
      NCH=LENOCC(CHMAIL)
      IF(NCH.EQ.0)THEN
#if defined(CERNLIB_OS9)
         if(idir.eq.1)then
            lcpu=lenocc(cpu)
            chdir=cpu(1:lcpu)//'/'//module
            CALL CZPUTA('1 Current Directory ='//chdir,ISTAT)
            go to 99
         endif
#endif
         CALL RZCDIR(CHP,'R')
         CALL CZPUTA('1 Current Directory ='//CHP,ISTAT)
         GO TO 99
      ENDIF
      CALL CLTOU(CHMAIL)
      CALL RZCDIR(CHMAIL,' ')
      IF(IQUEST(1).NE.0)GO TO 90
      CHDIR=CHCDIR(1)
      DO 10 I=1,NCHRZ
         IF(CHDIR.EQ.CHRZ(I))THEN
            IDIR=I
            GO TO 20
         ENDIF
  10  CONTINUE
      GO TO 90
*
  20  CALL CZPUTA('1 Remote Directory changed to //'//CHDIR,ISTAT)
      GO TO 99
*
*           Error
*
  90  CALL CZPUTA('3 Cannot set remote directory ',ISTAT)
*
  99  END
 
