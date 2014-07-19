*
* $Id: hnear.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: hnear.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE HNEAR(ID,IDC,IDNEXT)
*
*      If ID=0 returns first histogram identifier in IDC
*      If ID.ne.0 returns IDC=ID if ID exists, IDC=0 otherwise
*                 returns IDNEXT the identifier of the next hist.
*
      TEMPLATE PAWM
#include "cspack/pawc.inc"
      DIMENSION IQ(1),Q(1),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(Q(1),LQ(9)),(IQ(1),Q(1))
*
      IDNEXT=0
      IDC=0
      IF(LMAIN.LE.0)RETURN
      LCID =LQ(LMAIN-2)
      IF(LCID.EQ.0)RETURN
      IF(ID.EQ.0)THEN
         IDC=IQ(LCID-5)
         IDNEXT=IDC
         RETURN
      ENDIF
*
  10  IF(LCID.EQ.0)RETURN
      IF(IQ(LCID-5).EQ.ID)THEN
         IDC=ID
         LCID=LQ(LCID)
         IF(LCID.EQ.0)THEN
            IDNEXT=0
         ELSE
            IDNEXT=IQ(LCID-5)
         ENDIF
      ELSE
         LCID=LQ(LCID)
         GO TO 10
      ENDIF
*
      END
 
 
#endif
