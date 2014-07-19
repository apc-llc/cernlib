*
* $Id: stamp.F,v 1.2 1997/02/04 17:36:55 mclareni Exp $
*
* $Log: stamp.F,v $
* Revision 1.2  1997/02/04 17:36:55  mclareni
* Merge Winnt and 97a versions
*
* Revision 1.1.1.1.2.1  1997/01/21 11:32:18  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/02/15 17:48:41  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE STAMP(CDATE,CTIME,CSYST)
          CHARACTER*8 CDATE, CTIME, CSYST, CD, CT, CS
          REAL HD(2), HT(2)
#if defined(CERNLIB_APOLLO)
          DATA      CS        / 'APOLLO  ' /
#endif
#if defined(CERNLIB_NUMCD)
          DATA      CS        / 'CDC FTN5' /
#endif
#if defined(CERNLIB_NUMCR)
          DATA      CS        / 'CRAY XMP' /
#endif
#if defined(CERNLIB_NUMCV)
          DATA      CS        / 'CONVEX  ' /
#endif
#if defined(CERNLIB_NUMDE)
          DATA      CS        / 'DEC VAX ' /
#endif
#if defined(CERNLIB_DECS)
          DATA      CS        / 'DEC 3100' /
#endif
#if defined(CERNLIB_NUMIB1)
          DATA      CS        / 'MVS/F77 ' /
#endif
#if defined(CERNLIB_NUMIB2)
          DATA      CS        / 'VM/CMS  ' /
#endif
#if defined(CERNLIB_IBMRT)
          DATA      CS        / 'IBM RIOS' /
#endif
#if defined(CERNLIB_NUMND)
          DATA      CS        / 'NORD 500' /
#endif
#if defined(CERNLIB_SGI)
          DATA      CS        / 'SG Iris ' /
#endif
#if defined(CERNLIB_SUN)
          DATA      CS        / 'SunSparc' /
#endif
#if defined(CERNLIB_NUMUC)
          DATA      CS        / 'UNIVAC  ' /
#endif
#if defined(CERNLIB_NUMMS)
          DATA      CS        / 'MS DOS  ' /
#endif
#if defined(CERNLIB_NUMNT)
          DATA      CS        / 'Windows ' /
#endif
#if defined(CERNLIB_NUMLN)
          DATA      CS        / 'Linux   ' /
#endif
          CALL DATIMH(HD,HT)
#if defined(CERNLIB_NUMHIPRE)
          WRITE(CD,'(A8)') HD(1)
          WRITE(CT,'(A8)') HT(1)
#endif
#if defined(CERNLIB_NUMLOPRE)
          WRITE(CD,'(2A4)') HD
          WRITE(CT,'(2A4)') HT
#endif
          CDATE  =  CD(1:2)//'.'//CD(4:5)//'.'//CD(7:8)
          CTIME  =  CT(1:2)//':'//CT(4:5)//':'//CT(7:8)
          CSYST  =  CS
          END
