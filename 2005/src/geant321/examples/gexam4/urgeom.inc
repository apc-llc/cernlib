*
* $Id: urgeom.inc,v 1.1.1.1 1995/10/24 10:22:14 cernlib Exp $
*
* $Log: urgeom.inc,v $
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
* Geant
*
*
*
* urgeom.inc
*
#if !defined(CERNLIB_INCLUDE)
*
      COMMON/URGEOM/UTHICK,CSHIEL,ESHIEL,BTHICK,AIRGP1,CTHICK,AIRGP2,
     *              AIRGP3,EPCHAM,HMODL1,HSECT1,HMODL2,HSECT2,HMODL3,
     *              HSECT3,HLCALO,WGS1,WGS2,IGAST
C
#endif
*
* urgeom.inc
*
#if defined(CERNLIB_INCLUDE)
#include "incbeg.inc"
     +   urgeom
#include "incend.inc"
#endif
