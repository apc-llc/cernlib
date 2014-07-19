*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:23 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:23  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if !defined(CERNLIB_INTER)
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
*
      PROGRAM MAIN
*     program for fast simualtion by bootstrap lower energy
*       electrons/positrons and charged pions.
*
*                            Author: H. S. Chen
*
      COMMON/PAWC/H(90000)
      COMMON/GCBANK/Q(280000)
*
      CALL GZEBRA(280000)
*
      CALL HLIMIT(-90000)
*
*             Geant initialization
      CALL UGINIT
*
*             Test printings.
      CALL GPRINT('MATE',0)
      CALL GPRINT('TMED',0)
      CALL GPRINT('VOLU',0)
      CALL GPRINT('SETS',0)
*
*             Start events processing
      CALL GRUN
*
*             End of run
      CALL UGLAST
*
      END
#endif
