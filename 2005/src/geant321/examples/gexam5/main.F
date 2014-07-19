*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:17 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if !defined(CERNLIB_INTER)
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      PROGRAM GEXAM5
*
************************************************************************
*                                                                      *
C....................................................
C....    Try at COMPTEL simulation                  .
C....                                               .
C....                                       Eamonn  .
C....                                      TMA/ESTEC.
C......................................................
C.                                                    .
*                                                                      *
************************************************************************
*
      COMMON/PAWC/H(10000)
      COMMON/GCBANK/Q(80000)
*
*     ------------------------------------------------------------------
*
      CALL TIMEST(1E5)
*
      CALL GZEBRA(80000)
      CALL HLIMIT(-10000)
*
*     Initialise graphics
      CALL HPLINT(0)
*
*             Geant initialization
      CALL UGINIT
*
*             Test printings.
      CALL GPRINT('PART',0)
      CALL GPRINT('MATE',0)
      CALL GPRINT('TMED',0)
      CALL GPRINT('VOLU',0)
*
*             Start events processing
      CALL GRUN
*
*             End of run
      CALL UGLAST
*
      END
#endif
