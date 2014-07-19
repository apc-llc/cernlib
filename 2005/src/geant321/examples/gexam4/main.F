*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:14 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if !defined(CERNLIB_INTER)
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      PROGRAM GEXAM4
*
************************************************************************
*                                                                      *
*             This program simulates the L3 test beam setup            *
*             as described in  PITHA 85/11                             *
*              Experimental results published in this report can be    *
*              reproduced by this program.                             *
*                                                                      *
*         ********    Authors    R.Brun, F.Carminati  *********        *
*                                                                      *
************************************************************************
*
#if defined(CERNLIB_NTUPLE)
      PARAMETER (NMEMOR=100000)
#endif
#if !defined(CERNLIB_NTUPLE)
      PARAMETER (NMEMOR=10000)
#endif
      COMMON/PAWC/H(NMEMOR)
      COMMON/GCBANK/Q(80000)
*
*     ------------------------------------------------------------------
*
      CALL TIMEST(1E5)
*
      CALL GZEBRA(80000)
      CALL HLIMIT(-NMEMOR)
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
