*
* $Id: main.F,v 1.1.1.1 1995/10/24 10:22:12 cernlib Exp $
*
* $Log: main.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:12  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if !defined(CERNLIB_INTER)
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      PROGRAM GEXAM3
C.
C.
C.           Simulation program for the DLS experiment at LBL
C.             Original code from G.Roche LBL
C.             modified  by R.Brun
C.
C.           Kinematics is read from UNIT=4 (OPEN or JCL required)
C.                              and can be found in DECK,DATA32
C.
C.        DATA CARD USER FLAGS:
C.             SWIT(1)=1     ==>  CALL GPVERT AND GPKINE
C.                     2     ==>  CALL GPJXYZ
C.                     3     ==>  CALL GPHITS
C.             SWIT(2)=2     ==>  CALL GPCXYZ
C.                     3     ==>  CALL GDCXYZ
C.             SWIT(6)=1     ==>  TURN OFF TRACKING OF SECONDARIES
C.
C.
      COMMON/PAWC/H(10000)
      COMMON /GCBANK/ Q(150000)
*
      CALL TIMEST(1E5)
*
      CALL GZEBRA(150000)
      CALL HLIMIT(-10000)
*
      CALL HPLINT(0)
*
      CALL UGINIT
      CALL GRUN
      CALL UGLAST
*
      END
#endif
