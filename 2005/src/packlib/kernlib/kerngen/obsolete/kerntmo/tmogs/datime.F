*
* $Id: datime.F,v 1.1.1.1 1996/02/15 17:53:49 mclareni Exp $
*
* $Log: datime.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:49  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      Subroutine DATIME(id,it)
C
C CERN PROGLIB# Z007    DATIME          .VERSION KERNTMO  1.00  890208
C ORIG. 03/02/89  K.M.STORR
C
      integer*4 ndum(2),ntum(2),nd(2),nt(2)
      data ndum/4h01/0,4h1/89/,ntum/4h00.0,4h0.00/
C
c DATIME dummy - lacking appropriate clock function
      id=890101
      it=0000
      return
c DATIMH dummy - lacking appropriate clock function
      Entry DATIMH(nd,nt)
      nd(1)=ndum(1)
      nd(2)=ndum(2)
      nt(1)=ntum(1)
      nt(2)=ntum(2)
      return
c TIMEX returns execution time relative to the first call to TIMEST
      entry TIMEX(t)
      if(icall.eq.0)then
         t=0.
      else
         iend=itime()
         t=64.e-6*(iend-istart)
      endif
      return
c TIMEL returns time remaining until time limit set by call to TIMEST
      entry TIMEL(t)
      if(icall.eq.0)then
         t=999.
      else
         iend=itime()
         time_elapsed=64.e-6*(iend-istart)
         t=tlim-time_elapsed
      endif
      return
c TIMED returns execution time interval since last call to TIMED
      entry TIMED(t)
      iend=itime()
      t=64.e-6*(iend-iold)
      iold=iend
      return
c TIMEST initialises timing operations
      entry TIMEST(t)
      data icall/0/
      if(icall.eq.0)then
         istart=itime()
         iold=istart
         tlim=t
         icall=1
      endif
      return
c
      end
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
#ifdef CERNLIB_TCGEN_TIMEST
#undef CERNLIB_TCGEN_TIMEST
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
