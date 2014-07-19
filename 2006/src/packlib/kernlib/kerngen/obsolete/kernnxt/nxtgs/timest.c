/*
 * $Id: timest.c,v 1.1.1.1 1996/02/15 17:53:31 mclareni Exp $
 *
 * $Log: timest.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:31  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE TIMEST
  CERN PROGLIB# Z007    TIMEST          .VERSION KERNNXT  1.00  901105
  ORIG. 01/03/85  FCA
  */
#include <sys/types.h>
#include <sys/times.h>
 
struct tms tps;
static float timlim;
static time_t timstart, timlast;
static long init = 1;
float deftim = 999999.; /* FHH + stime !*/
float HZ = 60.;
 
#if defined(CERNLIB_QX_SC)
void timest_(timl)
#endif
#if defined(CERNLIB_QXNO_SC)
void timest(timl)
#endif
#if defined(CERNLIB_QXCAPT)
void TIMEST(timl)
#endif
float *timl;
{
 if (init != 0) {
    times(&tps);
    timlim = *timl;
    timstart = tps.tms_utime+tps.tms_cutime+tps.tms_stime+tps.tms_cstime;
    timlast  = timstart;
    init = 0;
 }
    return;
}
 
#if defined(CERNLIB_QX_SC)
void timex_(tx)
#endif
#if defined(CERNLIB_QXNO_SC)
void timex(tx)
#endif
#if defined(CERNLIB_QXCAPT)
void TIMEX(tx)
#endif
/*
  CERN PROGLIB# Z007    TIMEX           .VERSION KERNNXT  1.00  901105
  */
float *tx;
{
   time_t timnow;
   if (init) {
#if defined(CERNLIB_QX_SC)
       timest_(&deftim);
#endif
#if defined(CERNLIB_QXNO_SC)
       timest(&deftim);
#endif
#if defined(CERNLIB_QXCAPT)
       TIMEST(&deftim);
#endif
       *tx = 0.;
   }
   else {
       times(&tps);
       timnow = tps.tms_utime+tps.tms_cutime+tps.tms_stime+tps.tms_cstime;
       *tx = (float) (timnow - timstart) / HZ;
   }
   return;
}
 
#if defined(CERNLIB_QX_SC)
void timed_(td)
#endif
#if defined(CERNLIB_QXNO_SC)
void timed(td)
#endif
#if defined(CERNLIB_QXCAPT)
void TIMED(td)
#endif
/*
  CERN PROGLIB# Z007    TIMED           .VERSION KERNNXT  1.00  901105
  */
float *td;
{
   time_t timnow;
   if (init) {
#if defined(CERNLIB_QX_SC)
       timest_(&deftim);
#endif
#if defined(CERNLIB_QXNO_SC)
       timest(&deftim);
#endif
#if defined(CERNLIB_QXCAPT)
       TIMEST(&deftim);
#endif
       *td = deftim;
   }
   else {
       times(&tps);
       timnow = tps.tms_utime+tps.tms_cutime+tps.tms_stime+tps.tms_cstime;
       *td = (float) (timnow - timlast) / HZ;
       timlast = timnow;
   }
   return;
}
 
#if defined(CERNLIB_QX_SC)
void timel_(tl)
#endif
#if defined(CERNLIB_QXNO_SC)
void timel(tl)
#endif
#if defined(CERNLIB_QXCAPT)
void TIMEL(tl)
#endif
/*
  CERN PROGLIB# Z007    TIMEL           .VERSION KERNNXT  1.00  901105
  */
float *tl;
{
   time_t timnow;
   if (init) {
#if defined(CERNLIB_QX_SC)
       timest_(&deftim);
#endif
#if defined(CERNLIB_QXNO_SC)
       timest(&deftim);
#endif
#if defined(CERNLIB_QXCAPT)
       TIMEST(&deftim);
#endif
       *tl = deftim;
   }
   else {
       times(&tps);
       timnow = tps.tms_utime+tps.tms_cutime+tps.tms_stime+tps.tms_cstime;
       *tl = timlim - (float) (timnow - timstart) / HZ;
   }
   return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_TIMEST
#undef CERNLIB_TCGEN_TIMEST
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
