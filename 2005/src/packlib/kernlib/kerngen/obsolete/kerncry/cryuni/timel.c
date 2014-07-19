/*
 * $Id: timel.c,v 1.1.1.1 1996/02/15 17:52:44 mclareni Exp $
 *
 * $Log: timel.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:44  mclareni
 * Kernlib
 *
 */
#include "kerncry/pilot.h"
/*>    ROUTINE TIMEL
  CERN PROGLIB# Z007    TIMEL           .VERSION KERNCRY  1.15  910318
  ORIG. 28/04/88  FCA
  MOD.  28/02/91  FCA
*/
#include <sys/types.h>
#include <sys/category.h>
#include <sys/jtab.h>
#include <sys/resource.h>
#include <sys/times.h>
#include <sys/machd.h>

struct tms tms;
static float timlim, invhz;
static time_t timstart, timlast;
static long init = 1;
float deftim = 105263157000.;

int  getjtab();
long times();
char *getenv();
int  strcmp();

void TIMEST(timl)
float *timl;
{
 if (init != 0) {
    char *env;
    times(&tms);
    invhz = 1./ (float) HZ;
    timlim = *timl;
    timstart = tms.tms_utime+tms.tms_stime;
    timlast  = timstart;
    env = getenv("ENVIRONMENT");
    if ( strcmp( env, "BATCH" ) == 0 ) timlim = deftim ;
    else timlim = *timl + (float) timstart * invhz;
/*  printf("%f %f \n",timlim,*timl);  */
    init = 0;
 }
    return;
}

void TIMEL(tl)
/*
C
  CERN PROGLIB# Z007    TIMEL           .VERSION KERNCRY  1.15  910318
C
*/
float *tl;
{
   struct jtab jtab;
   time_t timnow, jtleft, ptleft;
   long plimit, iflag, limit();
   float timtst, timjob;

   if (init) {
       TIMEST(&deftim);
   }
   plimit = limit (C_PROC, 0, L_CPU, -1);
   if (plimit != -1) {
       timtst = (float) plimit * invhz;
       if (timtst < timlim) timlim = timtst;
   }
   times(&tms);
   timnow = tms.tms_utime+tms.tms_stime;
   timtst = timlim - (float) timnow * invhz;
   if (getjtab(&jtab) > 0) {
       jtleft = jtab.j_cpulimit - (jtab.j_ucputime + jtab.j_scputime);
       timjob = (float) jtleft * invhz;
       if (timjob < timtst ) timtst = timjob;
   }
   *tl = timtst;
   return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_CCGEN_TIMEL
#undef CERNLIB_CCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEST
#undef CERNLIB_TCGEN_TIMEST
#endif
