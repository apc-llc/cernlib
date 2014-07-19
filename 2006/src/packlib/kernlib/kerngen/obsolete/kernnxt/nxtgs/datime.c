/*
 * $Id: datime.c,v 1.1.1.1 1996/02/15 17:53:30 mclareni Exp $
 *
 * $Log: datime.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:30  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE DATIME
  CERN PROGLIB# Z007    DATIME          .VERSION KERNNXT  1.00  901105
  */
#include <sys/types.h>
#include <time.h>
 
struct tm *tp;
#if defined(CERNLIB_QX_SC)
#define slate slate_
struct { int  inum[39]; } slate_;
void datime_(id, it)
#endif
#if defined(CERNLIB_QXNO_SC)
struct { int  inum[39]; } slate;
void datime(id, it)
#endif
#if defined(CERNLIB_QXCAPT)
#define slate SLATE
struct { int  inum[39]; } SLATE;
void DATIME(id, it)
#endif
long *id, *it;
{
   time_t tloc = time(0);
   tp = localtime(&tloc);
   slate_.inum[0] = tp->tm_year + 1900;
   slate_.inum[1] = tp->tm_mon + 1;
   slate_.inum[2] = tp->tm_mday;
   slate_.inum[3] = tp->tm_hour;
   slate_.inum[4] = tp->tm_min;
   slate_.inum[5] = tp->tm_sec;
   *id  = tp->tm_year * 10000;
   *id += (tp->tm_mon + 1) * 100;
   *id += tp->tm_mday;
   *it  = tp->tm_hour * 100;
   *it += tp->tm_min;
   return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
