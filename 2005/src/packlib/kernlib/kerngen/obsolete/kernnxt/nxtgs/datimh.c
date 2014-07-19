/*
 * $Id: datimh.c,v 1.1.1.1 1996/02/15 17:53:30 mclareni Exp $
 *
 * $Log: datimh.c,v $
 * Revision 1.1.1.1  1996/02/15 17:53:30  mclareni
 * Kernlib
 *
 */
#include "kernnxt/pilot.h"
/*>    ROUTINE DATIMH
  CERN PROGLIB# Z007    DATIMH          .VERSION KERNNXT  1.00  901105
  */
#include <sys/types.h>
#include <time.h>
 
struct tm *tp;
#if defined(CERNLIB_QX_SC)
void datimh_(dh, th)
#endif
#if defined(CERNLIB_QXNO_SC)
void datimh(dh, th)
#endif
#if defined(CERNLIB_QXCAPT)
void DATIMH(dh, th)
#endif
char dh[7], th[7];
{
   time_t tloc = time(0);
   tp = localtime(&tloc);
   dh[0] = tp->tm_mday / 10 + '0';
   dh[1] = tp->tm_mday % 10 + '0';
   dh[2] = '/';
   dh[3] = (tp->tm_mon + 1) / 10 + '0';
   dh[4] = (tp->tm_mon + 1) % 10 + '0';
   dh[5] = '/';
   dh[6] = tp->tm_year / 10 + '0';
   dh[7] = tp->tm_year % 10 + '0';
   th[0] = tp->tm_hour / 10 + '0';
   th[1] = tp->tm_hour % 10 + '0';
   th[2] = '.';
   th[3] = tp->tm_min  / 10 + '0';
   th[4] = tp->tm_min  % 10 + '0';
   th[5] = '.';
   th[6] = tp->tm_sec  / 10 + '0';
   th[7] = tp->tm_sec  % 10 + '0';
   return;
}
/*> END <----------------------------------------------------------*/
#ifdef CERNLIB_TCGEN_DATIMH
#undef CERNLIB_TCGEN_DATIMH
#endif
