/*
 * $Id: timelcc.c,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
 *
 * $Log: timelcc.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
/*>    ROUTINE TIMELCC
*/
#include <sys/vlimit.h>
#include <sys/time.h>
#include <sys/resource.h>

long long int timelcc_(left)
double *left ;

{
  struct rusage *ru ;
  ru = (struct rusage *) malloc( sizeof(struct rusage) ) ;
  getrusage(RUSAGE_SELF,ru) ;
  *left = (double) vlimit ( LIM_CPU , -1 )
          -(double) ru->ru_utime.tv_sec
          -(double)ru->ru_utime.tv_usec/1000000.0
          -(double) ru->ru_stime.tv_sec
          -(double)ru->ru_stime.tv_usec/1000000.0;
 return(*left);
 }
/*> END <----------------------------------------------------------*/
