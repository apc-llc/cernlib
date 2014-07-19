/*
 * $Id: initcibm.c,v 1.1.1.1 1996/02/15 17:47:41 mclareni Exp $
 *
 * $Log: initcibm.c,v $
 * Revision 1.1.1.1  1996/02/15 17:47:41  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "_kernbit/pilot.h"
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_QSAA))
#pragma csect   (CODE,"INITCC")
#pragma linkage (initc,FORTRAN)
void initc(ii)
int ii;
{
}
#endif
