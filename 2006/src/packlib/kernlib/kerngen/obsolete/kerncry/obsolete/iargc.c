/*
 * $Id: iargc.c,v 1.1.1.1 1996/02/15 17:52:45 mclareni Exp $
 *
 * $Log: iargc.c,v $
 * Revision 1.1.1.1  1996/02/15 17:52:45  mclareni
 * Kernlib
 *
 */
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
extern int _argc;
IARGC()
{
      int i = _argc-1;
      return(i);
}
#ifdef CERNLIB_CRYUNI_IARGC
#undef CERNLIB_CRYUNI_IARGC
#endif
