/*
 * $Id: faexit.c,v 1.3 1997/09/02 12:56:52 mclareni Exp $
 *
 * $Log: faexit.c,v $
 * Revision 1.3  1997/09/02 12:56:52  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/03/14 17:01:23  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:27:23  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
 * Fatmen
 *
 */
#include "fatmen/pilot.h"
#if defined(CERNLIB_QXCAPT)
#ifndef CERNLIB_MSSTDCALL
  void FAEXIT(icode)
#else
  void _stdcall FAEXIT(icode)
#endif
#endif
#if defined(CERNLIB_QXNO_SC)
void faexit(icode)
#endif
#if defined(CERNLIB_QX_SC)
void faexit_(icode)
#endif
int *icode;
{
   exit(*icode);
}
