/*
 * $Id: iand.c,v 1.1.1.1 1996/02/15 17:50:04 mclareni Exp $
 *
 * $Log: iand.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:04  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
int iand_(ia,ib)
int *ia, *ib;
  { return *ia & *ib;}
int ior_(ia,ib)
int *ia, *ib;
  { return *ia | *ib;}
int ieor_(ia,ib)
int *ia, *ib;
  { return *ia ^ *ib;}
#endif
