/*
 * $Id: ishftl.c,v 1.1.1.1 1996/02/15 17:50:02 mclareni Exp $
 *
 * $Log: ishftl.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:02  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)

#include "kerngen/qf_f2c.h"

integer ishftl_(izw, nzb)
integer *izw, *nzb;
{
    return (*izw << *nzb);
} /* ishftl_ */

#endif
