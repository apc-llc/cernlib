/*
 * $Id: ishft.c,v 1.1.1.1 1996/02/15 17:50:01 mclareni Exp $
 *
 * $Log: ishft.c,v $
 * Revision 1.1.1.1  1996/02/15 17:50:01  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(CERNLIB_QF_F2C)
unsigned ishft_(izw, nzb)
unsigned *izw;
int      *nzb;
{
    if (*nzb > 0)       {return *izw << *nzb;}
    else if (*nzb == 0) {return *izw;}
    else                {return  (unsigned) (*izw) >> -(*nzb);}
} /* ishft_ */
#endif
