/*
 * $Id: setpriv.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: setpriv.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
 
/*
 * Copyright (C) 1988 by Frederic Hemmer
 * All rigths reserved
 */
 
/* setpriv.c - set process privileges    */
 
#include <stdio.h>
 
int
setpriv (mask)
char *mask;
{
 int status;
 int allpriv[2]= { 0xffffffff, 0xffffffff };
 
 if (!(status = sys$setprv(0, allpriv, 1, 0)) & 1 ) return(status);
 if (!(status = sys$setprv(1, mask, 1, 0)) & 1 ) return(status);
 return(0);
}
