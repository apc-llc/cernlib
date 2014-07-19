/*
 * $Id: setuser.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: setuser.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
 
/*
 * Copyright (C) 1988 by Frederic Hemmer
 * All rights reserved
 */
 
/* setuser.c - Set user name      */
 
#include <stdio.h>
 
int
setuser(s)
char *s;
{
 int  rc;
 
 if (rc = ctl$s$t_username(s)) return(rc);
 if (rc = jib$setuname(s)) return(rc);
 return(0);
}
