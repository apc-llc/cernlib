/*
 * $Id: sleep.c,v 1.2 1997/09/02 08:46:20 mclareni Exp $
 *
 * $Log: sleep.c,v $
 * Revision 1.2  1997/09/02 08:46:20  mclareni
 * WINNT mods, mostly cpp defines
 *
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
#if defined(CERNLIB_TCPSOCK) && defined(CERNLIB_IBMVM)
# include <stdlib.h>
# include <stdio.h>
sleep(num)
int num;
{
    char buf[50];
    sprintf(buf, "CP SLEEP %d SEC", num);
    system(buf);
    return(0);
}
#elif defined(CERNLIB_WINNT)
#include <windows.h>
 void sleep(int num){ Sleep(num*1000);}
#endif
