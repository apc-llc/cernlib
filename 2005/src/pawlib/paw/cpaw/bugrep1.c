/*
 * $Id: bugrep1.c,v 1.2 1997/11/05 11:32:58 mclareni Exp $
 *
 * $Log: bugrep1.c,v $
 * Revision 1.2  1997/11/05 11:32:58  mclareni
 * Correct interface for NT
 *
 * Revision 1.1.1.1  1996/03/01 11:38:58  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
#if !defined(CERNLIB_BUGREPORT)
/*CMZ :  2.05/17 30/08/94  16.20.02  by  Maarten Ballintijn*/
/*-- Author :    Rene Brun   26/08/94*/
/*
 *  bugreport.c  -  dummy implementation of the bugreport command
 *
 */

/* arange external symbol ... */
#if defined(CERNLIB_QX_SC)
#define         bugreprt        bugreprt_
#endif
#if defined(CERNLIB_IBM)
#pragma         linkage(bugreprt,FORTRAN)
#endif
#if defined(CERNLIB_QXCAPT)
#define         bugreprt  type_of_call BUGREPRT
#endif

#if defined(CERNLIB_IBM)
static
#endif
char *br_tmpl_bugrep[] = { 0 };


#if defined(CERNLIB_IBM)
static
#endif
char *br_tmpl_comment[] = { 0 };


void
bugreprt()
{
        printf( "BUGREPORT: not yet implemented\n" );
}

char *
buginfo()
{
        return "No version info available\n";
}
#endif
