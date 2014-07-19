/*
 * $Id: systems.c,v 1.4 1998/08/25 12:45:56 mclareni Exp $
 *
 * $Log: systems.c,v $
 * Revision 1.4  1998/08/25 12:45:56  mclareni
 * QMGLIBC changes for Linux RH51
 *
 * Revision 1.3  1997/09/02 14:24:26  mclareni
 * WINNT correction
 *
 * Revision 1.2  1997/02/04 17:34:10  mclareni
 * Merge Winnt and 97a versions
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:29:18  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/02/15 17:47:53  mclareni
 * Kernlib
 *
 */
#include "kernbit/pilot.h"
/* Small mods V.Fine 97/08/16
   for Windows NT
 */
 
/* Small mods G.Folger 94/09/23
 *     shorten lines to less than 80 chars
 *     add names defined in ANSI mode
 *     HPUX, IBMRT have _ for fotran too.
 *     shift_systems changed to void, all uses and return's are for void
 * /
 
/*
 * Copyright (C) 1993 by CERN/CN/SW/DC
 * All rights reserved
 */
 
#ifdef __hpux   /* to compile with -Aa option, ie, ansi */
#define  _HPUX_SOURCE
#endif

#ifdef __apollo
#define _INCLUDE_BSD_SOURCE
#endif /* __apollo */


#include <stdio.h>
#include <errno.h>
#if !defined(_OSK) && !defined(CERNLIB_WINNT)     /* OS 9 */
#  include <sys/wait.h>
#endif
#include <string.h>    /*  needed GF */

#ifdef _OSK   /* OS9 has no EINVAL */
#define EINVAL EOS_ILLARG
#endif


#ifndef lint
static char sccsid[] =
          "@(#)systems.c	1.2 6/8/94   CERN CN-SW/DC Felix Hassine";
#endif /* not lint */
 
#ifndef WIN32
  extern int errno;
#else
#  ifndef popen
#    define popen _popen
#  endif
#  ifndef pclose
#    define pclose _pclose
#  endif
#  ifndef sys_errlist
#    define sys_errlist _sys_errlist
#  endif
#endif

#if !defined(CERNLIB_QMGLIBC)
extern char     *sys_errlist[] ;        /* System error list            */
#endif
 
/*
 * The following function executes with popen()
 * the command specified by the string command
 * and stores the buflen first bytes of the stdout output
 * stream into buf. Stderr is not redirected nor closed.
 *
 * l	 : number of lines of the output. A line is assumed to
 * be defined by the lexical expression: ^.*\n
 * chars : Number of characters stored into buf
 * rc	 : returned code of the command
 * ovflw : Indicates if buf provided was large enough (0) or not (1).
 */
 
void shift_systems( command, buf, buflen, l, chars , rc, ovflw )
 
char    *command;
char    *buf;
int     buflen ;
int     *l;
int     *chars;
int     *rc;
int     *ovflw;
{
	FILE *ps ;
	int rcode ;
	char ebuf[1024];
	int i,count;
	char *b,*cp;
 
        if ( !strlen(command) ) {
                fprintf(stderr,"systems(): Command badly specified.\n");
                *rc= -EINVAL;
                *chars=0        ;
                *l=0        ;
                return;
        }
 
        if ( buflen <= 0) {
                *rc= -EINVAL;
                *chars=0        ;
                *l=0        ;
                return;
        }
 
#if defined(hpux) || defined(__hpux)
	cp = strrstr(command, "\\0") ;
	if ( cp != NULL  && strcspn(cp + 2, " ") == 0) {
		*cp='\0' ;
	}
#endif /* hpux */

	if ( (ps=(FILE *)popen(command,"r"))==NULL ) {
		fprintf(stderr,"systems(): popen(): %s\n",sys_errlist[errno] );
		*rc= -errno;
		*chars=0        ;
                *l=0        ;
                return;
	}
	
	rcode = fread(buf, 1, buflen , ps );
	if ( rcode < 0 ) {
		fprintf(stderr,"systems(): pipe fread(): %s\n",
                                                       sys_errlist[errno] );
		buf[0]='\n';
		*rc= -errno;
                *chars=0        ;
                *l=0        ;
                return;
	}
	else {
		i = rcode ;
		buf[i]='\0' ;
		rcode = strlen(buf) ;
#if defined(sgi) || defined(__sgi)
		if ( buflen > rcode)
			memset ( buf + rcode, ' ', buflen - rcode);
#endif
	}
 
	*chars = rcode ;
 
	/*
	 * Empty pipe before pclose !
	 */
	while (  fread(ebuf, 1,  1024, ps) > 0 ) ;
	*ovflw = (rcode >= buflen ? 1:0 ) ;
 
	rcode = pclose ( ps );
#ifndef WIN32
	*rc = WEXITSTATUS ( rcode ) ;
#else
	*rc =  rcode ;
#endif
	
	/*
	 * How many lines ?
	 */
	i = 0; 
	b = buf ;
	count= 0;
	while (i < strlen(buf) && (b= (char * )strchr( buf + i,'\n')) != NULL){
			i=(int) ( b-buf)+ 1;
			count ++ ;
	}
	*l = count;
 
}
 
/*
 * FORTRAN wrapper
 */
 
#if defined(sun) || defined(__sun) \
|| defined(sgi) || defined(__sgi) \
|| ( defined(ultrix) && defined (mips) ) \
|| ( defined(__ultrix) && defined (__mips) ) \
|| defined(hpux)  || defined(__hpux) \
|| ( defined(__osf__) && defined(__alpha) ) \
|| defined(_IBMR2) \
|| defined(__convexc__) \
|| defined(_OSK) \
|| defined(__linux) || defined(__FreeBSD__)

  void systems_( command, buf, buflen, l, chars, rc, ovflw )

#elif defined(apollo) || defined(__apollo) || defined(CERNLIB_QXNO_SC)

  void systems( command, buf, buflen, l, chars, rc, ovflw )

#elif defined(CERNLIB_MSSTDCALL) 

     void _stdcall SYSTEMS(command,comlen, buf, buflent, buflen, l, chars, rc, ovflw )
     int comlen,buflent;

#else
/*  if you end up here, then you must add your system.*/
    unported systems( command, buf, buflen, l, chars, rc, ovflw ,comlen, buflent)
#endif
 
char    *command;
char    *buf;
int     *buflen ;
int     *l;
int     *chars;
int     *rc;
int     *ovflw;
{
	(void) shift_systems ( command, buf, *buflen, l, chars, rc, ovflw );
	return ;
 
}
