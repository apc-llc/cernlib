/*
 * $Id: trace.c,v 1.2 2003/09/02 12:47:16 mclareni Exp $
 *
 * $Log: trace.c,v $
 * Revision 1.2  2003/09/02 12:47:16  mclareni
 * Mods to replace varargs.h (removed from gcc 3.3) with stdarg.h for Linux gcc
 * 3.3.
 *
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] =
"@(#)trace.c    1.5 7/13/94 CERN CN-SW/DC Frederic Hemmer";
#endif /* not lint */
 
/* trace.c      General tracing facility                                */
 
#include <stdio.h>              /* standard input/output definitions    */
#if !defined(LINUX)
#include <varargs.h>            /* variable argument list definitions   */
#else
#include <stdarg.h>
#endif  /* LINUX */
#if defined(vms)
#include <stdlib.h>             /* General utility definitions          */
#endif /* vms */
 
#if defined(vms) && defined(DECC)
#define __alpha
#endif
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define atoi    DECC$ATOI
#define fflush  DECC$FFLUSH
#define getenv  DECC$GETENV
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
static int      trace_initialized = 0;  /* trace initialized flag       */
static int      trace_level = 0;        /* dynamic trace level          */
static int      indent_level = 0;       /* dynamic indentation level    */
 
#if !defined(vms)
extern  char* getenv();         /* get environment variable             */
#endif /* vms */
 
/*VARARGS*/
#if !defined(LINUX) 
void print_trace(va_alist)    va_dcl
#else
void print_trace(int level, ...)
#endif  /* LINUX */
{
        va_list args;           /* arguments                            */
        register char *label;   /* leading label                        */
        register char *format;  /* print format string                  */
        register int i;         /* general purpose index                */
#if !defined(LINUX) 
        register int level;     /* trace level                          */
        va_start(args);         /* initialize to beginning of list      */
        level = va_arg(args, int);
#else
        va_start(args, level);
#endif  /* LINUX */
        if (trace_level < level) return;
        label = va_arg(args, char *);
        for (i=0; i< level+indent_level ; i++)       {
                fprintf(stdout," "); /* indentation                    */
        }
        fprintf(stdout,"%s: ", label);
        format = va_arg(args, char *);
        (void) vfprintf(stdout, format, args);
        fprintf(stdout,"\n");
        va_end(args);
        fflush(stdout);
}
 
void    init_trace(name)        /* initialize trace level               */
char    *name;                  /* environment variable name            */
{
        register char    *p;    /* general purpose char. string pointer */
 
        if (!trace_initialized) {
                if ((p = getenv(name)) != NULL)       {
                        if (atoi(p) != 0)       {
                                trace_level = atoi(p);
                                print_trace(0, "    **** ", "trace level set \
to %d", trace_level);
                        }
                }
                trace_initialized++;
        }
        indent_level++;
}
 
void    end_trace()             /* end trace level                      */
{
        if (indent_level > 0) indent_level--;
}
