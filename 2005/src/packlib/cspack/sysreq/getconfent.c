/*
 * $Id: getconfent.c,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
 *
 * $Log: getconfent.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1991-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] =  "@(#)getconfent.c       1.10 7/13/94 CERN CN-PDP/CS F. \
Hemmer";
#endif /* not lint */
 
/* getconfent.c - get configuration entry                               */
 
#ifndef PATH_CONFIG
#if !defined(vms)
#define PATH_CONFIG "/etc/shift.conf"
#else /* vms */
#define PATH_CONFIG "SYS$MANAGER:shift.conf"
#endif /* vms */
#endif /* PATH_CONFIG */
 
#include <stdio.h>                      /* Standard input/output        */
#include <ctype.h>                      /* Character manipulation       */
#include <string.h>                     /* String handling functions    */
#ifndef vms
#include <serrno.h>                     /* Special Error numbers        */
#else
#include "serrno.h"                     /* Special Error numbers        */
#endif /* vms */
 
#if defined(vms) && defined(DECC)
#define __alpha
#endif
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define fclose  DECC$FCLOSE
#define fgets   DECC$FGETS
#define fopen   DECC$FOPEN
#define getenv  DECC$GETENV
#define strcmp  DECC$STRCMP
#define strcpy  DECC$STRCPY
#define strtok  DECC$STRTOK
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
static char     in_line[BUFSIZ+1];
 
extern char* getenv();
static  char    value[BUFSIZ];
 
char *
getconfent(category, name, flags)
char    *category;
char    *name;
int     flags;
{
        char    *filename=PATH_CONFIG;
        FILE    *fp;
        char    *p, *cp;
        int     found = 0;
        char    *separator;
 
        if ((p = getenv("PATH_CONFIG")) != NULL)       {
                filename=p;
        }
 
#ifdef DEBUG
        log(LOG_DEBUG ,"getconfent(%s,%s,%d)\n",
            category, name, flags);
        log(LOG_DEBUG ,"getconfent: opening %s\n",filename);
#endif /* DEBUG */
        if ((fp = fopen(filename,"r")) == NULL)       {
                serrno = SENOCONFIG;
                return (NULL);
        }
 
        for (;;)        {
                p = fgets(in_line, BUFSIZ, fp);
                if (p == NULL)  {
                        break;
                }
#ifdef DEBUG
                log(LOG_DEBUG ,"getconfent: read <%s>",p);
#endif /* DEBUG */
                if (*p == '#') continue;
#ifdef DEBUG
                log(LOG_DEBUG ,"getconfent: processing <%s>",p);
#endif /* DEBUG */
                if ((cp = strtok(p," \t")) == NULL) continue; /* empty line */
                if (*cp == '#') continue; /* comment */
                if (strcmp(cp, category) == 0) { /* a category match     */
#ifdef DEBUG
                        log(LOG_DEBUG ,"getconfent: category <%s>\n", cp);
#endif /* DEBUG */
                        if ((cp = strtok(NULL," \t")) == NULL) continue;
                        if (*cp == '#') continue; /* comment, invalid */
                        if (strcmp(cp, name) == 0) {    /* A match */
#ifdef DEBUG
                                log(LOG_DEBUG ,"getconfent: category <%s>, \
processing<%s>\n",
                                    category, cp);
#endif /* DEBUG */
                                if (flags)     {
                                        /* Don't tokenize next arg */
                                        separator = "#\n";
                                } else {
                                        /* Don't tokenize next arg */
                                        separator = "#\t \n";
                                }
                                if ((cp = strtok(NULL, separator)) == NULL) 
continue; /* invalid format */
                                if (*cp == '#') continue; /* comment, invalid */
#ifdef DEBUG
                                log(LOG_DEBUG ,"getconfent: category <%s>, \
got<%s>\n",
                                    category, cp);
#endif /* DEBUG */
                                found++;
                                strcpy(value,cp);
                                break;
                        }
                        else {
                                continue;
                        }
                }
                else {
                        continue;
                }
        }
#ifdef DEBUG
        log(LOG_DEBUG ,"getconfent: closing %s\n",filename);
#endif /* DEBUG */
        if (fclose(fp)) return (NULL);
        if (!found)     {
                return(NULL);
        }
        else {
                return(value);
        }
}
