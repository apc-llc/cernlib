/*
 * $Id: sysreq.c,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
 *
 * $Log: sysreq.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] =
"@(#)sysreq.c   1.10 7/13/94 CERN CN-SW/DC Frederic Hemmer";
#endif /* not lint */
 
/* sysreq.c       sysreq command using network communication            */
 
#include <stdio.h>              /* Standard Input/Output                */
#include <string.h>             /* String manipulation routines         */
#ifndef vms
#include <sysreq.h>             /* Sysreq interface definitions         */
#include <serrno.h>             /* Special error numbers                */
#else
#include "sysreq.h"             /* SYSREQ specific definitions          */
#include "serrno.h"             /* Special Error numbers                */
#endif /* vms */
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define exit    DECC$EXIT
#define strcat  DECC$STRCAT
#endif /* vms && vax && COMPILE_NOPREFIX */
 
#define RECSEP  '\n'            /* SYSREQ's record separator            */
#define RECLEN  80              /* SYSREQ's max record length           */
 
extern int sysreq();
 
static char *
nextcmd(s)                      /* Get next command to issue            */
char    *s;
{
        register int    i;
 
        for (i= strlen(s);i>=(int)strlen(s)-RECLEN;i--)    {
                if (s[i] == RECSEP) return(s+i+1);
        }
        return (NULL);
}
 
main(argc, argv)
int     argc;
char    **argv;
{
        char    cmd[512];
        char    reptxt[2048];
        int     status, len, cmdl, i;
        char    *s;
 
        if (argc <= 2)  {
                fprintf(stderr,"SERVICE NAME INVALID OR OMITTED\n");
                exit(3);
        }
 
        strcpy(cmd, argv[2]);
        for (i=3;i<argc;i++)  {
                strcat(cmd, " ");;strcat(cmd,argv[i]);
        }
 
reissue:
        cmdl = strlen(cmd);
        len = sizeof(reptxt);
        status = sysreq(argv[1], cmd, &cmdl, reptxt, &len);
        switch (status) {
                case    0:      fprintf(stdout,"%s\n",reptxt);
                                exit(0);
                case -SEUBUF2SMALL:   fprintf(stdout,"%s\n",reptxt);
                                fprintf(stdout,"Data truncated\n");
                                break;
                case    2:
                                if ((s = nextcmd(reptxt)) == NULL)      {
                                        fprintf(stderr,
                        "Internal error: Unable to get next command\n");
                                }
                                else    {
                                        /* Discard last reptxt record   */
                                        *(s-1) = '\0';
                                        fprintf(stdout,"%s\n",reptxt);
                                        strcpy(cmd,s);
                                        goto reissue;
                                }
                                break;
                default:
                                if (status < 0) {
                                        sperror("sysreq");
                                        status = -status;
                                        len = 0;
                                }
                                if (len >0) fprintf(stdout,"%s\n",reptxt);
        }
        fprintf(stderr,"RC(%d)\n",status);
        exit(1);
}
