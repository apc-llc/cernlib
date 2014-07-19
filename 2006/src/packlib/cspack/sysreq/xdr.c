/*
 * $Id: xdr.c,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: xdr.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
#ifndef lint
static char sccsid[] = "@(#)xdr.c       1.16 7/13/94 CERN CN-SW/DC Frederic \
Hemmer";
#endif /* not lint */
 
/* xdr.c        eXternal Data Representation                            */
 
#if !defined(DEBUG)
#define DEBUG   0
#endif /* DEBUG */
#include <stdio.h>              /* standard Input/Output definitions    */
 
#ifdef VM
#ifdef SOCKETEMUL               /* no socket library available, use own */
#include "cms2unix.h"
#endif /* SOCKETEMUL */
#endif /* VM */
 
#ifndef vms
#include <config.h>             /* configuration parameters             */
#include <sysreq.h>             /* SYSREQ specific definitions          */
#include <sys/types.h>          /* standard data types                  */
#include <netinet/in.h>         /* internet data types                  */
#else
#include "config.h"             /* configuration parameters             */
#include "sysreq.h"             /* SYSREQ specific definitions          */
#include <types.h>              /* standard data types                  */
#include <in.h>                 /* internet data types                  */
#endif /* vms */
 
#include "net.h"                /* networking specifics                 */
 
#if DEBUG
#include <ctype.h>              /* Character type macros and defs.      */
#endif /* DEBUG */
 
#ifndef min
#define min(x, y)       (((x-y) > 0) ? y : x)
#endif /* min */
 
#ifndef VM
extern char *malloc();
#endif /* VM */
 
#ifdef VM
extern char ebcdictoascii[], asciitoebcdic[];
#endif /* VM */
 
#if defined(vms) && defined(DECC)
#define __alpha
#endif
 
#if defined(vms) && defined(__alpha) && defined(COMPILE_NOPREFIX)
#define free    DECC$FREE
#define htonl   DECC$HTONL
#define htons   DECC$HTONS
#define malloc  DECC$MALLOC
#define ntohl   DECC$NTOHL
#define ntohs   DECC$NTOHS
#endif /* vms && __alpha && COMPILE_NOPREFIX */
 
#if DEBUG
void                    /* Dump buffer's contents                       */
dump(buf, bufl)
char    *buf;
short   bufl;
{
        register int    i, j, n;
        register char   c;
 
#define WIDTH   17
        for (i=0;i<=bufl/WIDTH;i++)      {
                n = i*WIDTH;
                fprintf(stdout,"%.8x: ",buf+n);
                for(j=0; j<min(WIDTH,bufl-n); j++) {
                        c = *(buf+n+j);
                        if (isprint(c)) fprintf(stdout,"  %c ", c);
                        else fprintf(stdout,"<%.2x>",c);
                }
                fprintf(stdout,"\n");
        }
}
#endif /* DEBUG */
 
static char *           /* Externalize a character string               */
Xstr(buf, bufl)
char    *buf;
short   bufl;
{
#if EBCDIC
        CMXLATE(buf,ebcdictoascii,bufl);
#endif /* EBCDIC */
        return(buf);
}
 
static char *           /* Internalize a character string               */
Istr(buf, bufl)
char    *buf;
short   bufl;
{
#if EBCDIC
        CMXLATE(buf,asciitoebcdic,bufl);
#endif /* EBCDIC */
        return(buf);
}
 
int
SendWord(s, word)
int     s;
WORD    *word;
{
        WORD    buf;
 
        buf = htons(*word);
        if (netwrite(s ,((char *)&buf) + sizeof(WORD) - WORDSIZE, WORDSIZE)
                                != WORDSIZE)
                return (-1);
        return(WORDSIZE);
}
 
int
RecvWord(s, word)
int     s;
WORD    *word;
{
        WORD    buf;
 
        buf = 0;
        if (netread(s ,((char *)&buf) + sizeof(WORD) - WORDSIZE, WORDSIZE)
                        != WORDSIZE)
                return (-1);
        *word = ntohs(buf);
        return(WORDSIZE);
}
 
int
SendLong(s, longword)
int     s;
LONG *longword;
{
        LONG buf;
 
        buf = htonl(*longword);
        if (netwrite(s ,((char *)&buf) + sizeof(LONG) - LONGSIZE, LONGSIZE)
                        != LONGSIZE)
                return (-1);
        return(LONGSIZE);
}
 
int
RecvLong(s, longword)
int     s;
LONG    *longword;
{
        LONG    buf;
 
        buf = 0;
        if (netread(s ,((char *)&buf) + sizeof(LONG) - LONGSIZE, LONGSIZE)
                        != LONGSIZE)
                return (-1);
        *longword = ntohl(buf);
        return(LONGSIZE);
}
 
char *                  /* Receive a string from the net                */
RecvStr(s, size)
int     s;
int     *size;
{
        register int    len;
        int     s_len;
        char    *p1, *p2;
 
        if (RecvLong(s, &s_len) != LONGSIZE) return((char *)-1);
        if ((p1 = malloc(s_len+1)) == 0)   return((char *)-1);
        if (s_len != 0) {
                if ((len = netread(s, p1, s_len)) != s_len)        {
                        free(p1);
                        return((char *)-1);
                }
                p2 = Istr(p1, s_len+1);
                *size = s_len;
                p2[s_len] = '\0';
        }
        else    {
                *p1 = '\0';
                p2 = p1;
                *size = 0;
        }
        return(p2);
}
 
int
SendStr(s, buf, size)
int     s;
char    *buf;
int     size;
{
        int     len;
        char    *p;
 
        len = size;
        if (SendLong(s, &len) != LONGSIZE) return(-1);
        if (size !=0)   {
                p = Xstr(buf,size);
                if (netwrite(s, p, size) != size) return(-1);
        }
        return(size);
}
 
