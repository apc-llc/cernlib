/*
 * $Id: sysreq.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: sysreq.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
/*
 * @(#)sysreq.h 1.4 9/2/91 CERN CN-SW/DC Frederic Hemmer
 */
 
/* sysreq.h     Network interface to the SYSREQ communication system    */
 
/*
 * Most of this stuff should be moved in the source directory
 * user only need the library definitions
 */
 
#ifndef _SYSREQ_H_INCLUDED_
#define  _SYSREQ_H_INCLUDED_
 
#ifndef vms
#include <osdep.h>              /* Operating system dependencies        */
#else
#include "osdep.h"              /* Operating system dependencies        */
#endif /* vms */
 
/*
 * Important note: the SYSREQ request/reply blocks do NOT correspond
 * to D.J. Rigby original note. Currently the only way to get up to
 * date information is to look at source code. An example of this is
 * the SYSREQ ASSEMBLE Fortran interface to SYSREQ, using diagnose X'140'
 */
 
typedef struct  {       /* SYSREQ request header                        */
        WORD            length;         /* total length, excluded this  */
        WORD            maxrep;         /* maximum reply length         */
        LONG            msgid;          /* unique message id            */
        char            altid[8];       /* alternate user id            */
        LONG            atwtype;        /* application dependent        */
        LONG            checksum;       /* transmission checksum        */
        char            account[8];     /* Reserved for acnt****        */
        char            spare[8];       /* Reserved                     */
        char            usernm[8];      /* username of requestor        */
        char            sysnm[8];       /* requestor's system name      */
        char            service[8];     /* service name                 */
} reqhd;
 
typedef struct {        /* SYSREQ reply header                          */
        WORD            length;         /* total length, excluded this  */
        WORD            rcode;          /* SYSREQ return code           */
        LONG        msgid;          /* msgid from request buffer    */
} rephd;
 
/*
 * Request types codes
 */
 
#define SYSREQTYPE      1               /* Normal SYSREQ request        */
#define PROBETYPE       2               /* Probe the server             */
#define STATTYPE        3               /* Get statistics info          */
#define SHUTTYPE        4               /* Shutdown request             */
#define NULLTYPE        5               /* The NULL request             */
 
/*
 * Error codes          (OLD STYLE, now moved in serrno.h)
 */
 
#define CONFNFOUND      1001            /* sysreqrc not found           */
#define NOSUCHHOST      1002            /* Host not found               */
#define BADVERSION      1003            /* Version id mismatch          */
#define UBUF2SMALL      1004            /* User buffer too small        */
#define NOSUCHSERV      1005            /* Service unknown              */
#define PERMDENIED      1006            /* Permission denied            */
#define INVREQTYPE      1007            /* Invalid request type         */
#define INVADDRFAM      1008            /* Unsupported addressing fam.  */
#define INVCOMTYPE      1009            /* Unsupported commun. type     */
#define SYSCOMMERR      1010            /* SYSREQ communication error   */
 
/*
 *      System routines
 */
 
#ifndef VM
extern char     *malloc();              /* Memory allocation            */
#endif  /* VM */
extern char     *getenv();              /* Get environment info         */
 
/*
 *      Data internalization/externalization
 */
 
extern char     *RecvStr();             /* Receive a string from network*/
extern int      SendStr();              /* Send a string to network     */
extern int      RecvWord();             /* Receive a word from network  */
extern int      SendWord();             /* Send a word to network       */
extern int      RecvLong();             /* Receive a long from network  */
extern int      SendLong();             /* Send a long from network     */
 
#endif /* _SYSREQ_H_INCLUDED_ */
