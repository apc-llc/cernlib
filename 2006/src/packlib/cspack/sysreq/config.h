/*
 * $Id: config.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: config.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
/*
 * @(#)config.h 1.2 5/17/91 CERN CN-SW/DC Frederic Hemmer
 */
 
/* config.h     SYSREQ network interface configuration                  */
 
#ifndef _CONFIG_H_INCLUDED_
#define  _CONFIG_H_INCLUDED_
 
#define SYSREQ_NAME "sysreq"    /* Name of SYSREQ (in /etc/services)    */
#define SYSREQ_HOST "??????"    /* Name of SYSREQ default host          */
#define SYSREQ_PROTO "tcp"      /* Communication protocol               */
#define SYSREQ_PORT  4001       /* Well known port if not in services   */
#define SYSREQ_BACKLOG 5        /* Listen() backlog                     */
 
#define S_MAGIC 0x103090        /* Server magic number                  */
#define C_MAGIC 0x100360        /* Client magic number                  */
 
#define ACCEPT_RETRY_MAX   5    /* Number of accept() max retries       */
#define ACCEPT_RETRY_DELAY 5    /* Delay between accept() retries       */
 
#define SERVICESDB      1       /* Use /etc/services db if set          */
#ifdef VM
#define EBCDIC          1       /* ASCII/EBCDIC toggle, ASCII if 0      */
#else
#define EBCDIC          0       /* ASCII/EBCDIC toggle, ASCII if 0      */
#endif /* VM */
 
/*
 * SYSREQ configuration file
 */
 
#ifdef unix
#define SYSREQCONF      "/etc/sysreq.conf"
#endif /* unix */
#ifdef VM
#define SYSREQCONF      "PROFILE.SYSREQ"
#endif /* VM */
#ifdef vms
#define SYSREQCONF      "SYS$MANAGER:SYSREQ.CONF"
#endif /* vms */
 
#endif /* _CONFIG_H_INCLUDED_ */
