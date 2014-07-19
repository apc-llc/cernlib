/*
 * $Id: log.h,v 1.1.1.1 1996/03/08 15:44:25 mclareni Exp $
 *
 * $Log: log.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:25  mclareni
 * Cspack
 *
 */
/*
 * @(#)log.h    1.5 7/29/94 CERN CN-PDP/CS F. Hemmer
 */
 
/*
 * Copyright (C) 1990-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
/* log.h        generalized logging facilities                          */
 
#ifndef _LOG_H_INCLUDED_
#define _LOG_H_INCLUDED_
 
#define LOG_NOLOG       -1      /* Don't log                            */
 
#if defined(vax) && defined(vms) && !defined(__DECC)
/*
 * VAX/VMS C compiler 3.1 has a nasty bug around function pointer
 * assignements, when linked with libraries. Therefore we must
 * disable this code for vms. As a result, there is a loss in
 * functionality, because we cannot switch anymore between functions
 */
#define log     logit
#else
extern  void (*logfunc)();      /* logging function to use              */
#define log (*logfunc)          /* logging function name                */
#endif /* vax && vms && !__DECC */
 
#if !defined(vms) && !defined(VM)
#include <syslog.h>             /* system logger definitions            */
#else
#define LOG_EMERG       0       /* system is unusable                   */
#define LOG_ALERT       1       /* action must be taken immediately     */
#define LOG_CRIT        2       /* critical conditions                  */
#define LOG_ERR         3       /* error conditions                     */
#define LOG_WARNING     4       /* warning conditions                   */
#define LOG_NOTICE      5       /* normal but signification condition   */
#define LOG_INFO        6       /* informational                        */
#define LOG_DEBUG       7       /* debug-level messages                 */
#endif /* !vms && !VM */
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
#endif /* _LOG_H_INCLUDED_ */
