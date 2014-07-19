/*
 * $Id: serrno.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: serrno.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * Copyright (C) 1990,1991 by CERN/CN/SW/DC
 * All rights reserved
 */
 
/*
 * @(#)serrno.h 1.14 3/18/94  CERN CN-SW/DC Frederic Hemmer
 */
 
/* serrno.h     Special error numbers - not in errno.h                  */
 
#ifndef _SERRNO_H_INCLUDED_
#define _SERRNO_H_INCLUDED_
 
#define SEBASEOFF       1000            /* Base offset for special err. */
 
#define SENOERR         1000            /* No error                     */
#define SENOSHOST       1001            /* Host unknown                 */
#define SENOSSERV       1002            /* Service unknown              */
#define SENOTRFILE      1003            /* Not a remote file            */
#define SETIMEDOUT      1004            /* Has timed out                */
#define SEBADFFORM      1005            /* Bad fortran format specifier */
#define SEBADFOPT       1006            /* Bad fortran option specifier */
#define SEINCFOPT       1007            /* Incompatible fortran options */
#define SEFNAM2LONG     1008            /* File name too long           */
#define SENOCONFIG      1009            /* Can't open configuration file*/
#define SEBADVERSION    1010            /* Version ID mismatch          */
#define SEUBUF2SMALL    1011            /* User buffer too small        */
#define SEMSGINVRNO     1012            /* Invalid reply number         */
#define SEUMSG2LONG     1013            /* User message too long        */
#define SEENTRYNFND     1014            /* Entry not found              */
#define SEINTERNAL      1015            /* Internal error               */
#define SECONNDROP      1016            /* Connection closed by rem. end*/
#define SEMSGU2REP      1017            /* msg daemon unable to reply   */
#define SEMSGSYERR      1018            /* msg daemon system error      */
#define SENOPERM        1019            /* Permission denied            */
#define SEBADIFNAM      1020            /* Can't get interface name     */
#define SESRQCOMER      1021            /* SYSREQ communication error   */
#define SENOMAPDB       1022            /* Can't open mapping database" */
#define SENOMAPFND      1023            /* No user mapping              */
#define SENOVMSUSR      1024            /* Unable to set VMS user       */
#define SENORCODE       1025            /* RFIO communication error     */
#define SEHOSTREFUSED   1026            /* RFIO rejected connect attempt*/
 
#define SEMAXERR        1026            /* Maximum error number         */
 
#define SERRNO  (serrno - SEBASEOFF)    /* User convenience             */
 
extern  int     serrno;                 /* Global error number          */
extern  char    *sys_serrlist[];        /* Error text array             */
extern  char    *sstrerror();           /* Get error text string        */
 
#endif /* _SERRNO_H_INCLUDED_ */
