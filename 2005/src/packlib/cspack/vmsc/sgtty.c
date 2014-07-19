/*
 * $Id: sgtty.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: sgtty.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
/* sgtty.c - set and get terminal characteristics   */
 
/*
 * Copyright (C) 1988, 1989 by Frederic Hemmer
 * All rights reserved
 */
 
/*              was QMALPH */
#if defined(CERNLIB_QCDEC)
#define isatty  DECC$ISATTY
#define strlen  DECC$STRLEN
#define getname DECC$GETNAME
#endif
 
#include <unixio.h>  /* Unix (tm) style IO defs and macros */
#include <errno.h>  /* Error numbers   */
#include "sysreq/sgtty.h"  /* stty, gtty structures  */
#include <ssdef.h>  /* System services definitions  */
#include <iodef.h>  /* IO definitions   */
 
static struct {
 int  len;
 char *buf;
} devdsc = {0, ""};
 
typedef struct {
 short  status;  /* I/O status    */
 char ts;  /* transmit speed   */
 char rs;  /* receive speed (only if != ts  */
 char CR_cnt;  /* CR fill count   */
 char LF_cnt;  /* LF fill count   */
 char pf;  /* parity flags    */
 char unused;  /* unused    */
} SM_IOSB;  /* Set/Sense Mode/Char I/O status block  */
 
typedef struct {
 char class;  /* class    */
 char type;  /* type     */
 short pw;  /* page width    */
 int btc;  /* Basic Terminal Characteristics + pl */
} C_BUF;   /* Basic terminal characteristics buffer*/
 
static int AllocChan(fd) /* Allocate I/O channel to fd descr. */
int fd;
{
 char fs[256]; /* file specification   */
 int status, ch;
 
 if (getname(fd, fs, 1) == 0) {
  return(-1);
 }
 devdsc.len = strlen(fs);
 devdsc.buf = fs;
 if ((status = sys$assign (&devdsc, &ch, 0, 0)) != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = status;
  return(-1);
 }
 return(ch);
}
 
int gtty(fd, buf)  /* Get tty characteristics  */
int fd;
struct sgttyb *buf;
{
 SM_IOSB sm_iosb; /* Sense mode I/O status block  */
 C_BUF t_chars; /* Characteristics buffer  */
 int status,s;
 
 status = isatty(fd); /* Check fd is a tty   */
 if (status == 0) {
  errno = ENOTTY;
  return(-1);
 }
 if (status == -1) {
  return(-1);
 }
 if ((s = AllocChan(fd)) == -1) {
  return(-1);
 }
/*
 * Get terminal characteristics
 */
 if ((status = sys$qiow(0, s, IO$_SENSEMODE, &sm_iosb, 0, 0,
   &t_chars, sizeof(t_chars), 0, 0, 0, 0)) != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = status;
  return(-1);
 }
 if (sm_iosb.status != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = sm_iosb.status;
  return(-1);
 }
/*
 * Copy information back
 */
 buf->sg_pwidth = t_chars.pw;
 buf->sg_plength = (t_chars.btc & 0xff000000) >> (8*3);
 buf->sg_class = t_chars.class;
 buf->sg_type = t_chars.type;
 buf->sg_ispeed = sm_iosb.rs;
 buf->sg_ospeed = sm_iosb.ts;
 buf->sg_parity = sm_iosb.pf;
 buf->sg_crcnt = sm_iosb.CR_cnt;
 buf->sg_lfcnt = sm_iosb.LF_cnt;
 buf->sg_flags =  t_chars.btc & 0X00FFFFFF;
 if ((status = sys$dassgn(s)) != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = status;
  return(-1);
 }
 return(0);
}
 
int
stty (fd, buf)
int fd;
struct sgttyb *buf;
{
 SM_IOSB sm_iosb; /* Sense mode I/O status block  */
 C_BUF t_chars; /* Characteristics buffer  */
 char speed[2],fill[2]; /* CR/LF fill count   */
 int status,s;
 
 status = isatty(fd);
 if (status == 0) {
  errno = ENOTTY;
  return(-1);
 }
 if (status == -1) {
  return(-1);
 }
/*
 * Copy information back
 */
 t_chars.pw = buf->sg_pwidth;
 t_chars.btc = (((int) buf->sg_plength) << (8*3)) |
        (buf->sg_flags & (0x00ffffff));
 t_chars.class = buf->sg_class;
 t_chars.type = buf->sg_type;
 speed[0] = buf->sg_ospeed;
 speed[1] = buf->sg_ispeed;
 fill[0] = buf->sg_crcnt;
 fill[1] = buf->sg_lfcnt;
 if ((s = AllocChan(fd)) == -1) {
  return(-1);
 }
/*
 * Set terminal characteristics
 */
 if ((status = sys$qiow(0, s, IO$_SETMODE, &sm_iosb, 0, 0,
   &t_chars, sizeof(t_chars),
   *(unsigned short *) speed,
   *(unsigned short *) fill,
   0, 0 )) != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = status;
  return(-1);
 }
 if (sm_iosb.status != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = sm_iosb.status;
  return(-1);
 }
 if ((status = sys$dassgn(s)) != SS$_NORMAL) {
  errno = EVMSERR;
  vaxc$errno = status;
  return(-1);
 }
 return(0);
}
