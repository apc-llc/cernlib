/*
 * $Id: crypt.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: crypt.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
/*
 * Copyright (C) 1989,1990 by Frederic Hemmer           CERN-CN/SW/DB
 * All rights reserved                                  CERN-CN/SW/DC
 */
 
#if defined(CERNLIB_QMALPH)
#define strlen  DECC$STRLEN
#define toupper DECC$TOUPPER
#endif
 
#ifndef lint
static char     *ident="crypt.c         1.2 Frederic Hemmer CERN-DD/CN";
#endif /* lint */
 
/* vmscrypt, crypt.c - Password encryption    */
 
/*         */
/* Release Date Author  Description   */
/* -------    --------- ------------  --------------------------------- */
/*  1.0       13-Jul-89 F. Hemmer     Initial writing                   */
/*  1.1       31-Jul-90 F. Hemmer     VMS 5.4 support                   */
/*  1.2       10-Jan-90 F. Hemmer     Multi algorithm support           */
 
#include <errno.h>              /* Error numbers and codes              */
#include <ctype.h>              /* Character type defs and macros       */
#include <ssdef.h>              /* System services definitions          */
#include <uaidef.h>             /* User authorization definitions       */
#include "sysreq/pwd_vms.h"     /* Password structure definitions       */
 
#include <stdio.h>
 
#ifdef PREVMS54
extern int lgi$hpwd();  /* VMS's hash password routine  */
#endif /* PREVMS54 */
 
static char crypt_psw[9] = /* VMS specific, 8 for hashed psw, + '\0'*/
  "        ";
 
typedef struct {
 int len;
 char *buf;
} dsc;
 
char *
vmscrypt(user, psw, encrypt, salt)
char *user, *psw;
short encrypt, salt;
{
 dsc user_dsc, psw_dsc;
#ifdef PREVMS54
 dsc c_psw_dsc;
#endif /* PREVMS54 */
 struct  passwd  *pw;
 int status, i;
 
/* Note: we should check account flags to see if it is still valid */
 
/* Convert to upper-case */
 for (i=0;i<strlen(user);i++) {
  user[i] = toupper(user[i]);
 }
 for (i=0;i<strlen(psw);i++) {
  psw[i] = toupper(psw[i]);
 }
 
 user_dsc.len = strlen(user);
 user_dsc.buf = user;
 psw_dsc.len = strlen(psw);
 psw_dsc.buf = psw;
#ifdef PREVMS54
 c_psw_dsc.len = strlen(crypt_psw);
 c_psw_dsc.buf = crypt_psw;
#endif
 
#ifndef PREVMS54
 if ((pw = getpwnam(user)) == 0)     {
  return((char *)0);
 }
 status = sys$hash_password(&psw_dsc, pw->pw_encrypt,
       salt, &user_dsc, crypt_psw);
#else
 status = lgi$hpwd(&c_psw_dsc, &psw_dsc, encrypt, salt, &user_dsc);
#endif /* PREVMS54 */
 if (!(status & 0x1)) {
  vaxc$errno = status;
  errno = EVMSERR;
  crypt_psw[0] = '\0';
  return((char *) 0);
 }
 crypt_psw[9]='\0';
 return (crypt_psw);
}
 
/*
 *  NOTE: crypt() assumes getpwnam() (or similar) has been called
 *        prior crypt to retrieve the pwd data structure. vmscrypt()
 *        should be used otherwise.
 */
 
char *
crypt(key, salt)
char *key, *salt;
{
 short encrypt, r_salt;
 char *user;
 
 encrypt = *(char *)(salt-1);
 r_salt = (short) *(char *)(salt-3);
/* To be continued... */
 return (0);
}
