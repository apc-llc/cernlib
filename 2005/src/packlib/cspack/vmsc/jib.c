/*
 * $Id: jib.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: jib.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
 
/* Copyright (C) 1988 by Frederic Hemmer  CERN-DD/SW/DB */
/*          */
/* jib.c Manipulate software job information block info  */
/*         */
/* Release Date Author  Description   */
/* -------    --------- ------------  --------------------------------- */
/*  1.0       13-Jul-89 F. Hemmer     Initial writing   */
 
#define PUBLIC
#define PRIVATE static
 
#include <stdio.h>
#include <ctype.h>
#include <ssdef.h>
#include "sysreq/pcbdef.h"
#include "sysreq/jibdef.h"
 
globalref char *CTL$GL_PCB;
 
 
typedef struct {
  unsigned char count;
  char  reserved[3];
  long  item;
  char *  buf;
  long  bufl;
} ITEM;
 
PRIVATE
unsigned long
__$getjib (item, buf, bufl)
unsigned int item;
char   *buf;
unsigned int bufl;
 {
 register char   *p;
 register char  *jib;
 register unsigned int i;
 
 jib = CTL$GL_PCB+PCB$L_JIB;
 p = * (int *)jib + item;
 for (i=0; i<bufl; i++) {
  *(buf+i) = *(p+i);
 }
 return (0);
}
 
PRIVATE
unsigned long
__$setjib (item, buf, bufl)
unsigned int item;
char   *buf;
unsigned int bufl;
 {
 register char   *p;
 register char  *jib;
 register unsigned int i;
 
 jib = CTL$GL_PCB+PCB$L_JIB;
 p = * (int *)jib + item;
 for (i=0; i<bufl; i++) {
  *(p+i) = *(buf+i);
 }
 return (0);
}
 
PUBLIC  int
jib$getuname(username)
char * username;
 {
 ITEM jibinfo;
 char tmpnam[JIB$S_USERNAME];
 int i, rc;
 
/* To avoid access violation in kernel mode, first try to write dest. */
 for (i=0;i<JIB$S_USERNAME;i++) {
  *(username+i) = 0Xff;
 }
 jibinfo.count = 3;
 jibinfo.item = JIB$T_USERNAME;
 jibinfo.buf = tmpnam;
 jibinfo.bufl = JIB$S_USERNAME;
 rc = sys$cmkrnl(__$getjib,&jibinfo);
 if (rc) return (rc);
 for (i=0;i<JIB$S_USERNAME;i++) {
  *(username+i) = tmpnam[i];
 }
 *(username+i) = 0;
 return(0);
}
 
PUBLIC  int
jib$getacct(account)
char * account;
 {
 ITEM jibinfo;
 char tmpnam[JIB$S_ACCOUNT];
 int i, rc;
 
/* To avoid access violation in kernel mode, first try to write dest. */
 for (i=0;i<JIB$S_ACCOUNT;i++) {
  *(account+i) = 0Xff;
 }
 jibinfo.count = 3;
 jibinfo.item = JIB$T_ACCOUNT;
 jibinfo.buf = tmpnam;
 jibinfo.bufl = JIB$S_ACCOUNT;
 rc = sys$cmkrnl(__$getjib,&jibinfo);
 if (rc) return (rc);
 for (i=0;i<JIB$S_ACCOUNT;i++) {
  *(account+i) = tmpnam[i];
 }
 *(account+i) = 0;
 return(0);
}
 
PUBLIC  int
jib$setuname(username)
char * username;
 {
 ITEM jibinfo;
 int length, rc, i;
 char uname0[JIB$S_USERNAME];
 
/* Initialize string to blank     */
 
 for (i=0;i<JIB$S_USERNAME;i++) {
  uname0[i] = ' ';
 }
 
 if ((length = strlen(username)) > JIB$S_USERNAME) {
  length = JIB$S_USERNAME;
 }
 
 for (i=0;i<length;i++) {
  uname0[i] = username[i];
 }
 
 jibinfo.count = 3;
 jibinfo.item = JIB$T_USERNAME;
 jibinfo.buf = uname0;
 jibinfo.bufl = JIB$S_USERNAME; /* Blank padding  */
 rc = sys$cmkrnl(__$setjib,&jibinfo);
 if (rc) return (rc);
 return(0);
}
 
PUBLIC  int
jib$setacct(account)
char * account;
 {
 ITEM jibinfo;
 int length, rc,i;
 char acct0[JIB$S_ACCOUNT];
 
/* Initialize string to blank     */
 
 for (i=0;i<JIB$S_ACCOUNT;i++) {
  acct0[i] = ' ';
 }
 
 if ((length = strlen(account)) > JIB$S_ACCOUNT) {
  length = JIB$S_ACCOUNT;
 }
 
 for (i=0;i<length;i++) {
  acct0[i] = account[i];
 }
 
 jibinfo.count = 3;
 jibinfo.count = 3;
 jibinfo.item = JIB$T_ACCOUNT;
 jibinfo.buf = acct0;
 jibinfo.bufl = JIB$S_ACCOUNT; /* Blank padding  */
 rc = sys$cmkrnl(__$setjib,&jibinfo);
 if (rc) return (rc);
 return(0);
}
