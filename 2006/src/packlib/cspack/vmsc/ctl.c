/*
 * $Id: ctl.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: ctl.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
 
/* Copyright (C) 1988 by Frederic Hemmer  CERN-DD/SW/DB */
/*          */
/* ctl.c Manipulate software CTL$ data    */
/*         */
/* Release Date Author  Description   */
/* -------    --------- ------------  --------------------------------- */
/*  1.0       26-Jul-89 F. Hemmer     Initial writing   */
 
#define PUBLIC
#define PRIVATE static
 
#include <stdio.h>
#include <ssdef.h>
#include "sysreq/jibdef.h"
 
globalvalue char *CTL$T_USERNAME;
globalvalue char *CTL$T_ACCOUNT;
globalref char  *CTL$GL_PCB;
 
 
typedef struct {
  unsigned char count;
  char  reserved[3];
  long  item;
  char *  buf;
  long  bufl;
} ITEM;
 
PRIVATE unsigned long
__$getctl (item, buf, bufl)
unsigned int item;
char   *buf;
unsigned int bufl;
 {
 register char   *p;
 register unsigned int i;
 
 p =  item;
 for (i=0; i<bufl; i++) {
  *(buf+i) = *(p+i);
 }
 return (0);
}
 
PRIVATE unsigned long
__$setctl (item, buf, bufl)
unsigned int item;
char   *buf;
unsigned int bufl;
 {
 register char   *p;
 register unsigned int i;
 
 p =  item;
 for (i=0; i<bufl; i++) {
  *(p+i) = *(buf+i);
 }
 return (0);
}
 
 
PUBLIC  int
ctl$g$gl_pcb(pcb)
int * pcb;
 {
 ITEM ctlinfo;
 int i, rc;
 
/* To avoid access violation in kernel mode, first try to write dest. */
 
 *pcb = 0xffffffff;
 ctlinfo.count = 3;
 ctlinfo.item = CTL$GL_PCB;
 ctlinfo.buf = pcb;
 ctlinfo.bufl = sizeof(int);
 if (rc = sys$cmkrnl(__$getctl,&ctlinfo)) return(rc);
 return(0);
}
 
PUBLIC  int
ctl$g$t_username(uname)
char * uname;
 {
 ITEM ctlinfo;
 char tmpnam[JIB$S_USERNAME+1];
 int i, rc;
 
/* To avoid access violation in kernel mode, first try to write dest. */
 
 for (i=0;i<JIB$S_USERNAME;i++) {
  uname[i] = ' ';
 }
 ctlinfo.count = 3;
 ctlinfo.item = CTL$T_USERNAME;
 ctlinfo.buf = tmpnam;
 ctlinfo.bufl = JIB$S_USERNAME;
 if (rc = sys$cmkrnl(__$getctl,&ctlinfo)) return(rc);
 for (i=0;i<JIB$S_USERNAME;i++) {
  uname[i] = tmpnam[i];
 }
 uname[JIB$S_USERNAME]='\0';
 return(0);
}
 
PUBLIC  int
ctl$g$t_account(acct)
char * acct;
 {
 ITEM ctlinfo;
 char tmpnam[JIB$S_ACCOUNT+1];
 int i, rc;
 
/* To avoid access violation in kernel mode, first try to write dest. */
 
 for (i=0;i<JIB$S_ACCOUNT;i++) {
  acct[i] = ' ';
 }
 ctlinfo.count = 3;
 ctlinfo.item = CTL$T_ACCOUNT;
 ctlinfo.buf = tmpnam;
 ctlinfo.bufl = JIB$S_ACCOUNT;
 if (rc = sys$cmkrnl(__$getctl,&ctlinfo)) return(rc);
 for (i=0;i<JIB$S_ACCOUNT;i++) {
  acct[i] = tmpnam[i];
 }
 acct[JIB$S_ACCOUNT]='\0';
 return(0);
}
 
PUBLIC  int
ctl$s$t_username(uname)
char * uname;
 {
 ITEM ctlinfo;
 char tmpnam[JIB$S_USERNAME];
 int length;
 int i, rc;
 
/* Initialize the string to blanks    */
 
 for (i=0;i<JIB$S_USERNAME;i++) {
  tmpnam[i] = ' ';
 }
 if ((length = strlen(uname)) > JIB$S_USERNAME) {
  length = JIB$S_USERNAME;
 }
 for (i=0;i<length;i++) {
  tmpnam[i] = uname[i];
 }
 
 ctlinfo.count = 3;
 ctlinfo.item = CTL$T_USERNAME;
 ctlinfo.buf = tmpnam;
 ctlinfo.bufl =  JIB$S_USERNAME;
 if (rc = sys$cmkrnl(__$setctl,&ctlinfo)) return(rc);
 return(0);
}
 
 
PUBLIC  int
ctl$s$t_account(acct)
char * acct;
 {
 ITEM ctlinfo;
 char tmpnam[JIB$S_ACCOUNT];
 int length;
 int i, rc;
 
/* Initialize the string to blanks    */
 
 for (i=0;i<JIB$S_ACCOUNT;i++) {
  tmpnam[i] = ' ';
 }
 if ((length = strlen(acct)) > JIB$S_ACCOUNT) {
  length = JIB$S_ACCOUNT;
 }
 for (i=0;i<length;i++) {
  tmpnam[i] = acct[i];
 }
 
 ctlinfo.count = 3;
 ctlinfo.item = CTL$T_ACCOUNT;
 ctlinfo.buf = tmpnam;
 ctlinfo.bufl =  JIB$S_ACCOUNT;
 if (rc = sys$cmkrnl(__$setctl,&ctlinfo)) return(rc);
 return(0);
}
