/*
 * $Id: pcb.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: pcb.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
 
/* Copyright (C) 1988 by Frederic Hemmer  CERN-DD/SW/DB */
/*          */
/* pcb.c Retrieve software process control block info  */
/*         */
/* Release Date Author  Description   */
/* -------    --------- ------------  --------------------------------- */
/*  0.1       01-Aug-88 F. Hemmer     Initial writing   */
/*  1.0       13-Jul-89 F. Hemmer     add : __$setpcb  */
/*      pcb$getmbr, pcb$setmbr */
/*      pcb$getgrp, pcb$setgrp */
/*      pcb$getuic, pcb$setuic */
/*      pcb$setlname  */
/*         */
 
#if defined(CERNLIB_QMALPH)
#define strlen DECC$STRLEN
#endif
 
#define PUBLIC
#define PRIVATE static
 
#include <stdio.h>
#include <ctype.h>
#include <ssdef.h>
#include "sysreq/pcbdef.h"
 
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
__$getpcb (item, buf, bufl)
unsigned int item;
char   *buf;
unsigned int bufl;
 {
 register char   *p;
 register unsigned int i;
 
 p = CTL$GL_PCB+item;
 for (i=0; i<bufl; i++) {
 *(buf+i) = *(p+i);
 }
 return (0);
}
 
PRIVATE
unsigned long
__$setpcb (item, buf, bufl)
unsigned int item;
char   *buf;
unsigned int bufl;
 {
 register char   *p;
 register unsigned int i;
 
 p = CTL$GL_PCB+item;
 for (i=0; i<bufl; i++) {
  *(p+i) = *(buf+i);
 }
 return (0);
}
 
PUBLIC int
pcb$getepid(epid)
unsigned int *epid;
 {
 ITEM pcbinfo;
/*
 * To avoid access violation in kernel mode, try to write to destination
 */
 *epid = 0xffffffff;
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_EPID;
 pcbinfo.buf = epid;
 pcbinfo.bufl = sizeof(unsigned int);
 return( sys$cmkrnl(__$getpcb,&pcbinfo));
}
 
PUBLIC int
pcb$getuic(uic)
unsigned int *uic;
 {
 ITEM pcbinfo;
/*
 * To avoid access violation in kernel mode, try to write to destination
 */
 *uic = 0xffffffff;
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_UIC;
 pcbinfo.buf = uic;
 pcbinfo.bufl = sizeof(unsigned int);
 return( sys$cmkrnl(__$getpcb,&pcbinfo));
}
PUBLIC int
pcb$getmbr(mem)
unsigned int *mem;
 {
 ITEM pcbinfo;
/*
 * To avoid access violation in kernel mode, try to write to destination
 */
 *mem = 0xffffffff;
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_UIC;
 pcbinfo.buf = mem;
 pcbinfo.bufl = sizeof(unsigned int);
 return( sys$cmkrnl(__$getpcb,&pcbinfo));
}
PUBLIC int
pcb$getgrp(grp)
unsigned int *grp;
 {
 ITEM pcbinfo;
/*
 * To avoid access violation in kernel mode, try to write to destination
 */
 *grp = 0xffffffff;
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_UIC+2;
 pcbinfo.buf = grp;
 pcbinfo.bufl = sizeof(unsigned int);
 return( sys$cmkrnl(__$getpcb,&pcbinfo));
}
 
PUBLIC int
pcb$setuic(uic)
unsigned int *uic;
 {
 ITEM pcbinfo;
 
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_UIC;
 pcbinfo.buf = uic;
 pcbinfo.bufl = sizeof(unsigned int);
 return( sys$cmkrnl(__$setpcb,&pcbinfo));
}
 
PUBLIC int
pcb$setmbr(mem)
unsigned short *mem;
 {
 ITEM pcbinfo;
 
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_UIC;
 pcbinfo.buf = mem;
 pcbinfo.bufl = sizeof(unsigned short);
 return( sys$cmkrnl(__$setpcb,&pcbinfo));
}
 
PUBLIC int
pcb$setgrp(grp)
unsigned short *grp;
 {
 ITEM pcbinfo;
 
 pcbinfo.count = 3;
 pcbinfo.item = PCB$L_UIC+2;
 pcbinfo.buf = grp;
 pcbinfo.bufl = sizeof(unsigned short);
 return( sys$cmkrnl(__$setpcb,&pcbinfo));
}
 
PUBLIC  int
pcb$getlname(lname)
char * lname;
 {
 ITEM pcbinfo;
 char tmpnam[PCB$S_LNAME];
 int i, rc;
 
 pcbinfo.count = 3;
 pcbinfo.item = PCB$T_LNAME;
 pcbinfo.buf = tmpnam;
 pcbinfo.bufl = PCB$S_LNAME;
 rc = sys$cmkrnl(__$getpcb,&pcbinfo);
 if (rc) return (rc);
 for (i=0;i<tmpnam[0];i++) {
  *(lname+i) = tmpnam[i+1];
 }
 *(lname+i) = 0;
 return(0);
}
 
 
PUBLIC  int
pcb$setlname(lname)
char * lname;
 {
 ITEM pcbinfo;
 char tmpnam[PCB$S_LNAME];
 int i, rc,len;
 
 len = strlen(lname);
 tmpnam[0] = (len <= PCB$S_LNAME ? len : PCB$S_LNAME);
 for (i=0;i<tmpnam[0];i++) {
  tmpnam[i+1] = *(lname+i);
 }
 pcbinfo.count = 3;
 pcbinfo.item = PCB$T_LNAME;
 pcbinfo.buf = tmpnam;
 pcbinfo.bufl = PCB$S_LNAME;
 rc = sys$cmkrnl(__$setpcb,&pcbinfo);
 if (rc) return (rc);
 return(0);
}
 
