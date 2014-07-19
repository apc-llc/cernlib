/*
 * $Id: getpwnam.c,v 1.1.1.1 1996/03/08 15:44:28 mclareni Exp $
 *
 * $Log: getpwnam.c,v $
 * Revision 1.1.1.1  1996/03/08 15:44:28  mclareni
 * Cspack
 *
 */
#include "cspack/pilot.h"
/*
 * Copyright (C) 1989 by Frederic Hemmer
 * All rights reserved
 */
 
/* getpwnam.c - search for specified entry and return info  */
 
#if defined(CERNLIB_QMALPH)
#define strlen DECC$STRLEN
#define strcat DECC$STRCAT
#define strcpy DECC$STRCPY
#endif
 
#include "sysreq/pwd_vms.h"
#include <errno.h>
#include <ssdef.h>
#include <rmsdef.h>
#include "cspack/uaidef.h"
 
static char account[9+1];
static char clitables[32+1];
static char defcli[32+1];
static char defdev[32+1];
static char defdir[64+1];
static char dir[64+32];
static char defpriv[8];
static char expire[8];
static char lastlog_i[8];
static char lastlog_n[8];
static char lgicmd[64+1];
static char owner[32+1];
static char priv[8];
static char pwd[2+1+8+1]; /* Cheating !! salt,alg,pwd  */
static char pwd_date[8];
static char pwd_life[8];
static char pwd2[2+1+8+1]; /* See pwd    */
static char pwd2_date[8];
static char username[12+1];
 
static short accountl, clitablesl, defclil, defdevl, defdirl;
static short lgicmdl, ownerl, pwdl,pwd2l, usernamel, ignretl;
 
static struct passwd pw = {
 account,  /* Don't believe the manual, no size prefix ! */
 0, 0, 0, 0, 0,
 clitables+1,
 0,
 defcli+1, defdev+1, defdir+1, dir, defpriv,
 0, 0, 0, 0, 0, 0, 0, expire, 0, 0, 0,
 lastlog_i, lastlog_n, lgicmd+1, 0, 0, 0, 0, 0, 0, 0, 0,
 owner+1,
 0, 0, 0, 0, 0,
 priv, pwd+2+1, pwd_date, 0, pwd_life, pwd2+2+1, pwd2_date,
 0, 0, 0, 0, 0, 0, 0, 0, 0,
 username, 0, 0
};
 
typedef struct {
 short bufl;
 short code;
 char *buf;
 int *retl;
} Item;
 
typedef struct {
 int len;
 char *buf;
} descrip;
 
static struct {
 Item items[57];
 int eol;
} itemlist = {
 {
 9, UAI$_ACCOUNT,  account,  &accountl,
 2, UAI$_ASTLM,  &pw.pw_astlm,  &ignretl,
 3, UAI$_BATCH_ACCESS_P, &pw.pw_b_acc_p,  &ignretl,
 3, UAI$_BATCH_ACCESS_S, &pw.pw_b_acc_s,  &ignretl,
 2, UAI$_BIOLM,  &pw.pw_biolm,  &ignretl,
 4, UAI$_BYTLM,  &pw.pw_bytlm,  &ignretl,
 32, UAI$_CLITABLES,  clitables,  &clitablesl,
 4, UAI$_CPUTIM,  &pw.pw_cputim,  &ignretl,
 32, UAI$_DEFCLI,  defcli,   &defclil,
 32, UAI$_DEFDEV,  defdev,   &defdevl,
 64, UAI$_DEFDIR,  defdir,   &defdirl,
 8, UAI$_DEF_PRIV,  defpriv,  &ignretl,
 4, UAI$_DFWSCNT,  &pw.pw_dfwscnt,  &ignretl,
 2,  UAI$_DIOLM,  &pw.pw_diolm,  &ignretl,
 3, UAI$_DIALUP_ACCESS_P, &pw.pw_d_acc_p,  &ignretl,
 3, UAI$_DIALUP_ACCESS_S, &pw.pw_d_acc_s,  &ignretl,
 1, UAI$_ENCRYPT,  &pw.pw_encrypt,  &ignretl,
 1, UAI$_ENCRYPT2,  &pw.pw_encrypt2, &ignretl,
 2, UAI$_ENQLM,  &pw.pw_enqlm,  &ignretl,
 8, UAI$_EXPIRATION, expire,   &ignretl,
 2, UAI$_FILLM,  &pw.pw_fillm,  &ignretl,
 4, UAI$_FLAGS,  &pw.pw_flags,  &ignretl,
 4, UAI$_JTQUOTA,  &pw.pw_jtquota,  &ignretl,
 8, UAI$_LASTLOGIN_I, lastlog_i,  &ignretl,
 8, UAI$_LASTLOGIN_N, lastlog_n,  &ignretl,
 64, UAI$_LGICMD,  lgicmd,   &lgicmdl,
 3,  UAI$_LOCAL_ACCESS_P, &pw.pw_l_acc_p,  &ignretl,
 3,  UAI$_LOCAL_ACCESS_S, &pw.pw_l_acc_s,  &ignretl,
 2, UAI$_LOGFAILS,  &pw.pw_logfail,  &ignretl,
 2, UAI$_MAXACCTJOBS, &pw.pw_maxacctj, &ignretl,
 2, UAI$_MAXDETACH,  &pw.pw_maxdet,  &ignretl,
 2, UAI$_MAXJOBS,  &pw.pw_maxjobs,  &ignretl,
 3, UAI$_NETWORK_ACCESS_P, &pw.pw_n_acc_p,  &ignretl,
 3, UAI$_NETWORK_ACCESS_S, &pw.pw_n_acc_s,  &ignretl,
 32, UAI$_OWNER,  owner,   &ownerl,
 4, UAI$_PBYTLM,  &pw.pw_bytlm,  &ignretl,
 4, UAI$_PGFLQUOTA,  &pw.pw_pgflquota, &ignretl,
 4, UAI$_PRCCNT,  &pw.pw_prccnt,  &ignretl,
 1, UAI$_PRI,  &pw.pw_pri,  &ignretl,
 4, UAI$_PRIMEDAYS,  &pw.pw_primedays, &ignretl,
 8, UAI$_PRIV,  priv,   &ignretl,
 8, UAI$_PWD,  pwd+2+1,  &pwdl,
 8, UAI$_PWD_DATE,  pwd_date,  &ignretl,
 1, UAI$_PWD_LENGTH, &pw.pw_pwd_length, &ignretl,
 8, UAI$_PWD_LIFETIME, pwd_life,  &ignretl,
 8, UAI$_PWD2,  pwd2+2+1,     &pwd2l,
 8, UAI$_PWD2_DATE,         pwd2_date,    &ignretl,
 1, UAI$_QUEPRI,         &pw.pw_quepri,    &ignretl,
 3, UAI$_REMOTE_ACCESS_P,   &pw.pw_r_acc_p,  &ignretl,
 3, UAI$_REMOTE_ACCESS_S,   &pw.pw_r_acc_s,  &ignretl,
 2, UAI$_SALT,         &pw.pw_salt,  &ignretl,
 2, UAI$_SHRFILLM,  &pw.pw_shrfillm, &ignretl,
 2, UAI$_TQCNT,  &pw.pw_tqcnt,  &ignretl,
 4, UAI$_UIC,      &pw.pw_uic,  &ignretl,
 12, UAI$_USERNAME,  username,  &usernamel,
 4, UAI$_WSEXTENT,  &pw.pw_wsextent, &ignretl,
 4, UAI$_WSQUOTA,  &pw.pw_wsquota,  &ignretl
 
 },
 0
};
 
struct passwd *
getpwnam(name)
char  *name;
{
 descrip name_dsc;
 int rc;
 
 name_dsc.len = strlen(name);
 name_dsc.buf = name;
 
 if ((rc = sys$getuai(0, 0, &name_dsc, &itemlist, 0, 0, 0)) != SS$_NORMAL) {
  if (rc == RMS$_RNF) {
   return((struct passwd*) 0);
  }
  if (!(rc & 0x01)) {
   vaxc$errno = rc;
   errno = EVMSERR;
   return((struct passwd*) 0);
  }
 }
 unpad(account,accountl);
 unpad(clitables,clitablesl);
 unpad(defcli,defclil);
 unpad(defdev,defdevl);
 unpad(defdir,defdirl);
 strcpy(dir, pw.pw_defdev); strcat(dir, pw.pw_defdir);
 unpad(lgicmd, lgicmdl);
 unpad(owner,ownerl);
 *(pwd+1+2+pwdl)='\0';
 pwd[0] = *(char *)(&pw.pw_salt);
 pwd[1] = *(char *)(&pw.pw_salt+1);
 pwd[2] = pw.pw_encrypt;
 *(pwd2+1+2+pwd2l)='\0';
 pwd2[0] = *(char *)(&pw.pw_salt);
 pwd2[1] = *(char *)(&pw.pw_salt+1);
 pwd2[2] = pw.pw_encrypt2;
 pw.pw_gid = (unsigned short) ((pw.pw_uic >> 16) & 0xffff);
 pw.pw_uid = (unsigned short) ( pw.pw_uic & 0x0000ffff);
 unpad(username, usernamel);
 return (&pw);
}
 
unpad (p, l)
char *p;
int l;
{
 char  *cp;
 
 p[l]='\0';
 cp = p+l-1;
 while ((cp > p) && (*cp == ' ')) {
  cp--;
 }
 *(cp+1) = '\0';
}
