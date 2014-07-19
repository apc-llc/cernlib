/*
 * $Id: lists.c,v 1.2 1996/04/02 22:00:22 thakulin Exp $
 *
 * $Log: lists.c,v $
 * Revision 1.2  1996/04/02 22:00:22  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          13/11/95  18.35.55  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * lists.c
 *
 * Various duties related to list management etc.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    12/8/94
 *
 *************************************************************************/

#include "pfstagerd.h"

/*
 * Searches the clientlist for a matching username and ip address combo.
 */
struct client *
find_client(struct client *cll)
{
  struct client *cl;

  for (cl = clientlist; cl; cl = cl->ct_next) {
    if (streql(cl->ct_username,cll->ct_username) &&
        streql(cl->ct_ipaddr,cll->ct_ipaddr))
      return cl;
  }
  return (struct client *)0;
}

/*
 * Searches the peerlist for a matching ip address.
 */
struct peer *
find_peerbyip(struct peer *list,char *ipaddr)
{
  struct peer *pe;

  for (pe = list; pe; pe = pe->pr_next) {
    if (streql(pe->pr_ipaddr,ipaddr))
      return pe;
  }
  return (struct peer *)0;
}

/*
 * Searches the slavelist for a matching name or ip address / slave
 * machine name combo and returns the ip address of the slave.
 * Three name formats are checked in this order: ip address
 * (eg. 130.244.35.2), full domain name (eg. foo.bar.za),
 * machine name (eg. foo).
 */
char *
find_peeripbyname(char *name)
{
  struct peer *pe;

  for (pe = slavelist; pe; pe = pe->pr_next) {
    char *c = strchr(pe->pr_machname,'.');
    if (streql(name,pe->pr_ipaddr) ||
        streql(name,pe->pr_machname) ||
        c ? strneql(name,pe->pr_machname,c - pe->pr_machname) : 0)
      return dupstr(pe->pr_ipaddr);
  }
  return (char *)0;
}

/*
 * Searches the slavelist for a matching file path / slave
 * machine name combo and returns the ip address of the slave.
 */
char *
find_peeripbypath(char *path)
{
  struct peer *pe;
  char *s = path + strlen(nfspath);

  for (pe = slavelist; pe; pe = pe->pr_next) {
    char *c = strchr(pe->pr_machname,'.');
    if (!c) c = strchr(pe->pr_machname,'\0');
    if (strneql(s,pe->pr_machname,c - pe->pr_machname))
      return dupstr(pe->pr_ipaddr);
  }
  return (char *)0;
}

/*
 * Searches the stagelists of all clients for a matching stage id
 * and returns that stage.
 */
struct stage *
find_stagebyid(unsigned long stageid,struct client **clp)
{
  struct client *cl;
  struct stage *st;

  for (cl = clientlist; cl; cl = cl->ct_next)
    for (st = cl->ct_stagelist; st; st = st->sg_next)
      if (st->sg_stageid == stageid) {
        if (clp) *clp = cl;
        return st;
      }
  return (struct stage *)0;
}

/*
 * Searches the clientlist for a matching client id.
 */
struct client *
find_clientbyid(unsigned long clientid)
{
  struct client *cl;

  for (cl = clientlist; cl; cl = cl->ct_next) {
    if (cl->ct_clientid == clientid)
      return cl;
  }
  return (struct client *)0;
}

/*
 * Searches the stagelists of all clients for a matching stage id
 * and returns that client.
 */
struct client *
find_clientbystageid(unsigned long stageid)
{
  struct client *cl;
  struct stage *st;

  for (cl = clientlist; cl; cl = cl->ct_next)
    for (st = cl->ct_stagelist; st; st = st->sg_next) {
      if (st->sg_stageid == stageid)
        return cl;
    }
  return (struct client *)0;
}

/*
 * Searches the stagelists of all clients for a matching child process id
 * This function is only used by the slaves.
 */
struct stage *
find_stagebypid(pid_t pid)
{
  struct client *cl;
  struct stage *st;

  for (cl = clientlist; cl; cl = cl->ct_next)
    for (st = cl->ct_stagelist; st; st = st->sg_next) {
      if (st->sg_childpid == pid)
        return st;
  }
  return (struct stage *)0;
}

void
remove_transferbypid(pid_t pid)
{
  struct transfer *tr, *trr;

  for (tr = transferlist; tr; trr = tr, tr = tr->tf_next)
    if (tr->tf_pid == pid) {
      if (tr == transferlist)
        transferlist = tr->tf_next;
      else
        trr->tf_next = tr->tf_next;
      x_free(tr);
      break;
    }
}

/*
 * Deallocator routines for peer, client and stage structures.
 */
void
release_peer(struct peer *pe)
{
  if (pe->pr_machname) x_free(pe->pr_machname);
  if (pe->pr_ipaddr) x_free(pe->pr_ipaddr);
}

void
release_client(struct client *cl)
{
  if (cl->ct_username) x_free(cl->ct_username);
  if (cl->ct_machname) x_free(cl->ct_machname);
  if (cl->ct_ipaddr) x_free(cl->ct_ipaddr);
  if (cl->ct_fusername) x_free(cl->ct_fusername);
  if (cl->ct_passwd) x_free(cl->ct_passwd);
}

void
release_stage(struct stage *st)
{
  if (st->sg_sfname) x_free(st->sg_sfname);
  if (st->sg_tfname) x_free(st->sg_tfname);
  if (st->sg_prserver) x_free(st->sg_prserver);
  if (st->sg_ptfname) {
    int i;
    for (i = 0; i < st->sg_fnum; i++)
      if (st->sg_ptfname[i].pf_name) x_free(st->sg_ptfname[i].pf_name);
    x_free(st->sg_ptfname);
  }
  if (st->sg_masterip) x_free(st->sg_masterip);
  if (st->sg_serverip) x_free(st->sg_serverip);
  if (st->sg_otfname) x_free(st->sg_otfname);
  if (st->sg_psfname) x_free(st->sg_psfname);
  if (st->sg_lfname) x_free(st->sg_lfname);
  if (st->sg_slavelist) x_free(st->sg_slavelist);
}

void
remove_client(struct client *cl_dat)
{
  struct client *cl, *cll;

  for (cl = clientlist, cll = (struct client *)0; cl; ) {
    if (cl_dat == cl) {
      if (cll)
        cll->ct_next = cl->ct_next;
      else
        clientlist = cl->ct_next;
      release_client(cl);
      x_free(cl);
      break;
    }
    cll = cl;
    cl = cll->ct_next;
  }
}

void
remove_stage(struct client *cl_dat,struct stage *st_dat)
{
  struct stage *st, *stt;

  for (st = cl_dat->ct_stagelist, stt = (struct stage *)0; st; ) {
    if (st_dat == st) {
      if (stt)
        stt->sg_next = st->sg_next;
      else
        cl_dat->ct_stagelist = st->sg_next;
      release_stage(st);
      x_free(st);
      break;
    }
    stt = st;
    st = stt->sg_next;
  }

  /* can we remove the client as well? */
  if (!cl_dat->ct_stagelist)
    remove_client(cl_dat);
}

/*
 * Returns the max. length of the printable representation of
 * an unsigned long integer without any special formatting.
 */
size_t
prlen_ulong(unsigned long n)
{
  size_t i;
  for (i = 1; n > 9; i++) n /= 10;
  return i;
}

/*
 * Returns the max. length of the printable representation of
 * a signed long integer without any special formatting.
 */
size_t
prlen_long(long n)
{
  size_t i;
  if (n < 0)
    for (i = 2; n < -9; i++) n /= 10;
  else
    for (i = 1; n > 9; i++) n /= 10;
  return i;
}

/*
 * Returns the max. length of the printable representation of
 * a stage struct (conservative approach).
 */
size_t
prlen_stage(struct stage *st)
{
  int i;
  size_t len;

  len = strlen(st->sg_sfname ? st->sg_sfname : "") +
    strlen(st->sg_tfname ? st->sg_tfname : "") +
    strlen(st->sg_prserver ? st->sg_prserver : "") +
    prlen_long(st->sg_ftyp) + strlen(ftyptb[st->sg_ftyp]) +
    prlen_ulong(st->sg_fsiz) +
    prlen_long(st->sg_fnum) +
    prlen_long(st->sg_nowait);
  for (i = 0; i < st->sg_fnum; i++)
    len += strlen(st->sg_ptfname[i].pf_name ? st->sg_ptfname[i].pf_name : "") +
      prlen_ulong(st->sg_ptfname[i].pf_size);
  len += strlen(st->sg_masterip ? st->sg_masterip : "") +
    strlen(st->sg_serverip ? st->sg_serverip : "") +
    prlen_ulong(st->sg_stageid) +
    /* length of a formatted time string is 24 */
    prlen_ulong(st->sg_queuetime) + 24 +
    prlen_ulong(st->sg_starttime) + 24 +
    prlen_ulong(st->sg_endtime) + 24 +
    strlen(st->sg_otfname ? st->sg_otfname : "") +
    strlen(st->sg_psfname ? st->sg_psfname : "") +
    strlen(st->sg_lfname ? st->sg_lfname : "") +
    strlen(st->sg_slavelist ? st->sg_slavelist : "") +
    prlen_ulong(st->sg_fdone) +
    prlen_long(st->sg_status) + strlen(statustb[st->sg_status]) +
    prlen_long(st->sg_retcode) + strlen(st_retcodestr(st->sg_retcode)) +
    prlen_long(st->sg_childpid);
  return len;
}
