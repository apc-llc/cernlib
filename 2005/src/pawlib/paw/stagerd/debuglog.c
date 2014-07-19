/*
 * $Id: debuglog.c,v 1.2 1996/04/02 22:00:18 thakulin Exp $
 *
 * $Log: debuglog.c,v $
 * Revision 1.2  1996/04/02 22:00:18  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          13/11/95  18.38.47  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * debuglog.c
 *
 * When DEBUG_LOG is defined these functions produce a large amount
 * of debug messages while the program executes.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    12/8/94
 *
 *************************************************************************/

#include "pfstagerd.h"

#ifdef DEBUG_LOG

static FILE *debf;
static char null[] = "NULL";

void
dlog_open(char *s)
{
  if (!(debf = fopen(s,"a"))) return;
  setvbuf(debf,(char *)0,_IONBF,0);
}

void
dlog_close()
{
  if (!debf) return;
  fclose(debf);
}

void
dlog_msg(char *s,...)
{
  va_list arg_ptr;
  va_start(arg_ptr,s);
  if (!debf) return;
  vfprintf(debf,s,arg_ptr);
  fprintf(debf,"\n");
}

void
dvlog_error(char *s,va_list arg_ptr)
{
  if (!debf || !errno) return;
  vfprintf(debf,s,arg_ptr);
  fprintf(debf," : (%s)",strerror(errno));
  fprintf(debf,"\n");
}

void
dlog_error(char *s,...)
{
  va_list arg_ptr;
  va_start(arg_ptr,s);
  dvlog_error(s,arg_ptr);
  va_end(arg_ptr);
}

void
dlog_client(struct client *cl_dat)
{
  if (!debf) return;
  fprintf(debf,"\
ct_username = %s\n\
ct_machname = %s\n\
ct_ipaddr   = %s\n\
ct_fusername= %s\n\
ct_passwd   = %s\n\
ct_clientid = %lu\n\n",
    cl_dat->ct_username ? cl_dat->ct_username : null,
    cl_dat->ct_machname ? cl_dat->ct_machname : null,
    cl_dat->ct_ipaddr ? cl_dat->ct_ipaddr : null,
    cl_dat->ct_fusername ? cl_dat->ct_fusername : null,
    cl_dat->ct_passwd ? cl_dat->ct_passwd : null,
    cl_dat->ct_clientid);
}

void
dlog_stage(struct stage *st_dat)
{
  char queuebuf[26], startbuf[26], endbuf[26];

  if (!debf) return;
  fprintf(debf,"\
sg_sfname   = %s\n\
sg_tfname   = %s\n\
sg_prserver = %s\n\
sg_ftyp     = %d\n\
sg_fsiz     = %lu\n\
sg_nowait   = %d\n\
sg_fnum     = %d\n\
sg_ptfname  = %s\n\
sg_masterip = %s\n\
sg_serverip = %s\n\
sg_stageid  = %lu\n\
sg_queuetime= %s\
sg_starttime= %s\
sg_endtime  = %s\
sg_otfname  = %s\n\
sg_psfname  = %s\n\
sg_fdone    = %lu\n\
sg_status   = %d\n\
sg_retcode  = %d\n\
sg_childpid = %ld\n\
sg_lfname   = %s\n\n",
    st_dat->sg_sfname ? st_dat->sg_sfname : null,
    st_dat->sg_tfname ? st_dat->sg_tfname : null,
    st_dat->sg_prserver ? st_dat->sg_prserver : null,
    st_dat->sg_ftyp,
    st_dat->sg_fsiz,
    st_dat->sg_nowait,
    st_dat->sg_fnum,
    st_dat->sg_ptfname && st_dat->sg_ptfname[0].pf_name ?
      st_dat->sg_ptfname[0].pf_name : null,
    st_dat->sg_masterip ? st_dat->sg_masterip : null,
    st_dat->sg_serverip ? st_dat->sg_serverip : null,
    st_dat->sg_stageid,
    strcpy(queuebuf,ctime(&st_dat->sg_queuetime)),
    strcpy(startbuf,ctime(&st_dat->sg_starttime)),
    strcpy(endbuf,ctime(&st_dat->sg_endtime)),
    st_dat->sg_otfname ? st_dat->sg_otfname : null,
    st_dat->sg_psfname ? st_dat->sg_psfname : null,
    st_dat->sg_fdone,
    st_dat->sg_status,
    st_dat->sg_retcode,
    (long)st_dat->sg_childpid,
    st_dat->sg_lfname ? st_dat->sg_lfname : null);
}

void
dlog_sockaddr_in(struct sockaddr_in *addr)
{
  if (!debf) return;
  fprintf(debf,"\
addr->sin_family      = %d\n\
addr->sin_port        = %u\n\
addr->sin_addr.s_addr = %lu (%s)\n\n",
    addr->sin_family,
    ntohs(addr->sin_port),
    ntohl(addr->sin_addr.s_addr), inet_ntoa(addr->sin_addr));
}

#endif /* DEBUG_LOG */
