/*
 * $Id: dump.c,v 1.2 1996/04/02 22:00:20 thakulin Exp $
 *
 * $Log: dump.c,v $
 * Revision 1.2  1996/04/02 22:00:20  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          13/11/95  18.37.36  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * dump.c
 *
 * Routines for dumping out the status of the pfstagerd.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    22/8/94
 *
 *************************************************************************/

#include "pfstagerd.h"

static char empty[] = " <empty>\n";
static char null[] = "NULL";

static void
dump_peer(FILE *f,int i,struct peer *pe_dat)
{
  fprintf(f,"\
  peer %d\n\
    pr_machname     : %s\n\
    pr_ipaddr       : %s\n\
    pr_last         : %s\
    pr_conport      : %d\n\
    pr_lisport      : %d\n\
    pr_sock         : %d\n\
    pr_up           : %d\n\
    pr_keep         : %d\n",
    i, pe_dat->pr_machname ? pe_dat->pr_machname : null,
    pe_dat->pr_ipaddr ? pe_dat->pr_ipaddr : null,
    ctime(&pe_dat->pr_last),
    pe_dat->pr_conport, pe_dat->pr_lisport, pe_dat->pr_sock,
    pe_dat->pr_up, pe_dat->pr_keep);
}

static void
dump_config(FILE *f)
{
  fprintf(f,"\
mailbin             : %s\n\
ftpbin              : %s\n\
stagerbin           : %s\n\
fatmenbin           : %s\n\
dpmbin              : %s\n\
rmbin               : %s\n\
joblogname          : %s\n\
errlogname          : %s\n\
syslogchn           : %s\n\
nfspath             : %s\n\
tmppath             : %s\n\
dumpname            : %s\n\
admin               : %s\n\
piafhome            : %s\n\
pendingtime         : %d sec\n\
selecttimeout       : %d sec\n\
stagetimeout        : %d sec\n\
pinginterval        : %d sec\n\
stagecancel         : %d sec\n\
userumask           : %o\n\
bclio               :",
    mailbin ? mailbin : null, ftpbin ? ftpbin : null,
    stagerbin ? stagerbin : null, fatmenbin ? fatmenbin : null,
    dpmbin ? dpmbin : null, rmbin ? rmbin : null,
    joblogname ? joblogname : null, errlogname ? errlogname : null,
    syslogchn ? syslogchn : null, nfspath ? nfspath : null,
    tmppath ? tmppath : null, dumpname, admin ? admin : null,
    piafhome ? piafhome : null,
    pendingtime, selecttimeout, stagetimeout, pinginterval,
    stagecancel, (unsigned)userumask);
  if (bclio.bc_server)
    fprintf(f,"\n\
  bc_blocksize      : %s\n\
  bc_fileid         : %s\n\
  bc_format         : %s\n\
  bc_reclen         : %s\n\
  bc_datsiz         : %d MB\n\
  bc_version        : %d\n\
  bc_server         : %s\n\
  bc_port           : %d\n\
  bc_login          : %s\n\
  bc_retries        : %d\n\
  bc_wait           : %d sec\n",
      bclio.bc_blocksize, bclio.bc_fileid, bclio.bc_format,
      bclio.bc_reclen, bclio.bc_datsiz, bclio.bc_version, bclio.bc_server,
      bclio.bc_port, bclio.bc_login, bclio.bc_retries, bclio.bc_wait);
  else
    fprintf(f," <n/a>\n");
  fprintf(f,"\
bftp                :");
  if (bftp.bf_bufsize > 0) {
    fprintf(f,"\n\
  bf_bufsize        : %ld\n\
  bf_stripenum      : %d\n\
  bf_minsize        : %ld\n\
  bf_dotransfer     : %ld\n",
      bftp.bf_bufsize, bftp.bf_stripenum, bftp.bf_minsize,
      bftp.bf_dotransfer);
  }
  else
    fprintf(f," <n/a>\n");
  fprintf(f,"\
tcpparam            :\n\
  tc_rcvbuf         : %ld\n\
  tc_sndbuf         : %ld\n\
  tc_nodelay        : %d\n",
      tcpparam.tc_rcvbuf, tcpparam.tc_sndbuf, tcpparam.tc_nodelay);
}

static void
dump_client(FILE *f,int i,struct client *cl_dat)
{
  struct stage *st_dat;
  int j, k;

  fprintf(f,"\
  client %d\n\
    ct_username     : %s\n\
    ct_machname     : %s\n\
    ct_ipaddr       : %s\n\
    ct_fusername    : %s\n\
    ct_passwd       : %s\n\
    ct_clientid     : %lu\n",
    i, cl_dat->ct_username ? cl_dat->ct_username : null,
    cl_dat->ct_machname ? cl_dat->ct_machname : null,
    cl_dat->ct_ipaddr ? cl_dat->ct_ipaddr : null,
    cl_dat->ct_fusername ? cl_dat->ct_fusername : null,
    "<n/a>", cl_dat->ct_clientid);
  fprintf(f,"\
    ct_stagelist    :");
  if (cl_dat->ct_stagelist) {
    fprintf(f,"\n");
    for (st_dat = cl_dat->ct_stagelist, j = 0; st_dat;
         st_dat = st_dat->sg_next, j++) {
      char queuebuf[26], startbuf[26], endbuf[26];
      fprintf(f,"\
      stage %d\n\
        sg_sfname   : %s\n\
        sg_tfname   : %s\n\
        sg_prserver : %s\n\
        sg_ftyp     : %d%s%s%s\n\
        sg_fsiz     : %lu bytes\n\
        sg_nowait   : %d\n\
        sg_fnum     : %d\n\
        sg_ptfname  : ",
        j, st_dat->sg_sfname ? st_dat->sg_sfname : null,
        st_dat->sg_tfname ? st_dat->sg_tfname : null,
        st_dat->sg_prserver ? st_dat->sg_prserver : null,
        st_dat->sg_ftyp,
          st_dat->sg_ftyp > 0 ? " (" : "",
          st_dat->sg_ftyp > 0 ? ftyptb[st_dat->sg_ftyp] : "",
          st_dat->sg_ftyp > 0 ? ")" : "",
        st_dat->sg_fsiz,
        st_dat->sg_nowait,
        st_dat->sg_fnum);
      if (st_dat->sg_ptfname) {
        fprintf(f,"%s %ld %lu\n",
          st_dat->sg_ptfname[0].pf_name ? st_dat->sg_ptfname[0].pf_name : null,
            st_dat->sg_ptfname[0].pf_nrec,st_dat->sg_ptfname[0].pf_size);
        for (k = 1; k < st_dat->sg_fnum; k++)
          fprintf(f,"\
                    : %s %ld %lu\n",
            st_dat->sg_ptfname[k].pf_name ?
              st_dat->sg_ptfname[k].pf_name : null,
              st_dat->sg_ptfname[k].pf_nrec,st_dat->sg_ptfname[k].pf_size);
      }
      else
        fprintf(f,"%s\n",null);
      fprintf(f,"\
        sg_masterip : %s\n\
        sg_serverip : %s\n\
        sg_stageid  : %lu\n\
        sg_queuetime: %s\
        sg_starttime: %s\
        sg_endtime  : %s\
        sg_otfname  : %s\n\
        sg_psfname  : %s\n\
        sg_fdone    : %lu bytes\n\
        sg_status   : %d%s%s%s\n\
        sg_retcode  : %d (%s)\n\
        sg_childpid : %ld\n\
        sg_lfname   : %s\n\
        sg_slavelist: %s\n",
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
          st_dat->sg_status > 0 ? " (" : "",
          st_dat->sg_status > 0 ? statustb[st_dat->sg_status] : "",
          st_dat->sg_status > 0 ? ")" : "",
        st_dat->sg_retcode, st_retcodestr(st_dat->sg_retcode),
        (long)st_dat->sg_childpid,
        st_dat->sg_lfname ? st_dat->sg_lfname : null,
        st_dat->sg_slavelist ? st_dat->sg_slavelist : null);
    }
  }
  else
    fprintf(f,empty);
}

/*
 * Dump out everything essential about the internal
 * status of the pfstagerd.
 */
void
dumpstatus()
{
  FILE *f;
  struct peer *pe_dat;
  struct client *cl_dat;
  struct revector *rv_dat;
  struct transfer *tr_dat;
  time_t tim;
  char tbuf[26], mbuf[MAX_ULSTR + 2];
  int i;

  if (!dumpname) {
    errlog(LOG_INFO,"dumpstatus() : dump facility disabled");
    return;
  }
  if (!(f = fopen(dumpname,"a"))) {
    errlog(LOG_ERR,"dumpstatus() : cannot open %s",dumpname);
    return;
  }
  setvbuf(f,(char *)0,_IONBF,0);
  tim = time((time_t *)0);
  strcpy(tbuf,ctime(&tim));
  tbuf[24] = '\0';
  if (master) sprintf(mbuf," %ld",master_id);
  fprintf(f,"[%s]\n%s%s %s pid %ld at %s (%s)\n\n",
    tbuf,master ? "master" : "slave",master ? mbuf : "",
    myname,(long)getpid(),mymachname,myipaddr);
#ifdef DEBUG_ALLOC
  dump_xallocinfo(f);
#endif
  fprintf(f,"\
updateconfig        : %d\n\
doshutdown          : %d\n\
configfile          : %s\n\n",
    updateconfig,doshutdown,configfile);
  dump_config(f);
  fprintf(f,"\n\
selectlist          :");
  if (selectlist) {
    fprintf(f,"\n");
    for (pe_dat = selectlist, i = 0; pe_dat; pe_dat = pe_dat->pr_next, i++)
      dump_peer(f,i,pe_dat);
  }
  else
    fprintf(f,empty);
  if (master) {
    fprintf(f,"\n\
slavelist           :");
    if (slavelist) {
      fprintf(f,"\n");
      for (pe_dat = slavelist, i = 0; pe_dat; pe_dat = pe_dat->pr_next, i++)
        dump_peer(f,i,pe_dat);
    }
    else
      fprintf(f,empty);
  }
  fprintf(f,"\n\
clientlist          :");
  if (clientlist) {
    fprintf(f,"\n");
    for (cl_dat = clientlist, i = 0; cl_dat; cl_dat = cl_dat->ct_next, i++)
      dump_client(f,i,cl_dat);
  }
  else
    fprintf(f,empty);
  fprintf(f,"\n\
revectorlist        :");
  if (revectorlist) {
    fprintf(f,"\n");
    for (rv_dat = revectorlist, i = 0; rv_dat; rv_dat = rv_dat->rt_next, i++)
      fprintf(f,"\
  revector %d\n\
    rt_stageid      : %lu\n\
    rt_time         : %s",
        i,rv_dat->rt_stageid,ctime(&rv_dat->rt_time));
  }
  else
    fprintf(f,empty);
  fprintf(f,"\n\
transferlist        :");
  if (transferlist) {
    fprintf(f,"\n");
    for (tr_dat = transferlist, i = 0; tr_dat; tr_dat = tr_dat->tf_next, i++)
      fprintf(f,"\
  transfer %d\n\
    tr_pid          : %ld",
        i,(long)tr_dat->tf_pid);
  }
  else
    fprintf(f,empty);
  fprintf(f,"\n");

  if (ferror(f))
    errlog(LOG_ERR,"dumpstatus() : error writing %s",dumpname);
  fclose(f);
}
