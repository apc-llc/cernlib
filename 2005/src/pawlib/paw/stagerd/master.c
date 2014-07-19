/*
 * $Id: master.c,v 1.2 1996/04/02 22:00:24 thakulin Exp $
 *
 * $Log: master.c,v $
 * Revision 1.2  1996/04/02 22:00:24  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          13/11/95  18.35.53  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * master.c
 *
 * Routines peculiar to masters (names starting with "m_").
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    25/8/94
 *
 *************************************************************************/

#include "pfstagerd.h"

/* these are used a lot in report forms */
static char n_a[] = "<n/a>";
static char none[] = "<none>";

/*
 * Retrieve the client information.  If the client is already
 * in the list, just return it.  If not, add it to the list.
 */
static struct client *
m_get_clientspecs(int sock)
{
  struct client cl_tmp, *cl_dat;

  DLOG_MSG(("m_get_clientspecs(%d)",sock));

  cl_tmp = cl_init;

  if (!get_client(sock,&cl_tmp))
    goto errout;

  /* check if we already have this client booked, if not make new entry */
  if (!(cl_dat = find_client(&cl_tmp))) {
    cl_tmp.ct_clientid = NEW_CLIENTID;
    cl_tmp.ct_stagelist = (struct stage *)0;
    cl_dat = (struct client *)x_malloc(sizeof cl_dat[0]);
    *cl_dat = cl_tmp;
  }
  /* else see if any of the auxiliary info needs updating */
  else {
    if (cl_tmp.ct_machname &&
        !streql(cl_tmp.ct_machname,cl_dat->ct_machname)) {
      if (cl_dat->ct_machname) x_free(cl_dat->ct_machname);
      cl_dat->ct_machname = cl_tmp.ct_machname;
      cl_tmp.ct_machname = (char *)0;
    }
    if (cl_tmp.ct_passwd && !streql(cl_tmp.ct_passwd,cl_dat->ct_passwd)) {
      if (cl_dat->ct_passwd) x_free(cl_dat->ct_passwd);
      cl_dat->ct_passwd = cl_tmp.ct_passwd;
      cl_tmp.ct_passwd = (char *)0;
    }
    release_client(&cl_tmp);
  }
  DLOG_CLIENT(cl_dat);

  return cl_dat;

  errout:;

  release_client(&cl_tmp);
  return (struct client *)0;
}

/*
 * Consult disk pool manager for suitable file names.
 */
static int
m_diskpool(struct client *cl_dat,struct stage *st_dat)
{
  int i, sn;
  char rbuf[PATH_MAX + 1], *fname;
  char cmd[2 * PATH_MAX + 50];
  unsigned long fsize;
  FILE *f;

  /* if the file shouldn't be striped at all */
  if (bftp.bf_stripenum <= 0)
    sn = 1;
  else
    sn = bftp.bf_stripenum;
  st_dat->sg_ptfname = (struct pfinfo *)x_malloc(sn * sizeof(struct pfinfo));
  st_dat->sg_fnum = sn;
  fname = strrchr(st_dat->sg_tfname,'/');
  /* found a slash? */
  if (fname)
    fname++;
  else
    fname = st_dat->sg_tfname;
  fsize = st_dat->sg_fsiz / 1024;
  sprintf(cmd,"%s -r -n%d -s%luK -u%s %s",
    dpmbin,sn,fsize,cl_dat->ct_username,fname);
  if (!(f = popen(cmd,"r"))) {
    errlog(LOG_ERR,"m_diskpool() : can't open pipe (%s)",cmd);
    st_dat->sg_retcode = SGD_RET_POPEN;
    goto errout;
  }
  for (i = 0; i < sn && !feof(f) && !ferror(f); i++) {
    if (fscanf(f,"%s%*[^\n]",rbuf) < 1) break;
    fgetc(f);
    st_dat->sg_ptfname[i].pf_name = dupstr(rbuf);
  }
  if (ferror(f) || i <= 0) {
    errlog(LOG_ERR,"m_diskpool() : trouble getting file names (%s)",
      ferror(f) ? "ferror" : "pool exhausted?");
    pclose(f);
    st_dat->sg_retcode = SGD_RET_POPEN;
    goto errout;
  }
  if (i < sn) st_dat->sg_fnum = i;
  pclose(f);
  return 1;

  errout:;

  if (st_dat->sg_ptfname) {
    for (i = 0; i < sn; i++)
      if (st_dat->sg_ptfname[i].pf_name)
        x_free(st_dat->sg_ptfname[i].pf_name);
    x_free(st_dat->sg_ptfname);
  }
  return 0;
}

/* normalize the file path (expand '/../', '/./', and '//') */

static char *
m_norm_fname(char *fn)
{
  char *ss;
  for (ss = fn; (ss = strchr(ss,'/')) != (char *)0; ) {
    char *sx = (char *)0;
    if (ss[1] == '.') {
      if (ss[2] == '.' && ss[3] == '/') {
        sx = &ss[4];
        if (ss == fn) return (char *)0;
        for (; ss[-1] != '/'; ss--) ;
      }
      else if (ss[2] == '/') {
        sx = &ss[3];
        ss++;
      }
    }
    else if (ss[1] == '/') {
      sx = &ss[2];
      ss++;
    }
    if (sx) {
      if (!*sx) return (char *)0;
      memmove(ss,sx,strlen(sx) + 1);
      ss--;
    }
    else
      ss++;
  }
  return fn;
}

static int
m_expand_tfname(struct client *cl,struct stage *st)
{
  char *s, *ss, pp[PATH_MAX + 1];
  struct passwd *pw;

  if (!st->sg_tfname) {
    s = strrchr(st->sg_sfname,'/');
    if (s) {
      s++;
      /* ouch, the file name ended with a slash - no good */
      if (!*s) {
        st->sg_retcode = SGD_RET_TFNAME;
        return 0;
      }
    }
    else
      s = st->sg_sfname;
  }
  else
    s = st->sg_tfname;

  /* if we don't find user's password entry we must kick him out,
     because that means that something's quite wrong with this user */
  pw = getpwnam(cl->ct_username);
  if (pw && pw->pw_dir) {
    strcpy(pp,pw->pw_dir);
    if (pp[strlen(pp) - 1] != '/') strcat(pp,"/");
    strcat(pp,piafhome);
  }
  else {
    errlog(LOG_ERR,"m_expand_tfname() : user %s@%s has no %s",
      cl->ct_username,cl->ct_machname,
      pw ? "passwd entry" : "home directory");
    st->sg_retcode = pw ? SGD_RET_PW : SGD_RET_PWDIR;
    return 0;
  }

  /* is the file name relative to the user's home dir? */
  if (s[0] != '/') {
    ss = (char *)x_malloc(strlen(pp) + strlen(s) + 1);
    strcpy(ss,pp);
    strcat(ss,s);
    s = st->sg_tfname; /* kludge to ensure that we have something to free */
    st->sg_tfname = dupstr(ss);
  }
  else
    st->sg_tfname = dupstr(s);

#ifdef APPEND_HBOOK_EXT
  /* one more twist: append ".hbook" to files that don't have it -
     note that also files that really aren't hbook files get the
     same treatment because we find out too late */
  if (!streql(&st->sg_tfname[strlen(st->sg_tfname) - sizeof HBOOK_EXT + 1],
      HBOOK_EXT)) {
    ss = (char *)x_malloc(strlen(st->sg_tfname) + 7);
    strcpy(ss,st->sg_tfname);
    strcat(ss,HBOOK_EXT);
    x_free(st->sg_tfname);
    st->sg_tfname = ss;
  }
#endif

  ss = dupstr(st->sg_tfname);
  if (!m_norm_fname(ss)) {
    st->sg_retcode = SGD_RET_TFNAME;
    x_free(ss);
    return 0;
  }
  x_free(st->sg_tfname);
  st->sg_tfname = ss;

  /* file name too long? */
  if (strlen(st->sg_tfname) > (size_t)PATH_MAX) {
    errlog(LOG_ERR,"m_expand_tfname() : user %s@%s, file name %s too long",
      cl->ct_username,cl->ct_machname,st->sg_tfname);
    if (s) x_free(s);
    st->sg_retcode = SGD_RET_PATHLEN;
    return 0;
  }

  /* last check: does the path point to the piaf home dir? */
  if (!strneql(pp,st->sg_tfname,strlen(pp))) {
    if (s) x_free(s);
    st->sg_retcode = SGD_RET_PIAFHOME;
    return 0;
  }

  if (s) x_free(s);
  return 1;
}

/*
 * Figure out things about the file names and slave servers.
 */
static int
m_misc_setup(struct client *cl_dat,struct stage *st_dat)
{
  /* figure out the full physical path of the name for the file and
     check it - the path has to point to the person's piaf home dir */
  if (!m_expand_tfname(cl_dat,st_dat))
    return 0;

  if (!m_diskpool(cl_dat,st_dat))
    return 0;

#if 0
  /* check the format of the physical target file
     (tells which server to use) - it has to be at least
     approximately right or we'll die later */
  if (!strneql(st_dat->sg_ptfname,nfspath,strlen(nfspath)))
    return 0;
#endif

  /* figure out the ip address of one of the required slave server - if none
     of the servers has come up yet, we get null here and have to try
     again later */

  /* first try any preferred server name */
  if (st_dat->sg_prserver)
    st_dat->sg_serverip = find_peeripbyname(st_dat->sg_prserver);
  /* try to assign a random slave based on the target file name */
  if (!st_dat->sg_serverip) {
    int n = rand() % st_dat->sg_fnum;
    st_dat->sg_serverip = find_peeripbypath(st_dat->sg_ptfname[n].pf_name);
  }
  /* then check out the others */
  if (!st_dat->sg_serverip) {
    int i;
    for (i = 0; i < st_dat->sg_fnum && !st_dat->sg_serverip; i++)
      st_dat->sg_serverip = find_peeripbypath(st_dat->sg_ptfname[i].pf_name);
  }
  /* if still no luck, pick any available slave at random */
  if (!st_dat->sg_serverip) {
    struct peer *pe;
    int m, mm;
    for (pe = slavelist, m = 0; pe; pe = pe->pr_next)
      if (pe->pr_up) m++;
    if (m > 0) {
      mm = rand() % m;
      for (pe = slavelist, m = 0; pe; pe = pe->pr_next)
        if (pe->pr_up && m++ == mm) {
          st_dat->sg_serverip = dupstr(pe->pr_ipaddr);
          break;
        }
    }
  }

  return 1;
}

/*
 * Retrieve the actual specs of the file to be staged.
 */
static struct stage *
m_get_stagespecs(int sock)
{
  struct stage st_tmp, *st_dat;

  DLOG_MSG(("m_get_stagespecs(%d)",sock));

  st_tmp = st_init;

  if (!get_stage(sock,&st_tmp)) {
    release_stage(&st_tmp);
    return (struct stage *)0;
  }

  st_tmp.sg_masterip = dupstr(myipaddr);
  st_tmp.sg_stageid = NEW_STAGEID;
  st_tmp.sg_status = st_tmp.sg_retcode = 0;
  st_tmp.sg_queuetime = st_tmp.sg_starttime = time((time_t *)0);

  st_dat = (struct stage *)x_malloc(sizeof st_dat[0]);
  *st_dat = st_tmp;

  DLOG_STAGE(st_dat);

  return st_dat;
}

/*
 * Send the necessary client information to the slave.
 */
static int
m_put_clientspecs(int sock,struct client *cl_dat)
{
  DLOG_MSG(("m_put_clientspecs(%d)",sock));
  DLOG_CLIENT(cl_dat);

  return put_client(sock,cl_dat);
}

/*
 * Send the specs of the file to be staged to the slave.
 */
static int
m_put_stagespecs(int sock,struct stage *st_dat)
{
  DLOG_MSG(("m_put_stagespecs(%d)",sock));
  DLOG_STAGE(st_dat);

  st_dat->sg_starttime = time((time_t *)0);

  return put_stage(sock,st_dat);
}

/*
 * Retrieve the results from the slave about the file to be staged.
 */
static struct stage *
m_get_stageresults(int sock,struct stage *st,struct revector **rvpp)
{
  struct client cl_tmp, *cl_dat;
  struct stage st_tmp, *st_dat;

  DLOG_MSG(("m_get_stageresults(%d)",sock));

  cl_tmp = cl_init;
  st_tmp = st_init;

  if (!get_client(sock,&cl_tmp) || !get_stage(sock,&st_tmp))
    goto errout;

  /* check the client and stage id - if we don't find it, it means that
     either this stage belongs to another master and we're being
     used as a backup, or we've gone down in the past and this
     is just one of those earlier stages */

  /* Input parameter st is set when we have asked the slave to send
     data concerning one particular stage, which we want to update.
     In that case we don't have to search the list to find it. */

  st_dat = st ? st : find_stagebyid(st_tmp.sg_stageid,&cl_dat);

  if (st_dat) {
    /* ok, this is one of ours - update info */
    if (st || cl_dat->ct_clientid == cl_tmp.ct_clientid) {
      struct stage *stt = st_dat->sg_next;
      release_client(&cl_tmp);
      release_stage(st_dat);
      *st_dat = st_tmp;
      st_dat->sg_next = stt;
    }
    /* What the... stage id matches but not client id?  Must be
       a bug or an extremely old stage still floating around. */
    else {
      errlog(LOG_ERR,
        "m_get_stageresults() : stage id %lu ok, client id %lu != %lu",
         st_tmp.sg_stageid,cl_tmp.ct_clientid,cl_dat->ct_clientid);
      goto errout;
    }
  }
  /* somebody elses leftover (or maybe ours) */
  else {

    /* Oops, no - for some reason it seems to be ours or else such a
       stage that we've processed before.  Maybe there
       has been a break in communications so that the slave thinks
       we didn't get the info earlier and is now resending it although
       we actually did and have already mailed the user.  Just
       clear and tell the slave to forget. */
    *rvpp = find_revect(st_tmp.sg_stageid);
    if (*rvpp) {
      errlog(LOG_INFO,
      "m_get_stageresults() : stage %lu belonging to master %lu appears twice",
        st_tmp.sg_stageid,MAST_NUM(st_tmp.sg_stageid));
      goto errout;
    }
    else if (MAST_NUM(st_tmp.sg_stageid) == master_id) {
      errlog(LOG_INFO,
        "m_get_stageresults() : unknown stage %lu of master %lu",
        st_tmp.sg_stageid,MAST_NUM(st_tmp.sg_stageid));
      *rvpp = add_revect(st_tmp.sg_stageid);
      goto errout;
    }

    errlog(LOG_INFO,
      "m_get_stageresults() : accepting stage %lu belonging to master %lu",
      st_tmp.sg_stageid,MAST_NUM(st_tmp.sg_stageid));

    /* from now on treat this orphan as one of our own... */

    if (!(cl_dat = find_clientbyid(cl_tmp.ct_clientid))) {
      cl_dat = x_malloc(sizeof cl_tmp);
      *cl_dat = cl_tmp;
      cl_dat->ct_next = clientlist;
      clientlist = cl_dat;
    }
    else
      release_client(&cl_tmp);
    st_dat = x_malloc(sizeof st_tmp);
    *st_dat = st_tmp;
    st_dat->sg_next = cl_dat->ct_stagelist;
    cl_dat->ct_stagelist = st_dat;
  }

  /* The target file name may be corrected by the slave.
     If the name received from the slave is different, it means
     that the original target file name as delivered wasn't ok, or
     it was left out altogether so that slave had to fill it in. */

  /* staging end time */
  st_dat->sg_endtime = time((time_t *)0);

  DLOG_STAGE(st_dat);

  return st_dat;

  errout:;

  release_client(&cl_tmp);
  release_stage(&st_tmp);
  return (struct stage *)0;
}

static int
m_emit_stagerequest(struct peer *pe_dat,struct client *cl_dat,
  struct stage *st_dat)
{
  int sock;
  struct sockaddr_in *addrp;
  int ok, down;
#ifdef ACK_EVERYTHING
  int msg;
#endif

  DLOG_MSG(("m_emit_stagerequest() : connect to slave"));
  if (!(addrp = tcpconaddr(pe_dat)) ||
      (sock = tcpconnect(addrp)) < 0) {
    pe_dat->pr_up = 0;
    return 0;
  }
  ok = 1;

  /* if error in delivery, try again some other time */
  if ((down = !send_msg(sock,SGD_MSG_STAGE) ||
              !m_put_clientspecs(sock,cl_dat) ||
              !m_put_stagespecs(sock,st_dat)
#ifdef ACK_EVERYTHING
              || (msg = recv_msg(sock)) < 0) ||
      msg != SGD_MSG_OK
#else
      )
#endif
     ) {
    errlog(LOG_ERR,
      "m_emit_stagerequest() : trouble connecting to slave %s (%s)",
      pe_dat->pr_machname,down ? "conn" : "ack");
    if (down) pe_dat->pr_up = 0;
    ok = 0;
  }
  x_close(sock);
  pe_dat->pr_last = time((time_t *)0);

  return ok;
}

static char reportform1[] = "\
source file name            : %s\n\
target file name            : %s%s%s%s\n\
physical source file name   : %s\n\
physical target file name(s): %s\n";
static char reportform2[] = "\
                              %s\n";
static char reportform3[] = "\
file type                   : %s\n\
total file size in bytes    : %lu\n\
bytes transferred so far    : %lu (%d%%)\n\
assigned master             : %s (%s)\n\
preferred server            : %s\n\
assigned server             : %s%s%s%s\n\
stage id number             : %lu\n\
queueing time               : %s\
starting time               : %s%s\
%s                : %s%s\
stager return code          : %s\n\
stage completion status     : %s\n";

static char tersereportform[] = "\
%s %s %s (%d%% done)\n";

/*
 * Send mail to the client about a completed stage.
 */
static int
m_mail_client(struct client *cl_dat,struct stage *st_dat)
{
  char cmd[PATH_MAX + 50];
  char queuebuf[26], startbuf[26], endbuf[26], buf[100];
  char rbuf[PATH_MAX + 2 * MAX_ULSTR + 20];
  struct in_addr saddr;
  FILE *f;
  int ok, ret, dodiv, ccadmin, i;
  struct hostent *he = (struct hostent *)0;

  if (st_dat->sg_serverip) {
    saddr.s_addr = inet_addr(st_dat->sg_serverip);
    he = gethostbyaddr((char *)&saddr,sizeof(struct in_addr),AF_INET);
  }
  sprintf(buf,"%d (%s)",st_dat->sg_retcode,st_retcodestr(st_dat->sg_retcode));
  sprintf(cmd,"%s -t",mailbin);
  if (!(f = popen(cmd,"w"))) return 0;
  dodiv = st_dat->sg_fsiz > 10000000 || st_dat->sg_fdone > 10000000;
  ccadmin = admin && st_dat->sg_status == SGD_STATUS_ERROR;
  fprintf(f,"\
Subject: Staging %s (file %s)\n\
To: %s@%s\n\
%s%s%s\
\n\
Summary of the stage for user %s@%s:\n\
\n",
    st_dat->sg_status == SGD_STATUS_ERROR ? "error" : "successful",
    st_dat->sg_sfname,cl_dat->ct_username,cl_dat->ct_machname,
    ccadmin ? "Cc: " : "",
      ccadmin ? admin : "",
      ccadmin ? " (stager admin)\n" : "",
    cl_dat->ct_username, cl_dat->ct_machname);
  if (st_dat->sg_ptfname) {
    sprintf(rbuf,"%s (",st_dat->sg_ptfname[0].pf_name);
    if (st_dat->sg_ptfname[0].pf_nrec >= 0)
      sprintf(rbuf + strlen(rbuf),"%ld records, ",
        st_dat->sg_ptfname[0].pf_nrec);
    sprintf(rbuf + strlen(rbuf),"%lu bytes)",st_dat->sg_ptfname[0].pf_size);
  }
  else
    strcpy(rbuf,n_a);
  fprintf(f,reportform1,
    st_dat->sg_sfname,
    st_dat->sg_tfname ? st_dat->sg_tfname : none,
    st_dat->sg_otfname ? " (requested as: " : "",
    st_dat->sg_otfname ? st_dat->sg_otfname : "",
    st_dat->sg_otfname ? ")" : "",
    st_dat->sg_psfname ? st_dat->sg_psfname : n_a,
    rbuf);
  for (i = 1; i < st_dat->sg_fnum; i++) {
    sprintf(rbuf,"%s (",st_dat->sg_ptfname[i].pf_name);
    if (st_dat->sg_ptfname[i].pf_nrec >= 0)
      sprintf(rbuf + strlen(rbuf),"%ld records, ",
        st_dat->sg_ptfname[i].pf_nrec);
    sprintf(rbuf + strlen(rbuf),"%lu bytes)",st_dat->sg_ptfname[i].pf_size);
    fprintf(f,reportform2,rbuf);
  }
  fprintf(f,reportform3,
    ftyptb[st_dat->sg_ftyp],
    st_dat->sg_fsiz,
    st_dat->sg_fdone,
      (int)(st_dat->sg_fsiz > 0 ?
        ((dodiv ? st_dat->sg_fdone / 100 : st_dat->sg_fdone) * 100 /
         (dodiv ? st_dat->sg_fsiz / 100 : st_dat->sg_fsiz)) : 0),
    myipaddr, mymachname,
    st_dat->sg_prserver ? st_dat->sg_prserver : none,
    st_dat->sg_serverip ? st_dat->sg_serverip : n_a,
      he ? " (" : "",
      he ? he->h_name : "",
      he ? ")" : "",
    st_dat->sg_stageid,
    strcpy(queuebuf,ctime(&st_dat->sg_queuetime)),
    st_dat->sg_starttime > 0 ?
      strcpy(startbuf,ctime(&st_dat->sg_starttime)) : n_a,
    st_dat->sg_starttime > 0 ? "" : "\n",
    "ending time ",
    st_dat->sg_endtime > 0 ?
      strcpy(endbuf,ctime(&st_dat->sg_endtime)) : n_a,
    st_dat->sg_endtime > 0 ? "" : "\n",
    buf,
    statustb[st_dat->sg_status]);
  /* normally -1 in connection with pclose() means that the descriptor
     to be closed wasn't opened with popen(), but here we know this to be
     true - however, popen can return -1 for legitimate reasons if the
     child terminates without giving exit status, which can happen if
     e.g. a grandchild isn't properly waited for */
  if ((ret = pclose(f)) == 0 || ret == -1) ok = 1;
  else ok = 0;

  return ok;
}

/*
 * Send the status of the file to be staged to the client.
 */
static int
m_put_clientstatus(int sock,int verbose,struct stage *st_dat)
{
  int atend, dodiv, ret, i, doneperc;
  struct in_addr saddr;
  char queuebuf[26], startbuf[26], endbuf[26];
  struct hostent *he;
  size_t bufsiz;
  char *buf;
  char buf2[100];

  DLOG_MSG(("m_put_clientstatus(%d)",sock));
  DLOG_STAGE(st_dat);

  dodiv = st_dat->sg_fsiz > 10000000 || st_dat->sg_fdone > 10000000;
  doneperc = (int)(st_dat->sg_fsiz > 0 ?
              ((dodiv ? st_dat->sg_fdone / 100 : st_dat->sg_fdone) * 100 /
               (dodiv ? st_dat->sg_fsiz / 100 : st_dat->sg_fsiz)) : 0);
  if (verbose) {
    if (st_dat->sg_serverip) {
      saddr.s_addr = inet_addr(st_dat->sg_serverip);
      he = gethostbyaddr((char *)&saddr,sizeof(struct in_addr),AF_INET);
    }
    else
      he = (struct hostent *)0;

    atend = st_dat->sg_status == SGD_STATUS_DONE ||
            st_dat->sg_status == SGD_STATUS_ERROR;
    if (atend)
      sprintf(buf2,"%d (%s)",st_dat->sg_retcode,
        st_retcodestr(st_dat->sg_retcode));
    else {
      sprintf(buf2,n_a);
      st_dat->sg_endtime = time((time_t *)0);
    }
    /* buffer size approximately according to usage (slightly conservative) */
    bufsiz = prlen_stage(st_dat) + strlen(reportform1) +
      st_dat->sg_fnum * strlen(reportform2) + strlen(reportform3) + 50;
    buf = (char *)x_malloc(bufsiz);
    /* almost everything is sent */
    sprintf(buf,reportform1,
      st_dat->sg_sfname,
      st_dat->sg_tfname ? st_dat->sg_tfname : none,
      st_dat->sg_otfname ? " (orig. name: " : "",
      st_dat->sg_otfname ? st_dat->sg_otfname : "",
      st_dat->sg_otfname ? ")" : "",
      st_dat->sg_psfname ? st_dat->sg_psfname : n_a,
      st_dat->sg_ptfname[0].pf_name);
    for (i = 1; i < st_dat->sg_fnum; i++)
      sprintf(buf + strlen(buf),reportform2,
        st_dat->sg_ptfname[i].pf_name);
    sprintf(buf + strlen(buf),reportform3,
      ftyptb[st_dat->sg_ftyp],
      st_dat->sg_fsiz,
      st_dat->sg_fdone, doneperc,
      myipaddr, mymachname,
      st_dat->sg_prserver ? st_dat->sg_prserver : none,
      st_dat->sg_serverip ? st_dat->sg_serverip : n_a,
        he ? " (" : "",
        he ? he->h_name : "",
        he ? ")" : "",
      st_dat->sg_stageid,
      strcpy(queuebuf,ctime(&st_dat->sg_queuetime)),
      st_dat->sg_starttime > 0 ?
        strcpy(startbuf,ctime(&st_dat->sg_starttime)) : n_a,
      st_dat->sg_starttime > 0 ? "" : "\n",
      atend ? "ending time " : "current time",
      st_dat->sg_endtime > 0 ?
        strcpy(endbuf,ctime(&st_dat->sg_endtime)) : n_a,
      st_dat->sg_endtime > 0 ? "" : "\n",
      buf2,
      statustb[st_dat->sg_status]);
  }
  /* terse format */
  else {
    bufsiz = strlen(tersereportform) + strlen(ftyptb[st_dat->sg_ftyp]) +
      strlen(st_dat->sg_sfname) + strlen(statustb[st_dat->sg_status]) + 20;
    buf = (char *)x_malloc(bufsiz);
    sprintf(buf,tersereportform,ftyptb[st_dat->sg_ftyp],
      st_dat->sg_sfname,statustb[st_dat->sg_status],doneperc);
  }

  if (!sendstr(sock,buf)) ret = 0;
  else ret = 1;

  x_free(buf);
  return ret;
}

static void
m_dostatus(int sock,struct client *cl_dat,struct revector **rvpp)
{
  struct stage *st, *stt;
  int n;

  DLOG_MSG(("m_dostatus()"));

  if (!cl_dat->ct_stagelist) {
    /* nothing to send - this client no longer in the queue */
    sendul(sock,(unsigned long)0);
    release_client(cl_dat);
    x_free(cl_dat);
    return;
  }
  /* how many stages going on at the moment */
  for (st = cl_dat->ct_stagelist, n = 0; st; st = st->sg_next) n++;
  if (!sendul(sock,(unsigned long)n))
    return;
  for (st = cl_dat->ct_stagelist, stt = (struct stage *)0; st;
       stt = st, st = stt ? stt->sg_next : cl_dat->ct_stagelist) {
    /* we only have to ask slaves about those stages that are running */
    if (st->sg_status == SGD_STATUS_RUNNING) {
      int slavesock, ok;
      struct sockaddr_in *addrp;
      struct peer *pe;

      if (!(pe = find_slavebyip(st->sg_serverip))) {
        errlog(LOG_ERR,"m_dostatus() : peer list corrupted");
        return;
      }
      if (!(addrp = tcpconaddr(pe)) ||
          (slavesock = tcpconnect(addrp)) < 0) {
        errlog(LOG_INFO,"m_dostatus() : cannot connect to slave %s",
          st->sg_serverip);
        st->sg_status = SGD_STATUS_CONNECT;
        st->sg_starttime = time((time_t *)0);
        pe->pr_up = 0;
      }
      else {
        int down;
#ifdef ACK_EVERYTHING
        int msg;
#endif
        ok = 1;
        if ((down = !send_msg(slavesock,SGD_MSG_STATUS) ||
                    !sendul(slavesock,st->sg_stageid) ||
                    !m_get_stageresults(slavesock,st,rvpp)
#ifdef ACK_EVERYTHING
                    || (msg = recv_msg(slavesock)) < 0) ||
            msg != SGD_MSG_OK
#else
            )
#endif
           ) {
          errlog(LOG_ERR,
            "m_dostatus() : trouble connecting to slave %s (%s)",
            pe->pr_machname,down ? "conn" : "ack");
          ok = 0;
        }
        x_close(slavesock);
        pe->pr_last = time((time_t *)0);
        if (!ok) {
          if (down) {
            pe->pr_up = 0;
            st->sg_status = SGD_STATUS_CONNECT;
          }
          st->sg_starttime = time((time_t *)0);
        }
      }
    }
    if (!m_put_clientstatus(sock,0,st)) return;

    /* if status is error/done and nowait not set, it's our job to
       destroy the stage instance now that the info's been delivered */
    if (!st->sg_nowait &&
        (st->sg_status == SGD_STATUS_DONE ||
         st->sg_status == SGD_STATUS_ERROR)) {
      if (stt)
        stt->sg_next = st->sg_next;
      else
        cl_dat->ct_stagelist = st->sg_next;
      release_stage(st);
      x_free(st);
      st = stt;
    }
  }
  /* if the stagelist for a client got empty, destroy client too */
  if (!cl_dat->ct_stagelist)
    remove_client(cl_dat);
}

/*
 * Master operations.
 */
int
m_master_ops(int sock,struct revector **rvpp)
{
  struct client *cl_dat;
  struct stage *st_dat;
  struct peer *pe_dat;
  unsigned long ultmp;
  int msg;

  DLOG_MSG(("m_master_ops(%d)",sock));

  /* ask what he wants */
  if ((msg = recv_msg(sock)) < 0) return 0;

  switch (msg) {

  /* client asks the status of his ongoing stages */

  case SGD_MSG_STATUS:
    if (!(cl_dat = m_get_clientspecs(sock))) {
/*#if 0*/
#ifndef ACK_EVERYTHING
      send_msg(sock,SGD_MSG_ERROR);
#endif
      return 0;
    }
    m_dostatus(sock,cl_dat,rvpp);
/*#if 0*/
#ifndef ACK_EVERYTHING
    send_msg(sock,SGD_MSG_OK);
#endif
    break;

  /* client asks staging of a file to PIAF server */

  case SGD_MSG_STAGE:
    if (!(cl_dat = m_get_clientspecs(sock)) ||
        !(st_dat = m_get_stagespecs(sock))) {
      char *s;

      /* free the client struct only if it's really a new one,
         i.e., it's got an empty stagelist - we don't want to
         yank a client with ongoing stages */
      if (cl_dat && !cl_dat->ct_stagelist) {
        release_client(cl_dat);
        x_free(cl_dat);
      }
      s = get_peerip(sock,(struct sockaddr_in **)0);
      errlog(LOG_ERR,"m_master_ops() : can't get %s specs (client %s)",
        cl_dat ? "stage" : "client",
        s ? s : "<can't get ip address for peer>");
/*#if 0*/
#ifndef ACK_EVERYTHING
      send_msg(sock,SGD_MSG_ERROR);
#endif
      return 0;
    }
/*#if 0*/
#ifndef ACK_EVERYTHING
    send_msg(sock,SGD_MSG_OK);
#endif

    /* set up miscellaneous things - if it fails, mail client */
    if (!m_misc_setup(cl_dat,st_dat)) {
      st_dat->sg_status = SGD_STATUS_ERROR;
      if (st_dat->sg_nowait) {
        if (!m_mail_client(cl_dat,st_dat))
          errlog(LOG_ERR,"m_master_ops() : can't send mail to %s@%s",
            cl_dat->ct_username,cl_dat->ct_machname);
        if (!cl_dat->ct_stagelist) {
          release_client(cl_dat);
          x_free(cl_dat);
        }
        release_stage(st_dat);
        x_free(st_dat);
      }
      break;
    }

    /* ok, add a new client to the list */
    if (!cl_dat->ct_stagelist) {
      cl_dat->ct_next = clientlist;
      clientlist = cl_dat;
    }

    /* Put the new stage process to the client's stage list.
       One should really check whether the file to be staged is
       the same file as was specified for an earlier stage command,
       so that the same file won't get staged twice, but which
       parameters should one check to get a reliable answer?
       Maybe the client wants to have the same file staged twice
       under different names, but then it would make sense to
       make a link after the first stage is finished, but what
       if the stage requests go to different slaves, which don't
       know about this possibility, etc., etc.  This is really
       something that the master has to decide... but later. */

    st_dat->sg_next = cl_dat->ct_stagelist;
    cl_dat->ct_stagelist = st_dat;

    /* create a string of available slave ip-address/port combos
       so that the slave doing the staging can transfer parts of
       the file to other slaves */
    if (bftp.bf_dotransfer > 0) {
      size_t len;
      char *s;
      for (pe_dat = slavelist, len = 0; pe_dat; pe_dat = pe_dat->pr_next)
        if (pe_dat->pr_up)
          len += strlen(pe_dat->pr_machname) + strlen(pe_dat->pr_ipaddr) +
                 prlen_long((long)pe_dat->pr_conport) + 3;
      if (len > 0) {
        s = (char *)x_malloc(len);
        for (pe_dat = slavelist, len = 0; pe_dat; pe_dat = pe_dat->pr_next)
          if (pe_dat->pr_up) {
            sprintf(&s[len],"%s%s:%s:%d",len > 0 ? " " : "",
              pe_dat->pr_machname,pe_dat->pr_ipaddr,pe_dat->pr_conport);
            len += strlen(&s[len]);
          }
        st_dat->sg_slavelist = s;
      }
      else
        st_dat->sg_slavelist = (char *)0;
    }
    else
      st_dat->sg_slavelist = (char *)0;

    /* start talking to the slave - from now on we don't
       assume a working connection with the client any more */

    if (!st_dat->sg_serverip ||
        !(pe_dat = find_slavebyip(st_dat->sg_serverip)) ||
        !pe_dat->pr_up) {

      /* ugh, the requested slave isn't running */
      st_dat->sg_status = SGD_STATUS_PENDING;
      if (st_dat->sg_serverip && pe_dat) pe_dat->pr_up = 0;
      break;
    }

    /* ok, send staging request to slave - if we get
       an error here, it doesn't necessarily mean that
       the server is down, though (why the heck not??
       I forget what's the original idea behind that assertion) */
    if (m_emit_stagerequest(pe_dat,cl_dat,st_dat))
      st_dat->sg_status = SGD_STATUS_RUNNING;
    else
      st_dat->sg_status = SGD_STATUS_PENDING;
    break;

  /* slave tells us that it's ready for business, find its
     attributes and add to the slavelist */

  case SGD_MSG_INIT:
    {
      char *s;
      struct sockaddr_in *addrp;

      /* the port to use when connecting to the slave */
      if (!recvul(sock,&ultmp)) return 0;

      /* get the slave ip address */
      if (!(s = get_peerip(sock,&addrp))) {
        errlog(LOG_ERR,"m_master_ops() : can't get peername (MSG_INIT)");
        return 0;
      }

      /* if we have this slave already in the list, it means that it
         has gone down at one point and is now reconnecting, or it's
         just saying hello to us, else if this is the first connect,
         create new instance */
      if (!(pe_dat = find_slavebyip(s))) {
        struct peer pe_tmp;
        struct hostent *he;

        if (!(he = gethostbyaddr((char *)&addrp->sin_addr,
              sizeof(struct in_addr),AF_INET))) {
          errlog(LOG_ERR,"m_master_ops() : gethostbyaddr() error (peer %s)",s);
          return 0;
        }
        pe_tmp = pe_init;
        pe_tmp.pr_ipaddr = dupstr(s);
        pe_tmp.pr_machname = dupstr(he->h_name);
        pe_tmp.pr_next = slavelist;
        pe_dat = (struct peer *)x_malloc(sizeof pe_dat[0]);
        *pe_dat = pe_tmp;
        slavelist = pe_dat;
      }
      pe_dat->pr_conport = (int)ultmp;
      pe_dat->pr_last = time((time_t *)0);
      pe_dat->pr_up = 1;
    }
    break;

  /* slave reports a successful staging operation or it tells us to
     forget it because another master has already taken care of it */

  case SGD_MSG_OK:
  case SGD_MSG_FORGET:
    if (msg == SGD_MSG_OK) {
      if (!(st_dat = m_get_stageresults(sock,(struct stage *)0,rvpp))) {
        if (!*rvpp)
          errlog(LOG_ERR,"m_master_ops() : can't get stageresults");
        return 0;
      }
      pe_dat = find_slavebyip(st_dat->sg_serverip);
      cl_dat = find_clientbystageid(st_dat->sg_stageid);
      if (st_dat->sg_nowait && !m_mail_client(cl_dat,st_dat)) {
        errlog(LOG_ERR,"m_master_ops() : can't send mail to %s@%s",
          cl_dat->ct_username,cl_dat->ct_machname);
        pe_dat->pr_last = time((time_t *)0);
        pe_dat->pr_up = 1;
        return 0;
      }
    }
    else {
      if (!recvul(sock,&ultmp)) break;
      st_dat = find_stagebyid(ultmp,&cl_dat);
      pe_dat = find_slavebyip(st_dat->sg_serverip);
    }

    pe_dat->pr_last = time((time_t *)0);
    pe_dat->pr_up = 1;

    /* update log file */
    if (msg == SGD_MSG_OK)
      joblog(cl_dat,st_dat);

    /* if we should wait for the client to query the status */
    if (!st_dat->sg_nowait) break;

    /* remove the stage information and possibly client as well */
    remove_stage(cl_dat,st_dat);

    break;

  /* one of our slaves telling that he's still alive */

  case SGD_MSG_PING:
    {
      char *s;

      /* get the slave ip address */
      if (!(s = get_peerip(sock,(struct sockaddr_in **)0))) {
        errlog(LOG_ERR,"m_master_ops() : can't get peername (MSG_PING)");
        return 0;
      }
      if (!(pe_dat = find_slavebyip(s))) {
        errlog(LOG_ERR,"m_master_ops() : ping from unknown slave %s",s);
        return 0;
      }
      pe_dat->pr_last = time((time_t)0);
      pe_dat->pr_up = 1;
    }
    break;

  default:
    errlog(LOG_ERR,"m_master_ops() : strange message (%d)",msg);
    return 0;
  }

  return 1;
}

int
m_flush_pending(long *timeout)
{
  struct client *cl, *cll;
  struct stage *st, *stt;
  int ret;
  time_t now = time((time_t)0);

  DLOG_MSG(("m_flush_pending()"));

  *timeout = stagetimeout;
  ret = 0;
  for (cl = clientlist, cll = (struct client *)0; cl;
       cll = cl, cl = cll ? cll->ct_next : clientlist) {
    for (st = cl->ct_stagelist, stt = (struct stage *)0; st;
         stt = st, st = stt ? stt->sg_next : cl->ct_stagelist) {
      struct peer *pe;
      /* in fact, according to ANSI the time difference
         should be calculated using difftime() function
         which returns a double value, but in unix we know
         that time() always returns the number of seconds
         since the epoch, which we can directly use in
         calculations thus avoiding loading the floating
         point libraries */
      time_t tdiff = now - st->sg_starttime;
      int i;

      if (st->sg_status == SGD_STATUS_PENDING ||
          st->sg_status == SGD_STATUS_CONNECT) {

        /* if all slaves were down, see if one has come up */
        if (!st->sg_serverip) {
          if (st->sg_prserver)
            st->sg_serverip = find_peeripbyname(st->sg_prserver);
          if (!st->sg_serverip) {
            int n = rand() % st->sg_fnum;
            st->sg_serverip = find_peeripbypath(st->sg_ptfname[n].pf_name);
          }
          if (!st->sg_serverip)
            for (i = 0; i < st->sg_fnum && !st->sg_serverip; i++)
              st->sg_serverip = find_peeripbypath(st->sg_ptfname[i].pf_name);
        }
        /* ok, now we hopefully have an ip address - find the
           corresponding peer if it exists in our list */
        if (st->sg_serverip)
          pe = find_slavebyip(st->sg_serverip);
        else
          pe = (struct peer *)0;

        /* if the pending time is up, find any running slave */
        if ((!pe || !pe->pr_up) && tdiff >= pendingtime) {
          int m, mm;
          for (pe = slavelist, m = 0; pe; pe = pe->pr_next)
            if (pe->pr_up) m++;
          if (m > 0) {
            mm = rand() % m;
            for (pe = slavelist, m = 0; pe; pe = pe->pr_next)
              if (pe->pr_up && m++ == mm) {
                errlog(LOG_INFO,
              "m_flush_pending() : reassigning stage %lu from slave %s to %s",
                  st->sg_stageid,st->sg_serverip ? st->sg_serverip : n_a,
                  pe->pr_ipaddr);
                if (st->sg_serverip) x_free(st->sg_serverip);
                st->sg_serverip = dupstr(pe->pr_ipaddr);
                break;
              }
          }
        }

        if (pe && pe->pr_up && m_emit_stagerequest(pe,cl,st)) {
          st->sg_status = SGD_STATUS_RUNNING;
          if (stagetimeout >= 0) ret = 1;
        }
        else {
          if (pendingtime - tdiff < *timeout)
            *timeout = pendingtime - tdiff;
          if (*timeout < 0) *timeout = 0;
          ret = 1;
        }
      }

      /* is the timeout disabled? */

      else if (stagetimeout < 0)
        continue;

      /* time to check if the slave is still up with our stage
         or if we have a finished stage waiting for the client
         query but whose timeout has passed */

      else if (tdiff >= stagetimeout) {
        /* still running? */
        if (st->sg_status == SGD_STATUS_RUNNING) {
          pe = find_slavebyip(st->sg_serverip);
          if (pe->pr_up && ping_peer(pe)) {
            pe->pr_last = now;
            continue;
          }
          pe->pr_up = 0;
          st->sg_status = SGD_STATUS_CONNECT;
          errlog(LOG_INFO,"m_flush_pending() : no response pinging slave %s",
            pe->pr_machname);
        }
        /* nope, the client hasn't asked for its status - just zap it */
        else {
          if (stt)
            stt->sg_next = st->sg_next;
          else
            cl->ct_stagelist = st->sg_next;
          release_stage(st);
          x_free(st);
          st = stt;
        }
      }

      /* or just update the timeout for next check */

      else {
        if (stagetimeout - tdiff < *timeout)
          *timeout = stagetimeout - tdiff;
        if (*timeout < 0) *timeout = 0;
        ret = 1;
      }
    }

    /* if the stagelist for a client got empty, destroy client too */
    if (!cl->ct_stagelist) {
      if (cll)
        cll->ct_next = cl->ct_next;
      else
        clientlist = cl->ct_next;
      release_client(cl);
      x_free(cl);
      cl = cll;
    }
  }

  return ret;
}
