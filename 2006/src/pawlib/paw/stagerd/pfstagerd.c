/*
 * $Id: pfstagerd.c,v 1.1 1996/04/02 22:00:27 thakulin Exp $
 *
 * $Log: pfstagerd.c,v $
 * Revision 1.1  1996/04/02 22:00:27  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :  2.06/13 24/11/94  11.44.02  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * pfstagerd.c
 *
 * This daemon runs in PIAF machines, and answers PIAF requests from
 * PIAF sessions for staging of a file or tape on disk.  It accepts file
 * or tape identifiers or FATMEN paths as input and uses CLIO to transfer
 * the actual file.  In principle one should check for proper user
 * authentication, but other network protocols like telnet are
 * disabled in PIAF servers so the call can only come from an
 * already authenticated client.
 *
 * This code requires an ANSI C compiler with POSIX.1 extensions and
 * 4.3BSD networking libraries.
 *
 * 4.3BSD specifics : select(), socket(), bind(), accept(), send(),
 *                    recv(), gethostname(), gethostbyaddr(), getpeername(),
 *                    getsockname(), htons(), htonl(), ntohs(), ntohl(),
 *                    inet_ntoa()
 * POSIX.1 specifics: waitpid(), sigaction()
 * POSIX.2 specifics: popen(), pclose()
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    23/8/94
 *
 *************************************************************************/

#include "pfstagerd.h"

int master;         /* are we a master or slave */

char *myname;       /* name of the process (argv[0]) */
char *myipaddr;     /* ip address in dotted decimal */
char *mymachname;   /* host machine name */

char *mailbin;
char *ftpbin;
char *stagerbin;
char *fatmenbin;
char *dpmbin;
char *rmbin;
char *nfspath;
char *tmppath;
struct cliopar bclio;
struct ftppar bftp;
struct tcppar tcpparam;

char *syslogchn;          /* channel for the syslog facility */
char *errlogname;         /* name of the error log file */
static FILE *errlogf;
char *joblogname;         /* name of the job log file */
static FILE *joblogf;
char *dumpname;           /* name of the status dump file */

int pendingtime;          /* how many seconds a stagerequest is allowed to
                             be pending due to the requested server being down
                             before it's directed to another machine */
int selecttimeout;        /* how many seconds to spend in select() at a time */
int stagetimeout;         /* how many minutes the master waits for a slave
                             to finish staging before reissuing it */
int pinginterval;         /* how often idle slaves say hello to masters */
int stagecancel;          /* the maximum time actual staging operation
                             (file transfer) may take */
int userumask;            /* the user's default umask */

char *admin;              /* mailing address of the pfstagerd admin */

char *piafhome;           /* the directory for piaf work relative to user's
                             home directory */

char *configfile;

int updateconfig;         /* tells whether the config file should be updated */
int doshutdown;           /* tells that we should exit in an orderly manner */

unsigned long master_id;

struct client *clientlist;
struct peer *selectlist;    /* used by both slaves and masters
                               (these peers are used in select() loop) */
struct peer *slavelist;     /* only for masters */
struct revector *revectorlist;  /* only for slaves */
struct transfer *transferlist;  /* - " - */
/* these structs are init'ed to the correct zero values by the
   compilers so we can safely use them when initializing auto
   variables of the same type */
static struct cliopar clio_init = {"", "", "U", "", 200};
static struct ftppar ftp_init = {-1, -1, -1, -1};
static struct tcppar tcp_init = {-1, -1, 1};
struct client cl_init;
struct stage st_init;
struct peer pe_init;

static int stdstreams = 1;

char *ftyptb[] = {"","local","tape","fatmen"};
char *statustb[] =
  {"","error","completed","running","pending","broken connection"};

static void
update_errlog(int lev,char *s,va_list arg_ptr)
{
  if (errlogname) {
    /* try to be conservative with the buffer allocation */
    char buf[50 + 3 * PATH_MAX];
    int n;

    n = vsprintf(buf,s,arg_ptr);
    if (errno)
      n += sprintf(buf + n," : (%s)",strerror(errno));
    sprintf(buf + n,"\n");
    if (errlogf) {
      char ctimbuf[26], *lstr;
      time_t tim = time((time_t)0);

      strcpy(ctimbuf,ctime(&tim));
      ctimbuf[24] = '\0';
      switch (lev) {
      case LOG_INFO:
        lstr = "LOG_INFO";
        break;
      case LOG_ERR:
        lstr = "LOG_ERR";
        break;
      default:
        lstr = "LOG_?";
      }
      fprintf(errlogf,"%s %s %ld %s ",ctimbuf,myname,(long)getpid(),lstr);
      fputs(buf,errlogf);
    }
    else
      syslog(lev,buf);
  }
}

void
errlog(int lev,char *s,...)
{
  va_list arg_ptr;

#ifdef DEBUG_LOG
  va_start(arg_ptr,s);
  dvlog_error(s,arg_ptr);
  va_end(arg_ptr);
#endif
  va_start(arg_ptr,s);
  update_errlog(lev,s,arg_ptr);
  va_end(arg_ptr);
}

/*ARGSUSED*/
static void
prjoblog(int lev,char *s,...)
{
  va_list arg_ptr;

  if (!joblogf) return;
  va_start(arg_ptr,s);
  vfprintf(joblogf,s,arg_ptr);
  va_end(arg_ptr);
}

void
joblog(struct client *cl_dat,struct stage *st_dat)
{
  int i;
  size_t pssiz;
  char *psbuf;
#ifdef PRETTY_LOGTIME
  char queuebuf[26], startbuf[26], endbuf[26];
#endif
  void (*func)(int,char *,...);

  if (!joblogname) return;

  if (joblogf)
    func = prjoblog;
  else
    func = (void (*)(int,char *,...))syslog;
#ifdef PRETTY_LOGTIME
  strcpy(queuebuf,ctime(&st_dat->sg_queuetime));
  queuebuf[24] = '\0';
  strcpy(startbuf,ctime(&st_dat->sg_starttime));
  startbuf[24] = '\0';
  strcpy(endbuf,ctime(&st_dat->sg_endtime));
  endbuf[24] = '\0';
#endif
  for (i = 0, pssiz = 0; i < st_dat->sg_fnum; i++)
    pssiz += strlen(st_dat->sg_ptfname[i].pf_name) + 3 +
      prlen_long(st_dat->sg_ptfname[i].pf_nrec) +
      prlen_ulong(st_dat->sg_ptfname[i].pf_size);
  psbuf = (char *)x_malloc(pssiz);
  for (i = 0; i < st_dat->sg_fnum; i++) {
    if (i > 0) strcat(psbuf,",");
    sprintf(psbuf + strlen(psbuf),"%s:%ld:%lu",st_dat->sg_ptfname[i].pf_name,
      st_dat->sg_ptfname[i].pf_nrec,st_dat->sg_ptfname[i].pf_size);
  }
  (*func)(LOG_INFO,
#ifdef PRETTY_LOGTIME
    "%lu %s %s %s %lu %s %s %s %s %d %lu %lu %s %s %s %s %s %d %d\n",
#else
    "%lu %s %s %s %lu %s %s %s %s %d %lu %lu %s %s %lu %lu %lu %d %d\n",
#endif
    cl_dat->ct_clientid, cl_dat->ct_username,
    cl_dat->ct_machname, cl_dat->ct_ipaddr,
    st_dat->sg_stageid, st_dat->sg_sfname,
    st_dat->sg_tfname,
    st_dat->sg_otfname ? st_dat->sg_otfname : "*",
    psbuf, st_dat->sg_ftyp,
    st_dat->sg_fsiz, st_dat->sg_fdone,
    st_dat->sg_masterip, st_dat->sg_serverip,
#ifdef PRETTY_LOGTIME
    queuebuf, startbuf, endbuf,
#else
    (unsigned long)st_dat->sg_queuetime,
    (unsigned long)st_dat->sg_starttime,
    (unsigned long)st_dat->sg_endtime,
#endif
    st_dat->sg_retcode, st_dat->sg_status);
  x_free(psbuf);
}

void
fatal(int docore,char *s,...)
{
  va_list arg_ptr;

  va_start(arg_ptr,s);
  update_errlog(LOG_ERR,s,arg_ptr);
  va_end(arg_ptr);
  va_start(arg_ptr,s);
  if (stdstreams) {
    fprintf(stderr,"%s : ",myname);
    vfprintf(stderr,s,arg_ptr);
    if (errno)
      fprintf(stderr," : (%s)",strerror(errno));
    fprintf(stderr,"\n");
    va_end(arg_ptr);
    fflush(stderr);
  }
  if (docore)
    abort();
  else
    exit(1);
}

/*
 * Finds out our own ip address and host name.
 */
static void
set_myipaddr()
{
  struct hostent *he;
  char *tmp, buf[MAXHOSTNAMELEN + 1];

  if (gethostname(buf,sizeof buf) < 0)
    fatal(0,"set_myipaddr() : gethostname() fails");
  mymachname = dupstr(buf);
  if (!(he = gethostbyname(mymachname)))
    fatal(0,"set_myipaddr() : gethostbyname() fails");
  tmp = inet_ntoa(*(struct in_addr *)he->h_addr);
  myipaddr = dupstr(tmp);
}

void
get_child_stat(pid_t pid,int wstat,struct stage *st_dat)
{
  if (WIFEXITED(wstat)) {
    DLOG_MSG(("get_child_stat() : WIFEXITED"));
    /* if we don't find the stage, it's probably ok - this
       was most likely just some other child finishing
       (started by system() or popen()) */
    if (!st_dat)
      if (!(st_dat = find_stagebypid(pid))) {
        remove_transferbypid(pid);
        return;
      }
    st_dat->sg_retcode = (char)WEXITSTATUS(wstat);
    if (st_dat->sg_retcode != 0)
      st_dat->sg_status = SGD_STATUS_ERROR;
    else
      st_dat->sg_status = SGD_STATUS_DONE;
    DLOG_STAGE(st_dat);
  }
  else if (WIFSIGNALED(wstat)) {
    errlog(LOG_ERR,"get_child_stat() : WIFSIGNALED pid=%ld, sig=%ld%s",
      (long)pid,(long)WTERMSIG(wstat),
#ifdef WCOREDUMP
      WCOREDUMP(wstat) ? ", core dumped" : ", no core file"
#else
      ""
#endif
      );
    if (!st_dat)
      if (!(st_dat = find_stagebypid(pid))) {
        remove_transferbypid(pid);
        return;
      }
    st_dat->sg_retcode = SGD_RET_SIGNAL;
    st_dat->sg_status = SGD_STATUS_ERROR;
  }
  else if (WIFSTOPPED(wstat))
    errlog(LOG_ERR,"get_child_stat() : WIFSTOPPED pid=%ld, sig=%ld",
      (long)pid,(long)WSTOPSIG(wstat));
  else {
    errlog(LOG_ERR,"get_child_stat() : strange process termination pid=%ld",
      (long)pid);
    if (!st_dat)
      if (!(st_dat = find_stagebypid(pid))) {
        remove_transferbypid(pid);
        return;
      }
    st_dat->sg_retcode = SGD_RET_UNKNOWN;
    st_dat->sg_status = SGD_STATUS_ERROR;
  }
}

static void
sig_handler(int sig)
{
  int wstat;
  pid_t pid;
  int oerrno = errno;
  errno = 0;

  switch (sig) {
  case SIGUSR1:
    errlog(LOG_INFO,"sig_handler() : SIGUSR1");
    dumpstatus();
    break;
  case SIGTERM:
    errlog(LOG_INFO,"sig_handler() : SIGTERM");
    doshutdown = 1;
    break;
  case SIGHUP:
    errlog(LOG_INFO,"sig_handler() : SIGHUP");
    updateconfig = 1;
    break;
  case SIGPIPE:
    errlog(LOG_INFO,"sig_handler() : SIGPIPE");
    break;
  case SIGCHLD:
    DLOG_MSG(("sig_handler() : SIGCHLD"));
    for (;;) {
      errno = 0;
      pid = waitpid(-1,&wstat,WNOHANG | WUNTRACED);
      DLOG_MSG(("sig_handler() : pid=%ld",(long)pid));
      /* when pid == 0, it means that there are no more
         processes in the queue */
      if (pid == 0)
        break;
      else if (pid < 0) {
/*        errlog(LOG_ERR,"sig_handler() : pid=%ld",(long)pid);*/
        if (errno == EINTR) continue;
        break;
      }
      else
        get_child_stat(pid,wstat,(struct stage *)0);
    }
    break;
  default:
    errlog(LOG_INFO,"sig_handler() : strange signal (%d)",sig);
  }
  errno = oerrno;
}

static int
syslogfacility(char *buf,int *fac)
{
  if (streql(buf,"local0")) *fac = LOG_LOCAL0;
  else if (streql(buf,"local1")) *fac = LOG_LOCAL1;
  else if (streql(buf,"local2")) *fac = LOG_LOCAL2;
  else if (streql(buf,"local3")) *fac = LOG_LOCAL3;
  else if (streql(buf,"local4")) *fac = LOG_LOCAL4;
  else if (streql(buf,"local5")) *fac = LOG_LOCAL5;
  else if (streql(buf,"local6")) *fac = LOG_LOCAL6;
  else if (streql(buf,"local7")) *fac = LOG_LOCAL7;
  else return 0;
  return 1;
}

static int
set_bin(char **bin,char **blist,int n)
{
  int i;
  struct stat sbuf;

  if (*bin) {
    if (stat(*bin,&sbuf) == 0) return 1;
    errno = 0;
    return 0;
  }
  for (i = 0; i < n; i++)
    if (stat(blist[i],&sbuf) == 0) {
      *bin = dupstr(blist[i]);
      errno = 0;
      return 1;
    }
  errno = 0;
  return 0;
}

static int
bufscale(int c)
{
  return c == 'K' ? 1024 : c == 'M' ? 1024 * 1024 : 1;
}

static int
timescale(int c)
{
  return c == 'm' ? 60 : c == 'h' ? 60 * 60 : 1;
}

static void
read_config_file()
{
  static char syntax[] =
    "read_config_file() : config file syntax error on line %d (\"%s\")";
  char *buf, *buf2;
  int line, i1, i2, i3, i4, c;
  char c1, c2, c3;
  unsigned int ui;
  struct stat sbuf;
  FILE *f;
  struct peer *pe_dat;
  char *c_mailbin = (char *)0;
  char *c_ftpbin = (char *)0;
  char *c_stagerbin = (char *)0;
  char *c_fatmenbin = (char *)0;
  char *c_dpmbin = (char *)0;
  char *c_rmbin = (char *)0;
  char *c_joblogname = (char *)0;
  char *c_errlogname = (char *)0;
  char *c_syslogchn = (char *)0;
  char *c_nfspath = (char *)0;
  char *c_tmppath = (char *)0;
  char *c_dumpname = (char *)0;
  char *c_admin = (char *)0;
  char *c_piafhome = (char *)0;
  struct cliopar c_bclio;
  struct ftppar  c_bftp;
  struct tcppar  c_tcpparam;
  int c_syslogf;
  int c_pendingtime = -1;
  int c_selecttimeout = -1;
  int c_stagetimeout = -1;
  int c_pinginterval = -1;
  int c_stagecancel = -1;
  int c_userumask = -1;
  struct peer *c_selectlist = (struct peer *)0;

  c_bclio = clio_init;
  c_bftp = ftp_init;
  c_tcpparam = tcp_init;
  if (stat(configfile,&sbuf) < 0) {
    /* if we are here because we got a signal to update,
       don't die (because we probably have clients running)
       but leave the old config as is */
    if (updateconfig) {
      updateconfig = 0;
      return;
    }
    fatal(0,"read_config_file() : error doing stat on %s",configfile);
  }
  if (!(f = fopen(configfile,"r"))) {
    if (updateconfig) {
      updateconfig = 0;
      return;
    }
    fatal(0,"read_config_file() : error opening %s",configfile);
  }
  /* make sure we don't run out of buffer space for fscanf */
  buf = (char *)x_malloc(sbuf.st_size + 1);
  buf2 = (char *)x_malloc(sbuf.st_size + 1);
  for (line = 1; !feof(f) && !ferror(f); line++) {
    /* leading whitespace */
    fscanf(f,"%*[ \t]");
    /* empty line or eof */
    if ((c = fgetc(f)) == '\n' || feof(f)) continue;
    ungetc(c,f);
    /* comment? */
    if (c != '#') {
      fscanf(f,"%s",buf);
      if (streql(buf,"master")) {
        if (!master) {
          if (fscanf(f,"%s%d%d",buf,&i1,&i2) != 3) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          pe_dat = (struct peer *)x_malloc(sizeof pe_dat[0]);
          *pe_dat = pe_init;
          /* first see if the parameter is dotted decimal, then host name */
          if (inet_addr(buf) != (unsigned long)-1)
            pe_dat->pr_ipaddr = dupstr(buf);
          else {
            struct hostent *he = gethostbyname(buf);
            if (!he) {
              if (updateconfig) goto noupdate;
              fatal(0,"read_config_file() : host name %s on line %d unknown",
                buf,line);
            }
            pe_dat->pr_ipaddr =
              dupstr(inet_ntoa(*(struct in_addr *)he->h_addr));
          }
          pe_dat->pr_conport = i1;
          pe_dat->pr_lisport = i2;
          pe_dat->pr_keep = 0;
          pe_dat->pr_next = c_selectlist;
          c_selectlist = pe_dat;
        }
      }
      else if (streql(buf,"slave") || streql(buf,"client")) {
        if (master) {
          if (fscanf(f,"%d",&i1) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          pe_dat = (struct peer *)x_malloc(sizeof pe_dat[0]);
          *pe_dat = pe_init;
          pe_dat->pr_ipaddr = (char *)0;
          pe_dat->pr_lisport = i1;
          pe_dat->pr_keep = buf[0] == 's' ? 1 : 0;
          pe_dat->pr_next = c_selectlist;
          c_selectlist = pe_dat;
        }
      }
      else if (streql(buf,"pending")) {
        if (master) {
          c1 = ' ';
          if (fscanf(f,"%d%c",&i1,&c1) < 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_pendingtime = i1 * timescale(c1);
        }
      }
      else if (streql(buf,"pollcon")) {
        if (!master) {
          c1 = ' ';
          if (fscanf(f,"%d%c",&i1,&c1) < 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_selecttimeout = i1 * timescale(c1);
        }
      }
      else if (streql(buf,"stagetimeout")) {
        if (master) {
          c1 = ' ';
          if (fscanf(f,"%d%c",&i1,&c1) < 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_stagetimeout = i1 * timescale(c1);
        }
      }
      else if (streql(buf,"pinginterval")) {
        if (!master) {
          c1 = ' ';
          if (fscanf(f,"%d%c",&i1,&c1) < 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_pinginterval = i1 * timescale(c1);
        }
      }
      else if (streql(buf,"stagecancel")) {
        if (!master) {
          c1 = ' ';
          if (fscanf(f,"%d%c",&i1,&c1) < 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_stagecancel = i1 * timescale(c1);
        }
      }
      else if (streql(buf,"umask")) {
        if (!master) {
          if (fscanf(f,"%o",&ui) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_userumask = (int)ui;
        }
      }
      else if (streql(buf,"mail")) {
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (c_mailbin) x_free(c_mailbin);
        c_mailbin = dupstr(buf);
      }
      else if (streql(buf,"ftp")) {
        if (!master) {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_ftpbin) x_free(c_ftpbin);
          c_ftpbin = dupstr(buf);
        }
      }
      else if (streql(buf,"stager")) {
        if (!master) {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_stagerbin) x_free(c_stagerbin);
          c_stagerbin = dupstr(buf);
        }
      }
      else if (streql(buf,"fatmen")) {
        if (!master) {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_fatmenbin) x_free(c_fatmenbin);
          c_fatmenbin = dupstr(buf);
        }
      }
      else if (streql(buf,"dpmanager")) {
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (c_dpmbin) x_free(c_dpmbin);
        c_dpmbin = dupstr(buf);
      }
      else if (streql(buf,"rmutil")) {
        if (!master) {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_rmbin) x_free(c_rmbin);
          c_rmbin = dupstr(buf);
        }
      }
      else if (streql(buf,"merrlog") || streql(buf,"serrlog")) {
        if (master && buf[0] == 'm' || !master && buf[0] == 's') {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_errlogname) x_free(c_errlogname);
          c_errlogname = dupstr(buf);
        }
      }
      else if (streql(buf,"mjoblog") || streql(buf,"sjoblog")) {
        if (master && buf[0] == 'm' || !master && buf[0] == 's') {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_joblogname) x_free(c_joblogname);
          c_joblogname = dupstr(buf);
        }
      }
      else if (streql(buf,"syslogd")) {
        if (!master) fscanf(f,"%*s");
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (!syslogfacility(buf,&c_syslogf)) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        else {
          if (c_syslogchn) x_free(c_syslogchn);
          c_syslogchn = dupstr(buf);
        }
      }
      else if (streql(buf,"nfspath")) {
        size_t n;
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (c_nfspath) x_free(c_nfspath);
        c_nfspath = (char *)x_malloc(strlen(buf) + 2);
        strcpy(c_nfspath,buf);
        n = strlen(c_nfspath);
        if (c_nfspath[n - 1] != '/') c_nfspath[n] = '/';
      }
      else if (streql(buf,"tmppath")) {
        size_t n;
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (c_tmppath) x_free(c_tmppath);
        c_tmppath = (char *)x_malloc(strlen(buf) + 2);
        strcpy(c_tmppath,buf);
        n = strlen(c_tmppath);
        if (c_tmppath[n - 1] != '/') c_tmppath[n] = '/';
      }
      else if (streql(buf,"dumpfile")) {
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (c_dumpname) x_free(c_dumpname);
        c_dumpname = dupstr(buf);
      }
      else if (streql(buf,"admin")) {
        if (master) {
          if (fscanf(f,"%s",buf) != 1) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          if (c_admin) x_free(c_admin);
          c_admin = dupstr(buf);
        }
      }
      else if (streql(buf,"piafhome")) {
        size_t n;
        if (fscanf(f,"%s",buf) != 1) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        if (c_piafhome) x_free(c_piafhome);
        c_piafhome = (char *)x_malloc(strlen(buf) + 2);
        strcpy(c_piafhome,buf);
        n = strlen(c_piafhome);
        if (c_piafhome[n - 1] != '/') c_piafhome[n] = '/';
      }
      else if (streql(buf,"builtinclio")) {
        if (!master) {
          c1 = ' ';
          if (fscanf(f,"%d%s%d%s%d%d%c",&i1,buf,&i2,buf2,&i3,&i4,&c1) < 6) {
            if (updateconfig) goto noupdate;
            fatal(0,syntax,line,buf);
          }
          c_bclio.bc_version = i1;
          if (c_bclio.bc_server) x_free(c_bclio.bc_server);
          c_bclio.bc_server = dupstr(buf);
          c_bclio.bc_port = i2;
          if (c_bclio.bc_login) x_free(c_bclio.bc_login);
          c_bclio.bc_login = dupstr(buf2);
          c_bclio.bc_retries = i3;
          c_bclio.bc_wait = i4 * timescale(c1);
        }
      }
      else if (streql(buf,"builtinftp")) {
        int rem;
        c3 = ' ';
        if (fscanf(f,"%d%c%d%d%c%d%c",&i1,&c1,&i2,&i3,&c2,&i4,&c3) < 6 ||
            i1 <= 0) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        c_bftp.bf_bufsize = i1 * bufscale(c1);
        c_bftp.bf_stripenum = i2;
        c_bftp.bf_minsize = i3 * bufscale(c2);
        c_bftp.bf_dotransfer = i4 * bufscale(c3);
        if (c_bftp.bf_bufsize < c_bftp.bf_dotransfer)
          c_bftp.bf_bufsize = c_bftp.bf_dotransfer;
        rem = c_bftp.bf_bufsize % c_bftp.bf_dotransfer;
        if (rem > 0) c_bftp.bf_bufsize += c_bftp.bf_dotransfer - rem;
      }
      else if (streql(buf,"tcpparam")) {
        if (fscanf(f,"%d%c%d%c%d",&i1,&c1,&i2,&c2,&i3) < 5) {
          if (updateconfig) goto noupdate;
          fatal(0,syntax,line,buf);
        }
        c_tcpparam.tc_rcvbuf = i1 * bufscale(c1);
        c_tcpparam.tc_sndbuf = i2 * bufscale(c2);
        c_tcpparam.tc_nodelay = i3;
      }
      else {
        if (updateconfig) goto noupdate;
        fatal(0,syntax,line,buf);
      }
    }
    /* skip the rest of the line */
    fscanf(f,"%*[^\n]");
    /* swallow the newline */
    fgetc(f);
  }

  /* figure out all the necessary binaries */
  {
    /* all the standard places where various utilities could be */
    static char *sendmail[] = {"/usr/lib/sendmail","/usr/sbin/sendmail",
      "/usr/bin/sendmail","/bin/sendmail","/etc/sendmail",
      "/usr/local/bin/sendmail"};
    static char *pfget[] = {"/usr/local/bin/pfget"};
    static char *fmget[] = {"/usr/local/bin/fmget"};
    static char *rmutil[] = {"/usr/local/bin/pfrm"};
    static char *ftp[] = {"/usr/ucb/ftp","/usr/bsd/ftp","/usr/bin/ftp",
      "/bin/ftp"};
    static char *stager[] = {"/usr/local/bin/clioread"};
    static char nobin[] = "read_config_file() : no %sbin (%s)";
    static char null[] = "NULL";

    if (!set_bin(&c_mailbin,sendmail,SIZE(sendmail))) {
      if (updateconfig) goto noupdate;
      fatal(0,nobin,"mail",c_mailbin ? c_mailbin : null);
    }
    if (!set_bin(&c_dpmbin,pfget,SIZE(pfget))) {
      if (updateconfig) goto noupdate;
      fatal(0,nobin,"dpm",c_dpmbin ? c_dpmbin : null);
    }
    if (!master) {
      if (!set_bin(&c_fatmenbin,fmget,SIZE(fmget))) {
        if (updateconfig) goto noupdate;
        fatal(0,nobin,"fatmen",c_fatmenbin ? c_fatmenbin : null);
      }
      if (c_bftp.bf_bufsize > 0) {
        if (!set_bin(&c_rmbin,rmutil,SIZE(rmutil))) {
          if (updateconfig) goto noupdate;
          fatal(0,nobin,"rm",c_rmbin ? c_rmbin : null);
        }
      }
      else if (!set_bin(&c_ftpbin,ftp,SIZE(ftp))) {
        if (updateconfig) goto noupdate;
        fatal(0,nobin,"ftp",c_ftpbin ? c_ftpbin : null);
      }
      if (!c_bclio.bc_server && !set_bin(&c_stagerbin,stager,SIZE(stager))) {
        if (updateconfig) goto noupdate;
        fatal(0,nobin,"stager",c_stagerbin ? c_stagerbin : null);
      }
    }
  }

  /* normal termination, update config */

  if (mailbin) x_free(mailbin);
  mailbin = c_mailbin;

  if (ftpbin) x_free(ftpbin);
  ftpbin = c_ftpbin;

  if (stagerbin) x_free(stagerbin);
  stagerbin = c_stagerbin;

  if (fatmenbin) x_free(fatmenbin);
  fatmenbin = c_fatmenbin;

  if (dpmbin) x_free(dpmbin);
  dpmbin = c_dpmbin;

  if (rmbin) x_free(rmbin);
  rmbin = c_rmbin;

  if (c_pendingtime < 0) pendingtime = DEF_PENDINGTIME;
  else pendingtime = c_pendingtime;

  if (c_selecttimeout < 0) selecttimeout = DEF_SELECTTIMEOUT;
  else selecttimeout = c_selecttimeout;

  if (c_stagetimeout < 0) stagetimeout = DEF_STAGETIMEOUT;
  else stagetimeout = c_stagetimeout;

  if (c_pinginterval < 0) pinginterval = DEF_PINGINTERVAL;
  else pinginterval = c_pinginterval;

  if (c_stagecancel < 0) stagecancel = DEF_STAGECANCEL;
  else stagecancel = c_stagecancel;

  if (c_userumask < 0) userumask = DEF_USERUMASK;
  else userumask = c_userumask;

  if (nfspath) x_free(nfspath);
  nfspath = c_nfspath;
  if (!nfspath)
    nfspath = dupstr(DEF_NFSPATH);

  if (tmppath) x_free(tmppath);
  tmppath = c_tmppath;
  if (!tmppath)
    tmppath = dupstr(DEF_TMPPATH);

  if (dumpname) x_free(dumpname);
  dumpname = c_dumpname;

  if (admin) x_free(admin);
  admin = c_admin;

  if (piafhome) x_free(piafhome);
  piafhome = c_piafhome;
  if (!piafhome)
    piafhome = dupstr(DEF_PIAFHOME);

  if (bclio.bc_server) {
    x_free(bclio.bc_server);
    x_free(bclio.bc_login);
  }
  bclio = c_bclio;

  bftp = c_bftp;

  tcpparam = c_tcpparam;

  if (syslogchn) {
    x_free(syslogchn);
    closelog();
  }
  syslogchn = c_syslogchn;
  if (syslogchn)
#ifdef LOG_AUTH
    openlog("pfstagerd",LOG_PID | LOG_CONS,c_syslogf);
#else
    openlog("pfstagerd",LOG_PID | LOG_CONS);
#endif

  if (errlogname) x_free(errlogname);
  errlogname = c_errlogname;
  if (errlogf) {
    fclose(errlogf);
    errlogf = (FILE *)0;
  }
  if (errlogname && !streql(errlogname,"syslogd")) {
    errlogf = fopen(errlogname,"a");
    if (errlogf)
      setvbuf(errlogf,(char *)0,_IONBF,0);
  }

  if (joblogname) x_free(joblogname);
  joblogname = c_joblogname;
  if (joblogf) {
    fclose(joblogf);
    joblogf = (FILE *)0;
  }
  if (joblogname && !streql(joblogname,"syslogd")) {
    joblogf = fopen(joblogname,"a");
    if (joblogf)
      setvbuf(joblogf,(char *)0,_IONBF,0);
    else
      errlog(LOG_ERR,"read_config_file() : fopen returns null (file %s)",
        joblogname);
  }

  if (selectlist) {
    struct peer *ppe, *ppee;
    /* release the whole list */
    while (selectlist) {
      ppe = selectlist;
      selectlist = selectlist->pr_next;
      /* only listened to sockets need to be closed
         because we waited until all connects were closed */
      if (ppe->pr_up) {
        /* don't close socket but just re-use if it has the same port
           number - otherwise the 2MSL delay in closing sockets causes
           our forthcoming re-bind to fail for the same port */
        for (ppee = c_selectlist; ppee; ppee = ppee->pr_next)
          if (ppee->pr_lisport == ppe->pr_lisport) {
            /* only things to be copied are the
               socket num and the up status */
            ppee->pr_sock = ppe->pr_sock;
            ppee->pr_up = 1;
            break;
          }
        if (!ppee) x_close(ppe->pr_sock);
      }
      release_peer(ppe);
      x_free(ppe);
    }
  }
  selectlist = c_selectlist;
  updateconfig = 0;

  noupdate:;

  /* error, discard update */

  if (updateconfig) {
    if (c_mailbin) x_free(c_mailbin);
    if (c_ftpbin) x_free(c_ftpbin);
    if (c_stagerbin) x_free(c_stagerbin);
    if (c_fatmenbin) x_free(c_fatmenbin);
    if (c_dpmbin) x_free(c_dpmbin);
    if (c_rmbin) x_free(c_rmbin);
    if (c_syslogchn) x_free(c_syslogchn);
    if (c_nfspath) x_free(c_nfspath);
    if (c_tmppath) x_free(c_tmppath);
    if (c_dumpname) x_free(c_dumpname);
    if (c_admin) x_free(c_admin);
    if (c_piafhome) x_free(c_piafhome);
    if (c_bclio.bc_server) {
      x_free(c_bclio.bc_server);
      x_free(c_bclio.bc_login);
    }
    if (c_errlogname) x_free(c_errlogname);
    if (c_joblogname) x_free(c_joblogname);
    if (c_selectlist) {
      struct peer *ppe;
      /* release the whole list */
      while (c_selectlist) {
        ppe = c_selectlist;
        c_selectlist = c_selectlist->pr_next;
        release_peer(ppe);
        x_free(ppe);
      }
    }
    updateconfig = 0;
  }
  fclose(f);
  x_free(buf);
  x_free(buf2);
}

/*
 * Open sockets to the outside world.  In master mode all sockets
 * are listened to, in slave mode connect is made.
 */
static void
init_sockets()
{
  struct peer *pe_dat;

  if (master) {
    int cup = 0, sup = 0;
    for (pe_dat = selectlist; pe_dat; pe_dat = pe_dat->pr_next) {
      struct sockaddr_in *addrp;

      if (!pe_dat->pr_up) {
        /* masters listen to connections from others */
        if (!(addrp = tcplisaddr(pe_dat)))
          continue;
        DLOG_SOCKADDR_IN(addrp);
        if ((pe_dat->pr_sock = tcplisten(addrp)) < 0)
          continue;
      }
      pe_dat->pr_up = 1;
      if (pe_dat->pr_keep)
        sup = 1;
      else
        cup = 1;
    }
    if (!(cup && sup))
      fatal(0,
        "init_sockets() : master couldn't listen to sockets (sup=%d, cup=%d)",
        sup,cup);
  }
  else {
    for (pe_dat = selectlist; pe_dat; pe_dat = pe_dat->pr_next) {
      struct sockaddr_in *addrp;
      struct hostent *he;

      /* slaves do the initial connect */
      if (!(addrp = tcpconaddr(pe_dat)))
        continue;
      DLOG_SOCKADDR_IN(addrp);
      he = gethostbyaddr((char *)&addrp->sin_addr,sizeof(struct in_addr),
        AF_INET);
      if (he)
        pe_dat->pr_machname = dupstr(he->h_name);
    }
  }
}

/*************************************************************************
 *
 * The main program.  Calling for masters and slaves is different.
 * Note that command line parsing is less than clever, so the order and
 * format of the parameters is significant.
 *
 * Master:
 *
 * call:    pfstagerd [-p] -m mnum configfile
 *
 *          where mnum is the master id number (0...7),
 *          configfile is the full path to the configfile,
 *          -p causes the host name and process id to be
 *          printed to stdout if the program starts ok
 *
 * example: pfstagerd -m 0 /usr/local/etc/pfstagerd.conf
 *
 * Slave:
 *
 * call:    pfstagerd [-p] -s configfile
 *
 *          where configfile is the full path to the configfile
 *          -p as explained above
 *
 * example: pfstagerd -s /usr/local/etc/pfstagerd.conf
 *
 *************************************************************************/

int
main(int argc, char **argv)
{
  int printok = 0;

  myname = argv[0];
  errno = 0;

  /* this umask is for the internal use by pfstagerd -
     umask for users' files is determined in the config */
  umask(022);

  /* get arguments */
  {
    int c;
    extern int optind, opterr;
    extern char *optarg;

    opterr = 0;
    while ((c = getopt(argc,argv,"pm:s")) != EOF) {
      switch (c) {
      case 'p' :
        printok = 1;
        break;
      case 'm' :
        if (!isdigit(*optarg))
          fatal(0,"main() : no master id specified");
        master_id = (unsigned long)atol(optarg);
        master = 1;
        if (master_id >= MAX_MASTERS)
          fatal(0,"main() : invalid master id %d",master_id);
        break;
      case 's' :
        master = 0;
        break;
      case '?' :
      default :
        fatal(0,"main() : unknown option -%c",c);
      }
    }
    if (optind >= argc)
      fatal(0,"main() : config file name required");
    configfile = argv[optind];
  }

  read_config_file();

  DLOG_OPEN(master ? "master.log" : "slave.log");
  DLOG_MSG(("main() : calling sigaction()"));

  /* set all kinds of signals */
  {
    struct sigaction sact;

    sact.sa_handler = sig_handler;
    sigemptyset(&sact.sa_mask);
    sact.sa_flags = 0;

#ifdef SA_NOCLDSTOP
    /* don't worry if a child gets stopped - operator did it */
    sact.sa_flags |= SA_NOCLDSTOP;
#endif
#ifdef SA_INTERRUPT
    /* just in case - only old SunOS is supposed to have this */
    sact.sa_flags |= SA_INTERRUPT;
#endif

    /* needed for getting statuses of exiting children */
    if (!master && sigaction(SIGCHLD,&sact,(struct sigaction *)0) < 0)
      fatal(0,"main() : cannot set SIGCHLD");

    sact.sa_flags = 0;
#ifdef SA_RESTART
    /* other signals should restart any interrupted system
       functions if possible */
    sact.sa_flags |= SA_RESTART;
#endif

    /* for resetting config if necessary */
    if (sigaction(SIGHUP,&sact,(struct sigaction *)0) < 0)
      fatal(0,"main() : cannot set SIGHUP");

    /* for orderly shutdown */
    if (sigaction(SIGTERM,&sact,(struct sigaction *)0) < 0)
      fatal(0,"main() : cannot set SIGTERM");

    /* to prevent lost connections from killing us */
    if (sigaction(SIGPIPE,&sact,(struct sigaction *)0) < 0)
      fatal(0,"main() : cannot set SIGPIPE");

    /* for making status dumps */
    if (sigaction(SIGUSR1,&sact,(struct sigaction *)0) < 0)
      fatal(0,"main() : cannot set SIGUSR1");
  }

  /* initialize rand() */
  srand((unsigned int)time((time_t)0));

  /* find out who we are */
  set_myipaddr();

  init_sockets();

  /* set the process to be the process group leader and disassociate it
     from the controlling terminal - fork to get a unique pid first */
  {
    pid_t pid;
    if ((pid = fork()) < 0)
      fatal(0,"main() : fork() fails");
    else if (pid != 0)
      return 0;
    /* now that we should be running ok, print out an acknowledgement */
    if (printok) {
      printf("started %s %s %ld\n",master ? "master" : "slave",
        mymachname,(long)getpid());
      fflush(stdout);
    }
    fclose(stdin);
    fclose(stdout);
    fclose(stderr);
    stdstreams = 0;
  }
  setsid();

  /* ok, start waiting for calls */

  for (;;) {
    int sel, poll;
    fd_set fd;
    int maxsock;
    struct peer *pe_dat;
    struct timeval timeout;

    /* is it time to quit? */
    if (doshutdown && !clientlist)
      break;

    /* have we received a signal to reconfigure? */
    if (updateconfig) {
      read_config_file();
      init_sockets();
    }

    timeout.tv_usec = 0;
    if (master) {
      long ltmp;

      /* check any pending stage requests (only applicable to masters) -
         also sends reminders if the timeouts for running stages are up */
      poll = m_flush_pending(&ltmp);
      timeout.tv_sec = ltmp;
    }
    else {
      long ltmp;
      int fastpoll = 0;

      /* say hello to all masters that we haven't talked to lately */
      poll = s_ping_masters(&ltmp);
      timeout.tv_sec = ltmp;

      /* before going to select(), check if we must report ourselves
         to a master or if there are finished children */
      if (!doshutdown) fastpoll |= s_init_masters();
      fastpoll |= s_check_children();
      if (fastpoll) {
        poll = 1;
        timeout.tv_sec = selecttimeout;
      }
    }

    FD_ZERO(&fd);
    maxsock = 0;
    for (pe_dat = selectlist; pe_dat; pe_dat = pe_dat->pr_next)
      if (pe_dat->pr_up && (!doshutdown || pe_dat->pr_keep)) {
        FD_SET(pe_dat->pr_sock,&fd);
        /* find out the highest socket number for select() */
        if (maxsock < pe_dat->pr_sock) maxsock = pe_dat->pr_sock;
      }

    DLOG_MSG(("main() : maxsock = %d",maxsock));

    /* should never happen */
    if (maxsock >= FD_SETSIZE) {
      maxsock = FD_SETSIZE - 1;
      errlog(LOG_ERR,"main() : maxsock=%d, FD_SETSIZE=%d",maxsock,FD_SETSIZE);
    }

    /* blocks until something arrives or a signal occurs */
    DLOG_MSG((
      "main() : calling x_select() poll=%d, timeout={tv_sec=%ld,tv_usec=%ld}",
      poll,timeout.tv_sec,timeout.tv_usec));
    errno = 0;
    sel = x_select(maxsock + 1,&fd,(fd_set *)0,(fd_set *)0,
                   poll ? &timeout : (struct timeval *)0);

    /* something in the socket? */

    if (sel > 0) {
      for (pe_dat = selectlist; pe_dat; pe_dat = pe_dat->pr_next)
        if (pe_dat->pr_up && FD_ISSET(pe_dat->pr_sock,&fd)) {
          struct peer pe_tmp;
          pe_tmp = *pe_dat;

          /* accept the connection */
          DLOG_MSG(("main() : calling x_accept()"));
          pe_tmp.pr_sock =
            x_accept(pe_dat->pr_sock,(struct sockaddr *)0,(int *)0);
          if (pe_tmp.pr_sock >= 0) {
            char *s;
            struct revector *rv = (struct revector *)0;

#ifdef ACK_EVERYTHING
            int ok;
            if (master)
              ok = m_master_ops(pe_tmp.pr_sock,&rv);
            else
              ok = s_slave_ops(pe_tmp.pr_sock,&rv);
            send_msg(pe_tmp.pr_sock,ok ? SGD_MSG_OK : SGD_MSG_ERROR);
#else
            if (master)
              m_master_ops(pe_tmp.pr_sock,&rv);
            else
              s_slave_ops(pe_tmp.pr_sock,&rv);
#endif
            if (rv) {
              if (!(s = get_peerip(pe_tmp.pr_sock,(struct sockaddr_in **)0)))
                errlog(LOG_ERR,"main() : get_peerip fails");
            }
            x_close(pe_tmp.pr_sock);

            /* this call probably shouldn't be at the top level, but
               we have to kludge an ack / close previous connection
               before sending MSG_FORGET if necessary */
            if (rv && s) {
              struct peer *pe;
              if (!(pe = find_peerbyip(master ? slavelist : selectlist,s)))
                errlog(LOG_ERR,"main() : strange peer ip %s",s);
              else
                tell_to_forget(pe,rv);
            }
          }
        }
    }
    /* else its an exception of some kind,
       probably just a child signing off */
  }
  return 0;
}
