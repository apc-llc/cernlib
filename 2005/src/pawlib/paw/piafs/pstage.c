/*
 * $Id: pstage.c,v 1.3 1996/04/25 07:53:06 dinofm Exp $
 *
 * $Log: pstage.c,v $
 * Revision 1.3  1996/04/25 07:53:06  dinofm
 * VAX/VMS code wiped out.
 *
 * Revision 1.2  1996/04/02 21:54:00  thakulin
 * Changed pfstagerd.h and piaf.h include file paths.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :          30/01/96  15.44.33  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   11/08/94*/
/*************************************************************************
 *
 * pstage.c
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    11/8/94
 *
 *************************************************************************/

/* PSTAGE is tested in pfstagerd.h, so this line must come first */
#define PSTAGE pstage_

#include "stagerd/pfstagerd.h"
#include "piaf.h"

/* messages to pass */
static char *sgd_msg[] = {
  "msg_none",
  "msg_error",
  "msg_ok",
  "msg_stage",
  "msg_status",
  "msg_init",
  "msg_ping",
  "msg_forget",
  "msg_trans",
  (char *)0
};

/* file types */
static char *sgd_ftyp[] = {
  "ftyp_none",
  "ftyp_local",
  "ftyp_tape",
  "ftyp_fatmen",
  (char *)0
};

/* stage statuses */
static char *sgd_status[] = {
  "status_none",
  "status_error",
  "status_done",
  "status_running",
  "status_pending",
  "status_connect",
  (char *)0
};

static int
x_recv(int sock,char *buf,size_t len)
{
  size_t n;
  int nn, ret;

  ret = 1;
  for (n = 0; n < len; n += (size_t)nn) {
    errno = 0;
    while ((nn = recv(sock,&buf[n],len - n,0)) <= 0) {
      if (nn == 0) goto end;
      piafl_error("x_recv() : recv returns <= 0");
      if (errno == EINTR) continue;
      errno = 0;
      ret = 0;
      goto end;
    }
  }

  end:;

  return ret;
}

static int
x_send(int sock,char *buf,size_t len)
{
  size_t n;
  int nn;

  for (n = 0; n < len; n += (size_t)nn) {
    errno = 0;
    if ((nn = send(sock,&buf[n],len - n,0)) <= 0) {
      piafl_error("x_send() : send returns <= 0");
      if (errno == EINTR) continue;
      errno = 0;
      return 0;
    }
  }
  return 1;
}

static int
recvul(int sock,unsigned long *ul)
{
  char buf[MAX_ULSTR + 1];

  if (!x_recv(sock,buf,sizeof buf)) {
    return 0;
  }
  *ul = strtoul(buf,(char **)0,10);
  return 1;
}

static int
sendul(int sock,unsigned long ul)
{
  char buf[MAX_ULSTR + 1];

  sprintf(buf,"%lu",ul);
  if (!x_send(sock,buf,sizeof buf)) {
    return 0;
  }
  return 1;
}

static int
sendstr(int sock,char *s)
{
  size_t len = s ? strlen(s) : 0;

  if (!sendul(sock,(unsigned long)len))
    return 0;
  if (len > 0) {
    if (!x_send(sock,s,len)) {
      return 0;
    }
  }
  return 1;
}

static int
recvstr(int sock,char **s,size_t *len)
{
  unsigned long ultmp;

  if (!recvul(sock,&ultmp))
    return 0;
  *len = (size_t)ultmp;
  if (*s) free(*s);
  if (*len > 0) {
    *s = (char *)malloc(*len + 1);
    if (!x_recv(sock,*s,*len)) {
      return 0;
    }
    (*s)[*len] = '\0';
  }
  else
    *s = (char *)0;
  return 1;
}

static int
recvmstr(int sock,char **s)
{
  char buf[MAX_MSGSTR + 1];
  unsigned long ultmp;
  int i;
  size_t len;

  if (!recvul(sock,&ultmp))
    return -1;
  len = (size_t)ultmp;
  if (len == 0 || len > MAX_MSGSTR) {
    piafl_error("recvmstr() : bad len");
    return -1;
  }
  if (!x_recv(sock,buf,len)) {
    return -1;
  }
  buf[len] = '\0';
  for (i = 0; s[i]; i++)
    if (!strcmp(s[i],buf)) return i;
  piafl_error("recvmstr() : unknown message");
  return -1;
}

/*
 * Send the full client information.
 */
static int
put_client(int sock,struct client *cl_dat)
{
  /* user name */
  if (!sendstr(sock,cl_dat->ct_username))
    return 0;

  /* machine name */
  if (!sendstr(sock,cl_dat->ct_machname))
    return 0;

  /* ip address */
  if (!sendstr(sock,cl_dat->ct_ipaddr))
    return 0;

  /* ftp user name (optional) */
  if (!sendstr(sock,cl_dat->ct_fusername))
    return 0;

  /* user password on the client (optional) */
  if (!sendstr(sock,cl_dat->ct_passwd))
    return 0;

  /* client id */
  if (!sendul(sock,cl_dat->ct_clientid))
    return 0;

  return 1;
}

/*
 * Send the full specs of the file to be staged.
 */
static int
put_stage(int sock,struct stage *st_dat)
{
  int i;

  /* the source file */
  if (!sendstr(sock,st_dat->sg_sfname))
    return 0;

  /* the target file (optional) */
  if (!sendstr(sock,st_dat->sg_tfname))
    return 0;

  /* name of the preferred staging server (optional) */
  if (!sendstr(sock,st_dat->sg_prserver))
    return 0;

  /* file type */
  if (!send_ftyp(sock,st_dat->sg_ftyp))
    return 0;

  /* file size */
  if (!sendul(sock,st_dat->sg_fsiz))
    return 0;

  /* nowait flag */
  if (!sendul(sock,st_dat->sg_nowait))
    return 0;

  /* number of file stripes */
  if (!sendul(sock,(unsigned long)st_dat->sg_fnum))
    return 0;

  /* the physical target file(s) (optional) */
  for (i = 0; i < st_dat->sg_fnum; i++) {
    if (!sendstr(sock,st_dat->sg_ptfname[i].pf_name))
      return 0;
    if (!sendul(sock,(unsigned long)st_dat->sg_ptfname[i].pf_nrec))
      return 0;
    if (!sendul(sock,st_dat->sg_ptfname[i].pf_size))
      return 0;
  }

  /* master ip address */
  if (!sendstr(sock,st_dat->sg_masterip))
    return 0;

  /* server ip address */
  if (!sendstr(sock,st_dat->sg_serverip))
    return 0;

  /* stage id */
  if (!sendul(sock,st_dat->sg_stageid))
    return 0;

  /* queueing time */
  if (!sendul(sock,(unsigned long)st_dat->sg_queuetime))
    return 0;

  /* starting time */
  if (!sendul(sock,(unsigned long)st_dat->sg_starttime))
    return 0;

  /* ending time */
  if (!sendul(sock,(unsigned long)st_dat->sg_endtime))
    return 0;

  /* the original target file */
  if (!sendstr(sock,st_dat->sg_otfname))
    return 0;

  /* the physical source file (optional) */
  if (!sendstr(sock,st_dat->sg_psfname))
    return 0;

  /* how much of the file transferred */
  if (!sendul(sock,st_dat->sg_fdone))
    return 0;

  /* status indicator */
  if (!send_status(sock,st_dat->sg_status))
    return 0;

  /* stager return code */
  if (!sendul(sock,(unsigned long)st_dat->sg_retcode))
    return 0;

  /* the slavelist (optional) */
  if (!sendstr(sock,st_dat->sg_slavelist))
    return 0;

  return 1;
}

/*
 * Send the client information.
 */
static int
put_clientspecs(int sock,struct client *cl_dat)
{
  if (!put_client(sock,cl_dat))
    return 0;
  return 1;
}

/*
 * Send the specs of the file to be staged.
 */
static int
put_stagespecs(int sock,struct stage *st_dat)
{
  if (!put_stage(sock,st_dat))
    return 0;

  return 1;
}

/*
 * Cipher user password.
 */
static char *
cpass(char *s)
{
  static char mask[] = {
    '\241','\212','\307','\266','w','\375','^','\303','{','Z',
    '\327','\357','-','\210','\353','\347','\217','.','t','\212',
    '\374','\241','z','d','\327','\271','\255','\267','.','\3',
    '\320','\317','\215','\230','\206','\5','\225','\344','\310','\20'
  };
  static char buf[MAX_PASSWD + 1];
  int i, lpw;
  unsigned int seed = 0;

  /* cipher it */
  lpw = strlen(s);
  /* generate a 'unique' seed based on the password */
  for (i = 0; i < lpw; i++)
    seed += (unsigned int)((s[i] & 0xff) * (i + 1));
  srand(seed);
  for (i = 0; i < MAX_PASSWD; i++)
    buf[i] = (mask[i] ^ (i <= lpw ? s[i] : rand())) & 0xff;
  buf[MAX_PASSWD] = '\0';
  return buf;
}

static char *masterip, *client_ip, *client_host, *client_user, *client_pass;
static int masterport;

static int
stage_connect()
{
  struct sockaddr_in addr;
  int sock, so = 1;

  memset((void *)&addr,0,sizeof addr);
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr(masterip);
  addr.sin_port = htons(masterport);

  if ((sock = socket(AF_INET,SOCK_STREAM,0)) < 0) {
    piafl_error("stage_connect() : socket error");
    return -1;
  }
  if (setsockopt(sock,IPPROTO_TCP,TCP_NODELAY,(char *)&so,sizeof so) < 0) {
    piafl_error("stage_connect() : setsockopt error");
    goto err;
  }
  if (connect(sock,(struct sockaddr *)&addr,sizeof addr) < 0) {
    piafl_error("stage_connect() : connect error");
    goto err;
  }
  return sock;

  err:;

  close(sock);
  return -1;
}

static char *
fstrdup(char *str,int len)
{
  char *s;

  if (!str) return (char *)0;
  while (len > 0 && str[len - 1] == ' ') len--;
  if (len > 0) {
    int i;
    for (i = 0; str[i] == ' '; i++, len--) ;
    s = (char *)malloc(len + 1);
    strncpy(s,&str[i],len);
    s[len] = '\0';
  }
  else
    s = (char *)0;
  return s;
}

static int
query_stage(struct client *cl_spec,char ***lines)
{
  static char conerr[] =
    " *** Error talking to stager. Status query failed.\n";
  int i, n = 0;
  int sock = -1, msg;
  unsigned long ultmp;

  *lines = (char **)malloc(sizeof(char *));
  (*lines)[0] = (char *)malloc(sizeof conerr);
  strcpy((*lines)[0],conerr);

  errno = 0;

  /* user id stuff */

  if ((sock = stage_connect()) < 0) {
    piafl_error("query_stage() : can't connect");
    goto end;
  }
  if (!send_msg(sock,SGD_MSG_STATUS)) {
    piafl_error("query_stage() : send_msg trouble");
    goto end;
  }
  if (!put_clientspecs(sock,cl_spec)) {
    piafl_error("query_stage() : put_clientspecs trouble");
    goto end;
  }
  if (!recvul(sock,&ultmp)) {
    piafl_error("query_stage() : recvul trouble");
    goto end;
  }
  {
    /* tricks to reuse an already allocated buffer */
    char *tmp = (*lines)[0];
    free(*lines);
    *lines = (char **)malloc((ultmp + 1) * sizeof(char *));
    (*lines)[0] = tmp;
  }
  sprintf((*lines)[0],"Ongoing stages:     ");
  if (ultmp > 0)
    sprintf((*lines)[0] + strlen((*lines)[0]),"%lu\n",ultmp);
  else
    sprintf((*lines)[0] + strlen((*lines)[0]),"none\n");
  for (i = n = 0; i < (int)ultmp; i++) {
    char *buf = (char *)0;
    size_t len;
    if (!recvstr(sock,&buf,&len)) {
      if (buf) free(buf);
      continue;
    }
    if (buf)
      (*lines)[++n] = buf;
  }
  if ((msg = recv_msg(sock)) < 0)
    piafl_error("query_stage() : recv_msg trouble");
  else if (msg == SGD_MSG_ERROR)
    piafl_error("query_stage() : status query returns error");

  end:;

  if (sock >= 0) {
    shutdown(sock,2);
    close(sock);
  }
  return n;
}

static struct client cl_init;
static struct stage st_init;

/*
 * Global functions, fortran style.
 */

void
stginit(char *mip,int port,char *cip,char *host,char *user)
{
  if (!mip || !cip || !host || !user) return;
  if (masterip) free(masterip);
  masterip = strdup(mip);
  masterport = port;
  if (client_ip) free(client_ip);
  client_ip = strdup(cip);
  if (client_host) free(client_host);
  client_host = strdup(host);
  if (client_user) free(client_user);
  client_user = strdup(user);
}

#define CZPUTA czputa_
extern SUBROUTINE CZPUTA(char *,INTEGER *,int);
#define CALL_CZPUTA(line,status) CZPUTA(line,status,strlen(line))

SUBROUTINE PSTAGE(char *sfname,char *tfname,char *chopt,char *locl,
                  int len_sfname,int len_tfname,int len_chopt,int len_locl)
{
  static char conerr[] = " *** Error talking to stager. Stage failed.\n";
  int sock = -1, msg, i, nowait = 0;
  size_t fsize;
  char *pass;
  struct stage st_spec;
  struct client cl_spec;

  if (!client_ip || !client_host || !client_user) {
    printf(" *** Stager not initialised. Staging not available.\n");
    return;
  }

  errno = 0;

  for (i = 0; i < len_chopt; i++)
    if (chopt[i] == 'N') {
      nowait = 1;
      break;
    }

  st_spec = st_init;
  cl_spec = cl_init;

  /* stage stuff */

  st_spec.sg_sfname = fstrdup(sfname,len_sfname);
  st_spec.sg_tfname = fstrdup(tfname,len_tfname);
  for (i = 0; locl[i] == ' '; i++) ;
  if (isdigit(locl[i])) {
    long ll;
    char buf1[80], buf2[80];
    sscanf(&locl[i],"%ld%s%s",&ll,buf1,buf2);
    fsize = ll;
    cl_spec.ct_fusername = strdup(buf1);
    pass = strdup(buf2);
  }
  else {
    fsize = 0;
    pass = (char *)0;
  }
  if (fsize > 0)
    st_spec.sg_ftyp = SGD_FTYP_LOCAL;
  else if (sfname[0] == '/' && sfname[1] == '/')
    st_spec.sg_ftyp = SGD_FTYP_FATMEN;
  else
    st_spec.sg_ftyp = SGD_FTYP_TAPE;
  st_spec.sg_fsiz = fsize;

  /* user id stuff */

  cl_spec.ct_username = client_user;
  cl_spec.ct_machname = client_host;
  cl_spec.ct_ipaddr = client_ip;
  if (pass || fsize > 0) {
    if (client_pass) free(client_pass);
    client_pass = pass ? strdup(cpass(pass)) : (char *)0;
  }
  cl_spec.ct_passwd = client_pass;

  if ((sock = stage_connect()) < 0) {
    piafl_error("pstage() : can't connect");
    printf("%s",conerr);
    goto end;
  }
  if (!send_msg(sock,SGD_MSG_STAGE)) {
    piafl_error("pstage() : send_msg trouble");
    printf("%s",conerr);
    goto end;
  }
  if (!put_clientspecs(sock,&cl_spec)) {
    piafl_error("pstage() : put_clientspecs trouble");
    printf("%s",conerr);
    goto end;
  }
  if (!put_stagespecs(sock,&st_spec)) {
    piafl_error("pstage() : put_stagespecs trouble");
    printf("%s",conerr);
    goto end;
  }
  if ((msg = recv_msg(sock)) < 0) {
    piafl_error("pstage() : recv_msg trouble");
    printf("%s",conerr);
    goto end;
  }
  else if (msg == SGD_MSG_ERROR) {
    piafl_error("pstage() : stager returns error");
    printf("%s",conerr);
    goto end;
  }
  if (nowait)
    printf(" *** Stage job submitted successfully.\n");
  else {
#define DEF_STIME 2
    unsigned stime = DEF_STIME, unslept;
    int nstg, running, err;
    size_t sflen = strlen(st_spec.sg_sfname), outbuflen = sflen + 50;
    char **lines, *outbuf;
    float odone = 0.0;
    INTEGER istat;

    outbuf = (char *)malloc(outbuflen);
    do {
      for (unslept = stime; unslept > 0;)
        unslept = sleep(unslept);
      nstg = query_stage(&cl_spec,&lines);
      running = err = 0;
      for (i = 1; i <= nstg; i++) {
        char *l = strchr(lines[i],' ');
        if (l) for (; *l == ' '; l++) ;
        if (l && !strncmp(st_spec.sg_sfname,l,sflen) && *(l + sflen) == ' ') {
          float done = odone;
          l += sflen;
          for (; *l == ' '; l++) ;
          if (!strncmp(l,"completed",9) || !strncmp(l,"error",5)) {
            err = !strncmp(l,"error",5);
            break;
          }
          running = 1;
          for (; *l != ' '; l++) ;
          sscanf(l," (%g%%",&done);
          if (done > odone) {
            float rate = (done - odone) / stime;
            stime = (unsigned)(10 / rate);
            if (stime < DEF_STIME) stime = DEF_STIME;
            odone = done;
            if (strlen(lines[i]) + 20 > outbuflen) {
              free(outbuf);
              outbuflen = strlen(lines[i]) + 20;
              outbuf = (char *)malloc(outbuflen);
            }
            sprintf(outbuf,"STAGEW %s",lines[i]);
            if (outbuf[strlen(outbuf) - 1] == '\n')
              outbuf[strlen(outbuf) - 1] = '\0';
            CALL_CZPUTA(outbuf,&istat);
          }
        }
        free(lines[i]);
      }
      free(lines);
    } while (running);
    printf(" *** Stage result: %s %s\n",st_spec.sg_sfname,
      err ? "error" : "completed");
    free(outbuf);
  }

  end:;

  if (sock >= 0) {
    shutdown(sock,2);
    close(sock);
  }
  if (st_spec.sg_sfname) free(st_spec.sg_sfname);
  if (st_spec.sg_tfname) free(st_spec.sg_tfname);
  if (cl_spec.ct_fusername) free(cl_spec.ct_fusername);
  if (pass) free(pass);
}

void
stginfo()
{
  struct client cl_spec;
  int i, nstg;
  char **lines;

  if (!client_ip || !client_host || !client_user) return;

  cl_spec = cl_init;

  /* user id stuff */

  cl_spec.ct_username = client_user;
  cl_spec.ct_machname = client_host;
  cl_spec.ct_ipaddr = client_ip;

  nstg = query_stage(&cl_spec,&lines);
  for (i = 0; i < nstg + 1; i++) {
    printf("%s",lines[i]);
    free(lines[i]);
  }
  free(lines);
}
