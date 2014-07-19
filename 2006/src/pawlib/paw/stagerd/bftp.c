/*
 * $Id: bftp.c,v 1.2 1996/04/02 22:00:13 thakulin Exp $
 *
 * $Log: bftp.c,v $
 * Revision 1.2  1996/04/02 22:00:13  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          01/12/95  00.26.28  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * bftp.c
 *
 * Builtin ftp client for the pfstagerd.  Largely based on the ftp client
 * code from 4.3BSD NET/2 distribution and RFC959.  The original BSD code
 * contains the copyright below.  Names of all routines in this file
 * start with "ftp_".
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    25/8/94
 *
 *************************************************************************/

/*
 * Copyright (c) 1985, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "pfstagerd.h"

#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <arpa/telnet.h>
#include <arpa/ftp.h>

#if defined(sun) && defined(CERNLIB_SOLARIS)
/* if Meiko CS-2, this file should contain symbol ELF_MEIKO_ELAN1 */
# include <sys/elf.h>
# ifdef ELF_MEIKO_ELAN1
#  define MEIKO_CS2
static void ftp_setpfs(int,long);
# endif
#endif

#ifdef FTP_TESTING
#define x_select      select
#define x_listen      listen
#define x_accept      accept
#define x_send        send
#define x_bind        bind
#define x_setsockopt  setsockopt
#define x_connect     connect
#define x_close       close
#define x_shutdown    shutdown
#define x_socket      socket
#define x_malloc      malloc
#define x_free        free
#define dupstr        strdup
struct ftppar bftp;
struct cliopar bclio;
char *nfspath;
#endif

static void ftp_lostpeer(void);
static int ftp_setpeer(char *,char *,char *,char *,int);
static void ftp_setparm(int,int,int,int);
static int ftp_cd(char *);
static void ftp_quote(char *,...);
static void ftp_disconnect(void);
static int  ftp_initconn(void);
static FILE * ftp_dataconn(char *);
static void ftp_abort_remote(FILE *);
static int ftp_get(char *,struct pfinfo *,char *,int *,unsigned long,
  char *,char *);
static void ftp_tvsub(struct timeval *,struct timeval *,struct timeval *);
static void ftp_ptransfer(char *,long,struct timeval *,struct timeval *);
static char * ftp_hookup(char *,int);
static int  ftp_login(char *,char *,char *,char *);
static int  ftp_command(char *,...);
static int  ftp_getreply(int);

/* the lowest negative value of int data type - true for two's
   complement arithmetic, not so for one's complement or signed
   magnitude representations, which we don't care about anyway */
#define NO_CLIO_RET   ((int)((unsigned)-1 / 2 + 1))

/* RFC959 */

#define DEF_TYPE  TYPE_A
#define DEF_MODE  MODE_S
#define DEF_STRU  STRU_F

/*
 * Options and other state info.
 */
static int sendport;            /* use PORT cmd for each data connection */
static int connected;           /* connected to server */
static int code;                /* return/reply code for ftp command */
static int curtype = DEF_TYPE;  /* current file transfer type */
static int curmode = DEF_MODE;  /* current file transfer mode */
static int curstru = DEF_STRU;  /* current file transfer stru */

static FILE *cout, *cin, *scriptf;
static char reply_string[BUFSIZ]; /* last line of previous reply */
static struct sockaddr_in hisctladdr;
static struct sockaddr_in data_addr;
static int data = -1;
static struct sockaddr_in myctladdr;

static int cpend;   /* flag: if != 0, then pending server reply */

static int clioretry;
static int clioret;

#define ftp_settenex()        ftp_setparm(TYPE_L,FORM_N,STRU_F,MODE_S)
#define ftp_setascii()        ftp_setparm(TYPE_A,FORM_N,STRU_F,MODE_S)
#define ftp_setbinary()       ftp_setparm(TYPE_I,FORM_N,STRU_F,MODE_S)
#define ftp_site(line)        ftp_quote("SITE %s",line)

static void
ftp_lostpeer()
{
  if (connected) {
    if (!cout) {
      /* exception: calling shutdown directly */
      shutdown(fileno(cout), 1+1);
      fclose(cout);
      cout = (FILE *)0;
    }
    if (data >= 0) {
      x_shutdown(data, 1+1);
      data = -1;
    }
    connected = 0;
  }
}

/*
 * Connect to peer server and auto-login
 */
static int
ftp_setpeer(char *host,char *user,char *pass,char *acct,int port)
{
  if (port <= 0) {
    errlog(LOG_ERR,"ftp_setpeer() : bad port number %d (%s)",port,host);
    code = -1;
    return SGD_RET_FTPLOGIN;
  }
  host = ftp_hookup(host,port);
  if (host) {
    connected = 1;
    if (!ftp_login(host,user,pass,acct))
      return SGD_RET_FTPLOGIN;
    ftp_command("SYST");
    /* unix systems answer: "215 UNIX Type: L8" */
    if (strneql(reply_string,"215 TOPS20",10))
      ftp_settenex();
    else
      ftp_setbinary();
    return SGD_RET_OK;
  }
  return SGD_RET_FTPLOGIN;
}

struct trparm {
  int  tr_parm;
  char *tr_cmd;
};

static struct trparm trtype[] = {
  { TYPE_A, "A" },  /* ASCII */
/*  { TYPE_E, "E" },  /* EBCDIC */
  { TYPE_I, "I" },  /* Image */
  { TYPE_L, "L 8" }, /* Local byte Byte size */
  { 0 }
};
static struct trparm trform[] = {
  { FORM_N, "N" },  /* Non-print */
  { FORM_T, "T" },  /* Telnet format effectors */
  { FORM_C, "C" },  /* Carriage control (ASA) */
  { 0 }
};
static struct trparm trstru[] = {
  { STRU_F, "F" },  /* File */
  { STRU_R, "R" },  /* Record structure */
  { STRU_P, "P" },  /* Page structure */
  { 0 }
};
static struct trparm trmode[] = {
  { MODE_S, "S" },  /* Stream */
  { MODE_B, "B" },  /* Block */
  { MODE_C, "C" },  /* Compressed */
  { 0 }
};

/*
 * Set transfer type.
 */
static void
ftp_setparm(int type,int form,int stru,int mode)
{
  register struct trparm *p;

  for (p = trtype; p->tr_cmd; p++)
    if (type == p->tr_parm) break;
  if (type == TYPE_I || type == TYPE_L) {
    if (ftp_command("TYPE %s",p->tr_cmd) == COMPLETE)
      curtype = type;
  }
  else {
    register struct trparm *pp;
    for (pp = trform; pp->tr_cmd; pp++)
      if (form == pp->tr_parm) break;
    if (ftp_command("TYPE %s %s",p->tr_cmd,pp->tr_cmd) == COMPLETE)
      curtype = type;
  }
  if (stru != curstru) {
    for (p = trstru; p->tr_cmd; p++)
      if (stru == p->tr_parm) break;
    if (ftp_command("STRU %s",p->tr_cmd) == COMPLETE)
      curstru = stru;
  }
  if (mode != curmode) {
    for (p = trmode; p->tr_cmd; p++)
      if (mode == p->tr_parm) break;
    if (ftp_command("MODE %s",p->tr_cmd) == COMPLETE)
      curmode = mode;
  }
}

/*
 * Set current working directory on remote machine.
 */
static int
ftp_cd(char *dir)
{
  if (ftp_command("CWD %s",dir) == ERROR && code == 500)
    if (ftp_command("XCWD %s",dir) == ERROR) {
      errlog(LOG_ERR,"ftp_cd() : cannot cd to %s",dir);
      return SGD_RET_FTPCD;
    }
  return SGD_RET_OK;
}

static void
ftp_quote(char *format,...)
{
  char buf[BUFSIZ];
  int i;
  va_list arg_ptr;

  va_start(arg_ptr,format);
  vsprintf(buf,format,arg_ptr);
  va_end(arg_ptr);
  for (i = strlen(buf) - 1; i >= 0 && isspace(buf[i]); i--) ;
  buf[i + 1] = '\0';
  if (ftp_command(buf) == PRELIM)
    while (ftp_getreply(0) == PRELIM) ;
}

/*
 * Terminate session, but don't exit.
 */
static void
ftp_disconnect()
{
  if (!connected) return;
  ftp_command("QUIT");
  if (cout) fclose(cout);
  cout = (FILE *)0;
  connected = 0;
  data = -1;
}

/*
 * Need to start a listen on the data channel before we send the command,
 * otherwise the server's connect may fail.
 */
static int
ftp_initconn()
{
  register char *p, *a;
  int result, len, tmpno = 0;
  int on = 1;

  noport:;

  data_addr = myctladdr;
  if (sendport)
    data_addr.sin_port = 0;  /* let system pick one */
  if (data != -1)
    x_close(data);
  data = x_socket(AF_INET, SOCK_STREAM, 0);
  if (data < 0) {
    errlog(LOG_ERR,"ftp_initconn() : x_socket fails");
    if (tmpno)
      sendport = 1;
    return 1;
  }
  if (!tcpsockopt(data)) {
    errlog(LOG_ERR,"ftp_initconn() : tcpsockopt fails");
    goto bad;
  }
  if (!sendport)
    if (x_setsockopt(data,SOL_SOCKET,SO_REUSEADDR,(char *)&on,sizeof on) < 0) {
      errlog(LOG_ERR,"ftp_initconn() : setsockopt SO_REUSEADDR");
      goto bad;
    }
  if (x_bind(data,(struct sockaddr *)&data_addr,sizeof data_addr) < 0) {
    errlog(LOG_ERR,"ftp_initconn() : x_bind fails");
    goto bad;
  }
#if 0
  if (x_setsockopt(data,SOL_SOCKET,SO_DEBUG,(char *)&on,sizeof on) < 0)
    perror("ftp: setsockopt (ignored)");
#endif
  len = sizeof data_addr;
  if (getsockname(data,(struct sockaddr *)&data_addr,&len) < 0) {
    errlog(LOG_ERR,"ftp_initconn() : getsockname fails");
    goto bad;
  }
  x_listen(data,1);
  if (sendport) {
    a = (char *)&data_addr.sin_addr;
    p = (char *)&data_addr.sin_port;
#define  UC(b)  (((int)b)&0xff)
    result =
        ftp_command("PORT %d,%d,%d,%d,%d,%d",
          UC(a[0]), UC(a[1]), UC(a[2]), UC(a[3]),
          UC(p[0]), UC(p[1]));
    if (result == ERROR && sendport == -1) {
      sendport = 0;
      tmpno = 1;
      goto noport;
    }
    return result != COMPLETE;
  }
  if (tmpno)
    sendport = 1;
#ifdef IP_TOS
  on = IPTOS_THROUGHPUT;
  if (x_setsockopt(data,IPPROTO_IP,IP_TOS,(char *)&on,sizeof on) < 0)
    errlog(LOG_ERR,"ftp_init() : setsockopt IP_TOS (ignored)");
#endif
  return 0;

  bad:;

  x_close(data);
  data = -1;
  if (tmpno)
    sendport = 1;
  return 1;
}

static FILE *
ftp_dataconn(char *lmode)
{
  struct sockaddr_in from;
  int s, fromlen = sizeof from;

  s = x_accept(data,(struct sockaddr *)&from,&fromlen);
  if (s < 0) {
    errlog(LOG_ERR,"ftp_dataconn() : x_accept fails");
    x_close(data);
    data = -1;
    return (FILE *)0;
  }
  x_close(data);
  data = s;
#ifdef IP_TOS
  {
    int tos = IPTOS_THROUGHPUT;
    if (x_setsockopt(s,IPPROTO_IP,IP_TOS,(char *)&tos,sizeof tos) < 0)
      errlog(LOG_ERR,"ftp_dataconn() : x_setsockopt IP_TOS (ignored)");
  }
#endif
  return fdopen(data,lmode);
}

static void
ftp_abort_remote(FILE *din)
{
  char buf[BUFSIZ];
  int nfnd;
  struct fd_set mask;
  struct timeval td;

  /*
   * send IAC in urgent mode instead of DM because 4.3BSD places oob mark
   * after urgent byte rather than before as is protocol now
   */
  sprintf(buf,"%c%c%c",IAC,IP,IAC);
  if (send(fileno(cout),buf,3,MSG_OOB) != 3)
    errlog(LOG_ERR,"ftp_abort_remote() : send fails");
  fprintf(cout,"%cABOR\r\n",DM);
  fflush(cout);
  FD_ZERO(&mask);
  FD_SET(fileno(cin),&mask);
  if (din) FD_SET(fileno(din),&mask);
  td.tv_sec = 10;
  td.tv_usec = 0;
  if ((nfnd = x_select(32,&mask,(struct fd_set *)0,(struct fd_set *)0,&td))
      <= 0) {
    if (nfnd < 0)
      errlog(LOG_ERR,"ftp_abort_remote() : ftp_empty fails");
    ftp_lostpeer();
  }
  if (din && FD_ISSET(fileno(din),&mask))
    while (read(fileno(din),buf,BUFSIZ) > 0) ;
  if (ftp_getreply(0) == ERROR && code == 552)
    /* 552 needed for nic style abort */
    ftp_getreply(0);
  ftp_getreply(0);
}

#ifdef MEIKO_CS2
# include <sys/statvfs.h>
# include <nfs/nfsio.h>
static void
ftp_setpfs(int fd,long bufsiz)
{
  struct statvfs fsbuf;

  /* turning off client cache helps for buffers > 16K */
  if (bufsiz > 16 * 1024 &&
      fstatvfs(fd,&fsbuf) >= 0 &&
      streql(fsbuf.f_basetype,"pfs"))
    ioctl(fd,NFSIOCACHE,CLIENTNOCACHE);
}
#endif

static int
ftp_get(char *remote,struct pfinfo *pt,char *local,int *fnum,
  unsigned long fsiz,char *slavelist,char *uname)
{
  FILE *din = (FILE *)0;
  char *buf = (char *)0;
  register int i, j;
  int tnum;
  size_t ibytes, bytes, totbytes = 0;
  long c;   /* should be ssize_t, but not all systems have it */
  int is_hbook, ret;
  long lrec, nrec;
  struct peer *trans;
  struct timeval start, stop;
  static unsigned char hbook_key[8] = { 0, 60, 240, 136, 0, 16, 153, 139 };

  gettimeofday(&start,(struct timezone *)0);
  if (ftp_initconn()) {
    code = -1;
    return SGD_RET_FTPCONN;
  }
  if (ftp_command("RETR %s",remote) != PRELIM)
    return SGD_RET_FTPCONN;
  din = ftp_dataconn("r");
  if (!din) {
    ret = SGD_RET_FTPCONN;
    goto doabort;
  }

  buf = (char *)x_malloc((size_t)bftp.bf_bufsize);

  /* slurp in the initial buffer to scan for the hbook identifier */
  ibytes = 0;
  errno = 0;
  while (ibytes < bftp.bf_bufsize && (c = read(fileno(din),buf + ibytes,
         (size_t)(bftp.bf_bufsize - ibytes))) > 0)
    ibytes += c;
  if (c < 0 && errno != EPIPE) {
    errlog(LOG_ERR,"ftp_get() : read error from peer");
    ret = SGD_RET_FTPREAD;
    goto doabort;
  }
  is_hbook = 0;
  for (i = 0; i < ibytes; i++)
    if (!memcmp(&buf[i],hbook_key,sizeof hbook_key)) {
      int j = i / HBOOK_WORD_BYTES, k;
      unsigned long *ibuf = (unsigned long *)buf;
      unsigned long *dirs = &ibuf[j - 33];
      /* Hbook files keep integers in network byte order! */
      unsigned long *lb = &dirs[ntohl(dirs[24]) - 1];
      unsigned long *bmap = &lb[3];   /* record bit map */
      int nwrec = (int)ntohl(lb[0]);  /* number of record bits */

      lrec = (long)ntohl(lb[1]);  /* record length in words */
      /* this method should always work even when
         the Hbook file contains subdirectories */
      for (k = nwrec - 1; k >= 0; k--) {
        unsigned long bm = ntohl(bmap[k]);
        if (bm != 0) {
          int l;
          for (l = HBOOK_WORD_BITS - 1; l >= 0; l--)
            if (bm & (1 << l)) break;
          nrec = k * HBOOK_WORD_BITS + l + 1;
          break;
        }
      }
      if (lrec <= 0 || nrec <= 0) {
        errlog(LOG_ERR,"ftp_get() : %s : lrec=%ld, nrec=%ld",remote,lrec,nrec);
        ret = SGD_RET_HBOOKFORM;
        goto doabort;
      }
      fsiz = lrec * HBOOK_WORD_BYTES * nrec;   /* file size in bytes */
      is_hbook = 1;
      break;
    }

  /* set up file stripe sizes etc. */

  if (is_hbook && fsiz >= bftp.bf_minsize) {
    long rslice, rstump, fslice, fstump;

    rslice = nrec / *fnum;
    rstump = nrec % *fnum;
    if (rstump > 0) {
      rslice++;
      rstump = nrec - (*fnum - 1) * rslice;
    }
    else
      rstump = rslice;
    if (rstump < 0) rstump = 0;
    fslice = rslice * lrec * 4;
    fstump = rstump * lrec * 4;
    for (i = 0; i < *fnum; i++) {
      if (i < *fnum - 1) {
        pt[i].pf_size = fslice;
        pt[i].pf_nrec = rslice;
      }
      else {
        pt[i].pf_size = fstump;
        pt[i].pf_nrec = rstump;
      }
    }
  }

  /* either not Hbook file or too small - don't stripe */

  else {
    for (i = 0; i < *fnum; i++)
      if (pt[i].pf_name) {
        unlink(pt[i].pf_name);
        x_free(pt[i].pf_name);
        pt[i].pf_name = (char *)0;
      }
    /* if the file is too small to be striped, get a new file name */
    if (is_hbook) {
      char cmd[2 * PATH_MAX + 50];
      char rbuf[PATH_MAX + 1], *s;
      FILE *f;
      unsigned long fss = fsiz / 1024;
      if (!(s = strrchr(local,'/')))
        s = local;
      else
        s++;
      sprintf(cmd,"%s -r -s%luK -u%s %s",dpmbin,fss,uname,s);
      if (!(f = popen(cmd,"r")) || fscanf(f,"%s%*[^\n]",rbuf) < 1) {
        errlog(LOG_ERR,"ftp_get() : can't read pipe (%s)",cmd);
        ret = SGD_RET_POPEN;
        if (f) pclose(f);
        goto doabort;
      }
      pclose(f);
      pt[0].pf_name = dupstr(rbuf);
    }
    else
      pt[0].pf_name = dupstr(local);
    pt[0].pf_size = fsiz;
    pt[0].pf_nrec = -1;
    *fnum = 1;
  }

  /* remove an existing file first */
  {
    char cmd[2 * PATH_MAX + 50];

    sprintf(cmd,"%s -f %s >/dev/null 2>&1",rmbin,local);
    if (system(cmd) != 0) {
      errlog(LOG_ERR,"ftp_get() : system(%s) trouble",cmd);
      ret = SGD_RET_RMBIN;
      goto doabort;
    }
  }

  if (slavelist) {
    for (i = tnum = 0; slavelist[i]; i++)
      if (slavelist[i] == ' ') tnum++;
    tnum++;
    trans = (struct peer *)x_malloc(tnum * sizeof trans[0]);
    for (i = 0; i < tnum; i++)
      trans[i].pr_machname = strtok(i == 0 ? slavelist : (char *)0," ");
    for (i = 0; i < tnum; i++) {
      strtok(trans[i].pr_machname,":");
      trans[i].pr_ipaddr = strtok((char *)0,":");
      trans[i].pr_conport = atoi(strtok((char *)0,":"));
      trans[i].pr_up = 0;
    }
  }
  else
    trans = (struct peer *)0;

  for (i = 0; i < *fnum; i++) {
    pt[i].pf_peer = (struct peer *)0;
    if (trans) {
      char *s = pt[i].pf_name + strlen(nfspath);
      for (j = 0; j < tnum; j++) {
        char *ss = strchr(trans[j].pr_machname,'.');
        if (!ss) ss = strchr(trans[j].pr_machname,'\0');
        if (strneql(s,trans[j].pr_machname,ss - trans[j].pr_machname) &&
            !streql(myipaddr,trans[j].pr_ipaddr))
          break;
      }
      if (j < tnum) {
        if (trans[j].pr_up)
          pt[i].pf_peer = &trans[j];
        else {
          struct sockaddr_in *addrp;
          if ((addrp = tcpconaddr(&trans[j])) &&
              (trans[j].pr_sock = tcpconnect(addrp)) >= 0) {
/*            errlog(LOG_INFO,"ftp_get() : data transfer to %s",
              trans[j].pr_machname);*/
            trans[j].pr_up = 1;
            pt[i].pf_peer = &trans[j];
            send_msg(trans[j].pr_sock,SGD_MSG_TRANS);
          }
          else
            errlog(LOG_ERR,
              "ftp_get() : can't open data transfer to %s port %d",
              trans[j].pr_machname,trans[j].pr_conport);
        }
      }
    }
    if (!pt[i].pf_peer) {
      pt[i].pf_out = fopen(pt[i].pf_name,"w");
      if (!pt[i].pf_out) {
        errlog(LOG_ERR,"ftp_get() : cannot open %s",pt[i].pf_name);
        ret = SGD_RET_FOPEN;
        goto doabort;
      }
#ifdef MEIKO_CS2
      ftp_setpfs(fd,bftp.bf_bufsize);
#endif
    }
    else
      pt[i].pf_out = (FILE *)0;
  }

  switch (curtype) {
  case TYPE_I:
  case TYPE_L:
    errno = 0;
    bytes = 0;
    i = 0;
    if (pt[i].pf_peer) {
      send_msg(pt[i].pf_peer->pr_sock,SGD_MSG_TRANS);
      sendstr(pt[i].pf_peer->pr_sock,pt[i].pf_name);
      sendul(pt[i].pf_peer->pr_sock,pt[i].pf_size);
    }
    while ((c = ibytes > totbytes ? (long)ibytes :
            read(fileno(din),buf,(size_t)bftp.bf_bufsize)) > 0) {
      size_t w = 0;
      totbytes += c;
/*      fprintf(foo,"ibytes=%d, totbytes=%d, c=%d\n",ibytes,totbytes,c);/**/
      for (;;) {
        size_t d = c - w < pt[i].pf_size - bytes ?
                      c - w : pt[i].pf_size - bytes;
/*        fprintf(foo,"  i=%d, w=%d, bytes=%d, pt[i].pf_size=%d, d=%d\n",
            i,w,bytes,pt[i].pf_size,d);/**/
        if (d > 0) {
          long n;
          size_t done;
#ifdef lint
          n = 0;
#endif
          for (done = 0; done < d; done += n) {
            size_t todo = d - done;
            n = write(
                  pt[i].pf_peer ? pt[i].pf_peer->pr_sock :
                    fileno(pt[i].pf_out),
                  buf + w + done,
                  todo > bftp.bf_dotransfer ? bftp.bf_dotransfer : todo);
            if (n < 0) {
              if (pt[i].pf_peer)
                errlog(LOG_ERR,"ftp_get() : error transferring %s (%s %d)",
                  pt[i].pf_name,pt[i].pf_peer->pr_machname,
                  pt[i].pf_peer->pr_conport);
              else
                errlog(LOG_ERR,"ftp_get() : error writing %s",pt[i].pf_name);
              errno = 0;
              ret = SGD_RET_WRITE;
              goto doabort;
            }
          }
        }
        bytes += d;
        w += d;
        if (bytes >= pt[i].pf_size) bytes = 0;
        if (w >= c) {
          if (bytes == 0 && i < *fnum - 1) {
            i++;
            if (pt[i].pf_peer) {
              send_msg(pt[i].pf_peer->pr_sock,SGD_MSG_TRANS);
              sendstr(pt[i].pf_peer->pr_sock,pt[i].pf_name);
              sendul(pt[i].pf_peer->pr_sock,pt[i].pf_size);
            }
          }
          break;
        }
        if (i < *fnum - 1) {
          i++;
          if (pt[i].pf_peer) {
            send_msg(pt[i].pf_peer->pr_sock,SGD_MSG_TRANS);
            sendstr(pt[i].pf_peer->pr_sock,pt[i].pf_name);
            sendul(pt[i].pf_peer->pr_sock,pt[i].pf_size);
          }
        }
        else
          break;
      }
    }
    if (c < 0 && errno != EPIPE) {
      errlog(LOG_ERR,"ftp_get() : read error from peer");
      errno = 0;
      ret = SGD_RET_FTPREAD;
      goto doabort;
    }
#if 0
    if (d < c) {
      if (d < 0)
        fprintf(foo, "local3: %s: %s\n", local,
          strerror(errno));
      else
        fprintf(foo, "%s: short write\n", local);
    }
#endif
    break;
  case TYPE_E:
  case TYPE_A:
    break;
  }
  ftp_getreply(0);
  ret = SGD_RET_OK;
  if (is_hbook) {
    if (do_striping && fsiz >= bftp.bf_minsize) {
      FILE *lf;

      if (!(lf = fopen(local,"w"))) {
        errlog(LOG_ERR,"ftp_get() : cannot open %s",local);
        ret = SGD_RET_FOPEN;
        goto getout;
      }
      fprintf(lf,"%s\n%d %ld %ld %lu\n",FSTRIPE_SIG,*fnum,nrec,lrec,fsiz);
      for (i = 0; i < *fnum; i++)
        fprintf(lf,"%ld %lu %s\n",pt[i].pf_nrec,pt[i].pf_size,pt[i].pf_name);
      fclose(lf);
      /* use setuid bit to signify striped file (setuid bit has
         otherwise no meaning for data files) */
      chmod(local,S_ISUID | useruperm);
    }
    else
      symlink(pt[0].pf_name,local);
  }
  goto getout;

  doabort:;

  for (i = 0; i < *fnum; i++)
    if (pt[i].pf_name) unlink(pt[i].pf_name);

  /* abort using RFC959 recommended IP,SYNC sequence  */

  if (!cpend) {
    code = -1;
    goto getout;
  }
  ftp_abort_remote(din);
  code = -1;

  getout:;

  /* do clean-up and get outta here */

  gettimeofday(&stop,(struct timezone *)0);
  if (data >= 0) {
    x_close(data);
    data = -1;
  }
  if (din) fclose(din);
  if (buf) x_free(buf);
  for (i = 0; i < *fnum; i++) {
    if (pt[i].pf_peer) {
      if (pt[i].pf_peer->pr_up) {
#if 0
        char tmp;
        /* do a half-close on the socket disallowing writes and thus
           delivering EOF to the other party - then scan for an EOF
           which comes when the peer has finished writing the data
           and closes its part of the socket (we have to wait for all
           writes to finish or our consistency checks later may fail) */
        x_shutdown(pt[i].pf_peer->pr_sock,1);
        while (read(pt[i].pf_peer->pr_sock,&tmp,1) > 0) ;
#endif
        /* exchange explicit acks instead of half close tricks */
        send_msg(pt[i].pf_peer->pr_sock,SGD_MSG_OK);
        recv_msg(pt[i].pf_peer->pr_sock);
        pt[i].pf_peer->pr_up = 0;
      }
    }
    else if (pt[i].pf_out)
      fclose(pt[i].pf_out);
    chmod(pt[i].pf_name,useruperm);
  }
  if (trans) x_free(trans);
  if (totbytes > 0)
    ftp_ptransfer("received",totbytes,&start,&stop);
  return ret;
}

static void
ftp_tvsub(struct timeval *tdiff,struct timeval *t1,struct timeval *t0)
{
  tdiff->tv_sec = t1->tv_sec - t0->tv_sec;
  tdiff->tv_usec = t1->tv_usec - t0->tv_usec;
  if (tdiff->tv_usec < 0) {
    tdiff->tv_sec--;
    tdiff->tv_usec += 1000000;
  }
}

static void
ftp_ptransfer(char *direction,long bytes,struct timeval *t0,struct timeval *t1)
{
  struct timeval td;
  long bs;

  ftp_tvsub(&td,t1,t0);
  bs = bytes / (td.tv_sec == 0 ? 1 : td.tv_sec);
  fprintf(scriptf,"%ld bytes %s in %ld.%.2ld seconds (approx %ld bytes/s)\n",
    bytes,direction,td.tv_sec,td.tv_usec / 10000,bs);
}

static char *
ftp_hookup(char *host,int port)
{
  register struct hostent *hp = 0;
  int s, len;
  static char hostnamebuf[80];

  memset((void *)&hisctladdr,0,sizeof hisctladdr);
  hisctladdr.sin_addr.s_addr = inet_addr(host);
  if (hisctladdr.sin_addr.s_addr != (unsigned long)-1) {
    hisctladdr.sin_family = AF_INET;
    strncpy(hostnamebuf,host,sizeof hostnamebuf);
  } else {
    hp = gethostbyname(host);
    if (!hp) {
      errlog(LOG_ERR, "ftp_hookup() : gethostbyname fails (host %s)",host);
/*      herror((char *)0);*/
      code = -1;
      return (char *)0;
    }
    hisctladdr.sin_family = hp->h_addrtype;
    memcpy(&hisctladdr.sin_addr,hp->h_addr_list[0],hp->h_length);
    strncpy(hostnamebuf,hp->h_name,sizeof hostnamebuf);
  }
  s = x_socket(hisctladdr.sin_family,SOCK_STREAM,0);
  if (s < 0) {
    errlog(LOG_ERR,"ftp_hookup() : x_socket fails");
    code = -1;
    return (char *)0;
  }
  /* port already in network byte order */
  hisctladdr.sin_port = port;
  while (x_connect(s,(struct sockaddr *)&hisctladdr,sizeof hisctladdr) < 0) {
    if (hp && hp->h_addr_list[1]) {
      errlog(LOG_INFO,"ftp_hookup() : can't connect to address %s",
        inet_ntoa(hisctladdr.sin_addr));
      hp->h_addr_list++;
      memcpy(&hisctladdr.sin_addr,hp->h_addr_list[0],hp->h_length);
      errlog(LOG_INFO,"ftp_hookup() : trying alternate address %s",
        inet_ntoa(hisctladdr.sin_addr));
      x_close(s);
      s = x_socket(hisctladdr.sin_family,SOCK_STREAM,0);
      if (s < 0) {
        errlog(LOG_ERR,"ftp_hookup() : x_socket fails");
        code = -1;
        return (char *)0;
      }
      continue;
    }
    errlog(LOG_ERR,"ftp_hookup() : giving up connect");
    code = -1;
    goto bad;
  }
  len = sizeof myctladdr;
  if (getsockname(s,(struct sockaddr *)&myctladdr,&len) < 0) {
    errlog(LOG_ERR,"ftp_hookup() : getsockname fails");
    code = -1;
    goto bad;
  }
  if (!tcpsockopt(s)) {
    errlog(LOG_ERR,"ftp_hookup() : tcpsockopt fails");
    goto bad;
  }
#ifdef IP_TOS
  {
    int tos = IPTOS_LOWDELAY;
    if (x_setsockopt(s,IPPROTO_IP,IP_TOS,(char *)&tos,sizeof tos) < 0)
      errlog(LOG_ERR,"ftp_hookup() : x_setsockopt IP_TOS (ignored)");
  }
#endif
  cin = fdopen(s,"r");
  cout = fdopen(s,"w");
  if (!cin || !cout) {
    errlog(LOG_ERR,"ftp_hookup() : fdopen fails");
    if (cin)
      fclose(cin);
    if (cout)
      fclose(cout);
    code = -1;
    goto bad;
  }
/*  errlog(LOG_INFO,"ftp_hookup() : connected to %s",hostnamebuf);*/
  if (ftp_getreply(0) > 2) {   /* read startup message from server */
    if (cin)
      fclose(cin);
    if (cout)
      fclose(cout);
    code = -1;
    goto bad;
  }
#ifdef SO_OOBINLINE
  {
    int on = 1;

    if (x_setsockopt(s,SOL_SOCKET,SO_OOBINLINE,(char *)&on,sizeof on) < 0)
      errlog(LOG_ERR,"ftp_hookup() : x_setsockopt SO_OOBINLINE");
  }
#endif /* SO_OOBINLINE */

  return hostnamebuf;

  bad:;

  x_close(s);
  return (char *)0;
}

static int
ftp_login(char *host,char *user,char *pass,char *acct)
{
  int n, aflag = 0;

  n = ftp_command("USER %s",user);
  if (n == CONTINUE) {
    if (!pass) {
      errlog(LOG_ERR,"ftp_login() : password required for user %s@%s",
        user,host);
      return 0;
    }
    n = ftp_command("PASS %s",dcpass(pass));
    (void)dcpass((char *)0);
  }
  if (n == CONTINUE) {
    aflag = 1;
    n = ftp_command("ACCT %s",acct);
  }
  if (n != COMPLETE) {
    errlog(LOG_ERR,"ftp_login() : login failed for user %s@%s",user,host);
    return 0;
  }
  if (!aflag && acct)
    ftp_command("ACCT %s",acct);
  return 1;
}

/*VARARGS*/
static int
ftp_command(char *fmt,...)
{
  va_list ap;

  if (!cout) {
    errlog(LOG_ERR,"ftp_command() : no control connection for command %s",fmt);
    code = -1;
    return 0;
  }
  va_start(ap,fmt);
  vfprintf(cout,fmt,ap);
  va_end(ap);
  fprintf(cout,"\r\n");
  fflush(cout);
  cpend = 1;
  return ftp_getreply(streql(fmt,"QUIT"));
}

static int
ftp_getreply(int expecteof)
{
  register int c, n;
  register int dig;
  register char *cp;
  int originalcode = 0, continuation = 0;

  for (;;) {
    dig = n = code = 0;
    cp = reply_string;
    while ((c = getc(cin)) != '\n') {
      if (c == IAC) {     /* handle telnet commands */
        switch (c = getc(cin)) {
        case WILL:
        case WONT:
          c = getc(cin);
          fprintf(cout,"%c%c%c",IAC,DONT,c);
          fflush(cout);
          break;
        case DO:
        case DONT:
          c = getc(cin);
          fprintf(cout,"%c%c%c",IAC,WONT,c);
          fflush(cout);
          break;
        default:
          break;
        }
        continue;
      }
      dig++;
      if (c == EOF) {
        if (expecteof) {
          code = 221;
          return 0;
        }
        ftp_lostpeer();
        errlog(LOG_ERR,"ftp_getreply() : 421 service not available, \
remote server has closed connection");
        code = 421;
        return 4;
      }
      if (c != '\r')
        putc(c,scriptf);
      if (dig < 4 && isdigit(c))
        code = code * 10 + (c - '0');
      if (dig == 4 && c == '-') {
        if (continuation)
          code = 0;
        continuation++;
      }
      if (n == 0)
        n = c;
      if (cp < &reply_string[sizeof reply_string - 1])
        *cp++ = c;
    }
    putc(c,scriptf);
    fflush(scriptf);
    if (continuation && code != originalcode) {
      if (originalcode == 0)
        originalcode = code;
      continue;
    }
    *cp = '\0';
    if (n != '1')
      cpend = 0;
    if (code == 421 || originalcode == 421)
      ftp_lostpeer();
    else if (code == 888)
      clioretry = 1;
    else if (code == 999)
      clioret = atoi(reply_string + sizeof "999 RC " - 1);
    return n - '0';
  }
}

static void
ftp_sigalarm()
{
  exit(SGD_RET_TOOLONG);
}

static void
ftp_setalarm()
{
  struct sigaction sact;

  if (stagecancel < 0) return;
  sact.sa_handler = ftp_sigalarm;
  sigemptyset(&sact.sa_mask);
  sact.sa_flags = 0;
  if (sigaction(SIGALRM,&sact,(struct sigaction *)0) < 0) {
    errlog(LOG_ERR,"ftp_setalarm() : cannot set SIGALRM");
    return;
  }
  alarm(stagecancel);
}

int
ftp_clio(FILE *f,struct client *cl_dat,struct stage *st_dat,
  char *volid,char *fseqid,char *lbltyp,char *volser)
{
  int i, ret;

  scriptf = f;
  ftp_setalarm();
  if ((ret = ftp_setpeer(bclio.bc_server,bclio.bc_login,(char *)0,(char *)0,
      htons(bclio.bc_port))) != SGD_RET_OK)
    goto end;
  ftp_quote("site vers %d",bclio.bc_version);
  ftp_quote("site acct %s",cl_dat->ct_username);
  if (bclio.bc_blocksize)
    ftp_quote("site blksize %s",bclio.bc_blocksize);
  if (bclio.bc_fileid)
    ftp_quote("site dsn %s",bclio.bc_fileid);
  if (bclio.bc_format)
    ftp_quote("site format %s",bclio.bc_format);
  if (bclio.bc_reclen)
    ftp_quote("site lrecl %s",bclio.bc_reclen);
  ftp_quote("site blks %d",bclio.bc_datsiz);
  ftp_quote("site vid %s",volid);
  ftp_quote("site fseq %s",fseqid);
  ftp_quote("site label %s",lbltyp);
  ftp_quote("site vsn %s",volser);
  clioret = NO_CLIO_RET;
  clioretry = 0;
  for (i = 0; i < bclio.bc_retries; i++) {
    if ((ret = ftp_get(st_dat->sg_psfname,st_dat->sg_ptfname,
        st_dat->sg_tfname,&st_dat->sg_fnum,st_dat->sg_fsiz,
        st_dat->sg_slavelist,cl_dat->ct_username)) != SGD_RET_OK)
      goto end;
    if (!clioretry) break;
    sleep(bclio.bc_wait);
  }
  if (i >= bclio.bc_retries)
    ret = SGD_RET_CLIOTIMEOUT;
  else
    ret = clioret;
  ftp_quote("site srep");

  end:;

  ftp_disconnect();
  return ret;
}

int
ftp_ftp(FILE *f,struct client *cl_dat,struct stage *st_dat,
  char *fdir,char *fname)
{
  int ret;
  struct servent *sp;

  scriptf = f;
  ftp_setalarm();
  if (!(sp = getservbyname("ftp","tcp"))) {
    errlog(LOG_ERR,"ftp_ftp() : ftp/tcp: unknown service");
    return SGD_RET_FTPCONN;
  }
  if ((ret = ftp_setpeer(cl_dat->ct_ipaddr,cl_dat->ct_fusername,
      cl_dat->ct_passwd,(char *)0,sp->s_port)) != SGD_RET_OK)
    goto end;
  if (fdir && (ret = ftp_cd(fdir)) != SGD_RET_OK)
    goto end;
  ret = ftp_get(fname,st_dat->sg_ptfname,st_dat->sg_tfname,
    &st_dat->sg_fnum,st_dat->sg_fsiz,st_dat->sg_slavelist,
    cl_dat->ct_username);

  end:;

  ftp_disconnect();
  return ret;
}
