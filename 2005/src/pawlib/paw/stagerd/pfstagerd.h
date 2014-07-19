/*
 * $Id: pfstagerd.h,v 1.1 1996/04/02 22:00:40 thakulin Exp $
 *
 * $Log: pfstagerd.h,v $
 * Revision 1.1  1996/04/02 22:00:40  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
 * Paw
 *
 */
/*CMZ :          13/11/95  18.36.34  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * pfstagerd.h
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    12/8/94
 *
 *************************************************************************/

#ifndef PFSTAGERD_H
#define PFSTAGERD_H

/* all system include files are here so each file sees them */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <setjmp.h>
#include <signal.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>

#include <pwd.h>
#include <fcntl.h>
#include <syslog.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/stat.h>
#ifdef _AIX
#define NBBY 8
#include <sys/select.h>
#endif

#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

/* messages to pass */

#define SGD_MSG_NONE        0
#define SGD_MSG_ERROR       1     /* error occurred */
#define SGD_MSG_OK          2     /* an operation (whichever) was
                                     successfully completed */
#define SGD_MSG_STAGE       3     /* perform staging */
#define SGD_MSG_STATUS      4     /* status query */
#define SGD_MSG_INIT        5     /* slaves initiate connection */
#define SGD_MSG_PING        6     /* hello, are you there */
#define SGD_MSG_FORGET      7     /* forget about this stage */
#define SGD_MSG_TRANS       8     /* do a slave-slave data transfer */

/* file types */

#define SGD_FTYP_NONE       0
#define SGD_FTYP_LOCAL      1     /* file on user's local disk */
#define SGD_FTYP_TAPE       2     /* file on CERN tape server */
#define SGD_FTYP_FATMEN     3     /* FATMEN path */

/* stage statuses */

#define SGD_STATUS_NONE     0
#define SGD_STATUS_ERROR    1     /* an error occurred in staging */
#define SGD_STATUS_DONE     2     /* staging successfully completed */
#define SGD_STATUS_RUNNING  3     /* staging process running on a slave */
#define SGD_STATUS_PENDING  4     /* staging request not yet sent to slave */
#define SGD_STATUS_CONNECT  5     /* trouble (hopefully temporary) while
                                     talking to a slave */

/* pfstagerd return codes */

#define SGD_RET_OK          0
#define SGD_RET_STAGERBIN   1
#define SGD_RET_FTPBIN      2
#define SGD_RET_PATHLEN     3
#define SGD_RET_SFNAME      4
/* 5 is special (comes from CLIO) */
#define SGD_RET_FOPEN       6
#define SGD_RET_POPEN       7
#define SGD_RET_STAT        8
#define SGD_RET_PW          9
#define SGD_RET_PWDIR       10
#define SGD_RET_CHDIR       11
#define SGD_RET_CHMOD       12
#define SGD_RET_CHOWN       13
#define SGD_RET_FTPCONN     14
#define SGD_RET_FTPREAD     15
#define SGD_RET_FTPCD       16
#define SGD_RET_FTPLOGIN    17
#define SGD_RET_HBOOKFORM   18
#define SGD_RET_WRITE       19
#define SGD_RET_CLIOTIMEOUT 20
#define SGD_RET_SIGNAL      21
#define SGD_RET_FATMENFILE  22
#define SGD_RET_TOOLONG     23
#define SGD_RET_TAPEID      24
#define SGD_RET_FATMENBIN   25
#define SGD_RET_LOCKERR     26
#define SGD_RET_RMBIN       27
#define SGD_RET_FORKERR     28
#define SGD_RET_CONCHECK    29
#define SGD_RET_TFNAME      30
#define SGD_RET_PIAFHOME    31
#define SGD_RET_UNKNOWN     32

#define SGD_RET_LASTCODE    SGD_RET_UNKNOWN

/* some characteristic constants about Hbook files */

#define HBOOK_WORD_BYTES  4
#define HBOOK_WORD_BITS   (HBOOK_WORD_BYTES * 8)
#define HBOOK_EXT         ".hbook"

#define FSTRIPE_SIG     "###FSTRIPE-V001###"

#define do_striping     (bftp.bf_stripenum > 0)

/* data structure for the peers of the daemon process
   (of slaves kept by masters and of masters kept by slaves) */

struct peer {
  char          *pr_machname;   /* machine */
  char          *pr_ipaddr;     /* IP address */
  time_t        pr_last;        /* time of the last connection */
  int           pr_conport;     /* remote port to connect to */
  int           pr_lisport;     /* local port to listen to */
  int           pr_sock;        /* effective socket number when connected */
  int           pr_up;          /* peer state: up=1, down=0 */
  int           pr_keep;        /* keep listening to this peer until done
                                   (only meaningful when shutting
                                    down masters) */
  struct peer   *pr_next;
};

/* data structure for file stripes across disks */

struct pfinfo {
  char          *pf_name;
  unsigned long pf_size;
  long          pf_nrec;
  FILE          *pf_out;
  struct peer   *pf_peer;
};

/* struct for holding info about revectored stage acks */

struct revector {
  unsigned long rt_stageid;
  time_t        rt_time;
  struct revector *rt_next;
};

/* struct for handling slave-slave transfer requests */

struct transfer {
  pid_t         tf_pid;
  struct transfer *tf_next;
};

/* data structure for the ongoing staging processes of the client */

struct stage {

  /* fields that come from the client */
  char          *sg_sfname;     /* source file name */
  char          *sg_tfname;     /* target file name (optional) */
  char          *sg_prserver;   /* preferred server (optional) */
  int           sg_ftyp;        /* file type (local, tape, fatmen) */
  unsigned long sg_fsiz;        /* file size in bytes (slave may update) */
  int           sg_nowait;      /* nowait-mode (PAW session doesn't wait) */

  /* master fills in these */
  int           sg_fnum;        /* number of stripes */
  struct pfinfo *sg_ptfname;    /* physical target file name(s) */
  char          *sg_masterip;   /* which master (its ip address) */
  char          *sg_serverip;   /* which server (its ip address) */
  unsigned long sg_stageid;     /* internal id number for the stage */
  time_t        sg_queuetime;   /* time when stage operation queued */
  time_t        sg_starttime;   /* time when stage operation started */
  time_t        sg_endtime;     /* time when - " - ended */

  /* slave does the rest */
  char          *sg_otfname;    /* old target file name (if modified
                                   by the slave for some reason) */
  char          *sg_psfname;    /* physical source file name */
  unsigned long sg_fdone;       /* how many bytes of the file has
                                   been transferred */
  int           sg_status;      /* how are things proceeding */
  int           sg_retcode;     /* return code from the stager */

  /* these are only used internally */
  char          *sg_slavelist;  /* list of other available slaves */
  pid_t         sg_childpid;    /* pid of the slave child doing the staging */
  char          *sg_lfname;     /* lock file name */
  struct stage  *sg_next;
};

/* data structure for the clients kept by masters */

struct client {
  char          *ct_username;   /* client user */
  char          *ct_machname;   /* client machine (needed for sending mail) */
  char          *ct_ipaddr;     /* client IP address for FTP */
  char          *ct_fusername;  /* client user for FTP (may be different) */
  char          *ct_passwd;     /* user's password on client for FTP
                                   (optional) */
  unsigned long ct_clientid;    /* internal id number for the client */
  struct stage  *ct_stagelist;  /* list of ongoing stages */
  struct client *ct_next;
};

/* for the builtin ftp client to connect to clio */

struct cliopar {

  /* default parameters */
  char *bc_blocksize;
  char *bc_fileid;
  char *bc_format;
  char *bc_reclen;
  int  bc_datsiz;

  /* set in config file */
  int  bc_version;
  char *bc_server;
  int  bc_port;
  char *bc_login;
  int  bc_retries;
  int  bc_wait;
};

struct ftppar {
  long bf_bufsize;          /* size of the receive buffer */
  int  bf_stripenum;        /* max number of file stripes to use */
  long bf_minsize;          /* minimum stripe size */
  long bf_dotransfer;       /* buffer size for slave-slave transfers */
};

struct tcppar {
  long tc_rcvbuf;           /* TCP receive buffer size */
  long tc_sndbuf;           /* TCP send buffer size */
  int  tc_nodelay;          /* use TCP nodelay option or not */
};

/* number of elements in a table */
#define SIZE(s) (int)(sizeof(s) / sizeof((s)[0]))

/* to make string comparisons a bit more readable */
#define streql(a,b)     !strcmp(a,b)
#define strneql(a,b,n)  !strncmp(a,b,n)

/* IDBITS tells how many bits the client and stage id's can have.
   The only sensible value for this nowadays is 32, since most
   workstations are 32 bit machines.

   MASTERBITS is a magic constant setting bits allocated to master id
   in the client id and stage id fields.  Thereby the maximum number of
   masters that can coexist is 2^MASTERBITS.  If 3 bits are reserved
   for master id, one can have 8 masters with different id's with
   numbering going from 0 to 7.

   TIMESTAMPBITS allocates bits for the cycle of the entire id after
   which same id's start to be reused.  This cycle is expressed in
   seconds and it has to be "long" to prevent any spurious matches in id's.

   REQUESTBITS allocates bits for the max. number of staging requests that
   can be processed per second.  If more than the max. number arrives
   during the duration of the same timestamp value, the process waits
   for the timestamp to advance one step to keep id's unique.  No
   requests are ever discarded, however. */

#define IDBITS          32

#define MASTERBITS      3   /* max 2^3 = 8 masters */
#define TIMESTAMPBITS   25  /* max 2^25 = 33,554,432 seconds = 388 days */
#define REQUESTBITS     4   /* max 2^4 = 16 requests / second */

#define MAX_MASTERS     (1 << MASTERBITS)
#define MAX_TIMESTAMPS  (1 << TIMESTAMPBITS)
#define MAX_REQUESTS    (1 << REQUESTBITS)

#define NEW_CLIENTID    new_id(1)
#define NEW_STAGEID     new_id(0)

#define MAST_NUM(x)     ((x) >> (IDBITS - MASTERBITS))

/* how many times to try a system call (like close()) before giving up */
#define MAX_SYSATTEMPTS   100

/* for how many seconds to allow a recv() on a socket before giving up */
#define RECV_ALARMTIMEOUT 300

/* defaults for timeouts */
#define DEF_PENDINGTIME   (5 * 60)
#define DEF_SELECTTIMEOUT (5 * 60)
#define DEF_STAGETIMEOUT  -1
#define DEF_PINGINTERVAL  -1
#define DEF_STAGECANCEL   -1

/* default umask for the user - group readable, world nil */
#define DEF_USERUMASK     026

#define useruperm       (userumask ^ 0666)

/* default tmp and nfs paths (have to end in a slash) */
#define DEF_TMPPATH     "/tmp/"
#define DEF_NFSPATH     "/nfs/"

/* default receive buffer size for slave-slave data transfers */
#define DEF_TRRECVBUF   (16 * 1024)

/* default piafhome (has to end in a slash) */
#define DEF_PIAFHOME    "piaf/"

/* template for mktmp() */
#define TMP_TEMPLATE    "pfstagerdXXXXXX"

/* lock file extension */
#define LOCKFILE_EXT    ".#fslock#"

/* maximum length of client user password */
#define MAX_PASSWD      40

/* patches for stuff that should be defined in the system headers,
   but aren't always */
#ifndef PATH_MAX
# ifdef MAXPATHLEN
#  define PATH_MAX MAXPATHLEN
# else
#  define PATH_MAX 255
# endif
#endif
#if defined(ultrix) || defined(__hpux) || defined(_AIX)
extern void syslog(int,char *,...);
#endif
#ifndef S_ISLNK
# define S_ISLNK(mode)  (((mode) & S_IFMT) == S_IFLNK)
#endif
#ifndef LOG_AUTH
# define LOG_LOCAL0  0
# define LOG_LOCAL1  0
# define LOG_LOCAL2  0
# define LOG_LOCAL3  0
# define LOG_LOCAL4  0
# define LOG_LOCAL5  0
# define LOG_LOCAL6  0
# define LOG_LOCAL7  0
#endif
#ifndef LOG_PID
# define LOG_PID     0
#endif
#ifndef LOG_CONS
# define LOG_CONS    0
#endif

/* IBM tape identifier components */
#define MAX_VOLID   6
#define MAX_FSEQID  2
#define MAX_LBLTYP  3
#define MAX_VOLSER  6
#define MAX_TAPEID  (MAX_VOLID + MAX_FSEQID + MAX_LBLTYP + MAX_VOLSER + 3)

/* IBM tape identifier defaults */
#define FSEQID_DEFAULT  1
#define LBLTYP_DEFAULT  "sl"

/* max lengths for string conversion for unsigned numbers -
   should be enough for up to 64 bit longs */
#define MAX_ULSTR    20

/* max length of a message string */
#define MAX_MSGSTR   20

#define send_msg(s,m)     sendstr(s,sgd_msg[m])
#define send_ftyp(s,m)    sendstr(s,sgd_ftyp[m])
#define send_status(s,m)  sendstr(s,sgd_status[m])
#define recv_msg(s)       recvmstr(s,sgd_msg)
#define recv_ftyp(s)      recvmstr(s,sgd_ftyp)
#define recv_status(s)    recvmstr(s,sgd_status)

#define find_slavebyip(ip)  find_peerbyip(slavelist,ip)
#define find_masterbyip(ip) find_peerbyip(selectlist,ip)
#define tcplisaddr(p)       tcpaddr(0,p)
#define tcpconaddr(p)       tcpaddr(1,p)

#ifndef PSTAGE

/* external function prototypes */

/* sockio.c */

extern int recvul(int,unsigned long *);
extern int sendul(int,unsigned long);
extern int recvstr(int,char **,size_t *);
extern int recvmstr(int,char **);
extern int sendstr(int,char *);

/* debuglog.c */

#ifdef DEBUG_LOG
# define DLOG_OPEN(s)         dlog_open(s)
# define DLOG_REOPEN(s)       (dlog_close(), dlog_open(s))
# define DLOG_CLOSE()         dlog_close()
# define DLOG_MSG(s)          dlog_msg s
# define DLOG_ERROR(s)        dlog_error s
# define DLOG_CLIENT(s)       dlog_client(s)
# define DLOG_STAGE(s)        dlog_stage(s)
# define DLOG_SOCKADDR_IN(a)  dlog_sockaddr_in(a)
extern void dlog_open(char *);
extern void dlog_close(void);
extern void dlog_msg(char *,...);
extern void dvlog_error(char *,va_list);
extern void dlog_error(char *,...);
extern void dlog_client(struct client *);
extern void dlog_stage(struct stage *);
extern void dlog_sockaddr_in(struct sockaddr_in *);
#else
# define DLOG_OPEN(s)
# define DLOG_REOPEN(s)
# define DLOG_CLOSE()
# define DLOG_MSG(s)
# define DLOG_ERROR(s)
# define DLOG_CLIENT(s)
# define DLOG_STAGE(s)
# define DLOG_SOCKADDR_IN(a)
#endif /* DEBUG_LOG */

/* master.c */

extern int m_master_ops(int,struct revector **);
extern int m_flush_pending(long *);

/* slave.c */

extern int s_slave_ops(int,struct revector **);
extern int s_check_children(void);
extern int s_init_masters(void);
extern int s_ping_masters(long *);

/* child.c */

extern void c_child_ops(struct client *,struct stage *);

/* syscalls.c */

#ifdef DEBUG_ALLOC
extern void dump_xallocinfo(FILE *);
#endif
extern void * x_malloc(size_t);
extern void x_free(void *);
extern int x_select(int,fd_set *,fd_set *,fd_set *,struct timeval *);
extern int x_socket(int,int,int);
extern int x_setsockopt(int,int,int,char *,int);
extern int x_connect(int,struct sockaddr *,int);
extern int x_accept(int,struct sockaddr *,int *);
extern int x_bind(int,struct sockaddr *,int);
extern int x_listen(int,int);
extern int x_close(int);
extern int x_shutdown(int,int);

/* dump.c */

extern void dumpstatus(void);

/* codestr.c */

extern char * st_retcodestr(int);

/* pfstagerd.c */

extern void errlog(int,char *,...);
extern void joblog(struct client *,struct stage *);
extern void fatal(int,char *,...);
extern void get_child_stat(pid_t,int,struct stage *);

/* util.c */

/*extern char * true_tfname(char *);*/
extern int is_striped(char *);
extern unsigned long new_id(int);
extern char * dcpass(char *);
extern char * dupstr(char *);
extern struct revector * add_revect(unsigned long);
extern void remove_revect(struct revector *);
extern struct revector * find_revect(unsigned long);

/* tcputil.c */

extern int tcpsockopt(int);
extern char * get_peerip(int,struct sockaddr_in **);
extern int tcpconnect(struct sockaddr_in *);
extern int tcplisten(struct sockaddr_in *);
extern struct sockaddr_in * tcpaddr(int,struct peer *);
extern int ping_peer(struct peer *);
extern void tell_to_forget(struct peer *,struct revector *);

/* lists.c */

extern struct client * find_client(struct client *);
extern struct peer * find_peerbyip(struct peer *,char *);
extern char * find_peeripbyname(char *);
extern char * find_peeripbypath(char *);
extern struct stage * find_stagebyid(unsigned long,struct client **);
extern struct client * find_clientbyid(unsigned long);
extern struct client * find_clientbystageid(unsigned long);
extern struct stage * find_stagebypid(pid_t);
extern void remove_transferbypid(pid_t);
extern void release_peer(struct peer *);
extern void release_client(struct client *);
extern void release_stage(struct stage *);
extern void remove_client(struct client *);
extern void remove_stage(struct client *,struct stage *);
extern size_t prlen_ulong(unsigned long);
extern size_t prlen_long(long);
extern size_t prlen_stage(struct stage *);

/* transfer.c */

extern int put_client(int,struct client *);
extern int get_client(int,struct client *);
extern int put_stage(int,struct stage *);
extern int get_stage(int,struct stage *);

/* bftp.c */

extern int ftp_clio(FILE *,struct client *,struct stage *,
                    char *,char *,char *,char *);
extern int ftp_ftp(FILE *,struct client *,struct stage *,char *,char *);

/* global variables */

extern int master;
extern char *myname;
extern char *myipaddr;
extern char *mymachname;
extern char *mailbin;
extern char *ftpbin;
extern char *stagerbin;
extern char *fatmenbin;
extern char *dpmbin;
extern char *rmbin;
extern char *nfspath;
extern char *tmppath;
extern struct cliopar bclio;
extern struct ftppar bftp;
extern struct tcppar tcpparam;
extern char *syslogchn;
extern char *errlogname;
extern char *joblogname;
extern char *dumpname;
extern char *admin;
extern char *piafhome;
extern int pendingtime;
extern int selecttimeout;
extern int stagetimeout;
extern int pinginterval;
extern int stagecancel;
extern int userumask;
extern char *configfile;
extern int updateconfig;
extern int doshutdown;
extern unsigned long master_id;
extern struct client *clientlist;
extern struct peer *selectlist;
extern struct peer *slavelist;
extern struct revector *revectorlist;
extern struct transfer *transferlist;
extern struct client cl_init;
extern struct stage st_init;
extern struct peer pe_init;
extern char *ftyptb[];
extern char *statustb[];
extern char *sgd_msg[];
extern char *sgd_ftyp[];
extern char *sgd_status[];

#endif /* PSTAGE */

#endif /* PFSTAGERD_H */
