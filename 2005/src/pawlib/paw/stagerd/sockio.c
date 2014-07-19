/*
 * $Id: sockio.c,v 1.2 1996/04/02 22:00:32 thakulin Exp $
 *
 * $Log: sockio.c,v $
 * Revision 1.2  1996/04/02 22:00:32  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :  2.06/13 25/08/94  13.03.06  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * sockio.c
 *
 * Various receive and send functions are used throughout the code.
 * There are only three types of data transferred: unsigned longs,
 * fixed length message strings and variable length strings.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    15/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

static int alarmset;
static jmp_buf xrecv_jmp;

static void
xrecv_sigalarm()
{
  longjmp(xrecv_jmp,1);
}

static int
x_recv(int sock,char *buf,size_t len)
{
  size_t n;
  int nn, ret;


  /* now, this sure is a stupid kludge, but we just can't
     afford having the recv() block for ages when something
     goes wrong... */
  if (!alarmset) {
    struct sigaction sact;
    sact.sa_handler = xrecv_sigalarm;
    sigemptyset(&sact.sa_mask);
    sact.sa_flags = 0;
    if (sigaction(SIGALRM,&sact,(struct sigaction *)0) < 0)
      errlog(LOG_ERR,"x_recv() : cannot set SIGALRM");
    alarmset = 1;
  }
  if (setjmp(xrecv_jmp)) {
    errlog(LOG_ERR,"x_recv() : timeout (%d seconds)",RECV_ALARMTIMEOUT);
    ret = 0;
    goto end;
  }
  alarm(RECV_ALARMTIMEOUT);
#ifdef lint
  nn = 0;
#endif
  ret = 1;
  for (n = 0; n < len; n += (size_t)nn) {
#if 0
    int m = 0;
#endif
    DLOG_MSG(("x_recv() : len=%u, n=%u",len,n));
    errno = 0;
    while ((nn = recv(sock,&buf[n],len - n,0)) <= 0) {
      if (nn == 0) goto end;
#if 0
      /* tolerate EOF for a while */
      if (nn == 0 && m++ < MAX_SYSATTEMPTS) {
        errno = 0;
        continue;
      }
#endif
      errlog(LOG_INFO,"x_recv() : recv returns %d",nn);
      if (errno == EINTR) continue;
      errno = 0;
      ret = 0;
      goto end;
    }
    DLOG_MSG(("x_recv() : nn=%d",nn));
  }

  end:;

  alarm(0);
  return ret;
}

static int
x_send(int sock,char *buf,size_t len)
{
  size_t n;
  int nn;

#ifdef lint
  nn = 0;
#endif
  for (n = 0; n < len; n += (size_t)nn) {
    DLOG_MSG(("x_send() : len=%u, n=%u",len,n));
    errno = 0;
    if ((nn = send(sock,&buf[n],len - n,0)) <= 0) {
      errlog(LOG_INFO,"x_send() : send returns %d",nn);
      if (errno == EINTR) continue;
      errno = 0;
      return 0;
    }
    DLOG_MSG(("x_send() : nn=%d",nn));
  }
  return 1;
}

int
recvul(int sock,unsigned long *ul)
{
  char buf[MAX_ULSTR + 1];

  DLOG_MSG(("recvul() : x_recv(%d)",sock));
  if (!x_recv(sock,buf,sizeof buf)) {
    DLOG_ERROR(("recvul()"));
    return 0;
  }
  *ul = strtoul(buf,(char **)0,10);
  DLOG_MSG(("recvul() : buf=%s *ul=%lu",buf,*ul));
  return 1;
}

int
sendul(int sock,unsigned long ul)
{
  char buf[MAX_ULSTR + 1];

  sprintf(buf,"%lu",ul);
  DLOG_MSG(("sendul() : x_send(%d,%lu) buf=%s",sock,ul,buf));
  if (!x_send(sock,buf,sizeof buf)) {
    DLOG_ERROR(("sendul()"));
    return 0;
  }
  return 1;
}

/*
 * Receives string from network (length info comes first) and
 * returns an allocated buffer containing the string and its length
 * (len will be in the same form as when returned by strlen(), i.e.,
 * excluding the null terminator).  If len is zero, the string
 * was empty in which case strorage will not be allocated
 * and the actual x_recv() for the string is not performed.
 * If the string buffer passed to recvstr() was not empty (i.e.,
 * the pointer to it was not null) it is freed first.
 */
int
recvstr(int sock,char **s,size_t *len)
{
  unsigned long ultmp;

  DLOG_MSG(("recvstr() : recvul(%d)",sock));
  if (!recvul(sock,&ultmp))
    return 0;
  *len = (size_t)ultmp;
  if (*s) x_free(*s);
  if (*len > 0) {
    DLOG_MSG(("recvstr() : x_recv(%d)",sock));
    *s = (char *)x_malloc(*len + 1);
    if (!x_recv(sock,*s,*len)) {
      DLOG_ERROR(("recvstr()"));
      return 0;
    }
    DLOG_MSG(("recvstr() : *s=%s",*s));
  }
  else
    *s = (char *)0;
  return 1;
}

/*
 * Receives message string from the network and
 * returns the appropriate numeric token.  If error
 * occurs -1 is returned.
 */
int
recvmstr(int sock,char **s)
{
  char buf[MAX_MSGSTR + 1];
  unsigned long ultmp;
  int i;
  size_t len;

  DLOG_MSG(("recvmstr() : recvul(%d)",sock));
  if (!recvul(sock,&ultmp))
    return -1;
  len = (size_t)ultmp;
  if (len == 0 || len > MAX_MSGSTR) {
    errlog(LOG_ERR,"recvmstr() : len=%lu",(unsigned long)len);
    return -1;
  }
  DLOG_MSG(("recvmstr() : x_recv(%d)",sock));
  if (!x_recv(sock,buf,len)) {
    DLOG_ERROR(("recvstr()"));
    return -1;
  }
  buf[len] = '\0';
  DLOG_MSG(("recvstr() : buf=%s",buf));
  for (i = 0; s[i]; i++)
    if (streql(s[i],buf)) return i;
  errlog(LOG_ERR,"recvmstr() : unknown message (%s)",buf);
  return -1;
}

int
sendstr(int sock,char *s)
{
  size_t len = s ? strlen(s) : 0;

  DLOG_MSG(("sendstr() : sendul(%d,%lu)",sock,(unsigned long)len));
  if (!sendul(sock,(unsigned long)len))
    return 0;
  if (len > 0) {
    DLOG_MSG(("sendstr() : x_send(%d,%s)",sock,s));
    if (!x_send(sock,s,len)) {
      DLOG_ERROR(("sendstr()"));
      return 0;
    }
  }
  return 1;
}
