/*
 * $Id: tcputil.c,v 1.2 1996/04/02 22:00:35 thakulin Exp $
 *
 * $Log: tcputil.c,v $
 * Revision 1.2  1996/04/02 22:00:35  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :          14/11/95  18.20.10  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * tcputil.c
 *
 * Routines handling miscellaneous tcp connection tasks.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    17/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

int
tcpsockopt(int sock)
{
  int so;

  if (tcpparam.tc_rcvbuf >= 0) {
    so = (int)tcpparam.tc_rcvbuf;
    if (x_setsockopt(sock,SOL_SOCKET,SO_RCVBUF,(char *)&so,sizeof so) < 0)
      return 0;
  }
  if (tcpparam.tc_sndbuf >= 0) {
    so = (int)tcpparam.tc_sndbuf;
    if (x_setsockopt(sock,SOL_SOCKET,SO_SNDBUF,(char *)&so,sizeof so) < 0)
      return 0;
  }
  if (tcpparam.tc_nodelay) {
    so = 1;
    if (x_setsockopt(sock,IPPROTO_TCP,TCP_NODELAY,(char *)&so,sizeof so) < 0)
      return 0;
  }
  return 1;
}

int
tcpconnect(struct sockaddr_in *addrp)
{
  int sock;

  DLOG_MSG(("tcpconnect() : calling x_socket()"));
  if ((sock = x_socket(AF_INET,SOCK_STREAM,0)) < 0)
    return -1;
  DLOG_MSG(("tcpconnect() : calling tcpsockopt()"));
  if (!tcpsockopt(sock))
    return -1;
  DLOG_MSG(("tcpconnect() : calling x_connect()"));
  if (x_connect(sock,(struct sockaddr *)addrp,
      sizeof(struct sockaddr_in)) < 0) {
    x_close(sock);
    return -1;
  }

  return sock;
}

char *
get_peerip(int sock,struct sockaddr_in **addrpp)
{
  static struct sockaddr_in addr;
  int len = sizeof addr, oerrno, res;

  /* protect errno during getpeername() etc. */
  oerrno = errno;
  if (sock < 0) return (char *)0;
  if ((res = getpeername(sock,(struct sockaddr *)&addr,&len)) < 0)
    errlog(LOG_ERR,"get_peerip() : getpeername() error (socket %d)",sock);
  errno = oerrno;
  if (addrpp) *addrpp = &addr;
  return res < 0 ? (char *)0 : inet_ntoa(addr.sin_addr);
}

int
tcplisten(struct sockaddr_in *addrp)
{
  int sock, so;

  DLOG_MSG(("tcplisten() : calling x_socket()"));
  if ((sock = x_socket(AF_INET,SOCK_STREAM,0)) < 0)
    return -1;
  DLOG_MSG(("tcplisten() : calling tcpsockopt()"));
  if (!tcpsockopt(sock))
    return -1;
  DLOG_MSG(("tcplisten() : calling x_setsockopt()"));
  so = 1;
  if (x_setsockopt(sock,SOL_SOCKET,SO_REUSEADDR,(char *)&so,sizeof so) < 0)
    return -1;
  DLOG_MSG(("tcplisten() : calling x_bind()"));
  if (x_bind(sock,(struct sockaddr *)addrp,sizeof(struct sockaddr_in)) < 0) {
    x_close(sock);
    return -1;
  }
  DLOG_MSG(("tcplisten() : calling x_listen()"));
  if (x_listen(sock,5) < 0) {
    x_close(sock);
    return -1;
  }

  return sock;
}

struct sockaddr_in *
tcpaddr(int con,struct peer *pe_dat)
{
  static struct sockaddr_in addr_init;
  static struct sockaddr_in addr;
  unsigned long ia;

  DLOG_MSG(("tcpaddr()"));

  /* if con is zero or pr_ipaddr field is null, we set INADDR_ANY */
  ia = (con && pe_dat->pr_ipaddr) ? inet_addr(pe_dat->pr_ipaddr) : 0;
  /* malformed address */
  if (ia == (unsigned long)-1) {
    errlog(LOG_ERR,"tcpaddr() : malformed ip address %s",pe_dat->pr_ipaddr);
    return (struct sockaddr_in *)0;
  }
  addr = addr_init;
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = ia ? ia : htonl(INADDR_ANY);
  /* set the port */
  addr.sin_port = htons(con ? pe_dat->pr_conport : pe_dat->pr_lisport);

  DLOG_SOCKADDR_IN(&addr);

  return &addr;
}

int
ping_peer(struct peer *pe_dat)
{
  struct sockaddr_in *addrp;
  int sock;

  if (!(addrp = tcpconaddr(pe_dat)) ||
      (sock = tcpconnect(addrp)) < 0 ||
      !send_msg(sock,SGD_MSG_PING)
#ifdef ACK_EVERYTHING
      || recv_msg(sock) != SGD_MSG_OK
#endif
     ) {
    if (addrp && sock >= 0) x_close(sock);
    return 0;
  }
  x_close(sock);
  return 1;
}

/*
 * Tells the peer to forget about a particular stage - it's been
 * duly handled earlier.
 */
void
tell_to_forget(struct peer *pe,struct revector *rvp)
{
  int sock, down;
#ifdef ACK_EVERYTHING
  int msg;
#endif
  struct sockaddr_in *addrp;

  if ((down = !(addrp = tcpconaddr(pe)) ||
              (sock = tcpconnect(addrp)) < 0 ||
              !send_msg(sock,SGD_MSG_FORGET) ||
              !sendul(sock,rvp->rt_stageid)
#ifdef ACK_EVERYTHING
              || (msg = recv_msg(sock)) < 0) ||
      msg != SGD_RET_OK
#else
      )
#endif
     ) {
    errlog(LOG_ERR,"tell_to_forget() : trouble connecting to peer %s (%s)",
      pe->pr_ipaddr,down ? "conn" : "ack");
    if (down) pe->pr_up = 0;
    x_close(sock);
    return;
  }
  errlog(LOG_INFO,"tell_to_forget() : telling peer %s to forget stage %ld",
    pe->pr_ipaddr,rvp->rt_stageid);
  remove_revect(rvp);
  pe->pr_up = 1;
  pe->pr_last = time((time_t *)0);
  x_close(sock);
}
