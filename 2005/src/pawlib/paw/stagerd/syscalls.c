/*
 * $Id: syscalls.c,v 1.2 1996/04/02 22:00:34 thakulin Exp $
 *
 * $Log: syscalls.c,v $
 * Revision 1.2  1996/04/02 22:00:34  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :  2.06/13 29/09/94  15.40.09  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * syscalls.c
 *
 * System call and library function wrappers to restart interrupted
 * calls, handle errno etc.  All names start with "x_".
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    10/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

/* DEBUG_ALLOC stuff is for monitoring memory leaks in
   the development phase */

#ifdef DEBUG_ALLOC

#define ALIGN_OFFSET  sizeof(size_t)

static struct {
    unsigned long curalloc_bytes;
    unsigned long maxalloc_bytes;
    unsigned long curalloc_items;
    unsigned long maxalloc_items;
    unsigned long alloc_called;
    unsigned long free_called;
} alloc_info;

void
dump_xallocinfo(FILE *f)
{
  unsigned long eca = alloc_info.curalloc_items * sizeof(size_t);
  unsigned long ema = alloc_info.maxalloc_items * sizeof(size_t);

  fprintf(f,"\
current allocation  : %lu items  %lu + %lu = %lu bytes\n\
maximum allocation  : %lu items  %lu + %lu = %lu bytes\n\
allocator called    : %lu times\n\
deallocator called  : %lu times\n\n",
    alloc_info.curalloc_items,
    alloc_info.curalloc_bytes - eca,
    eca,
    alloc_info.curalloc_bytes,
    alloc_info.maxalloc_items,
    alloc_info.maxalloc_bytes - ema,
    ema,
    alloc_info.maxalloc_bytes,
    alloc_info.alloc_called,
    alloc_info.free_called);
}

#endif

/*
 * Allocate buffer and fill it with zeros.
 */
void *
x_malloc(size_t siz)
{
  void *p;
#ifdef DEBUG_ALLOC
  siz += ALIGN_OFFSET;
#endif
  errno = 0;
  p = malloc(siz);
  if (!p)
    fatal(1,"x_malloc() : cannot alloc %lu bytes",(unsigned long)siz);
  memset(p,0,siz);
  errno = 0;
#ifdef DEBUG_ALLOC
  alloc_info.curalloc_bytes += siz;
  if (alloc_info.curalloc_bytes > alloc_info.maxalloc_bytes)
      alloc_info.maxalloc_bytes = alloc_info.curalloc_bytes;
  alloc_info.curalloc_items++;
  if (alloc_info.curalloc_items > alloc_info.maxalloc_items)
      alloc_info.maxalloc_items = alloc_info.curalloc_items;
  alloc_info.alloc_called++;
  *(size_t *)p = siz;
  return((void *)((char *)p + ALIGN_OFFSET));
#else
  return p;
#endif
}

void
x_free(void *p)
{
#ifdef DEBUG_ALLOC
  void *pp = (void *)((char *)p - ALIGN_OFFSET);
#endif
  if (!p) {
    errlog(LOG_ERR,"x_free() : null pointer argument");
    return;
  }
#ifdef DEBUG_ALLOC
  alloc_info.curalloc_bytes -= *(size_t *)pp;
  alloc_info.curalloc_items--;
  alloc_info.free_called++;
  free(pp);
#else
  free(p);
#endif
}

int
x_select(int maxfd,fd_set *rfd,fd_set *wfd,fd_set *efd,struct timeval *to)
{
  int sel;
  errno = 0;
  sel = select(maxfd,rfd,wfd,efd,to);
  if (sel < 0) {
    if (errno == EINTR)
      errno = 0;
    else {
      char rbuf[MAX_ULSTR + 3], wbuf[MAX_ULSTR + 3], ebuf[MAX_ULSTR + 3];
      char tbuf[2 * MAX_ULSTR + 25];
      static char null[] ="NULL";

      if (rfd) sprintf(rbuf,"0x%x",*(int *)rfd);
      else strcpy(rbuf,null);
      if (wfd) sprintf(wbuf,"0x%x",*(int *)wfd);
      else strcpy(wbuf,null);
      if (efd) sprintf(ebuf,"0x%x",*(int *)efd);
      else strcpy(ebuf,null);
      if (to) sprintf(tbuf,"{tv_sec=%ld,tv_usec=%ld}",to->tv_sec,to->tv_usec);
      else strcpy(tbuf,null);
      errlog(LOG_ERR,
        "x_select() : select returns %d (maxfd=%d,rfd=%s,wfd=%s,efd=%s,to=%s)",
        sel,maxfd,rbuf,wbuf,ebuf,tbuf);
      abort(); /* !!! debugging !!! */
    }
  }
  /* impossible... kernel bug */
  else if (!to && sel == 0)
    errlog(LOG_ERR,"x_select() : select returns 0 without timeout");
  errno = 0;
  return sel;
}

int
x_socket(int family,int type,int protocol)
{
  int sock;
  errno = 0;
  sock = socket(family,type,protocol);
  if (sock < 0) errlog(LOG_ERR,"x_socket() : socket returns %d",sock);
  errno = 0;
  return sock;
}

int
x_setsockopt(int sock,int level,int optname,char *optval,int optlen)
{
  int err;
  errno = 0;
  err = setsockopt(sock,level,optname,optval,optlen);
  if (err < 0) errlog(LOG_ERR,"x_socket() : socket returns %d",err);
  errno = 0;
  return err;
}

int
x_connect(int sock,struct sockaddr *saddrp,int addrlen)
{
  int err;
  errno = 0;
  err = connect(sock,saddrp,addrlen);
  if (err < 0)
    errlog(LOG_ERR,"x_connect() : connect returns %d (peer %s, port %lu)",err,
      inet_ntoa(((struct sockaddr_in *)saddrp)->sin_addr),
      (unsigned long)ntohs(((struct sockaddr_in *)saddrp)->sin_port));
  errno = 0;
  return err;
}

int
x_accept(int sock,struct sockaddr *saddrp,int *addrlenp)
{
  int newsock;

  errno = 0;
  newsock = accept(sock,saddrp,addrlenp);
  if (newsock < 0) errlog(LOG_ERR,"x_accept() : accept returns %d",newsock);
  errno = 0;
  return newsock;
}

int
x_bind(int sock,struct sockaddr *saddrp,int addrlen)
{
  int err;

  errno = 0;
  err = bind(sock,saddrp,addrlen);
  if (err < 0) errlog(LOG_ERR,"x_bind() : bind returns %d",err);
  errno = 0;
  return err;
}

int
x_listen(int sock,int backlog)
{
  int err;

  errno = 0;
  err = listen(sock,backlog);
  if (err < 0) errlog(LOG_ERR,"x_listen() : listen returns %d",err);
  errno = 0;
  return err;
}

int
x_close(int fd)
{
  int n = 0;
  int err;

  /* already closed? */
  if (fd < 0) return 0;

  /* if interrupted by signal, try again (we really want to close
     all descriptors to avoid running out of descriptors later) */
  while ((errno = 0, (err = close(fd)) < 0) &&
          errno == EINTR && n < MAX_SYSATTEMPTS) n++;
  if (err < 0) errlog(LOG_ERR,"x_close() : close returns %d",err);
  errno = 0;
  return err;
}

int
x_shutdown(int fd,int how)
{
  int err;

  /* already closed? */
  if (fd < 0) return 0;

  errno = 0;
  err = shutdown(fd,how);
  if (err < 0) errlog(LOG_ERR,"x_shutdown() : shutdown returns %d",err);
  errno = 0;
  return err;
}
