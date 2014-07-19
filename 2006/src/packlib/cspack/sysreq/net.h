/*
 * $Id: net.h,v 1.1.1.1 1996/03/08 15:44:26 mclareni Exp $
 *
 * $Log: net.h,v $
 * Revision 1.1.1.1  1996/03/08 15:44:26  mclareni
 * Cspack
 *
 */
/*
 * @(#)net.h    1.3 7/29/94 CERN CN-PDP/CS F. Hemmer
 */
 
/*
 * Copyright (C) 1990-1994 by CERN CN-PDP/CS
 * All rights reserved
 */
 
/* tcp.h        TCP/IP on VMS variants                          */
 
/*
 * supported TCP's on VMS are :
 *
 * Win/TCP (the Wollongong Group)       thru    TWG
 * Multinet (TGV)                       thru    MULTINET
 * TcpWare (Process Software)           thru    TCPWARE
 * UCX (DEC)                            thru    UCX
 */
 
#if defined(vms) && (vms == 1)
 
#if defined(TGV) && (TGV == 1) && !defined(MULTINET)
#define MULTINET        1
#endif /* TGV && !MULTINET */
 
#if (!defined(TWG) && !defined(MULTINET) && !defined(TCPWARE) && !defined(UCX))
/* We generate a compiler error (#error not defined everywhere!) */
!!! YOU MUST SPECIFY either MULTINET or TWG or TCPWARE or UCX !!!
 
#endif /* !MULTINET && !TWG && !TCPWARE && !UCX */
 
#if defined(TWG) && (TWG == 1)
/* We generate a compiler error, because currently unsupported */
!!! Win/TCP is not supported in this version. Sorry !!!
 
extern int      uerrno;
extern int      vmserrno;
#define errno   (uerrno ? uerrno : vmserrno)
 
#endif /* TWG */
 
#if defined(MULTINET) && (MULTINET == 1)
 
#pragma nostandard
extern volatile int noshare      socket_errno;
extern volatile int noshare      vmserrno;
extern void socket_perror();
#pragma standard
 
/*
 * MULTINET doesn't prototype externals, which is a requirement
 * when using DEC C compiler on ALPHA/VMS 6.0 C - DEC C 013
 */
 
/*
#if defined(errno)
#undef errno
#define errno   (socket_errno ? socket_errno : vmserrno)
#endif /* errno
*/
 
#if defined(__ALPHA)
extern struct hostent  *gethostbyname();
extern struct hostent  *gethostbyaddr();
extern struct hostent  *gethostent();
extern struct servent  *getservbyname();
extern struct servent  *getservbyport();
extern struct servent  *getservent();
extern struct protoent *getprotobyname();
extern struct protoent *getprotobynumber();
extern struct protoent *getprotoent();
extern struct netent   *getnetbyname();
extern struct netent   *getnetbyaddr();
extern struct netent   *getnetent();
extern struct hostent *tcpware_gethostaddr();
 
extern int accept();
extern int bind();
extern int connect();
extern int getpeername();
extern int getsockname();
extern int getsockopt();
extern int listen();
extern int recvfrom();
extern int select();
extern int sendto();
extern int setsockopt();
extern int select();
extern int sendto();
extern int setsockopt();
extern int socket();
extern int socket_close();
extern int socket_ioctl();
extern int socket_read();
extern int socket_recv();
extern int socket_send();
extern int socket_write();
extern int shutdown();
extern int getdomainname();
extern int setdomainname();
extern int gethostid();
extern int gethostname();
extern int sethostname();
extern int pneterror();
/*
extern unsigned short htons();
extern unsigned short ntohs();
extern unsigned long  htonl();
extern unsigned long  ntohl();
*/
extern void bzero();
 
extern char *vms_errno_string();
 
#endif /* __ ALPHA */
#endif /* MULTINET */
 
#if defined(UCX) && (UCX == 1)
 
#include <ucx$inetdef.h>                /* UCX various constants & structs     \
 */
 
/*
 * The following is needed to implement the SIOCGIFCONF ioctl a la unix.
 */
 
#define ifr_addr                    IFR$R_DUMMY.IFR$T_ADDR
#define ifr_name                    IFR$T_NAME
 
struct  ifconf {
        int     ifc_len;                /* size of associated buffer */
        union {
                caddr_t ifcu_buf;
                struct  IFREQDEF *ifcu_req;
        } ifc_ifcu;
#define ifc_buf ifc_ifcu.ifcu_buf       /* buffer address */
#define ifc_req ifc_ifcu.ifcu_req       /* array of structures returned */
};
 
/*
 * The following is needed to emulate BSD 4.3 select API
 */
 
typedef int     fd_set;
 
#define FD_SET(n, p)    (*p |= (1 << n))
#define FD_CLR(n, p)    (*p &= ~(1 << n))
#define FD_ISSET(n, p)  (*p & (1 << n))
#define FD_ZERO(p)      (*p = 0)
#define FD_SETSIZE      32
 
#endif /* UCX */
 
#if defined(TCPWARE) && (TCPWARE == 1)
 
/*
 * TCPWARE include files are messy. Some define external entries
 * (required by DEC C compiler on ALPHA/VMS 6.0 C - DEC C 013)
 * others don't define them, and even others define them at the
 * wrong place.
 */
 
#if defined(_TCPWARE_NETDB_)    /* TCPWARE_INCLUDE:netdb.h      */
#if defined(__ALPHA)
extern struct hostent  *tcpware_gethostbyname();
extern struct hostent  *tcpware_gethostbyaddr();
extern struct hostent  *tcpware_gethostent();
extern struct servent  *tcpware_getservbyname();
extern struct servent  *tcpware_getservbyport();
extern struct servent  *tcpware_getservent();
extern struct protoent *tcpware_getprotobyname();
extern struct protoent *tcpware_getprotobynumber();
extern struct protoent *tcpware_getprotoent();
extern struct netent   *tcpware_getnetbyname();
extern struct netent   *tcpware_getnetbyaddr();
extern struct netent   *tcpware_getnetent();
#endif /* __ ALPHA */
#endif /* _TCPWARE_NETDB_ */
 
#if defined( _TCPWARE_SOCKET_)  /* TCPWARE_INCLUDE:socket.h     */
#if defined(__ALPHA)
extern int tcpware_accept();
extern int tcpware_bind();
extern int tcpware_connect();
extern int tcpware_getpeername();
extern int tcpware_getsockname();
extern int tcpware_getsockopt();
extern int tcpware_listen();
extern int tcpware_recvfrom();
extern int tcpware_select();
extern int tcpware_sendto();
extern int tcpware_setsockopt();
extern int tcpware_select();
extern int tcpware_sendto();
extern int tcpware_setsockopt();
extern int tcpware_socket();
extern int tcpware_socket_close();
extern int tcpware_socket_ioctl();
extern int tcpware_socket_read();
extern int tcpware_socket_recv();
extern int tcpware_socket_send();
extern int tcpware_socket_write();
extern int tcpware_shutdown();
extern int tcpware_getdomainname();
extern int tcpware_setdomainname();
extern int tcpware_gethostid();
extern struct hostent *tcpware_gethostaddr();
extern int tcpware_gethostname();
extern int tcpware_sethostname();
extern int tcpware_pneterror();
#endif /* __ ALPHA */
#endif /* _TCPWARE_SOCKET */
 
#if defined(_TCPWARE_TYPES_)    /* TCPWARE_INCLUDE:types.h      */
#if defined(__ALPHA)
extern unsigned short tcpware_htons();
extern unsigned short tcpware_ntohs();
extern unsigned long  tcpware_htonl();
extern unsigned long  tcpware_ntohl();
#endif /* __ALPHA */
#endif /* _TCPWARE_TYPES_ */
 
#endif /* TCPWARE */
 
#endif /* vms */
 
#if defined(vax) && defined(vms) && !defined(__DECC)
/*
 * VAX/VMS C compiler 3.x has a nasty bug around function pointer
 * assignements, when linked with libraries. Therefore we must
 * disable this code for vms. As a result, there is a loss in
 * functionality, because we cannot switch anymore between functions
 */
#define netread    s_recv
#define netwrite   s_send
#define netclose   s_close
#define netioctl   s_ioctl
#define neterror   s_errmsg
#else
extern int (*recvfunc)();
#define netread    (*recvfunc)
extern int (*sendfunc)();       /* Network send function                */
#define netwrite   (*sendfunc)
extern int (*closefunc)();      /* Network send function                */
#define netclose   (*closefunc)
extern int (*ioctlfunc)();      /* Network send function                */
#define netioctl  (*ioctlfunc)
extern char *(*errfunc)();        /* Network error function               */
#define neterror (*errfunc)
#endif /* vax && vms && ! __DECC */
