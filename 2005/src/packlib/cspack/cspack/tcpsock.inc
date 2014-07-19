*
* $Id: tcpsock.inc,v 1.1.1.1 1996/03/08 15:44:17 mclareni Exp $
*
* $Log: tcpsock.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:17  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_TCPSOCK_INC
#define CERNLIB_CSPACK_TCPSOCK_INC
*
*
* tcpsock.inc
*
#define localtime LOCALTIM
#define ruserpass RUSERPAS
#define inetd_sock_setup I_SETUP
#define inetd_sock_close I_CLOSE
#define server_sock_setup S_SETUP
#define client_sock_setup C_SETUP
#define sock_close S_CLOSE
#define sock_sendstr S_SNDST
#define sock_recvstr S_RCVST
#define sock_send S_SEND
#define sock_recv S_RECV
#define client_sock_setup_ C__SETU
#define inetd_sock_setup_ I__SETU
#define inetd_sock_close_ I__CLOS
#define server_sock_setup_ S__SETU
#define sock_close_ S__CLOS
#define sock_sendstr_ S__SNDS
#define sock_recvstr_ S__RCVS
#define sock_send_ S__SEND
#define sock_recv_ S__RECV
#define ssendstr_ SSND_ST
#define srecvstr_ SRCV_ST
#define initgroups INITGRO
#define ssendstr SSENSTR
#define srecvstr SRECSTR

#endif
