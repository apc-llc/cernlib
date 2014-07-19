*
* $Id: tsosub.inc,v 1.1.1.1 1996/03/08 15:44:17 mclareni Exp $
*
* $Log: tsosub.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:17  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_TSOSUB_INC
#define CERNLIB_CSPACK_TSOSUB_INC
*
* ext. def. for /TCPAW/TSOSUB and /TSOSER/TSOSER rpc rout.
*
* tsosub.inc
*
#define  TSOSUB   500000000
#define  VERSION  1
#define  TSOSPROC 1
 
struct info
{
      char *remote_machine;
      char *remote_user;
      char *remote_tty;
      char *mvs_userid;
      char *mvs_passwd;
      char *tso_command;
      unsigned short mvs_sport;
} info;
 
xdr_info(xdrsp,infop)
      XDR      *xdrsp;
      struct info  *infop;
{
      if (!xdr_wrapstring(xdrsp,&infop->remote_machine))
            return(0);
      if (!xdr_wrapstring(xdrsp,&infop->remote_user))
            return(0);
      if (!xdr_wrapstring(xdrsp,&infop->remote_tty))
            return(0);
      if (!xdr_wrapstring(xdrsp,&infop->mvs_userid))
            return(0);
      if (!xdr_wrapstring(xdrsp,&infop->mvs_passwd))
            return(0);
      if (!xdr_wrapstring(xdrsp,&infop->tso_command))
            return(0);
      if (!xdr_u_short(xdrsp,&infop->mvs_sport))
            return(0);
      return(1);
}
 
struct retn
{
      char *servreply;
      unsigned short mvs_sport;
} retn;
 
xdr_retn(xdrsp,retnp)
      XDR      *xdrsp;
      struct retn  *retnp;
{
      if (!xdr_wrapstring(xdrsp,&retnp->servreply))
            return(0);
      if (!xdr_u_short(xdrsp,&retnp->mvs_sport))
            return(0);
      return(1);
}
 

#endif
