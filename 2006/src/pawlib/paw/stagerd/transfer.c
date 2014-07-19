/*
 * $Id: transfer.c,v 1.2 1996/04/02 22:00:37 thakulin Exp $
 *
 * $Log: transfer.c,v $
 * Revision 1.2  1996/04/02 22:00:37  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :          11/12/95  22.33.23  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * transfer.c
 *
 * Functions to send and receive structs between machines.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    12/8/94
 *
 *************************************************************************/

#include "pfstagerd.h"

/*
 * Send the full client information.
 */
int
put_client(int sock,struct client *cl_dat)
{
  DLOG_MSG(("put_client(%d)",sock));
  DLOG_CLIENT(cl_dat);

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
 * Get the full specs of the client.
 */
int
get_client(int sock,struct client *cl_dat)
{
  size_t len;

  DLOG_MSG(("get_client(%d)",sock));

  /* user name */
  if (!recvstr(sock,&cl_dat->ct_username,&len) || len == 0)
    return 0;

  /* machine name */
  if (!recvstr(sock,&cl_dat->ct_machname,&len) || len == 0)
    return 0;

  /* ip address */
  if (!recvstr(sock,&cl_dat->ct_ipaddr,&len) || len == 0)
    return 0;

  /* ftp user name (optional) */
  if (!recvstr(sock,&cl_dat->ct_fusername,&len))
    return 0;

  /* user password on the client (optional) */
  if (!recvstr(sock,&cl_dat->ct_passwd,&len))
    return 0;

  /* client id */
  if (!recvul(sock,&cl_dat->ct_clientid))
    return 0;

  return 1;
}

/*
 * Send the full specs of the file to be staged.
 */
int
put_stage(int sock,struct stage *st_dat)
{
  int i;

  DLOG_MSG(("put_stage(%d)",sock));
  DLOG_STAGE(st_dat);

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
  if (!sendul(sock,(unsigned long)st_dat->sg_nowait))
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
 * Receive the full specs of the file to be staged.
 */
int
get_stage(int sock,struct stage *st_dat)
{
  size_t len;
  int i;
  static struct pfinfo pfinfo_init;
  unsigned long ultmp;

  DLOG_MSG(("get_stage(%d)",sock));
  DLOG_STAGE(st_dat);

  /* the source file */
  if (!recvstr(sock,&st_dat->sg_sfname,&len) || len == 0)
    return 0;

  /* the target file (optional) */
  if (!recvstr(sock,&st_dat->sg_tfname,&len))
    return 0;

  /* name of the preferred staging server (optional) */
  if (!recvstr(sock,&st_dat->sg_prserver,&len))
    return 0;

  /* file type */
  if ((st_dat->sg_ftyp = recv_ftyp(sock)) < 0)
    return 0;

  /* file size */
  if (!recvul(sock,&st_dat->sg_fsiz))
    return 0;

  /* nowait flag */
  if (!recvul(sock,&ultmp))
    return 0;
  st_dat->sg_nowait = (int)ultmp;

  /* number of file stripes */
  if (!recvul(sock,&ultmp))
    return 0;
  st_dat->sg_fnum = (int)ultmp;

  /* the physical target file */
  if (st_dat->sg_fnum > 0) {
    st_dat->sg_ptfname =
      (struct pfinfo *)x_malloc(st_dat->sg_fnum * sizeof(struct pfinfo));
    for (i = 0; i < st_dat->sg_fnum; i++)
      st_dat->sg_ptfname[i] = pfinfo_init;
    for (i = 0; i < st_dat->sg_fnum; i++) {
      if (!recvstr(sock,&st_dat->sg_ptfname[i].pf_name,&len))
        return 0;
      if (!recvul(sock,&ultmp))
        return 0;
      st_dat->sg_ptfname[i].pf_nrec = (int)ultmp;
      if (!recvul(sock,&ultmp))
        return 0;
      st_dat->sg_ptfname[i].pf_size = ultmp;
    }
  }

  /* master ip address */
  if (!recvstr(sock,&st_dat->sg_masterip,&len))
    return 0;

  /* server ip address */
  if (!recvstr(sock,&st_dat->sg_serverip,&len))
    return 0;

  /* stage id */
  if (!recvul(sock,&st_dat->sg_stageid))
    return 0;

  /* queueing time */
  if (!recvul(sock,&ultmp))
    return 0;
  st_dat->sg_queuetime = (time_t)ultmp;

  /* starting time */
  if (!recvul(sock,&ultmp))
    return 0;
  st_dat->sg_starttime = (time_t)ultmp;

  /* ending time */
  if (!recvul(sock,&ultmp))
    return 0;
  st_dat->sg_endtime = (time_t)ultmp;

  /* the original target file */
  if (!recvstr(sock,&st_dat->sg_otfname,&len))
    return 0;

  /* the physical source file (optional) */
  if (!recvstr(sock,&st_dat->sg_psfname,&len))
    return 0;

  /* how much of the file transferred */
  if (!recvul(sock,&st_dat->sg_fdone))
    return 0;

  /* status indicator */
  if ((st_dat->sg_status = recv_status(sock)) < 0)
    return 0;

  /* stager return code */
  if (!recvul(sock,&ultmp))
    return 0;
  st_dat->sg_retcode = (int)ultmp;

  /* the slavelist (optional) */
  if (!recvstr(sock,&st_dat->sg_slavelist,&len))
    return 0;

  DLOG_STAGE(st_dat);

  return 1;
}
