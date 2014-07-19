/*
 * $Id: msg.c,v 1.1.1.1 1996/03/01 11:39:30 mclareni Exp $
 *
 * $Log: msg.c,v $
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :  2.06/13 23/09/94  21.25.51  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * msg.c
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    6/6/94
 *
 *************************************************************************/

/* messages to pass */
char *sgd_msg[] = {
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
char *sgd_ftyp[] = {
  "ftyp_none",
  "ftyp_local",
  "ftyp_tape",
  "ftyp_fatmen",
  (char *)0
};

/* stage statuses */
char *sgd_status[] = {
  "status_none",
  "status_error",
  "status_done",
  "status_running",
  "status_pending",
  "status_connect",
  (char *)0
};
