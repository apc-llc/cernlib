/*
 * $Id: piafl.c,v 1.2 1996/04/25 07:53:05 dinofm Exp $
 *
 * $Log: piafl.c,v $
 * Revision 1.2  1996/04/25 07:53:05  dinofm
 * VAX/VMS code wiped out.
 *
 * Revision 1.1  1996/04/02 21:50:46  thakulin
 * Renamed from piafl.F.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.06/01 08/12/94  17.16.42  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   15/02/94*/

#include <sys/stat.h>
#include <sys/times.h>
#include <sys/types.h>
#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <unistd.h>
#include "piaf.h"


/* exported external routines */
#define PSBLOG   psblog_
#define PSELOG   pselog_
#define PIAFLG   piaflg_

/* imported external routines */
extern char *km_strip(char *);


/* globals */
LogStruct   ulog;                 /* user usage statistics */

/***********************************************************************
 *                                                                     *
 *   Removes multiple blanks from a string. Space for the new          *
 *   string is allocated and a pointer to it is returned.              *
 *                                                                     *
 ***********************************************************************/
static char *compress(char *s)
{
   char *r, *t;
   int   l, ib = 0;

   l = strlen(s);
   r = calloc(l+5, 1);    /* some extra space in case %'s need to be doubled */

   if (l == 0) {
      *r = '\0';
      return r;
   }

   t = r;
   while (*s) {
      if (*s != ' ') {
         *t = *s;
         t++;
         ib = 0;
         if (*s == '%') {   /* double % otherwise syslog() will eat it */
            *t = *s;
            t++;
         }
      } else {
         if (ib == 0) {
            *t = *s;
            t++;
            ib = 1;
         }
      }
      s++;
   }
   return r;
}

/***********************************************************************
 *                                                                     *
 * Open syslog facility.                                               *
 *                                                                     *
 ***********************************************************************/
void piafl_open(int master)
{
   if (master)
      openlog("piafserv", LOG_PID|LOG_CONS, LOG_LOCAL6);
   else
      openlog("piafslave", LOG_PID|LOG_CONS, LOG_LOCAL7);
}

/***********************************************************************
 *                                                                     *
 * Write begin of session information to syslog.                       *
 *                                                                     *
 ***********************************************************************/
void piafl_begin(void)
{
   char s[256];

   ulog.seq++;

   sprintf(s, "%d:%d:%s:%s:%s:%s:%s:%s:%d:%d:%d", ulog.seq, BEGIN, ulog.user,
           ulog.group, ulog.remote_user, ulog.remote_host,
           ulog.master_node, ulog.version, ulog.protocol,
           ulog.workstation_type, ulog.cache_size);
   syslog(LOG_INFO, s);
}

/***********************************************************************
 *                                                                     *
 * Write begin of execution message to syslog.                         *
 *                                                                     *
 ***********************************************************************/
void piafl_exec_begin(char *cmd)
{
   char  *ccmd;
   char  *s = malloc(strlen(ulog.user) + strlen(cmd) + 32);

   ulog.seq++;
   ulog.no_slaves = *running_slaves;
   ulog.no_slaves_active = *active_slaves;

   ccmd = compress(cmd);

   sprintf(s, "%d:%d:%s:%d:%s", ulog.seq, EXEC_BEGIN, ulog.user,
           ulog.no_slaves_active, ccmd);
   syslog(LOG_INFO, s);

   free(s);
   free(ccmd);
}

/***********************************************************************
 *                                                                     *
 * Write end of execution message to syslog.                           *
 *                                                                     *
 ***********************************************************************/
void piafl_exec_end(int ntype, int noc, int nor, int hits,
                    float rio, float vio, float cp, float rt,
                    int id, int noct, int chain, int isel)
{
   char   s[256];
   float  hitr;

   ulog.seq++;
   ulog.nt_tot++;
   ulog.nt_type = ntype;
   ulog.nt_id = id;
   ulog.nt_chain = chain;
   ulog.sel_func = isel;
   ulog.nt_columns = noct;
   ulog.no_columns = noc;
   ulog.tot_no_columns += noc;
   ulog.no_rows = nor;
   ulog.tot_no_rows += nor;
   ulog.hits = hits;
   if (noc)
      hitr = (float)hits/(float)noc * 100.0;
   else
      hitr = 0.0;
   ulog.tot_hits += hitr;
   ulog.rio = rio;
   ulog.tot_rio += rio;
   ulog.vio = vio;
   ulog.tot_vio += vio;
   ulog.cp_time = cp;
   ulog.tot_cp_time += cp;
   ulog.real_time = rt;
   ulog.tot_real_time += rt;

   sprintf(s, "%d:%d:%s:%d:%d:%d:%d:%.2f:%.3f:%.3f:%.3f:%.3f:%d:%d:%d:%d",
           ulog.seq, EXEC_END, ulog.user,
           ulog.nt_tot, ulog.nt_type, ulog.no_columns, ulog.no_rows,
           hitr, ulog.rio, ulog.vio, ulog.cp_time, ulog.real_time,
           ulog.nt_id, ulog.nt_columns, ulog.nt_chain, ulog.sel_func);

   syslog(LOG_INFO, s);
}

/***********************************************************************
 *                                                                     *
 * Write PUTA message to syslog.                                       *
 *                                                                     *
 ***********************************************************************/
void piafl_puta(int bytes, float seconds, float put_rate)
{
   char   s[256];

   ulog.seq++;
   ulog.put_io = (float) bytes / 1.e6;   /* in MB */
   ulog.tot_put_io += ulog.put_io;

   sprintf(s, "%d:%d:%s:%s:%.3f:%.3f:%.1f",
           ulog.seq, PUTA, ulog.user,
           "PUTA", ulog.put_io, seconds, put_rate);  /* rate in KB */

   syslog(LOG_INFO, s);
}

/***********************************************************************
 *                                                                     *
 * Write GETA message to syslog.                                       *
 *                                                                     *
 ***********************************************************************/
void piafl_geta(int bytes, float seconds, float get_rate)
{
   char   s[256];

   ulog.seq++;
   ulog.get_io = (float) bytes / 1.e6;   /* in MB */
   ulog.tot_get_io += ulog.get_io;

   sprintf(s, "%d:%d:%s:%s:%.3f:%.3f:%.1f",
           ulog.seq, GETA, ulog.user,
           "GETA", ulog.get_io, seconds, get_rate);  /* rate in KB */

   syslog(LOG_INFO, s);
}

/***********************************************************************
 *                                                                     *
 * Write session summary message to syslog.                            *
 *                                                                     *
 ***********************************************************************/
void piafl_summary(void)
{
   char   s[256];
   float  av_hits;

   ulog.seq++;

   if (ulog.nt_tot)
      av_hits = ulog.tot_hits/(float)ulog.nt_tot;
   else
      av_hits = 0.0;

   sprintf(s, "%d:%d:%s:%d:%d:%d:%.2f:%.3f:%.3f:%.3f:%.3f:%.3f:%.3f",
           ulog.seq, SUMMARY, ulog.user,
           ulog.nt_tot, ulog.tot_no_columns, ulog.tot_no_rows,
           av_hits, ulog.tot_rio, ulog.tot_vio,
           ulog.tot_cp_time, ulog.tot_real_time,
           ulog.tot_put_io, ulog.tot_get_io);

   syslog(LOG_INFO, s);
}

/***********************************************************************
 *                                                                     *
 * Write end of session message to syslog.                             *
 *                                                                     *
 ***********************************************************************/
void piafl_end(int status)
{
   char  s[64];

   ulog.seq++;

   switch (status) {
   case 0:
      sprintf(s, "%d:%d:%s:%s",
              ulog.seq, END, ulog.user, "** NORMAL TERMINATION **");
      break;
   case 1:
      sprintf(s, "%d:%d:%s:%s",
              ulog.seq, END, ulog.user, "** KILL TERMINATION **");
      break;
   case 2:
      sprintf(s, "%d:%d:%s:%s",
              ulog.seq, END, ulog.user, "** TIME-OUT TERMINATION **");
      break;
   }

   syslog(LOG_INFO, s);
}

/***********************************************************************
 *                                                                     *
 * Write INFO message to syslog.                                       *
 *                                                                     *
 ***********************************************************************/
void piafl_info(char *mess)
{
   char *s = malloc(strlen(mess)+strlen(ulog.user)+10);
   sprintf(s, "%s:INFO:%s", ulog.user, mess);

   syslog(LOG_INFO, s);

   free(s);
}

/***********************************************************************
 *                                                                     *
 * Write ERROR message to syslog.                                      *
 *                                                                     *
 ***********************************************************************/
void piafl_error(char *mess)
{
   char *s = malloc(strlen(mess)+strlen(ulog.user)+10);
   sprintf(s, "%s:ERROR:%s", ulog.user, mess);

   syslog(LOG_ERR, s);

   free(s);
}
/***********************************************************************
 *                                                                     *
 * Fortran callable interface routine to piafl_exec_begin.             *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSBLOG(char *cmd, int len_cmd)
{
   char  *s, *s1;

   s = malloc(len_cmd+1);
   strncpy(s, cmd, len_cmd);
   s[len_cmd] = '\0';
   s1 = km_strip(s);

   piafl_exec_begin(s1);

   free(s);
   free(s1);
}

/***********************************************************************
 *                                                                     *
 * Fortran callable interface routine to piafl_exec_end.               *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PSELOG(int *ntype, int *noc, int *nor, int *hits,
                  float *rio, float *vio, float *cp, float *rt,
                  int *id, int *noct, int *chain, int *isel)
{
   piafl_exec_end(*ntype, *noc, *nor, *hits, *rio, *vio, *cp, *rt,
                  *id, *noct, *chain, *isel);
}

/***********************************************************************
 *                                                                     *
 * Fortran callable interface routine to piafl_info and piafl_error    *
 * functions.                                                          *
 *                                                                     *
 ***********************************************************************/
SUBROUTINE PIAFLG(int *type, char *mess, int len_mess)
{
   char  *s;

   s = malloc(len_mess+1);
   strncpy(s, mess, len_mess);
   s[len_mess] = '\0';

   if (*type == 1)
      piafl_info(s);
   else if (*type == 2)
      piafl_error(s);

   free(s);
}





