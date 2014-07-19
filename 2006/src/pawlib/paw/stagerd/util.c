/*
 * $Id: util.c,v 1.2 1996/04/02 22:00:38 thakulin Exp $
 *
 * $Log: util.c,v $
 * Revision 1.2  1996/04/02 22:00:38  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:31  mclareni
 * Paw
 *
 */
/*CMZ :  2.06/13 17/09/94  02.20.59  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * util.c
 *
 * Miscellaneous routines having no other home.
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    17/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

#if 0
/*
 * Returns the true file name of the striped Hbook-file, possibly
 * amended by the appropriate ending.
 */
char *
true_tfname(char *fn)
{
  if (do_striping) {
    static char tfn[PATH_MAX + 1];
    strcpy(tfn,fn);
    strcat(tfn,FSTRIPE_TAG);
    return tfn;
  }
  return fn;
}
#endif

/*
 * Checks if a file is a striped Hbook file.  This is implicated
 * by it having the setuid bit and not being executable, plus a
 * special signature in the beginning of the file.
 */
int
is_striped(char *fn)
{
  struct stat sbuf;
  if (stat(fn,&sbuf) < 0) {
    errlog(LOG_ERR,"is_striped() : can't stat %s",fn);
    return 0;
  }
  /* regular file, suid bit up, none of the execute bits? */
  if (S_ISREG(sbuf.st_mode) && sbuf.st_mode & S_ISUID &&
      !(sbuf.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH))) {
    FILE *f;
    char buf[sizeof FSTRIPE_SIG];

    if (!(f = fopen(fn,"rb"))) {
      errlog(LOG_ERR,"is_striped() : can't open %s",fn);
      return 0;
    }
    /* verify the sig */
    if (fread(buf,1,sizeof FSTRIPE_SIG - 1,f) < sizeof FSTRIPE_SIG - 1 ||
        !strneql(buf,FSTRIPE_SIG,sizeof FSTRIPE_SIG - 7)) {
      fclose(f);
      return 0;
    }
    fclose(f);
    buf[sizeof FSTRIPE_SIG - 1] = '\0';
    return atoi(&buf[sizeof FSTRIPE_SIG - 7]);
  }
  return 0;
}

/*
 * Figure out new unique client and stage ids based on the master id,
 * current system time and request number.
 */
unsigned long
new_id(int iscl)
{
  static unsigned long client_id, stage_id;
  static time_t o_tim;
  unsigned long time_id, *idp;
  time_t tim;

  idp = iscl ? &client_id : &stage_id;
  tim = time((time_t)0);
  if (o_tim == 0) o_tim = tim;
  if (tim > o_tim)
    *idp = 0;
  else
    (*idp)++;
  if (*idp >= MAX_REQUESTS) {
    while ((tim = time((time_t)0)) == o_tim) ;
    *idp = 0;
  }
  o_tim = tim;
  time_id = (unsigned long)tim &
            (~(unsigned long)0 >> (IDBITS - TIMESTAMPBITS));

  return (master_id << (IDBITS - MASTERBITS)) |
          (time_id << (IDBITS - MASTERBITS - TIMESTAMPBITS)) | *idp;
}

/*
 * Decipher user password.
 */
char *
dcpass(char *s)
{
  static char mask[] = {
    '\241','\212','\307','\266','w','\375','^','\303','{','Z',
    '\327','\357','-','\210','\353','\347','\217','.','t','\212',
    '\374','\241','z','d','\327','\271','\255','\267','.','\3',
    '\320','\317','\215','\230','\206','\5','\225','\344','\310','\20'
  };
  static char buf[MAX_PASSWD + 1];
  int i;

  /* request to clear the plain text password */
  if (!s) {
    memset(buf,0,sizeof buf);
    return (char *)0;
  }
  /* or decipher it */
  for (i = 0; i < MAX_PASSWD; i++) {
    buf[i] = (mask[i] ^ s[i]) & 0xff;
    if (!buf[i]) break;
  }
  return buf;
}

char *
dupstr(char *s)
{
  char *ss = (char *)x_malloc(strlen(s) + 1);

  strcpy(ss,s);
  return ss;
}

/*
 * The next three functions handle the book keeping for the revectored
 * stage acknowledgements.
 */
struct revector *
add_revect(unsigned long stageid)
{
  struct revector *rv = (struct revector *)x_malloc(sizeof(struct revector));

  rv->rt_stageid = stageid;
  rv->rt_time = time((time_t *)0);
  rv->rt_next = revectorlist;
  revectorlist = rv;
  return rv;
}

void
remove_revect(struct revector *rvp)
{
  struct revector *rv, *rvv;

  for (rv = revectorlist, rvv = (struct revector *)0; rv; ) {
    if (rv == rvp) {
      if (rvv)
        rvv->rt_next = rv->rt_next;
      else
        revectorlist = rv->rt_next;
      x_free(rv);
      break;
    }
    rvv = rv;
    rv = rvv->rt_next;
  }
}

struct revector *
find_revect(unsigned long stageid)
{
  struct revector *rv;

  for (rv = revectorlist; rv; rv = rv->rt_next)
    if (rv->rt_stageid == stageid)
      return rv;
  return (struct revector *)0;
}
