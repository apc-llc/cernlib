/*
 * $Id: slave.c,v 1.2 1996/04/02 22:00:29 thakulin Exp $
 *
 * $Log: slave.c,v $
 * Revision 1.2  1996/04/02 22:00:29  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          13/11/95  14.12.21  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * slave.c
 *
 * Routines peculiar to slaves (names starting with "s_").
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    21/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

static char *
s_make_psfname(struct stage *st)
{
  char *s;

  /* expand the standard tape name */
  if (st->sg_ftyp == SGD_FTYP_TAPE) {
    int fs;
    char *stmp;
    char *volid, *fseqid, *lbltyp, *volser;

    s = dupstr(st->sg_sfname);
    volid = strtok(s,".");
    fseqid = strtok((char *)0,".");
    lbltyp = strtok((char *)0,".");
    volser = strtok((char *)0,".");
    if (fseqid) fs = atoi(fseqid);
    if (lbltyp)
      for (stmp = lbltyp; *stmp; stmp++) tolower(*stmp);

    /* check that the all components of the tape name are ok */
    if (strlen(volid) > (size_t)MAX_VOLID ||
        (fseqid && (fs <= 0 || fs > 99)) ||
        (lbltyp && !streql(lbltyp,"al") && !streql(lbltyp,"sl") &&
         !streql(lbltyp,"nl") && !streql(lbltyp,"blp")) ||
        (volser && strlen(volser) > (size_t)MAX_VOLSER)) {
      x_free(s);
      st->sg_retcode = SGD_RET_TAPEID;
      return (char *)0;
    }
    stmp = (char *)x_malloc(MAX_TAPEID + 1);
    sprintf(stmp,"%s.%d.%s.%s",volid,
      fseqid ? fs : FSEQID_DEFAULT,
      lbltyp ? lbltyp : LBLTYP_DEFAULT,
      volser ? volser : volid);
    x_free(s);
    s = stmp;
  }

  /* consult fatmen for expansion of the file name */
  else if (st->sg_ftyp == SGD_FTYP_FATMEN) {
    FILE *f;
    int ret;
    char cmd[2 * PATH_MAX + 50];
    char buf[PATH_MAX + 1];

    sprintf(cmd,"%s %s",fatmenbin,st->sg_sfname);
    if (!(f = popen(cmd,"r"))) {
      errlog(LOG_ERR,"s_make_psfname() : can't exec \"%s\"",cmd);
      st->sg_retcode = SGD_RET_FATMENBIN;
      return (char *)0;
    }
    if (fgets(buf,sizeof buf,f)) {
      size_t n = strlen(buf) - 1;
      if (buf[n] == '\n') buf[n] = '\0';
      if (buf[0] == ' ') {
        errlog(LOG_ERR,"s_make_psfname() : fatmen error (%s)",buf);
        st->sg_retcode = SGD_RET_FATMENBIN;
        return (char *)0;
      }
    }
    else {
      errlog(LOG_ERR,"s_make_psfname() : error reading pipe \"%s\"",cmd);
      st->sg_retcode = SGD_RET_FATMENBIN;
      return (char *)0;
    }
    /* for explanation why -1 is ok see pclose() in function m_mail_client() */
    if ((ret = pclose(f)) != 0 && ret != -1) {
      errlog(LOG_ERR,"s_make_psfname() : fatmen returns %d (%s)",ret,cmd);
      st->sg_retcode = SGD_RET_FATMENBIN;
      return (char *)0;
    }
    s = dupstr(buf);
  }

  /* normal file name - just copy */
  else
    s = dupstr(st->sg_sfname);

  return s;
}

static struct client *
s_get_clientspecs(int sock)
{
  struct client cl_tmp, *cl_dat;

  DLOG_MSG(("s_get_clientspecs(%d)",sock));

  cl_tmp = cl_init;

  if (!get_client(sock,&cl_tmp))
    goto errout;

  /* check if we already have this client booked, if not make new entry */
  if (!(cl_dat = find_clientbyid(cl_tmp.ct_clientid))) {
    cl_tmp.ct_stagelist = (struct stage *)0;
    cl_dat = (struct client *)x_malloc(sizeof cl_dat[0]);
    *cl_dat = cl_tmp;
  }
  /* else see if password info needs updating */
  else {
    if (cl_tmp.ct_passwd && !streql(cl_tmp.ct_passwd,cl_dat->ct_passwd)) {
      if (cl_dat->ct_passwd) x_free(cl_dat->ct_passwd);
      cl_dat->ct_passwd = cl_tmp.ct_passwd;
      cl_tmp.ct_passwd = (char *)0;
    }
    release_client(&cl_tmp);
  }
  DLOG_CLIENT(cl_dat);

  return cl_dat;

  errout:;

  release_client(&cl_tmp);
  return (struct client *)0;
}

/*
 * Retrieve the specs of the file to be staged from the master.
 */
static struct stage *
s_get_stagespecs(int sock)
{
  struct stage st_tmp, *st_dat;

  DLOG_MSG(("s_get_stagespecs(%d)",sock));

  st_tmp = st_init;

  if (!get_stage(sock,&st_tmp))
    goto errout;

  st_tmp.sg_status = st_tmp.sg_retcode = 0;

  /* figure out the physical source file name with full path info etc. */
  if (!(st_tmp.sg_psfname = s_make_psfname(&st_tmp)))
    st_tmp.sg_status = SGD_STATUS_ERROR;

  st_dat = (struct stage *)x_malloc(sizeof st_dat[0]);
  *st_dat = st_tmp;

  DLOG_STAGE(st_dat);

  return st_dat;

  errout:;

  release_stage(&st_tmp);
  return (struct stage *)0;
}

/*
 * Send the results to the master about the file to be staged.
 */
static int
s_put_stageresults(int sock,struct client *cl_dat,struct stage *st_dat)
{
  DLOG_MSG(("s_put_stageresults(%d)",sock));
  DLOG_STAGE(st_dat);

  if (!put_client(sock,cl_dat) || !put_stage(sock,st_dat))
    return 0;

  return 1;
}

static void
s_dostatus(int sock,struct client *cl_dat,struct stage *st_dat)
{
  int i;
  struct stat sbuf;

  DLOG_MSG(("s_dostatus()"));

  st_dat->sg_fdone = 0;
  st_dat->sg_status = SGD_STATUS_RUNNING;
  for (i = 0; i < st_dat->sg_fnum; i++) {
    errno = 0;
    if (stat(st_dat->sg_ptfname[i].pf_name,&sbuf) < 0 && errno != ENOENT)
      st_dat->sg_status = SGD_STATUS_ERROR;
    st_dat->sg_ptfname[i].pf_size = errno ? 0 : sbuf.st_size;
    st_dat->sg_fdone += st_dat->sg_ptfname[i].pf_size;
  }
  errno = 0;
  s_put_stageresults(sock,cl_dat,st_dat);
}

static void
s_child_care()
{
  struct client *cl_dat;
  struct stage *st_dat;
  struct transfer *tr_dat;
  int wstat;
  pid_t pid;

  /* now that we got a status request, check the status of our
     running children - actually this shouldn't be necessary, but
     it seems that SIGCHLD signals can get lost in some strange
     way even though we're supposedly using reliable POSIX signal
     semantics */
  for (cl_dat = clientlist; cl_dat; cl_dat = cl_dat->ct_next)
    for (st_dat = cl_dat->ct_stagelist; st_dat; st_dat = st_dat->sg_next)
      if (st_dat->sg_status == SGD_STATUS_RUNNING && st_dat->sg_childpid > 0) {
        for (;;) {
          errno = 0;
          pid = waitpid(st_dat->sg_childpid,&wstat,WNOHANG | WUNTRACED);
          if (pid < 0) {
            if (errno == EINTR)
              continue;
            /* has the child died without giving status?
               let's hope everything went well... */
            else if (errno == ECHILD)
              st_dat->sg_status = SGD_STATUS_DONE;
            else
              errlog(LOG_ERR,"s_slave_ops() : waitpid() returns %ld",
                (long)pid);
          }

          /* oops, our wait got the pid - it shouldn't have because
             we've got SIGCHLD trapped, which should always get
             priority... anyway, we need to do the honors */
          else if (pid > 0) {
            errlog(LOG_INFO,"s_slave_ops() : got pid from waitpid() (%ld)",
              (long)pid);
            get_child_stat(pid,wstat,st_dat);
          }

          /* else if pid == 0 the child is still running happily */
          break;
        }
      }

  /* then check possible ongoing data transfers */

  for (tr_dat = transferlist; tr_dat; tr_dat = tr_dat->tf_next)
    /* the same song as above for all transfers... */
    for (;;) {
      errno = 0;
      pid = waitpid(tr_dat->tf_pid,&wstat,WNOHANG | WUNTRACED);
      if (pid < 0) {
        if (errno == EINTR)
          continue;
        else if (errno == ECHILD)
          remove_transferbypid(pid);
        else
          errlog(LOG_ERR,"s_slave_ops() : waitpid() returns %ld",(long)pid);
      }
      else if (pid > 0) {
        errlog(LOG_INFO,"s_slave_ops() : got pid from waitpid() (%ld)",
          (long)pid);
        remove_transferbypid(pid);
      }
      break;
    }
}

/*
 * Slave operations.
 */
int
s_slave_ops(int sock,struct revector **rvpp)
{
  struct client *cl_dat;
  struct stage *st_dat;
  struct peer *pe_dat;
  int msg, rr;
  unsigned long ultmp;
  pid_t pid;

  DLOG_MSG(("s_slave_ops(%d)",sock));

  /* yes, master... */
  if ((msg = recv_msg(sock)) < 0) return 0;

  switch (msg) {

  /* client asks the status of its ongoing stages */

  case SGD_MSG_STATUS:
    if (!(rr = recvul(sock,&ultmp)) ||
        !(st_dat = find_stagebyid(ultmp,&cl_dat))) {
      /* if the stage ack has already been revectored to another
         master because of a temporary trouble in communication,
         tell this master to forget and junk the stage info */
      if (rr)
        *rvpp = find_revect(ultmp);
      return 0;
    }
    s_dostatus(sock,cl_dat,st_dat);
    pe_dat = find_masterbyip(st_dat->sg_masterip);
    pe_dat->pr_last = time((time_t *)0);
    pe_dat->pr_up = 1;
    break;

  /* master asks staging of file to PIAF server */

  case SGD_MSG_STAGE:
    if (!(cl_dat = s_get_clientspecs(sock)) ||
        !(st_dat = s_get_stagespecs(sock))) {
      /* free the client struct only if it's really a new one,
         i.e., it's got an empty stagelist - we don't want to
         yank a client with ongoing stages */
      if (cl_dat && !cl_dat->ct_stagelist) {
        release_client(cl_dat);
        x_free(cl_dat);
      }
      return 0;
    }

    pe_dat = find_masterbyip(st_dat->sg_masterip);
    pe_dat->pr_last = time((time_t *)0);
    pe_dat->pr_up = 1;

    /* if we got the same stage twice, we probably have a
       bug or misconfiguration somewhere */
    if (find_stagebyid(st_dat->sg_stageid,(struct client **)0) ||
        (*rvpp = find_revect(st_dat->sg_stageid))) {
      errlog(LOG_INFO,"s_slave_ops() : stage %lu appears twice",
        st_dat->sg_stageid);
      release_client(cl_dat);
      release_stage(st_dat);
      x_free(cl_dat);
      x_free(st_dat);
      /* give the master a-ok, so that it (hopefully) doesn't
         try to send it again */
      return 1;
    }

    /* lock the file by calling creat() with zero mode - if it
       fails, that means that another process already has the lock */
    if (st_dat->sg_status != SGD_STATUS_ERROR) {
      size_t fnlen = strlen(st_dat->sg_tfname) + sizeof LOCKFILE_EXT - 1;
      int denied = 0;

      if (fnlen > PATH_MAX) {
        errlog(LOG_ERR,"s_slave_ops() : name of the lockfile too long (%s%s)",
          st_dat->sg_tfname,LOCKFILE_EXT);
        st_dat->sg_retcode = SGD_RET_PATHLEN;
        st_dat->sg_status = SGD_STATUS_ERROR;
      }
      else {
        int fd;
        st_dat->sg_lfname = (char *)x_malloc(fnlen + 1);
        sprintf(st_dat->sg_lfname,"%s%s",st_dat->sg_tfname,LOCKFILE_EXT);
        errno = 0;
        if ((fd = creat(st_dat->sg_lfname,0)) < 0) {
          if (errno == EACCES) denied = 1;
          errlog(LOG_ERR,"s_slave_ops() : lock denied for stage %lu (%s)",
            st_dat->sg_stageid,st_dat->sg_lfname);
          st_dat->sg_retcode = SGD_RET_LOCKERR;
          st_dat->sg_status = SGD_STATUS_ERROR;
          errno = 0;
        }
        else
          x_close(fd);
      }
      /* if the lock was just denied (EACCES), delete stage info
         but tell master all is well with the assumption that
         another slave is doing this same stage right now and
         will report to the master in due time */
      if (denied) {
        release_client(cl_dat);
        release_stage(st_dat);
        x_free(cl_dat);
        x_free(st_dat);
        break;
      }
    }

    /* ok, add a new client to the list */
    if (!cl_dat->ct_stagelist) {
      cl_dat->ct_next = clientlist;
      clientlist = cl_dat;
    }

    st_dat->sg_next = cl_dat->ct_stagelist;
    cl_dat->ct_stagelist = st_dat;

    /*
     * fork a child to do the actual staging and get back
     * waiting for incoming requests from the master
     */

    if (st_dat->sg_status != SGD_STATUS_ERROR) {
      /* are we the child? */
      if ((pid = fork()) == 0) {
        x_close(sock);
        /* never returns - does exit() at the end */
        c_child_ops(cl_dat,st_dat);
      }
      /* out of memory or something? */
      else if (pid < 0) {
        errlog(LOG_ERR,"s_slave_ops() : cannot fork child");
        st_dat->sg_retcode = SGD_RET_FORKERR;
        st_dat->sg_status = SGD_STATUS_ERROR;
      }
      /* ok, the parent */
      else
        st_dat->sg_childpid = pid;
    }
    break;

  /* one of our masters probing if we're still alive */

  case SGD_MSG_PING:
    {
      char *s;

      /* get the master ip address */
      if (!(s = get_peerip(sock,(struct sockaddr_in **)0))) {
        errlog(LOG_ERR,"s_slave_ops() : can't get peername (MSG_PING)");
        return 0;
      }
      if (!(pe_dat = find_masterbyip(s))) {
        errlog(LOG_ERR,"s_slave_ops() : ping from unknown master %s",s);
        return 0;
      }
      pe_dat->pr_last = time((time_t)0);
      pe_dat->pr_up = 1;
    }
    break;

  /* master tells us to forget this stage because
     it has already taken care of it */

  case SGD_MSG_FORGET:
    if (!recvul(sock,&ultmp)) break;
    st_dat = find_stagebyid(ultmp,&cl_dat);
    pe_dat = find_masterbyip(st_dat->sg_masterip);
    pe_dat->pr_last = time((time_t *)0);
    pe_dat->pr_up = 1;

    /* update log file (it wasn't done before) */
    joblog(cl_dat,st_dat);

    unlink(st_dat->sg_lfname);

    /* remove the stage information */
    remove_stage(cl_dat,st_dat);

    break;

  /* another slave asks us to receive a data transfer */

  case SGD_MSG_TRANS:
    {
      struct transfer *tr_dat;

      /* fork a child to do the transfer and get back to select */

      if ((pid = fork()) == 0) {
        int ret = 1;
        size_t bsize = bftp.bf_dotransfer > DEF_TRRECVBUF ?
          (size_t)bftp.bf_dotransfer : (size_t)DEF_TRRECVBUF;
        char *buf = (char *)x_malloc(bsize);

        for (;;) {
          int c;
          unsigned long ibytes, totbytes;
          unsigned long fsize;
          size_t len;
          FILE *f;
          char *fn = (char *)0;

          if ((msg = recv_msg(sock)) < 0 || msg == SGD_MSG_OK ||
              !recvstr(sock,&fn,&len) || len == 0 || !recvul(sock,&fsize))
            break;
          if (!(f = fopen(fn,"w"))) {
            errlog(LOG_ERR,"s_slave_ops() : can't open %s",fn);
            goto out;
          }
#ifdef lint
          ibytes = 0;
#endif
/*          errlog(LOG_INFO,"s_slave_ops() : data transfer %s %lu",fn,fsize);*/
          for (totbytes = 0; totbytes < fsize; totbytes += ibytes) {
            unsigned long gbytes = fsize - totbytes < bsize ?
              fsize - totbytes : bsize;
            ibytes = 0;
            while (ibytes < gbytes && (c = read(sock,buf + ibytes,
                   (size_t)(gbytes - ibytes))) > 0)
              ibytes += c;
            if (c < 0 && errno != EPIPE) {
              errlog(LOG_ERR,
                "s_slave_ops() : transfer error (file %s, char %lu)",
                fn,ibytes);
              goto out;
            }
            if (ibytes > 0)
              if (write(fileno(f),buf,ibytes) < 0) {
                errlog(LOG_ERR,"s_slave_ops() : error writing %s",fn);
                goto out;
              }
          }
          fclose(f);
          x_free(fn);
        }
        ret = 0;

        out:;

        send_msg(sock,SGD_MSG_OK);
        x_close(sock);
        exit(ret);
      }
      else if (pid < 0) {
        errlog(LOG_ERR,"s_slave_ops() : cannot fork child for transfer");
        return 1;
      }
      tr_dat = (struct transfer *)x_malloc(sizeof tr_dat[0]);
      tr_dat->tf_pid = pid;
      tr_dat->tf_next = transferlist;
      transferlist = tr_dat;
    }
    break;

  default:
    errlog(LOG_ERR,"s_slave_ops() : strange message (%d)",msg);
    return 0;
  }

  /* every time someone calls us, check the status of the children */
  s_child_care();

  return 1;
}

int
s_check_children()
{
  struct client *cl, *cll;
  struct stage *st, *stt;
  struct peer *pe;
  struct sockaddr_in *addrp;
  struct stat sbuf;
  int isdown = 0;

  DLOG_MSG(("s_check_children()"));

  for (cl = clientlist, cll = (struct client *)0; cl;
       cll = cl, cl = cll ? cll->ct_next : clientlist) {
    for (st = cl->ct_stagelist, stt = (struct stage *)0; st;
         stt = st, st = stt ? stt->sg_next : cl->ct_stagelist) {
      if (st->sg_status == SGD_STATUS_DONE ||
          st->sg_status == SGD_STATUS_ERROR) {
        int ok, down, sock, i;
#ifdef ACK_EVERYTHING
        int msg;
#endif

        /* if we can't connect, postpone until later */
        if (!(pe = find_masterbyip(st->sg_masterip))) {
          errlog(LOG_ERR,"s_check_children() : can't find master %s",
            st->sg_masterip);
          continue;
        }
        if (!(addrp = tcpconaddr(pe)) ||
            (sock = tcpconnect(addrp)) < 0) {
          int ok2 = 0, m, mm;
          pe->pr_up = 0;
          isdown = 1;
          /* try to revector the acknowledgement to another master */
          for (pe = selectlist, m = 0; pe; pe = pe->pr_next)
            if (pe->pr_up) m++;
          if (m > 0) {
            mm = rand() % m;
            for (pe = selectlist, m = 0; pe; pe = pe->pr_next)
              if (pe->pr_up && m++ == mm) {
                if (!(addrp = tcpconaddr(pe)) ||
                    (sock = tcpconnect(addrp)) < 0) {
                  pe->pr_up = 0;
                  continue;
                }
                errlog(LOG_INFO,
            "s_check_children() : reassigning stage %lu from master %s to %s",
                  st->sg_stageid,st->sg_masterip,pe->pr_ipaddr);
                x_free(st->sg_masterip);
                st->sg_masterip = dupstr(pe->pr_ipaddr);
                add_revect(st->sg_stageid);
                ok2 = 1;
                break;
              }
          }
          if (!ok2) continue;
        }

        if (st->sg_status == SGD_STATUS_ERROR)
          ; /* nothing - just skip tests */

        /* a bunch of consistency checks for striped files */
        else if (do_striping && is_striped(st->sg_tfname)) {
          unsigned long totsize;
          long totrec, sumrec, reclen;
          int totfnum;
          FILE *f;

          /* read striped record info from the target file */
          if (!(f = fopen(st->sg_tfname,"r"))) {
            errlog(LOG_ERR,"s_check_children() : can't open tfname=%s",
              st->sg_tfname);
            st->sg_retcode = SGD_RET_CONCHECK;
            st->sg_status = SGD_STATUS_ERROR;
            totfnum = totsize = totrec = 0;
          }
          else {
            /* the two first lines */
            fscanf(f,"%*[^\n]");
            fgetc(f);
            fscanf(f,"%d%ld%ld%lu%*[^\n]",&totfnum,&totrec,&reclen,&totsize);
            fgetc(f);
          }
          st->sg_fdone = sumrec = 0;
          for (i = 0; i < st->sg_fnum; i++) {
            unsigned long ssiz;
            if (f) {
              if (!feof(f) && !ferror(f)) {
                fscanf(f,"%ld%lu%*[^\n]",&st->sg_ptfname[i].pf_nrec,&ssiz);
                fgetc(f);
              }
            }
            else
              st->sg_ptfname[i].pf_nrec = ssiz = 0;
            errno = 0;
            if (stat(st->sg_ptfname[i].pf_name,&sbuf) < 0) {
              errlog(LOG_ERR,
                "s_check_children() : stat() trouble with ptfname[%d]=%s",
                i,st->sg_ptfname[i].pf_name);
              st->sg_retcode = SGD_RET_CONCHECK;
              st->sg_status = SGD_STATUS_ERROR;
            }
            st->sg_ptfname[i].pf_size = errno ? 0 : sbuf.st_size;
            st->sg_fdone += st->sg_ptfname[i].pf_size;
            sumrec += st->sg_ptfname[i].pf_nrec;
            if (ssiz != st->sg_ptfname[i].pf_size) {
              errlog(LOG_ERR,
                "s_check_children() : ssiz=%lu,pf_size=%lu (%s)",
                ssiz,st->sg_ptfname[i].pf_size,st->sg_ptfname[i].pf_name);
              st->sg_retcode = SGD_RET_CONCHECK;
              st->sg_status = SGD_STATUS_ERROR;
            }
          }
          if (f) fclose(f);
          if (totfnum != st->sg_fnum ||
              totrec != sumrec ||
              totsize != st->sg_fdone ||
              reclen * totrec * HBOOK_WORD_BYTES != totsize) {
            errlog(LOG_ERR,
              "s_check_children() : (totfnum=%d,sg_fnum=%d), \
(totrec=%ld,sumrec=%ld), (totsize=%lu,sg_fdone=%lu), (reclen=%ld) (%s)",
              totfnum,st->sg_fnum,totrec,sumrec,totsize,st->sg_fdone,
              reclen,st->sg_tfname);
            st->sg_retcode = SGD_RET_CONCHECK;
            st->sg_status = SGD_STATUS_ERROR;
          }
        }

        /* if this isn't a striped file it can either be a symlink or
           a regular file - in the latter case use the target file only */
        else if (stat(st->sg_tfname,&sbuf) == 0 && sbuf.st_size > 0) {
          int n;
          char tmp[PATH_MAX + 1];
          for (i = 0; i < st->sg_fnum; i++)
            if (st->sg_ptfname[i].pf_name) {
              x_free(st->sg_ptfname[i].pf_name);
              st->sg_ptfname[i].pf_name = (char *)0;
            }
          /* check if this is an hbook file at all - if it is, it's
             really a symlink instead of a regular file */
          lstat(st->sg_tfname,&sbuf);
          if (S_ISLNK(sbuf.st_mode) &&
             (n = readlink(st->sg_tfname,tmp,sizeof tmp - 1)) > 0) {
            tmp[n] = '\0';
            st->sg_ptfname[0].pf_name = dupstr(tmp);
          }
          /* regular file */
          else {
#ifdef APPEND_HBOOK_EXT
            /* rename file to get rid of the ".hbook" extension */
            char *ss = dupstr(st->sg_tfname);
            ss[strlen(ss) - sizeof HBOOK_EXT + 1] = '\0';
            if (rename(st->sg_tfname,ss) < 0) {
              errlog(LOG_ERR,"s_check_children() : can't rename %s to %s",
                st->sg_tfname,ss);
              x_free(ss);
            }
            else {
              x_free(st->sg_tfname);
              st->sg_tfname = ss;
            }
#endif
            st->sg_ptfname[0].pf_name = dupstr(st->sg_tfname);
          }
          st->sg_ptfname[0].pf_nrec = -1;
          errno = 0;
          if (stat(st->sg_ptfname[0].pf_name,&sbuf) < 0 || sbuf.st_size == 0) {
            errlog(LOG_ERR,
          "s_check_children() : stat() trouble with ptfname[%d]=%s (size %ld)",
              0,st->sg_ptfname[0].pf_name,(unsigned long)sbuf.st_size);
            st->sg_retcode = SGD_RET_CONCHECK;
            st->sg_status = SGD_STATUS_ERROR;
          }
          st->sg_ptfname[0].pf_size = errno ? 0 : sbuf.st_size;
          st->sg_fdone = st->sg_ptfname[0].pf_size;
          st->sg_fnum = 1;
        }

        /* the file isn't there or it's zero length */
        else {
          errlog(LOG_ERR,
            "s_check_children() : stat() trouble with tfname=%s",
            st->sg_tfname);
          st->sg_retcode = SGD_RET_CONCHECK;
          st->sg_status = SGD_STATUS_ERROR;
        }

        if (st->sg_status == SGD_STATUS_DONE)
          st->sg_fsiz = st->sg_fdone;

        /* if there was an error remove the file */
        else if (st->sg_status == SGD_STATUS_ERROR) {
          char cmd[2 * PATH_MAX + 50];
          sprintf(cmd,"%s -f %s >/dev/null 2>&1",rmbin,st->sg_tfname);
          if (system(cmd) != 0)
            errlog(LOG_ERR,"s_check_children() : system(%s) trouble",cmd);
        }
        ok = 1;

        /* if the master doesn't get the info, don't destroy the local
           data struct - it will be retried the next time around */
        if ((down = !send_msg(sock,SGD_MSG_OK) ||
                    !s_put_stageresults(sock,cl,st)
#ifdef ACK_EVERYTHING
                    || (msg = recv_msg(sock)) < 0) ||
            msg != SGD_MSG_OK
#else
            )
#endif
           ) {
          errlog(LOG_ERR,
            "s_check_children() : trouble connecting to master %s (%s)",
            pe->pr_machname,down ? "conn" : "ack");
          ok = 0;
        }
        x_close(sock);
        if (down) {
          pe->pr_up = 0;
          isdown = 1;
        }
        pe->pr_last = time((time_t *)0);
        if (!ok) continue;

        /* ok, update log file and zap the local stage info */
        joblog(cl,st);
        if (stt)
          stt->sg_next = st->sg_next;
        else
          cl->ct_stagelist = st->sg_next;
#if 0
/* for testing (to keep huge files from taking too much space) */
            for (i = 0; i < st->sg_fnum; i++) {
              if (st->sg_ptfname[i].pf_size > 1000000)
                unlink(st->sg_ptfname[i].pf_name);
            }
#endif
        /* zap the lock file */
        unlink(st->sg_lfname);
        release_stage(st);
        x_free(st);
        st = stt;
      }
    }
    /* if the stagelist for a client is empty, destroy client */
    if (!cl->ct_stagelist) {
      if (cll)
        cll->ct_next = cl->ct_next;
      else
        clientlist = cl->ct_next;
      release_client(cl);
      x_free(cl);
      cl = cll;
    }
  }
  return isdown;
}

int
s_init_masters()
{
  struct peer *pe_dat;
  int isdown = 0, ok;

  DLOG_MSG(("s_init_masters()"));

  for (pe_dat = selectlist; pe_dat; pe_dat = pe_dat->pr_next) {
    int sock, down;
#ifdef ACK_EVERYTHING
    int msg;
#endif
    struct sockaddr_in *addrp;

    if (pe_dat->pr_up) continue;

    /* do the initial connect */
    if (!(addrp = tcpconaddr(pe_dat)) ||
        (sock = tcpconnect(addrp)) < 0) {
      DLOG_MSG(("s_init_masters() : tcpconnect failure"));
      isdown = 1;
      continue;
    }
    ok = 1;

    /* the port for the master to use when connecting back to the slave */
    if ((down = !send_msg(sock,SGD_MSG_INIT) ||
                !sendul(sock,(unsigned long)pe_dat->pr_lisport)
#ifdef ACK_EVERYTHING
                || (msg = recv_msg(sock)) < 0) ||
        msg != SGD_MSG_OK
#else
        )
#endif
       ) {
      errlog(LOG_ERR,
        "s_init_masters() : trouble connecting to master %s (%s)",
        pe_dat->pr_machname,down ? "conn" : "ack");
      ok = 0;
    }

    x_close(sock);
    pe_dat->pr_last = time((time_t *)0);
    if (!ok) {
      if (down) isdown = 1;
      continue;
    }

    /* ok, start listening to the same master */
    if (!(addrp = tcplisaddr(pe_dat)) ||
        (pe_dat->pr_sock = tcplisten(addrp)) < 0) {
      DLOG_MSG(("s_init_masters() : tcplisten failure"));
      isdown = 1;
      continue;
    }
    pe_dat->pr_up = 1;
  }
  return isdown;
}

int
s_ping_masters(long *timeout)
{
  struct peer *pe_dat;
  int ret = 0;

  *timeout = pinginterval;
  if (pinginterval >= 0) {
    time_t now = time((time_t)0);

    for (pe_dat = selectlist; pe_dat; pe_dat = pe_dat->pr_next) {
      time_t tdiff = now - pe_dat->pr_last;

      ret = 1;
      if (pe_dat->pr_up) {
        if (tdiff >= pinginterval) {
          /* check if he's up or down */
          if (ping_peer(pe_dat))
            pe_dat->pr_last = now;
          else {
            errlog(LOG_INFO,"s_ping_masters() : no response pinging master %s",
              pe_dat->pr_machname);
            pe_dat->pr_up = 0;
          }
        }
        /* the shortest time to the next alarm */
        else if (pinginterval - tdiff < *timeout)
          *timeout = pinginterval - tdiff;
        if (*timeout < 0) *timeout = 0;
      }
    }
  }
  return ret;
}
