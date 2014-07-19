/*
 * $Id: child.c,v 1.3 2005/04/18 15:23:12 mclareni Exp $
 *
 * $Log: child.c,v $
 * Revision 1.3  2005/04/18 15:23:12  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.2  1996/04/02 22:00:15  thakulin
 * Change program name to pfstagerd so as not to clash with the shift
 * stager.  Modify include file names and comments accordingly.
 *
 * Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
 * Paw
 *
 */
/*CMZ :          05/12/95  23.23.08  by  Timo Hakulinen*/
/*-- Author :    Timo Hakulinen   25/08/94*/
/*************************************************************************
 *
 * child.c
 *
 * Routines peculiar to slave children (names starting with "c_").
 *
 * author:  Timo Hakulinen, CERN / CN
 * date:    20/6/94
 *
 *************************************************************************/

#include "pfstagerd.h"

/*
 * Ftp to the client site to get a local file.
 */
static int
c_ftp_client(struct client *cl_dat,struct stage *st_dat)
{
  char cmd[PATH_MAX + 50];
  char dir[PATH_MAX + 1], *fdir, *fname;
  char tmp[PATH_MAX + 1];
  size_t len;
  FILE *f;
  int ret;

  DLOG_MSG(("c_ftp_client()"));

  /* separate the path from the file name for ftp - we assume
     a unix system at the other end */
  len = strlen(st_dat->sg_psfname);
  if (len > PATH_MAX) {
    errlog(LOG_ERR,"c_ftp_client() : path too long (%s)",st_dat->sg_psfname);
    return SGD_RET_PATHLEN;
  }
  strcpy(dir,st_dat->sg_psfname);
  fname = strrchr(dir,'/');
  /* found a slash? */
  if (fname) {
    *fname = '\0';
    fname++;
    /* ouch, the file name ended with a slash - no good */
    if (!*fname) {
      errlog(LOG_ERR,"c_ftp_client() : bad file name (%s)",st_dat->sg_psfname);
      return SGD_RET_SFNAME;
    }
    fdir = dir;
  }
  /* nope, it's either the file name as is or non-unix path */
  else {
    fname = dir;
    fdir = (char *)0;
  }
  
  /* builtin ftp client */
  
  if (bftp.bf_bufsize > 0) {
    if (!(f = tmpfile())) {
      errlog(LOG_ERR,"c_ftp_client() : tmpfile() failed : %s",
	     strerror(errno););
      return SGD_RET_FOPEN;
    }
    ret = ftp_ftp(f,cl_dat,st_dat,fdir,fname);
    fclose(f);
  }

  /* system ftp */

  else {
    sprintf(cmd,"%s -v -n %s > %s 2>&1",ftpbin,cl_dat->ct_ipaddr,tmp);
    if (!(f = popen(cmd,"w"))) {
      errlog(LOG_ERR,"c_ftp_client() : can't make pipe (%s)",cmd);
      return SGD_RET_POPEN;
    }

    /* write the ftp script */
    fprintf(f,"\
user %s %s\n\
%s%s%s\
get %s %s\n\
bye\n",
      cl_dat->ct_fusername,
      cl_dat->ct_passwd ? dcpass(cl_dat->ct_passwd) : "",
      fdir ? "cd " : "",
      fdir ? fdir : "",
      fdir ? "\n" : "",
      fname,
      st_dat->sg_ptfname[0].pf_name);
    /* wipe the plain text password from memory */
    (void)dcpass((char *)0);
    /* for explanation why -1 is ok see pclose() in function m_mail_client() */
    if ((ret = pclose(f)) == -1) ret = 0;
  }

  return ret;
}

/*
 * Start stager to the get a tape.
 */
static int
c_stage_tape(struct client *cl_dat,struct stage *st_dat)
{
  char cmd[2 * PATH_MAX + MAX_TAPEID + 18];
  char tapename[MAX_TAPEID + 1];
  char *volid, *fseqid, *lbltyp, *volser;
  int ret, i;

  DLOG_MSG(("c_stage_tape()"));

  if (strlen(st_dat->sg_psfname) >= sizeof tapename) {
    errlog(LOG_ERR,"c_stage_tape() : tape name too long (%s)",
      st_dat->sg_psfname);
    return SGD_RET_PATHLEN;
  }
  strcpy(tapename,st_dat->sg_psfname);
  volid = strtok(tapename,".");
  fseqid = strtok((char *)0,".");
  lbltyp = strtok((char *)0,".");
  volser = strtok((char *)0,".");

  /* builtin clio client */

  if (bclio.bc_server) {
    FILE *f;
    char tmp[PATH_MAX + 1], *s;


    /* builtin ftp client */

    if (bftp.bf_bufsize > 0) {
      if (!(f = tmpfile())) {
        errlog(LOG_ERR,"c_stage_tape() : tmpfile() failed : %s",
	       strerror(errno));
        return SGD_RET_FOPEN;
      }
      ret = ftp_clio(f,cl_dat,st_dat,volid,fseqid,lbltyp,volser);
      fclose(f);
    }

    /* system ftp */

    else {
      sprintf(cmd,"%s -n -v %s %d > %s 2>&1",
        ftpbin,bclio.bc_server,bclio.bc_port,tmp);
      for (i = 0; i < bclio.bc_retries; i++) {
        if (!(f = popen(cmd,"w"))) {
          errlog(LOG_ERR,"c_stage_tape() : can't make pipe (%s)",cmd);
          return SGD_RET_POPEN;
        }
        fprintf(f,"\
user %s\n\
quote site vers %d\n\
quote site acct %s\n\
quote site blksize %s\n\
quote site format %s\n\
quote site dsn %s\n\
quote site label %s\n\
quote site lrecl %s\n\
quote site blks %d\n\
quote site vsn %s\n\
quote site vid %s\n\
quote site fseq %s\n\
binary\n\
get %s %s\n\
quote site srep\n\
bye\n",
          bclio.bc_login, bclio.bc_version, cl_dat->ct_username,
          bclio.bc_blocksize, bclio.bc_format, bclio.bc_fileid,
          lbltyp, bclio.bc_reclen, bclio.bc_datsiz,
          volser, volid, fseqid,
          st_dat->sg_psfname, st_dat->sg_ptfname[0].pf_name);

      /* for explanation why -1 is ok see pclose() in
         function m_mail_client() */
        if ((ret = pclose(f)) == -1) ret = 0;

        /* analyze the answers of the clio server */
        {
          int ok = 0;
          FILE *ff;
          char *buf;
          struct stat sbuf;

          if (stat(tmp,&sbuf) < 0) {
            errlog(LOG_ERR,"c_stage_tape() : can't stat %s",tmp);
            return SGD_RET_STAT;
          }
          if (sbuf.st_size <= 0) {
            errlog(LOG_ERR,"c_stage_tape() : ftp script bad (%s)",tmp);
            return SGD_RET_FTPBIN;
          }
          if (!(ff = fopen(tmp,"r"))) {
            errlog(LOG_ERR,"c_stage_tape() : can't open script %s",tmp);
            return SGD_RET_FOPEN;
          }
          buf = (char *)x_malloc(sbuf.st_size + 1);
          fread(buf,1,sbuf.st_size,ff);
          fclose(ff);
          s = strstr(buf,"\n999 RC ");
          /* 880 means that clio server is full - sleep a while and retry */
          if (!s && strstr(buf,"\n880 "))
            sleep(bclio.bc_wait);
          else {
            /* get the return code */
            if (s) {
              s += sizeof "\n999 RC " - 1;
              ret = atoi(s);
            }
            else {
              errlog(LOG_ERR,"c_stage_tape() : strange error (s == NULL)");
              ret = SGD_RET_UNKNOWN;
            }
            ok = 1;
          }
          x_free(buf);
          if (ok) {
            unlink(tmp);
            break;
          }
        }
      }
      if (i >= bclio.bc_retries) {
        errlog(LOG_ERR,"c_stage_tape() : clio connection timeout (%d tries)",
          bclio.bc_retries);
        ret = SGD_RET_CLIOTIMEOUT;
      }
    }
  }

  /* or the actual clio script */

  else {
    char *fdir, *fname;

    /* separate the path from the file name - we need to do a chdir */
    fdir = dupstr(stagerbin);
    fname = strrchr(fdir,'/');
    /* found a slash? */
    if (fname) {
      *fname = '\0';
      fname++;
      /* ouch, the file name ended with a slash - no good */
      if (!*fname) {
        errlog(LOG_ERR,"c_stage_tape() : bad stagerbin path (%s)",stagerbin);
        return SGD_RET_STAGERBIN;
      }
    }
    /* nope, it's the file name as is - we gotta be in the right dir already */
    else {
      fname = fdir;
      fdir = (char *)0;
    }
    if (fdir && chdir(fdir) < 0) {
      errlog(LOG_ERR,"c_stage_tape() : can't chdir %s",fdir);
      return SGD_RET_CHDIR;
    }
    sprintf(cmd,"./%s -V %s%s%s%s%s%s%s %s",fname,volid,
      fseqid ? " -q " : "",fseqid ? fseqid : "",
      lbltyp ? " -l " : "",lbltyp ? lbltyp : "",
      volser ? " -v " : "",volser ? volser : "",
      st_dat->sg_ptfname[0].pf_name);
    ret = system(cmd);
  }

  return ret;
}

void
c_child_ops(struct client *cl_dat,struct stage *st_dat)
{
  int ret, i;
  char *s;
  struct passwd *pw;

  DLOG_REOPEN("child.log");

  DLOG_MSG(("c_child_ops()"));
  DLOG_STAGE(st_dat);

  if (!(pw = getpwnam(cl_dat->ct_username))) {
    errlog(LOG_ERR,"c_child_ops() : can't get pw entry for %s",
      cl_dat->ct_username);
    exit(SGD_RET_PW);
  }
  if (!pw->pw_dir) {
    errlog(LOG_ERR,"c_child_ops() : no home dir for %s",cl_dat->ct_username);
    exit(SGD_RET_PWDIR);
  }
  {
    char home[PATH_MAX + 1];
    size_t siz, siz1 = strlen(pw->pw_dir), siz2 = strlen(piafhome);
    int nosl1 = 1, nosl2 = 1;

    if (pw->pw_dir[siz1 - 1] == '/') nosl1 = 0;
    if (piafhome[siz2 - 1] == '/') nosl2 = 0;
    siz = siz1 + siz2 + nosl1;
    if (siz > PATH_MAX) {
      errlog(LOG_ERR,"c_child_ops() : path %s%s too long, (%s)",
        pw->pw_dir,piafhome,cl_dat->ct_username);
      exit(SGD_RET_CHDIR);
    }
    strcpy(home,pw->pw_dir);
    if (nosl1) strcat(home,"/");
    strcat(home,piafhome);
    if (!nosl2) home[siz - 1] = '\0';
    if (chdir(home) < 0) {
      errlog(LOG_ERR,"c_child_ops() : can't chdir to %s (%s)",
        home,cl_dat->ct_username);
      exit(SGD_RET_CHDIR);
    }
  }

  switch (st_dat->sg_ftyp) {

  case SGD_FTYP_LOCAL:
    ret = c_ftp_client(cl_dat,st_dat);
    break;

  case SGD_FTYP_TAPE:
    ret = c_stage_tape(cl_dat,st_dat);
    break;

  case SGD_FTYP_FATMEN:
    /* if it's a file name, fatmen returns "host:filename" -
       tapes don't have the embedded ':' */
    if (strchr(st_dat->sg_psfname,':')) {
      errlog(LOG_ERR,
        "c_child_ops() : fatmen path %s expansion %s corresponds to a file",
        st_dat->sg_sfname,st_dat->sg_psfname);
      exit(SGD_RET_FATMENFILE);
    }
    ret = c_stage_tape(cl_dat,st_dat);
    break;
  }

  /* ouch, transfer went bonkers - bury the dead */
  if (ret != 0) {
    unlink(st_dat->sg_tfname);
    for (i = 0; i < st_dat->sg_fnum; i++)
      unlink(st_dat->sg_ptfname[i].pf_name);
    exit(ret);
  }

  /* set file permissions for the files (should this
     info somehow be derived from the user's umask?) */

  /* the file in user dir is either a sym link to the one file,
     or in the case of multiple stripes a special file pointing
     to several data files */
/*  s = true_tfname(st_dat->sg_tfname);*/
  s = st_dat->sg_tfname;
  if (!is_striped(s) && chmod(s,useruperm) < 0) {
    errlog(LOG_ERR,"c_child_ops() : can't chmod tfname=%s",s);
    exit(SGD_RET_CHMOD);
  }
  /* gotta be root to do chown() / lchown() */
  if (getuid() == 0 &&
      (chown(s,pw->pw_uid,pw->pw_gid) < 0 ||
       lchown(s,pw->pw_uid,pw->pw_gid) < 0)) {
    errlog(LOG_ERR,"c_child_ops() : can't chown %s uid %ld gid %ld",
      s,(long)pw->pw_uid,(long)pw->pw_gid);
    exit(SGD_RET_CHOWN);
  }
  for (i = 0; i < st_dat->sg_fnum; i++) {
    if (chmod(st_dat->sg_ptfname[i].pf_name,useruperm) < 0) {
      errlog(LOG_ERR,"c_child_ops() : can't chmod ptfname[%d]=%s",
        i,st_dat->sg_ptfname[i].pf_name);
      exit(SGD_RET_CHMOD);
    }
    if (getuid() == 0 &&
        chown(st_dat->sg_ptfname[i].pf_name,pw->pw_uid,pw->pw_gid) < 0) {
      errlog(LOG_ERR,"c_child_ops() : can't chown %s uid %ld gid %ld",
        st_dat->sg_ptfname[i].pf_name,(long)pw->pw_uid,(long)pw->pw_gid);
      exit(SGD_RET_CHOWN);
    }
  }
  exit(ret);
}
