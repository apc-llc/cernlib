/*
 * cs_mktmpdir.c
 * Author: Kevin McCarty, March 4, 2005
 * Purpose: safely create a temporary directory in UNIX for COMIS to use.
 */

#if defined(CERNLIB_UNIX)

# include "comis/pilot.h"
# include <stdlib.h>
# include <string.h>
# include <stdio.h>
# include <errno.h>
# include <unistd.h>
# include <sys/stat.h>
# include <sys/types.h>

# if defined(CERNLIB_LINUX) || defined(CERNLIB_BSD)
#  define mksafetemp mkdtemp
# else

/* reimplement mkdtemp() for systems without it */
static char *mksafetemp(char *template)
{
  int i, len;
  char * tempcopy;
  FILE * randomfile = fopen("/dev/urandom", "r");
  if (!randomfile)
    randomfile = fopen("/dev/random", "r");
  if (!randomfile)
    return NULL;
  
  len = strlen(template);
  if (len < 6 || strncmp("XXXXXX", template + len - 6, 6)) {
    fclose(randomfile);
    errno = EINVAL;
    return NULL;
  }
  
 again:
  tempcopy = strdup(template);
  
  for (i = 0; i < 6; i++) {
    unsigned char randomchar;
    fread(&randomchar, 1, 1, randomfile);
    randomchar %= 62;
    if (randomchar < 10)
      randomchar += '0';
    else if (randomchar < 36)
      randomchar += ('A' - 10);
    else
      randomchar += ('a' - 36);
    tempcopy[i + len - 6] = randomchar;
  }

  if (mkdir(tempcopy, 0700)) {
    free(tempcopy);
    if (errno == EEXIST)
      goto again;
    else {
      fclose(randomfile);
      return NULL;
    }
  }

  strcpy(template, tempcopy);
  fclose(randomfile);
  free(tempcopy);
  return template;
}
# endif


/* CSTMPD: securely creates a temporary directory for use by COMIS and
 * returns the path in "result", a string of available size "length".
 * Any initial value in "result" is ignored.
 * The return value is the actual length of the directory path.
 * 
 * If a temporary directory previously created in the lifetime of the
 * program still exists and has the right owner and permissions (0700),
 * it is used instead.
 */

#if defined(CERNLIB_QX_SC)
# define Cstmpd cstmpd_
#endif
#if defined(CERNLIB_QXNO_SC)
# define Cstmpd cstmpd
#endif
#if defined(CERNLIB_QXCAPT)
# define Cstmpd CSTMPD
#endif
int type_of_call Cstmpd(char * result, int * length)
{
  static char * current_dir = 0;
  int i, mknewdir = 1;

  /* by default (if something goes wrong) return a blank string */
  memset(result, ' ', *length);
  
  if (current_dir && strlen(current_dir) < *length) {
    /* Check whether cached value of current_dir is still a usable
     * existing directory. */
    struct stat buf;

    if (lstat(current_dir, &buf) == 0) {
      if (buf.st_uid == geteuid() && S_ISDIR(buf.st_mode) 
	  && !S_ISLNK(buf.st_mode) && !(buf.st_mode & S_IWGRP)
	  && !(buf.st_mode & S_IWOTH))
	mknewdir = 0;
    }
  }
  
  /* Create a new temporary directory if necessary. */
  if (mknewdir) {
    char * template;
    char * tmpdir;
    
    free(current_dir);
    current_dir = malloc(*length);
    template = malloc(*length);
    if ((tmpdir = getenv("TMPDIR")) && *length > strlen(tmpdir) + 12)
      strcpy(template, tmpdir);
    else if (*length > 16)
      strcpy(template, "/tmp");
    else {
      free(template);
      free(current_dir);
      current_dir = 0;
      errno = ENAMETOOLONG;
      return 0;
    }
    strcat(template, "/comisXXXXXX");
    
    if (!mksafetemp(template)) {
      free(template);
      free(current_dir);
      current_dir = 0;
      return 0;
    }

    strcpy(current_dir, template);
  }

  /* Put current_dir into result and make sure result is in FORTRAN format
   * (no nul, ends with blanks), plus add a trailing slash. */
  strncpy(result, current_dir, *length - 1);
  result[strlen(current_dir)] = '/';
  for (i = strlen(current_dir) + 1; i < *length; i++)
    result[i] = ' ';
  return strlen(current_dir) + 1;
}

#if 0
/* test routine */
int main()
{
  char tmp[26];
  int length, len2;

  for (length = 25; length > 0; length--) {
    len2 = Cstmpd(tmp, &length);
    tmp[length] = 0;
    printf("length %d, tmpdir: `%s'\n", len2, tmp);
    tmp[len2] = 0;
    rmdir(tmp);
  }

  for (length = 25; length > 0; length--) {
    len2 = Cstmpd(tmp, &length);
    tmp[length] = 0;
    printf("length %d, tmpdir: `%s'\n", len2, tmp);
    tmp[len2] = 0;
  }

  return 0;
}
#endif

#endif /* CERNLIB_UNIX */
