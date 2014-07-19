/*
 * $Id: kmisc.c,v 1.11 1997/11/19 08:56:26 mclareni Exp $
 *
 * $Log: kmisc.c,v $
 * Revision 1.11  1997/11/19 08:56:26  mclareni
 * Convert C++ comment to C
 *
 * Revision 1.10  1997/10/23 16:53:58  mclareni
 * NT mods
 *
 *
 * Revision 1.10  1997/09/22 13:49:55  Fine
 * WNT mods:  fexpand, fsearch   
 *            slashsplit too, likely the last should be UNDO as before
 *
 * Revision 1.9  1997/04/24 13:49:55  gunter
 * Undo changes done by Valeri to slashsplit (this is HPUX only). He messed
 * this up quite a bit.
 *
 * Revision 1.8  1997/03/17 16:54:59  mclareni
 * WNT mods
 *
 * Revision 1.7  1996/10/17 16:06:03  gunter
 * If xsymlink returns error, return the file without prefixing the cwd.
 * ( This allows to open shift files with host:/path )
 *
 * Revision 1.6  1996/10/16 12:41:58  gunter
 * restructure xsymlinks.
 * Now, in case a part of the dir/file does not exist, we return an error.
 * This is needed for shift files: these typically do not exist in the filesystem, but their name then should not be changed eitehr.
 *
 * Revision 1.5  1996/10/08 15:04:09  cremel
 * Open file in mode 'a' (append; open for writing at end of file, or create
 * for writing). Add "\n".
 *
 * Revision 1.4  1996/10/08 13:19:56  cremel
 * Add new KUIP command /KUIP/FMESSAGE to write a message string into a file.
 * New corresponding action routine is kxfmessage (C code).
 *
 * Revision 1.3.2.1  1997/01/21 11:32:50  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.3  1996/04/15 14:48:45  cernlib
 * Remove extra , in lib$spawn
 *
 * Revision 1.2  1996/04/11 09:45:40  cernlib
 * VMS code changed in kxshell
 *
 * Revision 1.1.1.1  1996/03/08 15:32:58  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/00 06/03/95  12.07.28  by  Gunter Folger*/
/*-- Author :    Alfred Nathaniel   09/09/92*/
/* kmisc.c: miscellaneous functions */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/ksig.h"
#include "kuip/mkterm.h"


#ifdef HPUX

#include <symlink.h>

#define MAXPATHLEN 1024
static char xbuf[MAXPATHLEN];



/************************************************************************
 *                                                                      *
 * Return a NULL terminated array of strings. Each string containing    *
 * a part of the pathname.                                              *
 *                                                                      *
 ************************************************************************/
static
char **slashsplit(s)
     char *s;
{
  char *t,**r,**q;
  int t0;

  while( strchr(SLASHES,*s) )
    s++;

  if (!*s) {
    q = (char **) calloc(1,sizeof(char **));
    *q = NULL;
    return q;
  }
  for (t = s, t0 = 0; *t; t++)
    if (strchr(SLASHES,*t))
      t0++;
  q  = r = (char **) calloc((t0+2),sizeof(char **));
  while ((t = strpbrk( s, SLASHES )) != NULL)
    {
      *t = '\0';
      *q++ = str0dup(s);
      *t = '/';
      while (strchr(SLASHES,*t))
        t++;
      if (!*t)
        {
          *q = NULL;
          return r;
        }
      s = t;
    }
  *q++ = str0dup(s);
  *q = NULL;
  return r;
}

#include <errno.h>

/************************************************************************
 *                                                                      *
 * Expands symlinks and .. or . expressions,                            *
 * if flag = 0, only expand .. and . expressions.                       *
 * s is assumed to start from /, but need NOT to start with "/"! 	*
 *   ( actually this depends on xbuf, which should be empty, otherwise  *
 *     this routines just add to xbuf)					*
 *  return codes:							*
 *   TRUE (1) : success, a (modified) filename is in xbuf		*
 *   FALSE(0) : Failed, probably a dir/file component not found		*
 *                                                                      *
 ************************************************************************/
static
int	xsymlinks(s, flag) /**/
char	*s;
int	flag;
{
	char	**pp, **opp;
	char	xbuf2[MAXPATHLEN], xbuf3[MAXPATHLEN];
	int	t0;

	opp = pp = slashsplit(s);
	for (; *pp; pp++) {
		if (!strcmp(*pp, ".")) {
			free(*pp);
			/*   continue;   */
		} else if (!strcmp(*pp, "..")) {
			char	*p;

			free(*pp);
			if (!strcmp(xbuf, "/"))
				continue;
			p = xbuf + strlen(xbuf);
			while (*--p != '/')
				;
			*p = '\0';
			/*  continue;   */
		} else if ( flag ) {
			sprintf(xbuf2, "%s/%s", xbuf, *pp);
			t0 = readlink(xbuf2, xbuf3, MAXPATHLEN);
			if (t0 == -1 ) {
				if (errno == EINVAL ) {  /* is not a symlink */
					strcat(xbuf, "/");
					strcat(xbuf, *pp);
					free(*pp);
				} else {  /* real error, clean up and exit */
					goto abort;
				}
			} else {
				xbuf3[t0] = '\0'; /* STUPID */
				if (*xbuf3 == '/') {	  /* absolute symlink,*/
					strcpy(xbuf, ""); /* restart new xbuf */
				}
				if ( ! xsymlinks(xbuf3, flag)) { 
					/* error, clean up */
					goto abort;
				}
				free(*pp);
			}
		} else {
			strcat(xbuf, "/");
			strcat(xbuf, *pp);
			free(*pp);
		}
	}
	free(opp);
	return 1;

/* error, clean up and exit */
abort:
	for (; *pp; pp++) free(*pp);
	free(*opp);
	return 0;
}



/************************************************************************
 *                                                                      *
 * Expand symlinks in s, and remove other weird things (.., .).         *
 * This routine allocates space for the result, free this string after  *
 * usage to prevent memory leaks.                                       *
 *                                                                      *
 ************************************************************************/
char *fsymlink( const char *s )
{
  strcpy(xbuf,"");
  if (*s != '/') {
    char fullp[MAXPATHLEN];
    getcwd(fullp, MAXPATHLEN);
    strcat(fullp, "/");
    strcat(fullp, s);
    if ( ! xsymlinks(fullp+1,1))
      return str0dup(s);
  } else if ( ! xsymlinks(s+1,1))
    return str0dup(s);
  if (!*xbuf)
    return str0dup("/");
  return str0dup(xbuf);
}

#endif


/*
 * expand ~ into user's home directory
 * expand $var for environment variables
 * append default filetype if missing
 * the pointer returned is allocated
 */
char *fexpand( const char *fname,
              const char *ftype ) /* default file extension */
{
  char *path = strdup( fname );
  char *p;

#ifdef UNIX
  if( path[0] == '~'
     && strchr( SLASHES, path[1] ) != NULL ) {
    p = str2dup( "$HOME", &path[1] );
    free( path );
    path = p;
  }

  if( path[0] == '$' || path[0] == '~' ) {
    char *repl = NULL;
    char *slash = strpbrk( path, SLASHES );
    char Slash = '\0';

    if( slash != NULL ) {
      Slash = *slash;
      *slash = '\0';
    }

    if( path[0] == '$'
       && (repl = getenv( &path[1] )) == NULL
       && (repl = getenv( strupper( &path[1] ) )) == NULL ) {
      printf( " *** Environment variable %s not defined\n", path );
    }
# ifndef MSDOS
    else if( path[0] == '~' ) {
      struct passwd *pwd = getpwnam( &path[1] );
      if( pwd == NULL ) {
        printf( " *** User %s not found in password file\n", &path[1] );
      }
      else {
        repl = pwd->pw_dir;
      }
    }
# endif

    if( slash != NULL )
      *slash = Slash;

    if( repl != NULL ) {
      p = strdup( repl );
      if( slash != NULL ) 
      {
      /* Check whether $ had its own "end" slash V.Fine 22.09.97 */
        char *eov= &p[strlen(p)-1];
        if (strchr(SLASHES,*eov)) *eov = '\0'; /* cut one extra slash off */

        p = mstrcat( p, slash );
      }
      free( path );
      path = p;
    }
  }

#endif

  if( path[0] == '-' ) {
    /* if "-file" remove "-" and leave rest untouched */
    strcpy( path, path + 1 );
  }

  else if( ftype != NULL ) {
#ifdef vms
    /* watch out for VMS "disk:[dir.sub]file" */
    p = strrchr( path, ']' );
#define _done_
#endif
#ifdef UNIX
    /* watch out for Unix "dir.sub/file" */
    if( (p = strrpbrk( path, SLASHES )) != NULL ) {
      /* skip over "/." in "dir/.file" */
      p += 2;
    }
#define _done_
#endif
#ifdef _done_
#undef _done_
    if( p == NULL )
      p = path;
#else
    p = path;

#endif
    if( strchr( p, '.' ) == NULL ) {
      /* append default file type missing */
      path = mstr2cat( path, ".", ftype );
    }
  }

  return path;
}


/*
 * test file existance and return file path or NULL
 * the search path can be a comma or colon separated list of directories
 */
char *fsearch( const char *fname,
              const char *ftype,
              const char *dir_path ) /* comma(!) separated search path */
{
#if defined(vms) || defined(MSDOS) || defined(CERNLIB_WINNT)
  static char sep[] = ",";      /* colon could be "disk:[dir]" */
#else
  static char sep[] = ",:";
#endif
  static char *path = NULL;
  char *fspec = fexpand( fname, ftype );
  char *dpath;
  char *dir;
  FILE *stream;

  if( path != NULL ) {
    free( path );
    path = NULL;
  }

  /* if file spec contains already a directory don't apply search path */
#ifdef IBMVM
  if( strchr( fspec, '.' ) != strrchr( fspec, '.' ) )
    path = fspec;               /* "file.type.fm" */
#endif
#ifdef UNIX
# if defined(MSDOS) || defined(CERNLIB_WINNT)
  if( strchr( fspec, ':' ) != NULL )
    path = fspec;              /* "drive:file" */
# endif
  if( strpbrk( fspec, SLASHES ) != NULL )
    path = fspec;               /* "dir/file" */
#endif
#ifdef vms
  if( strchr( fspec, ':' ) != NULL || strchr( fspec, ']' ) != NULL )
    path = fspec;               /* "disk:file" or "[dir]file" */
#endif

  if( path != NULL ) {
    if( (stream = fopen( path, "r" )) != NULL
       || (stream = fopen( ku_fcase( path ), "r" )) != NULL ) {
      fclose( stream );
#ifdef DBMALLOC
      malloc_mark( path );
#endif
    }
    else {
      free( path );
      path = NULL;
    }
    return path;
  }

  if( dir_path == NULL ) {
    char *cdir;
#ifdef IBMVM
    cdir = "*";                 /* search all disks */
#define _done_
#endif
#ifdef UNIX
    cdir = ".";
#define _done_
#endif
#ifdef vms
    cdir = " ";   /*    cdir = "[]"; causes trouble for VMS search lists */
#define _done_
#endif
#ifdef _done_
#undef _done_
#else
    cdir = "";
#endif
    dpath = strdup( cdir );
  }
  else {
    dpath = strdup( dir_path );
  }
 
#ifndef WIN32_TEST 
  for( dir = strtok( dpath, sep ); dir != NULL; dir = strtok( NULL, sep ) ) {
    char *p;

#ifdef IBMVM
    path = str3dup( fspec, ".", dir );
#else
    path = strdup( dir );
# ifdef UNIX
    if( strchr( SLASHES, dir[strlen(dir)-1] ) == NULL )
      path = mstrccat( path, SLASHES[0], 1 );
# endif
    path = mstrcat( path, fspec );
#endif

    /* in case search path contains "~" or "$var" */
    p = fexpand( path, NULL );
    free( path );
    path = p;

    if( (stream = fopen( path, "r" )) != NULL
       || (stream = fopen( ku_fcase( path ), "r" )) != NULL ) {
      fclose( stream );
#ifdef DBMALLOC
      malloc_mark( path );
#endif
      break;
    }
    else {
      free( path );
      path = NULL;
    }
  }
#else 
  {
    char *lookup = fexpand(dpath,NULL);
    char *buffer;
    char *env = "__CERN_Search";

    path = str2dup(lookup,fspec); 

/*   Create a temporary environment variable */

     if (!lookup) 
		 buffer = str3dup( env, "=","./" );
     else
     {
		 buffer = str3dup(env,"=",lookup );
         free(lookup);
     }
     _putenv(buffer);
     free(buffer);
  
     _searchenv(fspec, env, path );
 
/*  Check access  */
 
     if (access(path,00) == -1)
     {
       free(path);
       path = NULL;
     }
   }
#endif /* WIN32 */
  free( fspec );
  free( dpath );

  return path;
}


/*
 * like system() but allow to give shell name
 */
int shsystem( const char *host_shell,
             const char *line )
{
#ifdef SIGNAL_POSIX
  /*
   * Because we have to tweak the signal catching before spawning off the
   * shell we only bother to implement it if the system provides Posix
   * compliant signal handling.
   */
  struct sigaction act;
  struct sigaction save_intr;
  struct sigaction save_quit;
  sigset_t mask;
  sigset_t save_mask;
  int save_errno;
  int pid;
  int status;

  /* ignore SIGINT and SIGQUIT */

  sigemptyset( &act.sa_mask );
  act.sa_flags = 0;
  act.sa_handler = SIG_IGN;

  sigaction( SIGINT, &act, &save_intr );
  sigaction( SIGQUIT, &act, &save_quit );

  /* block SIGCHLD */

  sigemptyset( &mask );
  sigaddset( &mask, SIGCHLD );
  sigprocmask( SIG_BLOCK, &mask, &save_mask );

  if( (pid = vfork()) == 0 ) {
    /* child: restore signal handling before starting shell */

    sigaction( SIGINT, &save_intr, NULL );
    sigaction( SIGQUIT, &save_quit, NULL );
    sigprocmask( SIG_SETMASK, &save_mask, NULL );

    execl( host_shell, host_shell, "-c", line, NULL );
/* if execl returns , this is an error */
    printf( " *** Cannot exec host_shell (%s) ***\n", host_shell );
    exit(1);
  }
  else if( pid > 0 ) {
    /* parent: wait for child */
    if( waitpid( pid, &status, 0 ) != pid )
      status = -1;
  }
  else {
    /* fork failed */
    status = -1;
  }

  /* restore original signal handling */

  save_errno = errno;
  sigaction( SIGINT, &save_intr, NULL );
  sigaction( SIGQUIT, &save_quit, NULL );
  sigprocmask( SIG_SETMASK, &save_mask, NULL );
  errno = save_errno;

  return status;
#else
  return system( line );
#endif
}


/*
 * action routine for /KUIP/SET_SHOW/HOST_SHELL
 */
int kxhostshell()
{
  char *shell_name = ku_gete();

  if( strcmp( shell_name, "?" ) == 0 ) {
    ku_alfa();
    printf( " Current host shell: %s\n", kc_value.set_host_shell );
  }
  else {
    if( kc_value.set_host_shell != NULL )
      free( kc_value.set_host_shell );
    kc_value.set_host_shell = strdup( shell_name );

#ifdef DBMALLOC
    malloc_mark( kc_value.set_host_shell );
#endif
  }
  return 0;
}


/*
 * action routine for /KUIP/SHELL
 */
int kxshell()
{
  char *shell_cmd = ku_gete();
  int status = 0;

#ifdef APOLLO
  short connc = 4;
  stream_$id_t connv[4];
  pgm_$proc proc;
  status_$t istat;

  connv[0] = ios_$stdin;
  connv[1] = ios_$stdout;
# ifdef _BFMT__COFF             /* If sr10 or later */
  connv[2] = ios_$errout;
  connv[3] = ios_$stderr;
# else
  connv[2] = ios_$errin;
  connv[3] = ios_$errout;
# endif

  proc.p = NULL;
#endif

  ku_alfa();
  if( shell_cmd[0] == '\0' ) {
    if( kc_flags.in_motif ) {
      printf( " Cannot run shell from Motif mode\n" );
      return -1;
    }
    else {

#ifdef IBMVM
      printf( " Type 'return' to return to the command processor\n" );
      system( "SUBSET" );
#define _done_
#endif

#ifdef IBMMVS
      F77_CALL_C(Kmvssh," ",1);
#define _done_
#endif

#ifdef vms
      $DESCRIPTOR(cmd_string,"$ RETURN:==LOGOFF");
      $DESCRIPTOR(sys_command,"SYS$COMMAND");
      printf( " Type 'return' to return to the command processor\n" );
      lib$spawn( &cmd_string, &sys_command, &sys_command );
#define _done_
#endif

#ifdef APOLLO
      if( strcmp( kc_value.set_host_shell, "/com/sh" ) == 0 ) {
        /*
         * Use pgm_$invoke() rather than system() because /com/sh uses
         * a fourth standard stream.  Memory can be overwritten if that
         * stream is bogus.
         * Reported by Robert Franchisseur (robert@m23.limsi.fr).
         */
        short argc = 2;
        pgm_$arg argv[2];
        pgm_$arg_ptr argv_ptr[2];
        int i;

        strcpy( argv[0].chars, "/com/sh" );
        strcpy( argv[1].chars, "-start" );
        for( i = 0; i < argc; i++ ) {
          argv[i].len = strlen( argv[i].chars );
          argv_ptr[i] = &argv[i];
        }

        printf( " Type 'return' to return to the command processor\n" );
        pgm_$invoke( argv[0].chars, argv[0].len, argc, argv_ptr,
                    connc, connv, pgm_$wait, &proc, &istat );
        if( istat.all != status_$ok ) {
          status = istat.all;
          error_$print( istat );
        }
      }
      else {
        printf( " Type 'exit' to return to the command processor\n" );
        if( system( kc_value.set_host_shell ) == -1 )
          perror( kc_value.set_host_shell );
      }
#define _done_
#endif

#ifdef _done_
#undef _done_
#else
      printf( " Type 'exit' to return to the command processor\n" );
# ifdef MSDOS
      text_mode__();
# endif
      if( system( kc_value.set_host_shell ) == -1 )
        perror( kc_value.set_host_shell );
#endif
    }
  }
  else {

#ifdef IBMVM
    char *cmd_string = strdup( shell_cmd );
    char *exec_name = strtok( cmd_string, " " );
    char *exec_file = fsearch( exec_name, "EXEC", "*" );

    strcpy( cmd_string, shell_cmd );
    if( exec_file != NULL ) {
      /* if a file "cmd EXEC *" exists prepend "EXEC" to execute it */
      char *p = str2dup( "EXEC ", cmd_string );
      free( cmd_string );
      cmd_string = p;
    }
    status = system( cmd_string );
    if( status >= 0 && status <= 4 )
      status = 0;               /* ignore informational messages */
    else
      printf( " Command '%s' returned status code %d\n", cmd_string, status );
    free( cmd_string );
#define _done_
#endif

#ifdef IBMMVS
    F77_CALL_C(Kmvssh,shell_cmd,strlen(shell_cmd));
#define _done_
#endif

#ifdef vms
    /*
     * Write command line into file because lib$spawn doesn't do
     * symbol substitution.
     */
    FILE *stream;
    char *cmd_string;
    struct dsc$descriptor_s cmd_dsc;

    if ( strchr(shell_cmd,'\'') == NULL ) {
        var_descriptor( cmd_dsc, shell_cmd );
        status = lib$spawn( &cmd_dsc );
    } else {
        stream = fopen( kc_flags.appl_file, "w" );
        /* in case the command calls an editor */
        /* fprintf( stream, "$ ASSIGN/USER SYS$COMMAND SYS$INPUT\n" ); */
        fprintf( stream, "$ %s\n", shell_cmd );
        fclose( stream );

        var_descriptor( cmd_dsc, kc_flags.appl_file );
        status = lib$spawn( &cmd_dsc );
        remove( kc_flags.appl_file );
    }

    if( status & 1 )            /* lowest bit flags success */
      status = 0;
    if( status != 0 )
      printf( " Command '%s' returned status code %d\n", shell_cmd, status );
#define _done_
#endif

#ifdef APOLLO
    if( strcmp( kc_value.set_host_shell, "/com/sh" ) == 0 ) {
      short argc = 4;
      struct {
        short len;
        char chars[1024];       /* default size of pgm_$arg is only 128 */
      } argv[4];
      pgm_$arg_ptr argv_ptr[4];
      int i;

      strcpy( argv[0].chars, "/com/sh" );
      strcpy( argv[1].chars, "-start" );
      strcpy( argv[2].chars, "-c" );
      strcpy( argv[3].chars, shell_cmd );
      for( i = 0; i < argc; i++ ) {
        argv[i].len = strlen( argv[i].chars );
        argv_ptr[i] = (pgm_$arg*)&argv[i];
      }

      pgm_$invoke( argv[0].chars, argv[0].len, argc, argv_ptr,
                  connc, connv, pgm_$wait, &proc, &istat );
      if( istat.all != status_$ok ) {
        status = istat.all;
        error_$print( istat );
      }
    }
    else {
      status = shsystem( kc_value.set_host_shell, shell_cmd );
      if( status == -1 )
        perror( shell_cmd );
      else if( status != 0 )
        printf( " *** %s returned exit code %d\n", shell_cmd, status );
    }
#define _done_
#endif

#ifdef _done_
#undef _done_
#else
# ifdef MSDOS
    text_mode__();
# endif
    status = shsystem( kc_value.set_host_shell, shell_cmd );
    if( status == -1 ) {
      perror( shell_cmd );
    }
    else if( status != 0 ) {
# ifdef WIFEXITED
      if( WIFEXITED( status ) ) {
        status = WEXITSTATUS( status );
        printf( " *** %s returned exit code %d\n", shell_cmd, status );
      }
      else
# endif
        printf( " *** %s returned status code %d\n", shell_cmd, status );
    }
#endif
  }
  return status;
}


/*
 * action routine for /KUIP/SET/LCDIR (set working directory)
 */
int kxlcdir()
{
  int status = 0;

#if defined(UNIX) || defined(vms)

  char *dir = ku_gets();
  char  cwd[256];

  ku_alfa();

  if( dir[0] != '\0' ) {
    char *path;

    if( strcmp( dir, "." ) == 0 ) {
      path = strdup( kc_flags.init_wdir );
    }
#ifdef vms
    else if( strcmp( dir, "~" ) == 0 ) {
      path = strdup( getenv( "HOME" ) );
    }
#endif
    else {
      path = fexpand( dir, NULL ); /* for ~ expansion */
    }

    if( chdir( path ) == -1 ) {
      perror( " *** LCDIR" );
      free( path );
      return -1;
    }

    free( path );
  }

  if( getcwd( cwd, (sizeof cwd) ) == NULL ) {
    perror( " *** LCDIR" );
    status = -1;
  }
  else {
    printf( " Local directory is %s\n", cwd );

    /* set system current working directory in kxterm */
    if (kc_flags.use_kxterm) {
       char *s = malloc(strlen(cwd) + 6);
       sprintf(s, "lcd(%s)", cwd);
       send_single_kxterm_cmd(s);
       free(s);
    }
  }

#else

  ku_alfa();
  printf( " LCD: not implemented for this system\n" );

#endif

  return status;
}


/*
 * action routine for /KUIP/MESSAGE
 */
int kxmessage()
{
  char *msg = ku_gete();

  ku_alfa();
  printf( " %s\n", msg );
  return 0;
}

/*
 * action routine for /KUIP/FMESSAGE
 */
int kxfmessage()
{
  char *msg = ku_gets();
  char *fname = ku_getf();
  FILE *stream = NULL;

  if( fname[0] != '\0' ) {
    fname = fexpand( fname, NULL );
    stream = fopen( fname, "a" );
    free( fname );
  }

  if( stream == NULL ) {
    ku_alfa();
    printf( " %s\n", msg );
  } else {
    fputs( msg, stream );
    fputs( "\n", stream );
    fclose( stream );
  }

  return 0;
}


/*
 * action routine for /KUIP/WAIT
 */
int kxwait()
{
  char *msg = ku_gets();
#ifdef IBMMVS
  int seconds = 0;
#else
  int seconds = ku_getr();

  if( seconds > 0 ) {

    if( msg[0] != '\0' ) {
      ku_alfa();
      printf( " %s\n", msg );
    }

# ifdef IBMVM
#   pragma linkage(SLEEPF,FORTRAN)
    SLEEPF( &seconds );
# elif !defined(CERNLIB_WINNT)
    sleep( seconds );
# else
    Sleep( seconds*1000 );
# endif
  }
#endif

  if( seconds <= 0 ) {
    const char *answer;
    if( msg[0] == '\0' )
      msg = "Type <CR> to continue or Q to quit";

    answer = ku_proc( msg, NULL );
    if( answer == NULL || *answer == 'Q' ) {
      kc_flags.quit_macro = 1;
      return KUMAC_UNWIND;
    }
  }

  return 0;
}


/*
 * action routine for /KUIP/IDLE
 */
int kxidle()
{
  int seconds = ku_geti();
  char *cmd = ku_gete();

  if( cmd[0] != '\0' ) {
    if( kc_value.idle_cmd != NULL )
      free( kc_value.idle_cmd );
    kc_value.idle_cmd = strdup( cmd );
  }
  else if( kc_value.idle_cmd == NULL )
    seconds = 0;

  kc_value.idle_time = seconds;
  return 0;
}


/*
 * return the user name
 */
char *k_userid()
{
  char *user_name;

#if defined(UNIX) && !defined(MSDOS)
  struct passwd *pwd = getpwuid( getuid() );
  user_name = pwd->pw_name;
#define _done_
#endif

#ifdef vms
  user_name = cuserid( NULL );
#define _done_
#endif

#ifdef _done_
#undef _done_
#else
  user_name = NULL;
#endif

  if( user_name == NULL )
    user_name = "unknown";

  return user_name;
}

#ifndef IBM370                  /* for IBM it's a Fortran routine */
void F77_ENTRY_C(Kuserid,chuser)
/* { */
  fstrset( chuser, len_chuser, k_userid() );
}
#endif


