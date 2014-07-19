/*
 * $Id: kedit.c,v 1.3 2005/04/18 15:23:10 mclareni Exp $
 *
 * $Log: kedit.c,v $
 * Revision 1.3  2005/04/18 15:23:10  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.2  1997/03/17 16:54:57  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:32:45  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/17 19/08/94  12.53.04  by  Alfred Nathaniel*/
/*-- Author :    Alfred Nathaniel   06/11/92*/
/* kedit.c: file editing */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"

static char *editor_cmd  = NULL; /* command which envoked edit server */
static char *editor_file = NULL; /* file which was edited */
static char  editor_tmp[22];     /* file to use as a lock file */


/*
 * set the user routine called when edit server exits
 */
void F77_ENTRY_E(Kueusr,fun)
/* { */
  kjmpaddr.user_edit_F = fun;
}


/*
 * test if executable exists
 */
LOGICAL F77_ENTRY_C(Kuqexe,chname)
/* { */
  char *name = fstrdup( chname, len_chname );
  int exists = (ku_qexe( name ) != NULL);
  free( name );
  return ku_true( exists );
}


char *ku_qexe( const char *name )
{
  char *path;
  char *dirpath;

#ifdef IBMVM
  dirpath = NULL;
  path = fsearch( name, "MODULE", "*" );
#define _done_
#endif

#ifdef UNIX
  dirpath = str0dup( getenv( "PATH" ) );
# if defined(MSDOS) || defined(CERNLIB_WINNT)
  path = fsearch( name, "EXE", dirpath );
# else
  path = fsearch( name, NULL, dirpath );
# endif
#define _done_
#endif

#ifdef vms
  dirpath = str0dup( getenv( "KUIPPATH" ) );
  path = fsearch( name, "EXE", dirpath );
#define _done_
#endif

#ifdef _done_
#undef _done_
#else
  dirpath = NULL;
  path = NULL;
#endif

  if( dirpath != NULL )
    free( dirpath );

  return path;
}


/*
 * return flag whether edit server will be used
 */
LOGICAL F77_ENTRY(Kuqsvr)
/* { */
  return ku_true( kc_flags.use_server );
}


/*
 * envoke editor on file
 */

void F77_ENTRY_CI(Kuedit,chfile,status)
/* {  */
  char *file = fstrdup( chfile, len_chfile );

  *status = ku_edit( file, 0 );
  free( file );
}


/*
 * edit file using the server if possible
 */
void F77_ENTRY_CI(Kuesvr,chfile,status)
/* { */
  if( kc_flags.use_server ) {
    char *file = fstrdup( chfile, len_chfile );
    *status = ku_edit( file, 1 );
    if( *status == 1 )
      *status = 0;
    free( file );
  }
  else
    *status = 1;
}


#ifdef vms
/*
 * call if a nowait spawned editor terminates
 */
static
void edit_server_ast( cmd_line )
     char *cmd_line;
{
  if( kc_flags.editor_cbuf == NULL ) {
    kc_flags.editor_cbuf = mstrcat( cmd_line, "\n" );
  }
  else {
    kc_flags.editor_cbuf = mstr2cat( kc_flags.editor_cbuf, cmd_line, "\n" );
    free( cmd_line );           /* allocated in ku_edit */
  }

  kc_flags.editor_exit = 1;
}
#endif


/*
 * edit a file
 * returns 0 if edit successful
 * returns 1 if file unchanged or sent to edit server
 * returns 2 on error
 */
int ku_edit( const char *file,
            int use_server )
{
  char *path = ku_home( file, "kumac" );
  int status = 2;
  char *line;
  char *p;

  if( use_server && kc_flags.use_server ) {
#if defined(UNIX) && !defined(CERNLIB_WINNT)
    int fid;
    
    /* Try to securely create the temporary file for the edit server.
     * Has to be done in the parent process because the server has no
     * way of passing back the name of a created temp file. */
    strcpy( editor_tmp, "/usr/tmp/kuesvrXXXXXX" );
    fid = mkstemp( editor_tmp );
    if (fid == -1) {
      strcpy( editor_tmp, "/tmp/kuesvrXXXXXX" );
      fid = mkstemp( editor_tmp );
      if (fid == -1) {
	perror( "mkstemp" );
	return status;
      }
    }
    close( fid );
    
    line = strdup( "kuesvr -p " );
    line = mstricat( line, getpid() );
    line = mstr2cat( line, " -t ", editor_tmp );
    line = mstr4cat( line, " -c ", ku_path(), " -e '",
                    kc_value.set_host_editor );
    if( (p = strrchr( line, '&' )) != NULL )
      *p = ' ';                 /* remove & from editor command */
    line = mstr3cat( line, "' ", path, " &" );
    if( shsystem( kc_value.set_host_shell, line ) == 0 )
      status = 1;
    free( line );
#endif /* UNIX */

#ifdef vms
    unsigned int flags = CLI$M_NOWAIT;
    int istat;
    char *cmd_string = str4dup( "$ ", kc_value.set_host_editor, " ", path );
    struct dsc$descriptor_s cmd_dsc;
    var_descriptor( cmd_dsc, cmd_string );

    line = str3dup( path, " ", ku_path() ); /* free'd in edit_server_ast */

    if( kc_flags.in_motif ) {
      /*
       * In Motif mode we cannot spawn the "CREATE/TERM/WAIT EDIT file" command
       * directly because SYS$INPUT will be assigned to the mailbox.
       * Instead we have to envoke a command script which, for the first
       * command has still assigned the mailbox, but then gets SYS$INPUT
       * assigned to the real terminal window ??????????????????????????
       */
      FILE *script = fopen( "sys$login:kuipedit.com", "w" );
      char fname[256];
      fgetname( script, fname );
      fprintf( script, "$ show logical sys$input\n" );
      fprintf( script, "%s\n", cmd_string );
      fprintf( script, "$ delete %s\n", fname );
      fclose( script );
      free( cmd_string );
      cmd_string = str2dup( "$ @", fname );
      var_descriptor( cmd_dsc, cmd_string );
    }

    istat = lib$spawn( &cmd_dsc, NULL, NULL, &flags,
                      NULL,     /* process name */
                      NULL,     /* process id */
                      NULL,     /* completion status */
                      NULL,     /* completion efn */
                      edit_server_ast,
                      line );
    if( istat == SS$_NORMAL )
      status = 1;

    free( cmd_string );
#endif /* vms */
  }
  else {
    KmTimeStamp stamp1;
    KmTimeStamp stamp2;

#ifdef APOLLO
    if( strcasecmp( kc_value.set_host_editor, "DM" ) == 0 ) {
      pad_$window_desc_t edit_pad;
      stream_$id_t stream;
      status_$t istat;
      int writable = 1;

      edit_pad.top = kc_window.edit_pad.top;
      edit_pad.left = kc_window.edit_pad.left;
      edit_pad.width = kc_window.edit_pad.width;
      edit_pad.height = kc_window.edit_pad.height;

      pad_$create_window( path, strlen( path ), pad_$edit, 1,
                         edit_pad, &stream, &istat );
      if( istat.all == ios_$insufficient_rights ) {
        pad_$create_window( path, strlen( path ), pad_$read_edit, 1,
                           edit_pad, &stream, &istat );
        writable = 0;
      }
      if( istat.all == status_$ok ) {
        if( writable )
          pad_$edit_wait( stream, &istat );
        if( istat.all == status_$ok ) {
          status = 0;
        }
        else if( istat.all == pad_$edit_quit ) {
          status = 1;
        }
        else {
          error_$print( istat );
        }
        stream_$close( stream, &istat );
      }
      if( istat.all != status_$ok ) {
        status = 2;
        error_$print( istat );
      }
      free( path );
      return status;
    }
#endif /* APOLLO */

#ifdef vms
    int istat;
    char *cmd_string;
    struct dsc$descriptor_s cmd_dsc;

    if( strncasecmp( kc_value.set_host_editor, "EDT", 3 ) == 0
       || strncasecmp( kc_value.set_host_editor, "TPU", 3 ) == 0 ) {
      /* invoke the callable EDT or TPU */

      /* these functions are not prototyped in any system include file ? */
      extern int EDT$EDIT( struct dsc$descriptor_s* );
      extern int TPU$TPU( struct dsc$descriptor_s* );

      get_stamp( path, &stamp1 );
      if( strcasecmp( kc_value.set_host_editor, "EDT" ) == 0 ) {
        var_descriptor( cmd_dsc, path );
        istat = EDT$EDIT( &cmd_dsc );
      }
      else {
        cmd_string = str3dup( kc_value.set_host_editor, " ", path );
        var_descriptor( cmd_dsc, cmd_string );
        istat = TPU$TPU( &cmd_dsc );
        free( cmd_string );
      }
      get_stamp( path, &stamp2 );
      status = same_stamp( &stamp1, &stamp2 );
      free( path );
      return status;
    }
#endif /* vms */

#ifdef IBMMVS
    INTEGER istat = 0;
    strupper( path );
    F77_CALL_Cx(Kmvsed,path,strlen(path),&istat);
    status = istat;
#else /* IBMMVS */

 
    get_stamp( path, &stamp1 );
 
# ifdef IBMVM

    line = str2dup( "/KUIP/SHELL '", kc_value.set_host_editor );
    if( (p = strchr( line, '(' )) != NULL )
      *p = '\0';                /* remove "( xedit-options" */
    line = mstr2cat( line, " ", path );
    while( (p = strchr( line, '.' )) != NULL )
      *p = ' ';                 /* change "fn.ft.fm" to "fn ft fm" */
    if( (p = strchr( kc_value.set_host_editor, '(' )) != NULL )
      line = mstr2cat( line, " ", p );  /* append "( xedit-options" */
    line = mstrcat( line, "'" );

# else /* IBMVM */

    line = str5dup( "/KUIP/SHELL '", kc_value.set_host_editor, " ",
                   path, "'" );
    if( (p = strchr( line, '&' )) != NULL )
      *p = ' ';                 /* remove & from Unix editor command */

# endif /* IBMVM */

    status = ku_exel( line );

# if defined(UNIX) && !defined(CERNLIB_WINNT)
    /* "emacs -nw" sometime returns a non-zero exit code ?!? */
    if( status == 130
       && strstr( kc_value.set_host_editor, "emacs" ) != NULL
       && strstr( kc_value.set_host_editor, "-nw" ) != NULL ) {
      printf( " *** Bogus non-zero exit code ignored\n" );
      status = 0;
    }
# endif /* UNIX */

    if( status == 0 ) {
 
      get_stamp( path, &stamp2 );
      status = same_stamp( &stamp1, &stamp2 );
 
    }
    else {
      status = 2;
    }
    free( line );
#endif /* IBMMVS */
  }
  free( path );
  return status;
}


void ku_shut()
{
#ifdef USE_EDIT_SERVER
  if( kc_flags.use_server )
    remove( editor_tmp );
#endif
}


void F77_ENTRY_C(Kupad,chfile)
/* { */
  char *file = fstrdup( chfile, len_chfile );
  ku_pad( file, 0 );
  free( file );
}


void ku_pad( const char *fname,
            int is_temp )       /* flag if file should be removed afterwards */
{
  char *file = strdup( fname );
  char *cmd;
#ifdef IBMVM
  char *profile = "KUHELP.XEDIT.A";
  FILE *stream = fopen( profile, "w" );
#endif
#ifdef IBMMVS
  INTEGER iluno = is_temp ? kc_flags.uhlp_luno : 0;
#endif
  char *p;

  ku_fcase( file );

#ifdef APOLLO
  if( strcasecmp( kc_value.set_host_pager, "DM" ) == 0 ) {
    pad_$window_desc_t help_pad;
    stream_$id_t stream;
    status_$t istat;
    short fontw;
    short fonth;
    short fnlen;
    FILE *input = fopen( file, "r" );
    char line[1024];
    int height = 0;

    if( input == NULL ) {
      printf( " *** File %s not found\n", file );
      return;
    }

    pad_$inq_font( stream_$stdout, &fontw, &fonth, NULL, 0, &fnlen, &istat );
    if( istat.all != status_$ok )
      fonth = 15;

    while( fgets( line, (sizeof line), input ) != NULL )
      height += fonth;          /* count number of lines in file */
    fclose( input );

    if( kc_window.help_pad.top + height > 980 )
      kc_window.help_pad.height = 980 - kc_window.help_pad.top;
    else
      kc_window.help_pad.height = height;

    if( kc_window.help_pad.left + kc_window.help_pad.width > 1280 )
      kc_window.help_pad.width = 1280 - kc_window.help_pad.left;

    help_pad.top = kc_window.help_pad.top;
    help_pad.left = kc_window.help_pad.left;
    help_pad.width = kc_window.help_pad.width;
    help_pad.height = kc_window.help_pad.height;

    pad_$create_window( file, strlen(file), pad_$read_edit, 1,
                       help_pad, &stream, &istat );
    if( istat.all != status_$ok ) {
      error_$print( istat );
      return;
    }
    if( (kc_flags.style & KmSTYLE_G) != 0 )
      pad_$edit_wait( stream, &istat );
    stream_$close( stream, &istat );

    if( kc_window.shift_max > 1 ) {
      kc_window.help_pad.top  += kc_window.voffset * kc_window.shift_dir;
      kc_window.help_pad.left += kc_window.hoffset * kc_window.shift_dir;
      kc_window.shift_now += kc_window.shift_dir;
      if( kc_window.shift_now < 1
         || kc_window.shift_now >= kc_window.shift_max ) {
        kc_window.shift_dir = -kc_window.shift_dir;
        kc_window.shift_now += kc_window.shift_dir;
      }
    }
    return;
  }
#endif

#ifdef UNIX
# if defined(MSDOS) || defined(CERNLIB_WINNT)
  if( strcmp( kc_value.set_host_pager, "more" ) == 0 )
    cmd = str3dup( "type ", file, " | more" );
  else
    cmd = str3dup( kc_value.set_host_pager, " ", file );
  text_mode__();
  system( cmd );
  free( cmd );
  if( is_temp )
    remove( file );
# else
  cmd = str4dup( "(", kc_value.set_host_pager, " ", file );
  if( is_temp )
    cmd = mstr4cat( cmd, "; /bin/cp /dev/null ", file, "; /bin/rm ", file );
  cmd = mstrcat( cmd, ")" );
  if( (p = strchr( cmd, '&' )) != NULL ) {
    /* move ampersand to the end */
    *p = ' ';
    cmd = mstrcat( cmd, " &" );
  }
  shsystem( kc_value.set_host_shell, cmd );
  free( cmd );
# endif
#endif

#ifdef vms
  cmd = str3dup( kc_value.set_host_pager, " ", file );
  system( cmd );
  free( cmd );
  if( is_temp )
    remove( file );
#endif

#ifdef IBMVM
  if( stream == NULL ) {
    printf( " *** Cannot open %s\n", profile );
    return;
  }
  fprintf( stream, "\
SET PREFIX OFF\n\
SET SCALE ON 2\n\
SET VERIFY ON 1 *\n\
SET CURLINE ON 3\n" );
  fclose( stream );
                                /* NOLOCK needed for SFS */
  cmd = str3dup( "XEDIT ", file, " ( NOLOCK PROF KUHELP" );
  while( (p = strchr( cmd, '.' )) != NULL )
    *p = ' ';
  system( cmd );
  free( cmd );
  remove( profile );
#endif

#ifdef IBMMVS
  F77_CALL_Cx(Kmvspg,file,strlen(file),&iluno);
  if( is_temp )
    remove( file );
#endif

  free( file );
}


/*
 * return file and command name to the user edit routine
 */
void F77_ENTRY_CC(Ksvpar,file,cmd)
/* { */
  fstrset( file, len_file, editor_file );
  fstrset( cmd, len_cmd, editor_cmd );
}


/*
 * scan the description file written by the edit server
 */
void check_edit_server()
{
#ifdef USE_EDIT_SERVER

  if( kjmpaddr.user_edit_F != NULL && kc_flags.editor_exit != 0 ) {
    char *lock_file = editor_tmp;
    int fid;
    int len;
    int nbytes;
    char *s;

    kc_flags.editor_exit = 0;

#ifdef UNIX

    /* /tmp cannot be locked on Sun if allocated on swap space */
    fid = open( lock_file, O_RDONLY );
    if( fid == -1 ) {
      perror( "kugsvr: open tempfile read-only" );
      return;
    }
    else {
      close( fid );
      fid = open( lock_file, O_RDWR );
      if( fid == -1 ) {
        perror( "kugsvr: open tempfile read-write" );
        return;
      }
    }

    /* lock the file */
    if( lockf( fid, F_LOCK, (off_t)0 ) == -1) {
      perror( "kugsvr: lock file" );
      close( fid );
      return;
    }

    /* read the complete file */
    kc_flags.editor_cbuf = malloc( BUFSIZ + 1 );
    kc_flags.editor_cbuf[0] = '\0';
    len = 0;
    while( (nbytes = read( fid, &kc_flags.editor_cbuf[len], BUFSIZ )) > 0 ) {
      /* null terminate the string at the end of the bytes read */
      len += nbytes;
      kc_flags.editor_cbuf[len] = '\0';
      kc_flags.editor_cbuf = realloc( kc_flags.editor_cbuf,
                                 strlen( kc_flags.editor_cbuf ) + BUFSIZ + 1 );
    }

    if( ftruncate( fid, (off_t)0 ) == -1 )
      perror( "kugsvr: ftruncate" );

    if( lockf( fid, F_ULOCK, (off_t)0 ) == -1)
      perror("kugsvr: un-lockf");

    close(fid);

#endif

    /*
     * For VMS the buffer is filled in edit_server_ast()
     */

    s = kc_flags.editor_cbuf;
    while( *s != '\0' ) {
      editor_file = s;
      s = strchr( s, ' ' );
      if( s == NULL )
        break;
      *s++ = '\0';

      editor_cmd = s;
      s = strchr( s, '\n' );
      if( s == NULL )
        break;
      *s++ = '\0';

      if( strcmp( editor_cmd, "/KUIP/EDIT" ) != 0 ) {
        /* call user routine for all non-standard edit commands */
        (*kjmpaddr.user_edit_F)();
      }
    }
    free( kc_flags.editor_cbuf );
    kc_flags.editor_cbuf = NULL;
    editor_file = NULL;
    editor_cmd = NULL;
  }
#endif /* USE_EDIT_SERVER */
}


/*
 * action routine for /KUIP/SET_SHOW/HOST_EDITOR
 */
int kxhosteditor()
{
  char *editor = ku_gets();

  if( strcmp( editor, "?" ) == 0 ) {
    printf( " Current host editor is '%s'\n", kc_value.set_host_editor );
  }
  else if( strcasecmp( editor, "WINDOW" ) == 0 ) {
    kc_window.edit_pad.top = ku_geti();
    kc_window.edit_pad.left = ku_geti();
    kc_window.edit_pad.width = ku_geti();
    kc_window.edit_pad.height = ku_geti();
  }
  else if( strcasecmp( editor, "PAD" ) == 0 ) {
    kc_window.help_pad.top = ku_geti();
    kc_window.help_pad.left = ku_geti();
    kc_window.help_pad.width = ku_geti();
    kc_window.help_pad.height = ku_geti();
    kc_window.hoffset = ku_geti();
    kc_window.voffset = ku_geti();
    kc_window.shift_max = ku_geti();
    kc_window.shift_now = 1;
    kc_window.shift_dir = 1;
  }
  else {
    if( kc_value.set_host_editor != NULL )
      free( kc_value.set_host_editor );
    kc_value.set_host_editor = strdup( editor );

    kc_flags.use_server = 0;
#ifdef USE_EDIT_SERVER

# ifdef UNIX
    if( strchr( kc_value.set_host_editor, '&' ) != NULL ) {
      /* must be a workstation and kuesvr in search path to use server */
      kc_flags.use_server =
        (getenv( "DISPLAY" ) != NULL) && (ku_qexe( "kuesvr" ) != NULL);

        /* vi must be wrapped into xterm */
      if( kc_flags.use_server
         && ( strncmp( kc_value.set_host_editor, "vi ", 3 ) == 0
             || strcmp( kc_value.set_host_editor, "vi&" ) == 0 ) ) {
        char *p = kc_value.set_host_editor;
        kc_value.set_host_editor = str2dup( "xterm -e ", p );
        free( p );
      }
    }
# endif

# ifdef vms
    strupper( kc_value.set_host_editor );

    if( kc_flags.in_motif ) {
      /*
       * We have to spawn a nowait process because otherwise we get stuck
       * when pressing the right mouse button.
       * Instead of the callable TPU we have to use the EDIT/TPU command
       * and create a terminal window if not using the TPU Motif driver.
       */

      if( strncmp( kc_value.set_host_editor, "EDT", 3 ) == 0
         || strncmp( kc_value.set_host_editor, "TPU", 3 ) == 0 ) {

        char *p = kc_value.set_host_editor;
        kc_value.set_host_editor = str2dup( "EDIT/", p );
        free( p );

        if( strstr( kc_value.set_host_editor, "=DECW" ) == NULL
           && strstr( kc_value.set_host_editor, "=MOTIF" ) == NULL ) {

          p = kc_value.set_host_editor;
          kc_value.set_host_editor = str2dup( "CREATE/TERM/WAIT ", p );
          free( p );
        }
      }
    }

    if( strncmp( kc_value.set_host_editor, "CRE", 3 ) == 0
       || strstr( kc_value.set_host_editor, "=DECW" ) != NULL
       || strstr( kc_value.set_host_editor, "=MOTIF" ) != NULL ) {

      kc_flags.use_server = 1;
    }
# endif

#endif

#ifdef DBMALLOC
    malloc_mark( kc_value.set_host_editor );
#endif
  }
  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/HOST_PSVIEWER
 */
int kxhostpsviewer()
{
  char *psviewer = ku_gets();

  if( strcmp( psviewer, "?" ) == 0 ) {
    printf( " Current PostScript Viewer  is '%s'\n",
            kc_value.set_host_psviewer );
  }
  else {
    if( kc_value.set_host_psviewer != NULL )
      free( kc_value.set_host_psviewer );
    kc_value.set_host_psviewer = strdup( psviewer );

#ifdef DBMALLOC
    malloc_mark( kc_value.set_host_psviewer );
#endif
  }
  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/HOST_PAGER
 */
int kxhostpager()
{
  char *pager = ku_gets();

  if( strcmp( pager, "?" ) == 0 ) {
    printf( " Current host pager is '%s'\n", kc_value.set_host_pager );
  }
  else {
    if( kc_value.set_host_pager != NULL )
      free( kc_value.set_host_pager );
    kc_value.set_host_pager = strdup( pager );

#ifdef DBMALLOC
    malloc_mark( kc_value.set_host_pager );
#endif
  }
  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/HOST_PRINTER
 */
int kxhostprinter()
{
  char *cmd = ku_gets();
  char *ext = ku_gets();

  if( strcmp( cmd, "?" ) == 0 ) {
    if( kc_value.set_print_cmds == NULL ) {
      printf( " No host printer command defined.\n" );
    }
    else {
      int i;
      printf( " Filetype | Host printer command\n" );
      printf( " ---------|---------------------\n" );
      for( i = 0; kc_value.set_print_cmds[i] != NULL; i++ ) {
        printf( " %-9s| %s\n",
               kc_value.set_print_exts[i], kc_value.set_print_cmds[i] );
      }
    }
  }
  else if( cmd[0] == '\0' ) {
    /* delete command */
    if( kc_value.set_print_cmds != NULL ) {
      int i;
      for( i = 0; kc_value.set_print_cmds[i] != NULL; i++ ) {
        if( strcmp( kc_value.set_print_exts[i], ext ) == 0 ) {
          free( kc_value.set_print_cmds[i] );
          free( kc_value.set_print_exts[i] );
          do {
            kc_value.set_print_cmds[i] = kc_value.set_print_cmds[i+1];
            kc_value.set_print_exts[i] = kc_value.set_print_exts[i+1];
          } while( kc_value.set_print_cmds[++i] != NULL );
          break;
        }
      }
    }
  }
  else {
    int i;

    cmd = strdup( cmd );
    if( strstr( cmd, "$*" ) == NULL && strstr( cmd, "$-" ) == NULL )
      cmd = mstrcat( cmd, " $*" );

    if( kc_value.set_print_cmds == NULL ) {
      kc_value.set_print_cmds = (char**)malloc( 2 * sizeof(char*) );
      kc_value.set_print_cmds[0] = cmd;
      kc_value.set_print_cmds[1] = NULL;
      kc_value.set_print_exts = (char**)malloc( 2 * sizeof(char*) );
      kc_value.set_print_exts[0] = strdup( ext );
      kc_value.set_print_exts[1] = NULL;

#ifdef DBMALLOC
      malloc_mark( kc_value.set_print_cmds );
      malloc_mark( kc_value.set_print_cmds[0] );
      malloc_mark( kc_value.set_print_exts );
      malloc_mark( kc_value.set_print_exts[0] );
#endif
      return 0;
    }

    for( i = 0; kc_value.set_print_cmds[i] != NULL; i++ ) {
      if( strcasecmp( kc_value.set_print_exts[i], ext ) == 0 ) {
        free( kc_value.set_print_cmds[i] );
        kc_value.set_print_cmds[i] = cmd;

#ifdef DBMALLOC
        malloc_mark( kc_value.set_print_cmds[i] );
#endif
        return 0;
      }
    }
    kc_value.set_print_cmds = (char**)realloc( (char*)kc_value.set_print_cmds,
                                              (i+2) * sizeof(char*) );
    kc_value.set_print_exts = (char**)realloc( (char*)kc_value.set_print_exts,
                                              (i+2) * sizeof(char*) );
    kc_value.set_print_cmds[i+1] = NULL;
    kc_value.set_print_exts[i+1] = NULL;

    while( i > 0 && strlen( kc_value.set_print_exts[i-1] ) < strlen( ext ) ) {
      kc_value.set_print_cmds[i] = kc_value.set_print_cmds[i-1];
      kc_value.set_print_exts[i] = kc_value.set_print_exts[i-1];
      i--;
    }
    kc_value.set_print_cmds[i] = cmd;
    kc_value.set_print_exts[i] = strdup( ext );

#ifdef DBMALLOC
    malloc_mark( kc_value.set_print_cmds );
    malloc_mark( kc_value.set_print_cmds[i] );
    malloc_mark( kc_value.set_print_exts );
    malloc_mark( kc_value.set_print_exts[i] );
#endif
  }
  return 0;
}


/*
 * action routine for /KUIP/PRINT
 */
int kxprint()
{
  if( kc_value.set_print_cmds != NULL ) {
    char *file = ku_gets();
    char *file_end = file + strlen( file );
    FILE *stream = fopen( file, "r" );
    int i;

#ifdef IBMVM
    char *p;
    if( strchr( file, '.' ) != (p = strrchr( file, '.' )) && p != NULL )
      file_end = p;
#endif

#ifdef vms
    char *p;
    if( (p = strchr( file, ';' )) != NULL )
      file_end = p;
#endif

    if( stream == NULL ) {
      printf( " *** KUIP/PRINT: file %s not found\n", file );
      return -1;
    }
    else
      fclose( stream );

    for( i = 0; kc_value.set_print_cmds[i] != NULL; i++ ) {
      int n = strlen( kc_value.set_print_exts[i] );
      if( strncasecmp( file_end - n, kc_value.set_print_exts[i], n ) == 0 ) {
        char *cmd = str3dup( "/KUIP/SHELL '",
                            kc_value.set_print_cmds[i], "'" );
        char *p;
        while( (p = strstr( cmd, "$*" )) != NULL ) {
          char *newcmd = strndup( cmd, p - cmd );
          newcmd = mstr2cat( newcmd, file, p + 2 );
          free( cmd );
          cmd = newcmd;
        }
        while( (p = strstr( cmd, "$-" )) != NULL ) {
          char *newcmd = strndup( cmd, p - cmd );
          newcmd = mstrncat( newcmd, file, file_end - file - n );
          newcmd = mstrcat( newcmd, p + 2 );
          free( cmd );
          cmd = newcmd;
        }
#ifdef IBMVM
        while( (p = strchr( cmd, '.' )) != NULL )
          *p = ' ';
#endif
        ku_exel( cmd );
        free( cmd );
        return 0;
      }
    }
  }
  printf( " *** KUIP/PRINT: no host printer command defined\n" );
  return -1;
}


/*
 * action routine for /KUIP/EDIT
 */
int kxedit()
{
  char *file = ku_getf();
  char *path = fsearch( file, NULL, NULL );

  if( path == NULL )
    path = fsearch( file, "kumac", kc_flags.macro_path );

  if( path == NULL ) {
    /* file not found in search path */

    path = ku_home( file, "kumac" );

    if( kc_flags.macro_path != NULL ) {
      /* if file spec does'nt contain directory use first in search path */
      char *default_dir = strdup( kc_flags.macro_path );
      char *p;
      if( (p = strchr( default_dir, ',' )) != NULL )
        *p = '\0';

#ifdef IBMVM
      if( strchr( path, '.' ) == strrchr( path, '.' ) ) {
        path = mstr2cat( path, ".", default_dir );
      }
#endif

#ifdef vms
      if( strchr( path, ':' ) == NULL && strchr( path, ']' ) == NULL ) {
        p = str2dup( default_dir, path );
        free( path );
        path = p;
      }
#endif

#ifdef UNIX
      if( strpbrk( path, SLASHES ) == NULL ) {
        p = strdup( default_dir );
        if( strchr( SLASHES, p[strlen(p)-1] ) == NULL )
          p = mstrccat( p, SLASHES[0], 1 );
        p = mstrcat( p, path );
        free( path );
        path = p;
      }
#endif

      free( default_dir );
    }

    ku_edit( path, 1 );
    free( path );
  }
  else {
    ku_edit( path, 1 );
  }

  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/COLUMNS
 */
int kxcolumns()
{
  int width = ku_geti();

  if( width < 0 ) {
    char *columns = getenv( "COLUMNS" );
    if( columns != NULL && isdigit( columns[0] ) )
      width = atoi( columns );
    else
      width = 80;
  }

  if( width == 0 )
    printf( " Current number of columns: %d\n", kc_window.term_width );
  else {
    if( width < 40 ) {
      printf( " *** COLUMNS: minimum screen width is 40\n" );
      width = 80;
    }
    sprintf( kc_value.set_columns, "%d", width );
    kc_window.term_width = width;
#ifndef IBM370
    gl_setwidth( width );
#endif
  }
  return 0;
}


/*
 * action routine for /KUIP/MACRO/LIST
 */
int kxmlist()
{
  char *mname = ku_getf();
  char *dpath = kc_flags.macro_path;
  char *dir;

  if( dpath == NULL ) {
    dpath = ".";
#ifdef IBMVM
    dpath = "a";
#endif
#ifdef vms
    dpath = "[]";
#endif
  }
  dpath = strdup( dpath );

  for( dir = strtok( dpath, "," ); dir != NULL; dir = strtok( NULL, "," ) ) {

#ifdef IBMVM
    char *line = str5dup( "LFILE ", mname, "* KUMAC ", dir, " (DATE" );
    system( line );
    free( line );
#endif

#ifdef UNIX
# ifdef MSDOS
#  ifdef WINNT
    char *kumac = "*.kumac";
#  else
    char *kumac = "*.kum";
#  endif
    char *line = str2dup( "dir ", dir );
    if( dir[0] != '\0' && dir[strlen(dir)-1] != '\\' )
      line = mstrcat( line, "\\" );
    line = mstr3cat( line, mname, kumac, " | more" );
    text_mode__();
    system( line );
# else
    char *line = str2dup( "ls -l ", dir );
    if( dir[0] != '\0' && dir[strlen(dir)-1] != '/' )
      line = mstrcat( line, "/" );
    line = mstr2cat( line, mname, "*.kumac" );
    shsystem( kc_value.set_host_shell, line );
# endif
    free( line );
#endif

#ifdef vms
    char *fspec_string = str3dup( dir, mname, "*.kumac" );
    struct dsc$descriptor_s fspec_dsc;
    struct dsc$descriptor_d match_dsc;
    int context = 0;
    int istat;

    var_descriptor( fspec_dsc, fspec_string );

    match_dsc.dsc$b_dtype   = DSC$K_DTYPE_T;
    match_dsc.dsc$b_class   = DSC$K_CLASS_D;
    match_dsc.dsc$w_length  = 0;
    match_dsc.dsc$a_pointer = NULL;

    while( 1 ) {
      istat = lib$find_file( &fspec_dsc, &match_dsc, &context );
      if( istat != RMS$_NORMAL ) {
        break;
      }
      else {
        char *mpath = strndup( match_dsc.dsc$a_pointer,
                              match_dsc.dsc$w_length );
        printf( " %s\n", mpath );
        free( mpath );
      }
    }
    lib$find_file_end( &context );
    free( fspec_string );
#endif

  }
  free( dpath );

  return 0;
}



/*
 * action routine for /KUIP/PSVIEW
 */
int kxpsview()
{
  char *file = ku_getf();
  char *path = fsearch( file, NULL, NULL );
  int status = 0;

  if( path == NULL )
    path = fsearch( file, "ps", NULL );

  if( path != NULL ) {
    /* file is found */

#ifdef vms
    int istat;
    unsigned int flags = CLI$M_NOWAIT;
    char *line = str4dup( "$ ",kc_value.set_host_psviewer, " ", path );
    struct dsc$descriptor_s cmd_dsc;

    var_descriptor( cmd_dsc, line );
    istat = lib$spawn( &cmd_dsc, NULL, NULL, &flags );
    free( line );

#else
# ifdef UNIX

    char *line = str4dup( kc_value.set_host_psviewer, " ", path, " &" );
    shsystem( kc_value.set_host_shell, line );
    free( line );

# else

    printf( " *** PSVIEW not implemented\n" );
    status = -1;

# endif

#endif

    free( path );
  }
  else {
    printf( " *** PSVIEW: file %s not found\n", file );
    status = -1;
  }
  return status;
}


