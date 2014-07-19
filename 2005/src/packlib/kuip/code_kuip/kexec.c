/*
 * $Id: kexec.c,v 1.6 1998/12/17 14:20:00 couet Exp $
 *
 * $Log: kexec.c,v $
 * Revision 1.6  1998/12/17 14:20:00  couet
 * - new routine to delete the temporary vectors created
 *  by $SIGMA. On NT a direct call to kuvdel didn't work
 *
 * Revision 1.5  1997/03/24 11:10:18  gunter
 * Mods for WinNT; Apply F77_ENTRY to several Ku... routines; this
 * was missing from Kutime, Kutim0, Kustat, Kugeti, Kugetr and Kunpar.
 *
 * Revision 1.4  1997/03/11 09:43:40  cremel
 * Bug correction in command WAIT (after answering "Q" to a "WAIT" statement,
 * the ability to issue several HISTOGRAM/PLOT statements on a single line,
 * separated by semicolons, disappears).
 * kexec.c : add a test on the variable "kc_flags.in_macro".
 * kuvers.c : increase version number to 2.07/13
 *
 * Revision 1.3  1996/05/01 12:08:36  gunter
 * Replace several strcpy by memmove
 *
 * Revision 1.2  1996/04/11 09:41:52  cernlib
 * Protect for an empty string in fill_arg_list
 *
 * Revision 1.1.1.1  1996/03/08 15:32:56  mclareni
 * Kuip
 *
 */
/*-- Author :    Alfred Nathaniel   14/08/92*/
/* kexec.c: command execution */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/ksig.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/klink.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"


static KmCommand  *curr_cmd;
static int   next_arg;
static char *list_arg;


static int   call_action( KmCommand* );
static int   exec_multiple_cmds( const char*, int, int(*)() );
static int   exec_single_cmd( const char*, int, int(*)(), int );
static int   exec_template( const char*, const char*, int, int(*)() );
static int   fill_arg_list( KmCommand*, char*, int, int(*)(), int );
static char* find_value( char*, INTEGER*, REAL* );
static int   get_chval( char*, int, char*(*)() );
static char* next_value( const char* );
static const char* next_word( const char* );
static const char* prompt_argument( KmParameter* );
static int   valid_option( KmParameter*, const char*, int );


/*
 * action routine for /MACRO/DEFAULTS
 */
int kxmdefaults()
{
  char *path = ku_gets();
  char *mdef = ku_getc();

  if( strcmp( path, "?" ) == 0 ) {
    ku_alfa();
    if( kc_flags.macro_path == NULL ) {
      printf( " *** No macro search path defined\n" );
    }
    else {
      printf( " Macro search path: %s\n", kc_flags.macro_path );
    }
  }
  else if( strcmp( path, "." ) == 0 ) {
    if( kc_flags.macro_path != NULL ) {
      free( kc_flags.macro_path );
      kc_flags.macro_path = NULL;
    }
    kc_flags.macro_search = KmMACRO_COMMAND;
    kc_value.defaults_path = ".";
  }
  else {
    if( kc_flags.macro_path != NULL )
      free( kc_flags.macro_path );
    kc_flags.macro_path = strdup( path );
    kc_value.defaults_path = kc_flags.macro_path;
    kc_value.defaults_order = "-COMMAND";
  }

  if( strcmp( mdef, "A" ) == 0 || strcmp( mdef, "AUTO" ) == 0 ) {
    kc_flags.macro_search = KmMACRO_AUTO;
    kc_value.defaults_order = "-AUTO";
  }
  else if( strcmp( mdef, "AR" ) == 0 || strcmp( mdef, "AUTOREVERSE" ) == 0 ) {
    kc_flags.macro_search = KmMACRO_AUTOREVERSE;
    kc_value.defaults_order = "-AUTOREVERSE";
  }
  else if( strcmp( mdef, "C" ) == 0 || strcmp( mdef, "COMMAND" ) == 0 ) {
    kc_flags.macro_search = KmMACRO_COMMAND;
    kc_value.defaults_order = "-COMMAND";
  }
  else {
    char *value;
    switch( kc_flags.macro_search ) {
    case KmMACRO_COMMAND:
      value = "Command";
      break;
    case KmMACRO_AUTO:
      value = "Auto";
      break;
    case KmMACRO_AUTOREVERSE:
      value = "AutoReverse";
      break;
    default:
      value = "Undefined";
      break;
    }
    ku_alfa();
    printf( " Macro search order is %s\n", value );
  }

  return 0;
}


/*
 * action routine for /MACRO/TRACE
 */
int kxmtrace()
{
  char *onoff = ku_getc();
  char *level = ku_getc();

  if( strcmp( onoff, "ON" ) == 0 ) {
    kc_flags.trace_on = 1;
  }
  else {
    kc_flags.trace_on = 0;
  }

  if( strcmp( level, "WAIT" ) == 0 ) {
    kc_flags.trace_wait = 1;
    kc_flags.trace_wait_save = 1;
  }
  else if( strcmp( level, "FULL" ) == 0 ) {
    kc_flags.trace_full = 1;
    kc_flags.trace_debug = 0;
  }
  else if( strcmp( level, "DEBUG" ) == 0 ) {
    kc_flags.trace_full = 1;
    kc_flags.trace_debug = 1;
  }
  else if( strcmp( level, "TEST" ) == 0 ) {
    kc_flags.trace_test = 1;
  }
  else {
    kc_flags.trace_wait = 0;
    kc_flags.trace_wait_save = 0;
    kc_flags.trace_full = 0;
    kc_flags.trace_debug = 0;
    kc_flags.trace_test = 0;
  }

  return 0;
}


int exec_cmd_string( const char *cmd_string,
                    int interactive,
                    int (*display_panel)() )
{
  int status = 0;

  check_edit_server();

  if( !kc_flags.in_macro
     && kc_flags.in_application && !kc_flags.appl_called ) {

    if( strcasecmp( cmd_string, kc_flags.appl_exit ) != 0 ) {
      KmCommand *cmd = kc_flags.appl_cmd;

      /* call the application command with argument string */
      reset_arg_list( cmd );
      cmd->argc = 1;
      cmd->argv = (char**)malloc( sizeof(char*) );
      cmd->argoffs = (int*)malloc( sizeof(int) );
      cmd->argv[0] = strdup( cmd_string );
      cmd->argoffs[0] = 0;
      cmd->argline = strdup( cmd_string );
      status = exec_decoded_cmd( kc_flags.appl_cmd );
    }
    else {
      /* leave application */

      /* flag can be reset only here because it is tested in KUAPPL */
      kc_flags.in_application = 0;
      if( kc_flags.curr_prompt != NULL )
        free( kc_flags.curr_prompt );
      kc_flags.curr_prompt = strdup( kc_value.set_prompt );
    }
  }
  else if( cmd_string != NULL && cmd_string[0] != '\0' ) {

    kc_alias.substitutions = 100; /* protect against recursive aliases */

    status = exec_multiple_cmds( cmd_string, interactive, display_panel );

    if( kc_flags.last_cmd != NULL )
      free( kc_flags.last_cmd );
    kc_flags.last_cmd = strdup( cmd_string ); /* for $LAST function */
#ifdef DBMALLOC
    malloc_mark( kc_flags.last_cmd );
#endif
  }

  if( kjmpaddr.user_grfl_F != NULL )
    (*kjmpaddr.user_grfl_F)();  /* flush graphics output */

  return status;
}


/*
 * split the line into semicolon separated commands and execute each of them
 * substitute command aliases
 * if -AutoReverse try auto exec
 * apply COMMAND logic
 * execute command
 */
static
int exec_multiple_cmds( const char *cmd_string,
                       int interactive,
                       int (*display_panel)() )
{
  char *cmd_line = strdup( cmd_string );
  char *line = cmd_line;
  int status = 0;

  while( line[0] != '\0' ) {
    int want_prompt = interactive;
    int block_panel;
    char *tail;

    while( *line == ' ' )       /* kill leading blanks */
      line++;

    if( line[0] == '*' ) {      /* comment line */
      break;
    }
    else if( line[0] == '+' ) { /* suppress panel display */
      line++;
      want_prompt = 0;
    }
    else if( line[0] == '-' ) { /* force panel display */
      line++;
      want_prompt = -1;
    }
    while( *line == ' ' )
      line++;

    /* extract single command */
    tail = split_at_semicolon( line );
    if( line[0] == '\0' ) {
      line = tail;
      /* ignore empty lines */
      continue;
    }
    /* for multiple commands block the Motif panel until OK or Cancel */
    block_panel = (tail[0] != '\0');

    status = exec_single_cmd( line, want_prompt, display_panel, block_panel );

    if( status == KUMAC_UNWIND )
      break;

    if( tail[0] == '&' ) {
      /* cmd1 ;& cmd2 : execute second command only if first one succeeded */
      if( status != 0 )
        break;
      line = tail + 1;
    }
    else if( tail[0] == '!' ) {
      /* cmd1 ;! cmd2 : execute second command only if first one failed */
      if( status == 0 )
        break;
      line = tail + 1;
    }
    else {
      /* cmd1 ; cmd2 : execute second command anyway */
      line = tail;
    }
  }
  free( cmd_line );
  return status;
}


static
int exec_single_cmd( const char *cmd_line,
                    int interactive,
                    int (*display_panel)(),
                    int block_panel )
{
  KmCommand **cmd_list;
  KmCommand *cmd;
  char *line;
  char *cmd_path;
  char *cmd_args;
  char *p;
  int status;

  line = strdup( cmd_line );

  if( kc_flags.in_macro ) {

    /* substitute system functions before variable arguments are replaced */
    if( (p = subst_sysfun( line, 1 )) != NULL ) {
      free( line );
      line = p;
    }
  }

  if( (p = subst_var_alias( line )) != NULL ) {
    free( line );
    line = p;
  }

  if( (p = subst_cmd_alias( line )) != NULL ) {
    free( line );
    line = p;

    /* start again because alias value can contain several commands */
    status = exec_multiple_cmds( line, interactive, display_panel );
    free( line );
    return status;
  }

  if( (p = strchr( line, '=' )) != NULL ) {
    /* variable assignment */
    char *name = strtrim( strndup( line, p - line ) );

    if( hash_lookup( kc_alias.global_value, name, NULL ) != NULL ) {
      char *value = ku_expr( p + 1 );

      hash_insert( kc_alias.global_value, name, value, 0 );
      free( value );
      free( name );
      return 0;
    }
    free( name );
  }

  if( kc_alias.translate ) {
    static char _KUIP_ALIAS_[] = "/KUIP/ALIAS/";

    /* separate command path */
    cmd_path = strdup( line );
    if( (p = strchr( cmd_path, ' ' )) != NULL )
      *p = '\0';

    /* substitute argument aliases except for ALIAS commands */
    cmd = search_command( cmd_path, &cmd_list );
    if( (cmd == NULL || strncmp( cmd->path, _KUIP_ALIAS_,
                                (sizeof _KUIP_ALIAS_)-1 ) != 0)
       && (p = subst_arg_alias( line )) != NULL ) {
      free( line );
      line = p;
    }
    free( cmd_path );
  }

  /* substitute system functions */
  if( (p = subst_sysfun( line, 1 )) != NULL ) {
    free( line );
    line = p;
  }

  if( kc_flags.in_macro && kc_flags.trace_on ) {

    if( kc_flags.trace_full ) {
      ku_alfa();
      printf( " >>>>>> %s\n", line );
    }

    if( kc_flags.trace_wait ) {
      switch( ku_more( "Execute", line ) ) {
      case 1:                   /* Yes */
        break;
      case 2:                   /* No */
        free( line );
        return -1;
        /*break;*/
      case 3:                   /* Quit */
        free( line );
        kc_flags.quit_macro = 1;
        return KUMAC_UNWIND;
        /*break;*/
      case 4:                   /* Go */
        kc_flags.trace_wait = 0; /* no more questions */
        break;
      }
    }
  }

  cmd_path = strdup( line );
  if( (p = strchr( cmd_path, ' ' )) != NULL ) {
    *p = '\0';
    cmd_args = strdup( p + 1 );
  }
  else {
    cmd_args = strdup( "" );
  }

  if( !kc_flags.in_macro && kc_flags.macro_search == KmMACRO_AUTOREVERSE ) {
    /* look for macro before commands */
    char *fname = strdup( cmd_path );
    char *hash = strchr( fname, '#' );
    if( hash != NULL )          /* in case "mfile#mname" chop off "#mname" */
      *hash = '\0';

    if( fsearch( fname, "kumac", kc_flags.macro_path ) != NULL ) {
      free( cmd_path );
      free( cmd_args );
      cmd_path = strdup( "/MACRO/EXEC" );
      cmd_args = strdup( line );
    }
    free( fname );
  }

  if( (cmd = search_command( cmd_path, &cmd_list )) == NULL &&
     !kc_flags.in_macro && kc_flags.macro_search == KmMACRO_AUTO ) {
    /* look for macro if command not found */
    char *fname = strdup( cmd_path );
    char *hash = strchr( fname, '#' );
    if( hash != NULL )  /* in case "mfile#mname" chop off "#mname" */
      *hash = '\0';

    if( fsearch( fname, "kumac", kc_flags.macro_path ) != NULL ) {
      cmd = search_command( "/MACRO/EXEC", NULL );
      free( cmd_args );
      cmd_args = strdup( line );
    }
    free( fname );
  }

  if( cmd == NULL ) {
    if( cmd_list == NULL && kc_value.set_command != NULL ) {
      status = exec_template( line, kc_value.set_command,
                             interactive, display_panel );
    }
    else {
      if( kjmpaddr.disp_cmd_list_C != NULL ) {
        (*kjmpaddr.disp_cmd_list_C)(cmd_path, cmd_list, 0);
      } else {
        print_cmd_list( cmd_list, cmd_path );
      }
      status = -1;
    }
  }
  else {
    status = fill_arg_list( cmd, cmd_args,
                           interactive, display_panel, block_panel );
  }
  free( cmd_path );
  free( cmd_args );
  free( line );

  /* delete the temp vectors created by $SIGMA system function */
  if ( kc_flags.temp_vectors > 0 ) {
    INTEGER NB = kc_flags.temp_vectors;
    Kxdsig ( &NB );
    kc_flags.temp_vectors = 0;
  }

  /* delete the temp vectors created for non-contiguous subvector */
  if( Kcutil.NVADD > 0 ) {
    INTEGER IX = Pawc->IXPAWC + 1;
    Mzwipe( &IX );
    Kcutil.NVADD = 0;
  }

  return status;
}


/*
 * action routine for /KUIP/SET_SHOW/APPLICATION
 */
int kxapplication()
{
  char *appl_path = ku_getc();
  char *appl_exit = ku_gets();

  return ku_sapp( appl_path, appl_exit );
}


char *ku_appl( int *appl_luno,
              int *in_macro )
{
  if( kc_flags.in_macro && kc_flags.in_application )
    *appl_luno = kc_flags.appl_luno;
  else
    *appl_luno = 5;
  *in_macro = kc_flags.in_macro;
  return kc_flags.appl_exit;
}


void F77_ENTRY_IIC(Kuappl,lun,macflg,chexit)
/* { */
  int appl_luno;
  int in_macro;
  char *appl_exit = ku_appl( &appl_luno, &in_macro );
  *lun = appl_luno;
  *macflg = in_macro;
  fstrset( chexit, len_chexit, appl_exit );
}


int ku_sapp( const char *appl_path,
            const char *appl_exit )
{
  if( appl_path[0] == '\0' ) {
    kc_flags.in_application = 0;
    if( kc_flags.curr_prompt != NULL )
      free( kc_flags.curr_prompt );
    kc_flags.curr_prompt = strdup( kc_value.set_prompt );
    return 0;
  }
  if( (kc_flags.appl_cmd = search_command( appl_path, NULL )) == NULL ) {
    printf( " *** Unknown application %s\n", appl_path );
    return -1;
  }
  kc_flags.in_application = 1;
  if( kc_flags.curr_prompt != NULL )
    free( kc_flags.curr_prompt );
  if( strstr( kc_value.set_prompt, "[]" ) != NULL )
    kc_flags.curr_prompt = str2dup( kc_flags.appl_cmd->name, " []" );
  else
    kc_flags.curr_prompt = str2dup( kc_flags.appl_cmd->name, " >" );

  if( appl_exit[0] == '\0' )
    appl_exit = "EXIT";

  if( kc_flags.appl_exit != NULL )
    free( kc_flags.appl_exit );
  kc_flags.appl_exit = strdup( appl_exit );

  if( kc_flags.in_macro ) {
    kc_flags.appl_stream = fopen( kc_flags.appl_file, "w" );
  }
  return 0;
}


void F77_ENTRY_CC(Kusapp,chpath,chexit)
/* { */
  char *appl_path = fstrdup( chpath, len_chpath );
  char *appl_exit = fstrdup( chexit, len_chexit );
  ku_sapp( appl_path, appl_exit );
  free( appl_path );
  free( appl_exit );
}


/*
 * action routine for /MACRO/DATA
 */
int kxmdata()
{
  int appl_luno;
  int in_macro;
  char *appl_exit = ku_appl( &appl_luno, &in_macro );
  FILE *stream = fopen( ku_fcase( appl_exit ), "w" );
  char line[256];

  if( stream == NULL ) {
    printf( " *** MACRO/DATA: cannot open file %s\n", appl_exit );
    return 1;
  }
  while( ku_read( appl_luno, line, 255 ) >= 0
        && strcasecmp( appl_exit, line ) != 0 ) {
    fprintf( stream, "%s\n", line );
  }
  fclose( stream );

  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/TIMING
 */
int kxtiming()
{
  char *set_ON  = "ON";
  char *set_OFF = "OFF";
  char *set_ALL = "ALL";
  char *value = ku_getc();
  KmTiming timing;

  if( strcmp( value, set_OFF ) == 0 ) {
    timing = KmTIMING_OFF;
    kc_value.set_timing = set_OFF;
  }
  else if( strcmp( value, set_ALL ) == 0 ) {
    timing = KmTIMING_ALL;
    kc_value.set_timing = set_ALL;
  }
  else {
    timing = KmTIMING_ON;
    kc_value.set_timing = set_ON;
  }

  if( kc_flags.timing != timing ) {
    if( timing == KmTIMING_OFF ) {
      kc_flags.real_time = time( NULL );
      kc_flags.user_time = clock();
    }
    else
      ku_time( kc_flags.real_time, kc_flags.user_time );
    kc_flags.timing = timing;
  }

  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/COMMAND
 */
int kxcommand()
{
  char *value = ku_gets();

  ku_cmdl( value );

  return 0;
}


void F77_ENTRY_C(Kucmdl,chpath)
/* { */
  char *path = fstrdup( chpath, len_chpath );

  ku_cmdl( path );
  free( path );
}


void ku_cmdl( const char *template )
{
  if( template[0] == '\0' ) {
    ku_alfa();
    if( kc_value.set_command == NULL )
      printf( " COMMAND line not defined\n" );
    else
      printf( " COMMAND line is : %s\n", kc_value.set_command );
  }
  else if( strcmp( template, "*" ) == 0 ) {
    if( kc_value.set_command != NULL ) {
      free( kc_value.set_command );
      kc_value.set_command = NULL;
    }
  }
  else {
    if( kc_value.set_command != NULL )
      free( kc_value.set_command );
    kc_value.set_command = strdup( template );
  }
}


/*
 * substitute template and execute
 */
static
int exec_template( const char *line,
                  const char *template,
                  int interactive,
                  int (*display_panel)() )
{
  char *old_template = kc_value.set_command;
  char *new_line = strdup( "" );
  int status;
  int i;

  for( i = 0; template[i] != '\0'; i++ ) {

    if( template[i] == '$' ) {

      if( template[i+1] == '*' ) {
        new_line = mstrcat( new_line, line );
        i++;
        continue;
      }

      else if( template[i+1] >= '1' && template[i+1] <= '9' ) {
        int n = template[i+1] - '1';
        char *args = strdup( line );
        char *word = strqtok( args );
        while( word != NULL && n-- > 0 )
          word = strqtok( NULL );
        if( word != NULL )
          new_line = mstrcat( new_line, word );
        free( args );
        i++;
        continue;
      }
    }

    new_line = mstrccat( new_line, template[i], 1 );
  }

  /* avoid infinite recursion of template is an invalid command */
  kc_value.set_command = NULL;
  status = exec_multiple_cmds( new_line, interactive, display_panel );
  free( new_line );
  kc_value.set_command = old_template;
  return status;
}


/*
 * check if string contains only valid options
 */
static
int valid_option( KmParameter *par,
                 const char *opt_string,
                 int len_opt_string )
{
  int valid = 1;

  if( par->range_count > 0 ) {  /* accept everything if no range defined */
    char *options = strupper( strndup( opt_string, len_opt_string ) );
    int maxlen = mstrlen( par->range_value, par->range_count );
    int n;

    /*
     * start with longest range values first because short ones
     * could be substrings of longer ones
     */
    for( n = maxlen; n > 0 && *options != '\0'; n-- ) {
      int j;

      for( j = 0; j < par->range_count; j++ ) {
        int len = strlen( par->range_value[j] );
        if( len == n ) {
          char *val = strupper( strdup( par->range_value[j] ) );
          char *p = strstr( options, val );
          free( val );
          if( p != NULL ) {
            /* remove this option */
	    memmove( p, p + len , strlen(p+len) +1);
/* overlapping            strcpy( p, p + len );*/
          }
        }
      }
    }

    for( n = strlen( options ); n > 0 && options[n-1] == ' '; n-- )
      options[n-1] = '\0';      /* strip trailing blanks */

    if( *options != '\0' )
      valid = 0;                /* option not in range */

    free( options );
  }

  return valid;
}


void reset_arg_list( KmCommand *cmd )
{
  /* clear current values */
  if( cmd->argc > 0 ) {
    int i;

    for( i = 0; i < cmd->argc; i++ )
      free( cmd->argv[i] );
    cmd->argc = 0;
    free( (char*)cmd->argv );
    cmd->argv = NULL;
    free( cmd->argline );
    cmd->argline = NULL;
    free( (char*)cmd->argoffs );
    cmd->argoffs = NULL;
  }
}


/*
 * locate the next blank outside quotes and return pointer to it
 */
static
const char *next_word( const char *line )
{
  int quoted = 0;
  while( *line != '\0' ) {
    if( *line == '\'' )
      quoted = !quoted;
    else if( *line == ' ' && !quoted )
      return line;
    line++;
  }
  return line;
}


/*
 * remove quotes and string concatenations
 */
char *clean_word( char *word )
{
  char *p;
  int quoted = 0;
  int strip_quotes;

  for( p = word; p[0] != '\0'; p++ ) {

    if( p[0] == '\'' ) {

      if( !quoted ) {
        quoted = 1;
      }

      else if( p[1] == '/' && p[2] == '/' && p[3] == '\'' ) {
        /* contract "'abc'//'def'" to "'abcdef'" */
	  memmove(p, p + 4 , strlen(p+4) + 1);
/*        strcpy( p, p + 4 );*/
        p--;
      }

      else if( p[1] == '/' && p[2] == '/' && p[3] != '\0' ) {
        char *conc = strstr( p + 3, "//" );
        if( conc != NULL ) {
          *conc = '\0';
          if( strchr( p + 3, '\'' ) != NULL ) {
            *conc = '/';
            conc = NULL;
          }
          else {
            *conc = '/';
          }
        }
        if( conc == NULL ) {
          /* contract "'abc'//def" to "'abcdef'" */
	    memmove( p, p + 3, strlen(p+3) + 1 );
/*          strcpy( p, p + 3 );*/
          strcat( p, "'" );
        }
        else {
          /* contract "'abc'//def//ghi" to "'abcdef'//ghi" */
          /*               ^p    ^conc */
          strncpy( p, p + 3, conc - p - 3 ); /* "'abcdefdef//ghi" */
          conc[-3] = '\'';                   /* "'abcdef'ef//ghi" */
	    memmove( conc - 2, conc, strlen(conc) + 1);
/*          strcpy( conc - 2, conc );*/
        }
      }

      else {
        quoted = 0;
      }
    }

    else if( !quoted && p[1] == '/' && p[2] == '/' ) {

      if( p[3] == '\'' ) {
        /* contract "abc//'def'" to "'abcdef'" */
        char *s = str2dup( "'", word );
        int n = p - word;
        strcpy( s + n + 2, p + 4 );
        strcpy( word, s );
        free( s );
        p++;
        quoted = 1;
      }
      else if( p[3] != '\0' ) {
        /* contract "abc//def" to "abcdef"; leave "abc//" unchanged */
	memmove( p + 1, p + 3, strlen(p + 3) + 1 );
/*        strcpy( p + 1, p + 3 );*/
      }
    }

  }

  strip_quotes = ( word[0] == '\'' && word[strlen(word)-1] == '\'' );

  if( strip_quotes ) {
    /* test if there are transitions between quoted and not quoted */
    for( p = word + 1; p[0] != '\0'; p++ ) {
      if( p[0] == '\'' ) {
        if( p[1] == '\'' ) {
          /* "'abc''def'" is okay */
          p++;
        }
        else if( p[1] != '\0' ) {
          /* in "'abc'def'ghi'" we want to keep the quotes */
          strip_quotes = 0;
          break;
        }
      }
    }
  }

  for( quoted = 0, p = word; p[0] != '\0'; p++ ) {
    if( p[0] == '\'' ) {
      if( !quoted ) {
        quoted = 1;
      }
      else if( p[1] == '\'' ) {
        /* contract "'abc''def'" to "'abc'def'" */
	memmove( p, p + 1, strlen(p+1) + 1 );
/*        strcpy( p, p + 1 );*/
      }
      else {
        quoted = 0;
      }
    }
  }

  if( strip_quotes ) {
    /* contract "'abcdef'" to "abcdef" */
    for( p = word; p[2] != '\0'; p++ )
      p[0] = p[1];
    p[0] = '\0';
  }

  return word;
}


/*
 * parse the command string
 * find the corresponding command structure
 * fill the argument list
 * check the argument list for completeness and validity
 * return the pointer to the command structure or NULL if something is wrong
 *
 * interactive ==  1 : prompt for missing mandatory parameters or wrong values
 * interactive ==  0 : bail out if something is wrong
 * interactive == -1 : display panel in any case
 */
static
int fill_arg_list( KmCommand *cmd,
                  char *arg_line,
                  int want_prompt,
                  int (*display_panel)(), /* routine to display Motif panel */
                  int block_panel )
{
  char *macro_args = NULL;
  int i;

  reset_arg_list( cmd );
  while( *arg_line == ' ' )
    arg_line++;
  if ( strlen(arg_line) > 0 ) {
    char *p=arg_line + strlen(arg_line) -1;
    while (*p == ' ' )
      *p--='\0';
  }

  if( strcasecmp( cmd->name, "EXEC" ) == 0 ) {
    /*
     * special treatment for "EXEC mname args":
     * chop off arg_line after mname and keep quoting in macro arguments
     */
    macro_args = arg_line;
    while( *macro_args != ' ' && *macro_args != '\0' )
      macro_args++;
    while( *macro_args == ' ' )
      macro_args++;
    if( *macro_args != '\0' )
      macro_args[-1] = '\0';
    else
      macro_args = NULL;
  }

  if( *arg_line != '\0' ) {
    const char *argv = arg_line;
    int argc = 0;
    int blanks = 0;

    while( 1 ) {
      const char *next_argv = next_word( argv );
      int len_argv = next_argv - argv;
      int next_argc = argc + 1;
      char *alloced_argv;
      const char *value;

      if( len_argv == 1 && argv[0] == '!' && argc < cmd->total ) {
        /* "!" == use default value */
        if( (argv = cmd->par[argc]->dfault) != NULL )
          len_argv = strlen( argv );
      }

      else if( len_argv == 2 && argv[0] == '!' && argv[1] == '!'
              && argc < cmd->total ) {
        /* "!?" == use last value or default */
        if( (argv = cmd->par[argc]->last) != NULL
           || (argv = cmd->par[argc]->dfault) != NULL )
          len_argv = strlen( argv );
      }

      else if( len_argv > 1 && argv[0] == '-' && argc < cmd->total
              && (cmd->par[argc]->flags & KmFLAG_MINUS) == 0 ) {
        /* "-value" for a parameter without explicit MINUS flag */
        int try_chopt = 0;

        if( cmd->par[argc]->type == KmTYPE_INT ) {
          /* make sure that it is not an integer number */
          for( i = 1; i < len_argv; i++ ) {
            if( !isdigit( argv[i] ) ) {
              try_chopt = 1;
              break;
            }
          }
        }
        else if( cmd->par[argc]->type == KmTYPE_REAL ) {
          /* make sure that it is not a real number */
          if( isdigit( argv[1] ) || argv[1] == '.' ) {
            for( i = 2; i < len_argv; i++ ) {
              if( strchr( "-12345.678eE+90", argv[i] ) == NULL ) {
                try_chopt = 1;
                break;
              }
            }
          }
          else {
            try_chopt = 1;
          }
        }
        else {
          try_chopt = 1;
        }

        if( try_chopt ) {
          /* try if there is a option parameter for which value is valid */
          for( i = 0; i < cmd->total; i++ ) {
            KmParameter *par = cmd->par[i];
            if( par->type == KmTYPE_OPTION ) {
              if( valid_option( par, argv + 1, len_argv - 1 ) ) {
                next_argc = argc; /* do not increment argument count */
                argc = i;
                argv++;
                len_argv--;
                break;
              }
            }
          }
        }
      }

      else if( (value = strchr( argv, '=' )) != NULL && value < next_argv ) {
        int len = value - argv;
        /* argument is NAME=VALUE */
        for( i = 0; i < cmd->total; i++ ) {
          KmParameter *par = cmd->par[i];
          if( len >= par->abbrev && len <= strlen( par->name )
             && strncasecmp( argv, par->name, len ) == 0 ) {
            next_argc = i + 1; /* move argument count */
            argc = i;
            argv += len + 1;
            len_argv -= len + 1;
            break;
          }
        }
      }

      if( argv != NULL )
        alloced_argv = clean_word( strndup( argv, len_argv ) );
      else
        alloced_argv = NULL;

      if( cmd->argc < argc + 1 ) {
        int k;
        /* adjust argument arrays */
        if( cmd->argc == 0 ) {
          cmd->argv = (char**)malloc( (argc+1) * sizeof(char*) );
          cmd->argoffs = (int*)malloc( (argc+1) * sizeof(int) );
        }
        else {
          cmd->argv = (char**)realloc( (char*)cmd->argv,
                                      (argc+1) * sizeof(char*) );
          cmd->argoffs = (int*)realloc( (char*)cmd->argoffs,
                                       (argc+1) * sizeof(int) );
        }
        for( k = cmd->argc; k < argc + 1; k++ ) {
          cmd->argv[k] = NULL;
          cmd->argoffs[k] = 0;
        }
        cmd->argc = argc + 1;
      }

      /* assign argument value */
      if( cmd->argv[argc] == NULL ) {
        if( argc < cmd->total
           && (cmd->par[argc]->flags & KmFLAG_VARARG) != 0 ) {
          /* protect commas inside the value */
          char *p;
          while( (p = strchr( alloced_argv, ',' )) != NULL )
            *p = '\n';
        }
        cmd->argv[argc] = alloced_argv;
        cmd->argoffs[argc] = blanks;
        blanks = 0;
      }
      else if( alloced_argv != NULL ) {
        /* append to existing value */
        if( argc < cmd->total && cmd->par[argc]->type == KmTYPE_OPTION ) {
          /* options are simply concatenated */
          cmd->argv[argc] = mstrcat( cmd->argv[argc], alloced_argv );
        }
        else if( argc < cmd->total
                && (cmd->par[argc]->flags & KmFLAG_VARARG) != 0 ) {
          /* separate by comma and protect commas inside the value */
          char *p;
          while( (p = strchr( alloced_argv, ',' )) != NULL )
            *p = '\n';
          cmd->argv[argc] = mstr2cat( cmd->argv[argc], ",", alloced_argv );
        }
        else {
          /* other argument are separate by blank */
          cmd->argv[argc] = mstr2cat( cmd->argv[argc], " ", alloced_argv );
        }
        free( alloced_argv );   /* malloc'ed in clean_word() */
      }

      if( argc < cmd->total ) {
        /* check that a constant has not been assigned a different value */
        KmParameter *par = cmd->par[argc];

        if( (par->flags & KmFLAG_CONST) != 0
           && strcmp( cmd->argv[argc], par->dfault ) != 0 ) {
          printf( " *** Parameter %s of %s is defined as a constant!\n",
                 par->name, cmd->name );
          free( cmd->argv[argc] );
          cmd->argv[argc] = strdup( par->dfault );
        }
      }

      if( *next_argv != '\0' ) {
        argv = next_argv + 1;
        while( *argv == ' ' ) {
          argv++;
          blanks++;
        }
        if( argc >= cmd->total
           || (cmd->par[argc]->flags & KmFLAG_VARARG) == 0 )
          argc = next_argc;
      }
      else {
        for( i = 0; i < cmd->argc; i++ ) {
          if( i == 0 )
            cmd->argline = strdup( "" );
          else
            cmd->argline = mstrccat( cmd->argline, ' ', cmd->argoffs[i] + 1 );

          cmd->argoffs[i] = strlen( cmd->argline );
          if( cmd->argv[i] == NULL ) {
            char *dfault = NULL;
            if( i < cmd->total ) {
              KmParameter *par = cmd->par[i];
              if( par->dfault != NULL )
                dfault = par->dfault;
              else if( i >= cmd->mandatory )
                dfault = par->last;
            }
            cmd->argv[i] = strdup( (dfault != NULL) ? dfault : "" );
          }
          else
            cmd->argline = mstrcat( cmd->argline, cmd->argv[i] );
        }
        break;                  /* while(1) */
      }
    }
  }

  /* check that all mandatory parameters are there */
  if( cmd->argc < cmd->mandatory ) {
    if( cmd->argc == 0 ) {
      cmd->argv = (char**)malloc( cmd->mandatory * sizeof(char*) );
      cmd->argline = strdup( "" );
      cmd->argoffs = (int*)malloc( cmd->mandatory * sizeof(int) );
      cmd->argoffs[0] = 0;
    }
    else {
      cmd->argv = (char**)realloc( (char*)cmd->argv,
                                  cmd->mandatory * sizeof(char*) );
      cmd->argoffs = (int*)realloc( (char*)cmd->argoffs,
                                   cmd->mandatory * sizeof(int) );
    }
    for( i = cmd->argc; i < cmd->mandatory; i++ ) {
      cmd->argv[i] = NULL;
    }
    for( i = cmd->argc + 1; i < cmd->mandatory; i++ ) {
      cmd->argoffs[i] = cmd->argoffs[i-1];
    }
  }

  for( i = 0; i < cmd->mandatory; i++ ) {
    if( i >= cmd->argc || cmd->argv[i] == NULL ) {
      if( want_prompt ) {
        if( display_panel == NULL ) {
          /* prompt on command line */
          cmd->argv[i] = strdup( prompt_argument( cmd->par[i] ) );
          if( cmd->argline[0] != '\0' )
            cmd->argline = mstrcat( cmd->argline, " " );
          cmd->argoffs[i] = strlen( cmd->argline );
          cmd->argline = mstrcat( cmd->argline, cmd->argv[i] );
          cmd->argc++;
        }
        else {
          /* display panel */
          return (*display_panel)( cmd, i, block_panel );
        }
      }
      else {
        ku_alfa();
        printf( " *** Mandatory parameter %s missing\n", cmd->par[i]->name );
        return 1;
      }
    }
  }

  if( macro_args != NULL ) {
    /* insert macro arguments for EXEC command */
    cmd->argv = (char**)realloc( (char*)cmd->argv,
                                (cmd->argc+1) * sizeof(char*) );
    cmd->argoffs = (int*)realloc( (char*)cmd->argoffs,
                                 (cmd->argc+1) * sizeof(int) );
    cmd->argline = mstrcat( cmd->argline, " " );
    cmd->argoffs[cmd->argc] = strlen( cmd->argline );
    cmd->argv[cmd->argc] = strdup( macro_args );
    cmd->argline = mstrcat( cmd->argline, macro_args );
    cmd->argc++;
  }

#ifdef DBMALLOC
  if( cmd->argc > 0 ) {
    malloc_mark( cmd->argv );
    malloc_mark( cmd->argoffs );
    malloc_mark( cmd->argline );
    for( i = 0; i < cmd->argc; i++ ) {
      malloc_mark( cmd->argv[i] );
    }
  }
#endif

  if( want_prompt < 0 && display_panel != NULL ) {
    /* force panel display */
    return (*display_panel)( cmd, i, block_panel );
  }
  return exec_decoded_cmd( cmd );
}


/*
 * execute the command
 */
int exec_decoded_cmd( KmCommand *cmd )
{
  int status;
  int do_timing = (kc_flags.timing == KmTIMING_ALL
                   || ( kc_flags.timing == KmTIMING_ON
                       && !kc_flags.in_macro ));
  clock_t user_time = clock();
  time_t  real_time = time( NULL );
  int i;

  /* save current values in last */
  for( i = 0; i < cmd->total; i++ ) {
    KmParameter *par = cmd->par[i];
    if( i >= cmd->mandatory && par->dfault == NULL ) {
      /*
       * optional parameters without default:
       * overwrite last value by command line argument
       * otherwise keep value from prompt in next_argument()
       */
      if( i < cmd->argc ) {
        if( par->last != NULL )
          free( par->last );
        par->last = strdup( cmd->argv[i] );
#ifdef DBMALLOC
        malloc_mark( par->last );
#endif
      }
    }
    else {
      if( par->last != NULL )
        free( par->last );
      if( i < cmd->argc ) {
        par->last = strdup( cmd->argv[i] );
#ifdef DBMALLOC
        malloc_mark( par->last );
#endif
      }
      else {
        par->last = NULL;
      }
    }
  }

  curr_cmd = cmd;
  if( cmd->list_par > 0 && cmd->list_par <= cmd->argc ) {
    /* execute command for each value in a comma separated list */
    while( 1 ) {
      char *p;
      int paren = 0;
      int comma = 0;
      for( p = cmd->argv[cmd->list_par-1]; *p != '\0'; p++ ) {
        /* look for comma outside balanced parentheses */
        if( *p == '(' )
          paren++;
        else if( *p == ')' )
          paren--;
        else if( paren == 0 && *p == ',' ) {
          comma = 1;
          *p++ = '\0';
          break;
        }
      }
      status = call_action( cmd );
      /*  if( kc_flags.quit_macro ) */
      if( kc_flags.in_macro && kc_flags.quit_macro )
        status = KUMAC_UNWIND; /* quit macro execution */
      if( !comma || status != 0 )
        break;
      strcpy( cmd->argv[cmd->list_par-1], p );
    }
  }
  else {
    /* command has no parameter of list type */
    status = call_action( cmd );
  }
  if( do_timing )
    ku_time( real_time, user_time );

  return status;
}


/*
 * print real and CPU time
 */
void ku_time( time_t real_time,
             clock_t user_time )
{
  double cptime = (double)( clock() - user_time ) / CLOCKS_PER_SEC;
  double rtime = difftime( time( NULL ), real_time ) + 0.5;
  int seconds;
  int minutes;
  int hours;

  hours = rtime / 3600;
  rtime -= hours * 3600;
  minutes = rtime / 60;
  rtime -= minutes * 60;
  seconds = rtime;

  ku_alfa();
  printf( " Real time %s:%s:%s, CP time %.3f\n",
         strfromi( hours, 2 ), strfromi( minutes, 2 ), strfromi( seconds, 2 ),
         cptime );
}


/*
 * return difference in real time since last call in format hh:mm:ss
 */
void F77_ENTRY_C(Kirtim,chtime)
/* { */
  static int initialized = 0;
  static time_t time0;
  time_t time1 = time( NULL );
  double rtime;
  int seconds;
  int minutes;
  int hours;
  char timebuf[9];

  if( !initialized ) {
    initialized = 1;
    time0 = time( NULL );
  }
  rtime = difftime( time1, time0 );
  time0 = time1;
  rtime += 0.5;
  hours = rtime / 3600;
  rtime -= hours * 3600;
  minutes = rtime / 60;
  rtime -= minutes * 60;
  seconds = rtime;

  strcpy( timebuf, "00:00:00" );
  if( hours < 100 ) {
    timebuf[0] += hours / 10;
    timebuf[1] += hours % 10;
  }
  else {
    timebuf[0] = '*';
    timebuf[1] = '*';
  }
  timebuf[3] += minutes / 10;
  timebuf[4] += minutes % 10;
  timebuf[6] += seconds / 10;
  timebuf[7] += seconds % 10;

  fstrset( chtime, len_chtime, timebuf );
}


static  time_t last_real_time;
static clock_t last_user_time;

/*
 * print timing since last call to KUTIME/KUTIM0
 */
void F77_ENTRY(Kutime)
/* { */
  ku_time( last_real_time, last_user_time );
  last_real_time = time( NULL );
  last_user_time = clock();
}


/*
 * reset timing
 */
void F77_ENTRY(Kutim0)
/* { */
  last_real_time = time( NULL );
  last_user_time = clock();
}


/*
 * call the action routine
 */
static
int do_action( cmd )
     KmCommand *cmd;
{
#ifdef DBMALLOC
  unsigned long histid1, histid2, orig_size, current_size;

  orig_size = malloc_size( &histid1 );
#endif

#ifdef vms
  if( kc_flags.action_nesting == 0 && kc_flags.in_motif )
    VAXC$ESTABLISH( signal_handler );
#endif

  ku_intr( 1 );                 /* allow interrupts */

  kc_flags.action_nesting++;
  if( cmd->action_F != NULL ) {
    IQUEST(1) = 0;
    (*cmd->action_F)();
  }
  else if( cmd->action_C != NULL ) {
    IQUEST(1) = (*cmd->action_C)();
  }
  kc_flags.action_nesting--;

#ifdef DBMALLOC
  current_size = malloc_size( &histid2 );

  if( current_size != orig_size ) {
    /* check for memory leaks in action routine */
    malloc_list( 2, histid1, histid2 );
  }
#endif

  return 1;
}


static
int call_action( KmCommand *cmd )
{
  int status;

  if( kc_flags.in_macro && kc_flags.trace_test )
    return 0;                   /* TRACE ON TEST */

  if( kjmpaddr.disp_flush_C != NULL )
    (*kjmpaddr.disp_flush_C)(); /* eat up queued Motif events */

  if( cmd->action_F == NULL && cmd->action_C == NULL ) {
    /* no action routine -- execute HELP path */
    char *line = str3dup( "/KUIP/HELP '", cmd->path, "'" );
    status = ku_exel( line );
    free( line );
    return status;
  }

  next_arg = 0;
  if( do_action( cmd ) == 1 )
    status = IQUEST(1);
  else {
    /* this branch only taken for VMS */
    reset_break();
    status = KUMAC_UNWIND;
  }

#ifdef vms
  /*
   * On VMS we are not allowed to break and reenter the Motif mainloop.
   * Therefore we allow ^C only to be delivered while being inside the
   * action routine.
   */
  if( kc_flags.in_motif )
    ku_intr( 0 );               /* disable interrupts */
#endif

  if( kc_break.error_msg != NULL ) {
    /*
     * print the runtime message stored by the error handler
     * in order to avoid recursive I/O
     */
    ku_alfa();
    puts( kc_break.error_msg );
    free( kc_break.error_msg );
    kc_break.error_msg = NULL;
  }

  /* increment execution counter */
  if( kc_flags.in_macro )
    cmd->xcount += XCOUNT_OFFSET;
  else if( (cmd->xcount & XCOUNT_MASK) < XCOUNT_MASK )
    cmd->xcount += 1;

  if( kc_break.soft_intr ) {
    kc_break.soft_intr = 0;
    status = KUMAC_UNWIND;
  }

  return status;
}


/*
 * return statistic about command execution:
 *   IWHICH == 0 : number of commands in total (macro + keyboard)
 *   IWHICH == 1 : number of different commands in total
 *   IWHICH == 2 : number of commands from keyboard
 *   IWHICH == 3 : number of different commands from keyboard
 *   IWHICH == 4 : number of commands inside macro
 *   IWHICH == 5 : number of different commands inside macro
 *   otherwise reset counters
 */
INTEGER F77_ENTRY_I(Kustat,iwhich )
/* { */
  KmMenu **menu_list = root_menu_list( "" );
  KmMenu *menu;
  int which = *iwhich;
  INTEGER count = 0;
  int n;

  for( n = 0; (menu = menu_list[n]) != NULL; n++ ) {
    KmCommand *cmd;
    for( cmd = menu->cmds; cmd != NULL; cmd = cmd->next ) {
      switch( which ) {

      case 0:
        count += (cmd->xcount & XCOUNT_MASK) + (cmd->xcount >> XCOUNT_SHIFT);
        break;

      case 1:
        if( cmd->xcount != 0 )
          count++;
        break;

      case 2:
        count += (cmd->xcount & XCOUNT_MASK);
        break;

      case 3:
        if( (cmd->xcount & XCOUNT_MASK) != 0 )
          count++;
        break;

      case 4:
        count += (cmd->xcount >> XCOUNT_SHIFT);
        break;

      case 5:
        if( (cmd->xcount >> XCOUNT_SHIFT) != 0 )
          count++;
        break;

      default:                  /* reset counters */
        cmd->xcount = 0;
      }
    }
  }
  free( menu_list );
  return count;
}


/*
 * prompt for missing argument
 */
static
const char *prompt_argument( KmParameter *par )
{
  char *dfault;

  if( par->last != NULL )
    dfault = par->last;
  else if( par->dfault != NULL )
    dfault = par->dfault;
  else
    dfault = "";

  return ku_pros( par->prompt, dfault );
}


/*
 * chop off the line at the first unquoted semicolon
 * ignore semicolons followed by something which could be a VMS version number
 * return the remaining line or pointer to '\0'
 */
char *split_at_semicolon( char *line )
{
  int quoted = 0;
  char *tail = "";
  char *p = line;

  while( *p != '\0' ) {

    if( *p == '\'' ) {
      quoted = !quoted;
    }
    else if( *p == ';' && !quoted
            && ( p[1] == '\0'
                || strchr( "0123456789+-?[*", p[1] ) == NULL ) ) {

      tail = p + 1;
      while( *tail == ' ' ) {
        tail++;
      }
      break;
    }
    p++;
  }

  while( p > line && p[-1] == ' ' )
    p--;

  *p = '\0';

  return tail;
}


/*
 * execute command with echo
 */
int ku_exec( const char *cmd_string )
{
  if( kc_flags.echo_command > 0 ) {
    ku_alfa();
    printf( "%s> %s\n", kc_flags.echo_prompt, cmd_string );
  }

  return ku_exel( cmd_string );
}


int ku_exel( const char *cmd_string )
{
  return exec_cmd_string( cmd_string, 1, kjmpaddr.disp_panel_C );
}


void F77_ENTRY_C(Kcexec,chline)
/* { */
  char *cmd_string = fstr0dup( chline, len_chline );
  if( cmd_string != NULL ) {
    ku_exel( cmd_string );
    free( cmd_string );
  }
}


INTEGER F77_ENTRY_C(Kuexec,chline)
/* { */
  INTEGER status = 0;
  char *cmd_string = fstr0dup( chline, len_chline );
  if( cmd_string != NULL ) {
    status = ku_exec( cmd_string );
    free( cmd_string );
  }
  return status;
}


INTEGER F77_ENTRY_C(Kuexel,chline)
/* { */
  INTEGER status = 0;
  char *cmd_string = fstr0dup( chline, len_chline );
  if( cmd_string != NULL ) {
    status = ku_exel( cmd_string );
    free( cmd_string );
  }
  return status;
}


/*
 * return the next argument
 */
static
char *next_value( const char *called_by )
{
  char *value = find_value( NULL, NULL, NULL );

  if( value == NULL ) {
    ku_alfa();
    printf( " *** Command %s requests undefined parameter #%d with %s\n",
           curr_cmd->path, next_arg + 1, called_by );
    value = "";
  }
  else {
    int i;
    for( i = strlen( value ); i > 0; i-- ) {
      if( value[i-1] != ' ' )
        break;
    }
    if( i == 0 )                /* return empty string if all blanks */
      value = "";
  }

  next_arg++;
  return value;
}


/*
 * find the value to return to the next KUGETx call:
 *  ---- command line argument
 *   or  default value
 *   or  last value for optional parameters without default
 * if one of the input pointers is defined store it as last value
 */
static
char *find_value( char *cval,
                 INTEGER *ival,
                 REAL *rval )
{
  char *value = NULL;

  if( next_arg < curr_cmd->argc )
    value = curr_cmd->argv[next_arg];

  if( value == NULL && next_arg < curr_cmd->total ) {
    KmParameter *par = curr_cmd->par[next_arg];
    value = par->dfault;

    /* set last value for optional parameters without default */
    if( value == NULL && next_arg >= curr_cmd->mandatory ) {

      if( cval != NULL )
        value = cval;
      else if( ival != NULL )
        value = strfromi( (int)*ival, 0 );
      else if( rval != NULL )
        value = strfromd( (double)*rval, 0 );

      if( value != NULL ) {
        if( par->last != NULL )
          free( par->last );
        par->last = strdup( value );
      }

      value = par->last;
    }
  }
  return value;
}


void ku_spy( const char *flag )
{
  static int save_next_arg;

  if( strcasecmp( flag, "ON" ) == 0 ) {
    save_next_arg = next_arg;
  }
  else if( strcasecmp( flag, "OFF" ) == 0 ) {
    if( next_arg > save_next_arg )
      next_arg = save_next_arg;
  }
}


void F77_ENTRY_C(Kuspy,chopt)
/* { */
  char *opt = fstrdup( chopt, len_chopt );
  ku_spy( opt );
  free( opt );
}


/*
 * generic KUGETx for character arguments
 */
static
int get_chval( char *chval,
              int len,
              char *(*getx)() )
{
  char *cval = fstrdup( chval, len );
  find_value( cval, NULL, NULL );
  free( cval );
  return fstrset( chval, len, (*getx)() );
}


/*
 * get argument in uppercase
 */
char *ku_getc()
{
  char *value = next_value( "KUGETC" );
  strupper( value );
  if( list_arg != NULL )
    free( list_arg );
  list_arg = str0dup( value );

#ifdef DBMALLOC
  if( list_arg != NULL )
    malloc_mark( list_arg );
#endif

  return value;
}

void F77_ENTRY_CI(Kugetc,chval,length)
/* { */
  *length = get_chval( chval, len_chval, ku_getc );
}


/*
 * get all arguments to end of line
 */
char *ku_gete()
{
  if( next_arg < curr_cmd->argc )
    return curr_cmd->argline + curr_cmd->argoffs[next_arg];
  return next_value( "KUGETE" );
}

void F77_ENTRY_CI(Kugete,chpar,length)
/* { */
  *length = get_chval( chpar, len_chpar, ku_gete );
}


/*
 * get argument with FILECASE conversion
 */
char *ku_getf()
{
  char *value = next_value( "KUGETF" );
  ku_fcase( value );
  if( list_arg != NULL )
    free( list_arg );
  list_arg = str0dup( value );

#ifdef DBMALLOC
  if( list_arg != NULL )
    malloc_mark( list_arg );
#endif

  return value;
}

void F77_ENTRY_CI(Kugetf,chpar,length)
/* { */
  *length = get_chval( chpar, len_chpar, ku_getf );
}


/*
 * get argument without case conversion
 */
char *ku_gets()
{
  char *value = next_value( "KUGETS" );
  if( list_arg != NULL )
    free( list_arg );
  list_arg = str0dup( value );

#ifdef DBMALLOC
  if( list_arg != NULL )
    malloc_mark( list_arg );
#endif

  return value;
}

void F77_ENTRY_CI(Kugets,chpar,length)
/* { */
  *length = get_chval( chpar, len_chpar, ku_gets );
}


/*
 * retrieve the next item from the list loaded with KUGETC or KUGETS
 */
char *ku_getl()
{
  static char *list_this = NULL;

  if( list_this != NULL ) {
    free( list_this );
    list_this = NULL;
  }

  if( list_arg != NULL ) {
    char *p;
    int quoted = 0;
    int paren = 0;
    int comma = 0;

    for( p = list_arg; *p != '\0'; p++ ) {
      /* look for separator outside balanced parentheses or quotes */
      /* \n is used to protect commas in VARARG lists */
      if( *p == '\n' )
        *p = ',';
      else if( *p == '\'' )
        quoted = !quoted;
      else if( !quoted ) {
        if( *p == '(' )
          paren++;
        else if( *p == ')' )
          paren--;
        else if( paren == 0 && *p == ',' ) {
          comma = 1;
          *p++ = '\0';
          break;
        }
      }
    }

    if( comma ) {
      /* pass the first item as argument and remove it from the rest */
      list_this = strdup( list_arg );
      while( *p == ' ' )
        p++;                    /* skip leading blanks */
	memmove( list_arg, p, strlen(p) + 1 );
/*      strcpy( list_arg, p );*/
    }
    else {
      /* pass the complete thing */
      list_this = strdup( list_arg );
      free( list_arg );
      list_arg = NULL;
    }
#ifdef DBMALLOC
    malloc_mark( list_this );
#endif
  }

  return list_this;
}

void F77_ENTRY_CI(Kugetl,chpar,length)
/* { */
  *length = get_chval( chpar, len_chpar, ku_getl );
}


/*
 * get integer argument
 */
int ku_geti()
{
  char *value = strdup( next_value( "KUGETI" ) );
  KmParameter *par = (next_arg > curr_cmd->total)
    ? NULL : curr_cmd->par[next_arg-1];

  while( 1 ) {
    int do_prompt = 0;
    int ival = 0;
    double dval;

    if( ku_math( value, &dval ) == 0 ) {
      ival = dval;

      if( par != NULL ) {
        /* check against parameter definition */
        if( par->type != KmTYPE_INT ) {
          printf( " *** %s: KUGETI called for non-I parameter %s\n",
                 curr_cmd->path, par->name );
        }
        else {
          KmParInt *pnum = (KmParInt*)par->ptype;

          if( pnum->range_lower != NULL && ival < atoi( pnum->range_lower ) ) {
            printf( " *** %s: %s=%d less than lower range value %s\n",
                   curr_cmd->path, par->name, ival, pnum->range_lower );
            do_prompt = 1;
          }
          if( pnum->range_upper != NULL && ival > atoi( pnum->range_upper ) ) {
            printf( " *** %s: %s=%d greater than upper range value %s\n",
                   curr_cmd->path, par->name, ival, pnum->range_upper );
            do_prompt = 1;
          }
          if( par->range_count > 0 ) {
            int found = 0;
            int i;
            for( i = 0; i < par->range_count; i++ ) {
              if( ival == atoi( par->range_value[i] ) ) {
                found =1;
                break;
              }
            }
            if( !found ) {
              printf( " *** %s: %s=%d not in list of allowed values\n",
                   curr_cmd->path, par->name, ival );
              do_prompt = 1;
            }
          }
        }
      }
    }
    else {
      printf( " *** %s: invalid integer number '%s'\n",
             curr_cmd->path, value );
      do_prompt = 1;
    }

    if( do_prompt ) {
      const char *p = prompt_argument( par );
      free( value );
      if( p == NULL )
        return 0;
      value = strdup( p );
    }
    else {
      free( value );
      return ival;
    }
  }
}

void F77_ENTRY_I(Kugeti,ipar)
/* { */
  find_value( NULL, ipar, NULL );
  *ipar = ku_geti();
}


/*
 * get real argument
 */
double ku_getr()
{
  char *value = strdup( next_value( "KUGETR" ) );
  KmParameter *par = (next_arg > curr_cmd->total)
    ? NULL : curr_cmd->par[next_arg-1];

  while( 1 ) {
    int do_prompt = 0;
    double rval;

    if( ku_math( value, &rval ) == 0 ) {

      if( par != NULL ) {
        /* check against parameter definition */
        if( par->type != KmTYPE_REAL ) {
          printf( " *** %s: KUGETR called for non-R parameter %s\n",
                 curr_cmd->path, par->name );
        }
        else {
          KmParReal *pnum = (KmParReal*)par->ptype;

          if( pnum->range_lower != NULL && rval < atof( pnum->range_lower ) ) {
            printf( " *** %s: %s=%f less than lower range value %s\n",
                   curr_cmd->path, par->name, rval, pnum->range_lower );
            do_prompt = 1;
          }
          if( pnum->range_upper != NULL && rval > atof( pnum->range_upper ) ) {
            printf( " *** %s: %s=%f greater than upper range value %s\n",
                   curr_cmd->path, par->name, rval, pnum->range_upper );
            do_prompt = 1;
          }
          if( par->range_count > 0 ) {
            int found = 0;
            int i;
            for( i = 0; i < par->range_count; i++ ) {
              if( rval == atof( par->range_value[i] ) ) {
                found =1;
                break;
              }
            }
            if( !found ) {
              printf( " *** %s: %s=%f not in list of allowed values\n",
                   curr_cmd->path, par->name, rval );
              do_prompt = 1;
            }
          }
        }
      }
    }
    else {
      printf( " *** %s: invalid real number '%s'\n",
             curr_cmd->path, value );
      do_prompt = 1;
    }

    if( do_prompt ) {
      const char *p = prompt_argument( par );
      free( value );
      if( p == NULL )
        return 0;
      value = strdup( p );
    }
    else {
      free( value );
      return rval;
    }
  }
}

void F77_ENTRY_R(Kugetr,rpar)
/* { */
  find_value( NULL, NULL, rpar );
  *rpar = ku_getr();
}


/*
 * return number of arguments in command line
 */
int ku_npar()
{
  return curr_cmd->argc;
}

INTEGER F77_ENTRY(Kunpar)
/* { */
  return ku_npar();
}


/*
 * return command name and number of arguments
 */
void F77_ENTRY_CI(Kupatl,chpath,npar)
/* { */
  fstrset( chpath, len_chpath, curr_cmd->name );
  *npar = ku_npar();
}


/*
 * return command path and number of arguments
 */
void F77_ENTRY_CII(Kupath,chpath,nlev,npar)
/* { */
  char *path = strdup( &curr_cmd->path[1] );
  char *p;

  *nlev = 0;
  for( p = strtok( path, "/" ); p != NULL; p = strtok( NULL, "/" ) ) {
    fstrset( chpath, len_chpath, p );
    chpath += len_chpath;
    *nlev += 1;
  }
  free( path );
  *npar = ku_npar();
}

char *ku_path()
{
  return curr_cmd->path;
}


