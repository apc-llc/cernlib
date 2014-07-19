/*
 * $Id: kalias.c,v 1.2 1998/11/10 13:14:11 couet Exp $
 *
 * $Log: kalias.c,v $
 * Revision 1.2  1998/11/10 13:14:11  couet
 * - Y2K
 *
 * Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/05 01/06/95  16.59.02  by  Gunter Folger*/
/*-- Author :    Alfred Nathaniel   18/11/92*/
/* kalias.c: alias translations */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"


static char* repl_alias( const char* );
static char* sys_cptime(void);
static char* sys_date(void);
static char* sys_rtime(void);
static char* sys_time(void);


/*
 * action routine for /KUIP/ALIAS/CREATE
 */
int kxacreate()
{
  char *name = ku_gets();
  char *value = ku_gets();
  char *option = ku_getc();

  if( name[0] == '\0' || len_alias( name, '\0' ) != strlen( name ) ) {
    ku_alfa();
    printf( " *** Invalid alias name '%s'\n", name );
    return -1;
  }

  kc_alias.translate = 1;       /* translation on */

  if( strchr( option, 'C' ) != NULL ) {
    char *alias = hash_lookup( kc_alias.arg_table, name, NULL );
    if( alias != NULL ) {
      ku_alfa();
      printf( " *** Argument alias '%s' superseded by command alias\n", name );
      hash_remove( kc_alias.arg_table, name );
    }
    hash_insert( kc_alias.cmd_table, name, value, 0 );
  }
  else {
    char *alias = hash_lookup( kc_alias.cmd_table, name, NULL );
    if( alias != NULL ) {
      ku_alfa();
      printf( " *** Command alias '%s' superseded by argument alias\n", name );
      hash_remove( kc_alias.cmd_table, name );
    }
    /* apply alias translations in value unless inhibited by option N */
    if( strchr( option, 'N' ) == NULL ) {
      char *p;
      if( (p = subst_arg_alias( value )) != NULL ) {
        hash_insert( kc_alias.arg_table, name, p, 0 );
        free( p );
        return 0;
      }
      else if( kc_alias.substitutions <= 0 ) {
        /* recursive alias */
        hash_remove( kc_alias.arg_table, name );
        return -1;
      }
    }
    hash_insert( kc_alias.arg_table, name, value, 0 );
  }

  return 0;
}


/*
 * action routine for /KUIP/ALIAS/TRANSLATION
 */
int kxatranslation()
{
  char *option = ku_getc();

  if( strcmp( option, "ON" ) == 0 )
    kc_alias.translate = 1;

  else if( strcmp( option, "OFF" ) == 0 )
    kc_alias.translate = 0;

  else
    printf( " Current alias translation: %s\n",
           kc_alias.translate ? "ON" : "OFF" );

  return 0;
}


/*
 * action routine for /KUIP/ALIAS/DELETE
 */
int kxadelete()
{
  char *pattern = ku_getc();
  int n = hash_entries( kc_alias.cmd_table );

  if( n > 0 ) {
    HashArray *values = hash_array( kc_alias.cmd_table );
    int i;

    for( i = 0; i < n; i++ ) {
      char *name = values[i].name;

      if( ku_match( name, pattern, 1 ) ) {
        hash_remove( kc_alias.cmd_table, name );
      }
    }
    free( (char*)values );
  }

  if( (n = hash_entries( kc_alias.arg_table )) > 0 ) {
    HashArray *values = hash_array( kc_alias.arg_table );
    int i;

    for( i = 0; i < n; i++ ) {
      char *name = values[i].name;

      if( ku_match( name, pattern, 1 ) ) {
        hash_remove( kc_alias.arg_table, name );
      }
    }
    free( (char*)values );
  }

  return 0;
}


/*
 * action routine for /KUIP/ALIAS/LIST
 */
int kxalist()
{
  char *pattern = ku_getc();
  static char *header[] = { "Argument", "Command" };
  HashTable *table[2];
  int k;

  table[0] = kc_alias.arg_table;
  table[1] = kc_alias.cmd_table;

  for( k = 0; k < 2; k++ ) {
    int n = hash_entries( table[k] );
    if( n == 0 ) {
      printf( " No %s aliases defined.\n", header[k] );
    }
    else {
      HashArray *alias = hash_array( table[k] );
      int i;

      printf( " %s aliases:\n", header[k] );
      for( i = 0; i < n; i++ ) {
        char *name = alias[i].name;
        char *value = (char*)alias[i].value;

        if( ku_match( name, pattern, 1 ) ) {
          printf( " %-10s => %s\n", name, value );
        }
      }
      free( (char*)alias );
    }
  }
  return 0;
}


/*
 * return the number of leading characters which are valid for an alias name
 */
int len_alias( const char *line,
              int line_1 )
{
  char *sep = " /,<=>:;.%'()";
  int len = 0;

  /*
   * An alias name must be preceeded (line_1) and followed by one of the
   * separator characters or '\0'.
   */
  if( strchr( sep, line_1 ) != NULL ) {

    while( line[len] != '\0'
          && ( isalnum( line[len] )
              || strchr( "_@-$", line[len] ) != NULL ) ) {
      len++;
    }

    if( strchr( sep, line[len] ) == NULL )
      len = 0;
  }
  return len;
}


/*
 * if line contains a command alias substitute it
 * and return the allocated result
 * otherwise return NULL
 */
char *subst_cmd_alias( line )
     char *line;
{
  if( kc_alias.translate && kc_alias.substitutions > 0 ) {
    char *cmd;
    char *args;
    char *p;

    while( *line == ' ' )
      line++;

    cmd = strdup( line );

    /* separate command name from argument list  */
    if( (p = strchr( cmd, ' ' )) != NULL ) {
      *p = '\0';
      for( args = p + 1; *args == ' '; )
        args++;
    }
    else
      args = "";

    if( (p = hash_lookup( kc_alias.cmd_table, cmd, NULL )) != NULL
       || (p = hash_lookup( kc_alias.arg_table, cmd, NULL )) != NULL ) {

      char *new_line = strdup( p );

      if( *args != '\0' )
        new_line = mstr2cat( new_line, " ", args );

      if( --kc_alias.substitutions <= 0 ) {
        ku_alfa();
        printf( " *** Recursive command alias in %s\n", new_line );
      }

      free( cmd );
      return new_line;
    }
    free( cmd );
  }
  return NULL;
}


/*
 * if line contains argument aliases substitute them
 * and return the allocated result
 * otherwise return NULL
 */
char *subst_arg_alias( line )
     char *line;
{
  char *p;
  if( kc_alias.translate
     && (p = repl_alias( line )) != NULL ) {
    char *new_line = p;

    kc_alias.substitutions = 100;

    while( --kc_alias.substitutions > 0 ) {

      if( (p = repl_alias( new_line )) == NULL )
        return new_line;

      free( new_line );
      new_line = p;
    }
    ku_alfa();
    printf( " *** Recursive argument alias in %s\n", line );
    free( new_line );
  }
  return NULL;
}


static
char *repl_alias( const char *line )
{
  const char *p;

  for( p = line; *p != '\0'; p++ ) {
    int len = len_alias( p, (p == line) ? '\0' : p[-1] );

    /* isolate the possible alias name */
    if( len > 0 ) {
      char *name = strndup( p, len );
      char *value = hash_lookup( kc_alias.arg_table, name, NULL );

      free( name );
      if( value != NULL ) {
        /* substitute the alias name by its value */
        int lhead = p - line;

        /* contract ABC//DEF to ABCDEF in case that is an alias */
        if( lhead > 2 && p[-1] == '/' && p[-2] == '/'
           && p[-3] != '\'' && p[-3] != ' ' )
          lhead -= 2;

        return mstr2cat( strndup( line, lhead ), value, p + len );
      }
      p += len - 1;
    }
    else if( *p == '\'' ) {
      /* skip until end of quoted string */
      while( *++p != '\'' ) {
        if( *p == '\0' )
          return NULL;
      }
    }
  }
  return NULL;
}


/*
 * return value of a variable "[var]"
 */
char *var_value( const char *var )
{
  int n = strlen( var );
  char *name = strndup( var + 1, n - 2 );
  char *value;
  char *p;

  if( name[0] == '%' ) {
    /* replace "[%var]" by "[[var]]" */
    name[0] = '[';
    name = mstrcat( name, "]" );
  }

  if( (p = subst_var_alias( name )) != NULL ) {
    free( name );
    name = p;
  }

  value = hash_lookup( kc_alias.var_table, name, NULL );

  if( value == NULL ) {
    int i;
    for( i = 0; name[i] != '\0'; i++ ) {
      if( !isdigit( name[i] ) ) {
        free( name );
        return strdup( var );
      }
    }
    /* undefined "[n]" are replaced by "' '" */
    value = " ";
  }

  free( name );

  return strdup( value );
}


char *subst_var_alias( line )
     char *line;
{
  static int subst_calls=0;
  char *p;

  subst_calls++;
  if ( subst_calls > 100 ) {
     ku_alfa();
     printf(" Recursive variable assignment stopped in line \n %s\n",line);
     return(NULL);
  }

  if( (p = repl_variable( line )) != NULL ) {
    char *new_line = p;

    while( (p = repl_variable( new_line )) != NULL ) {
      free( new_line );
      new_line = p;
    }
    subst_calls--;
    return new_line;
  }
  subst_calls--;
  return NULL;
}


/*
 * replace a variable inside line by its value
 */
char *repl_variable( line )
     char *line;
{
  char *p;

  for( p = line; *p != '\0'; p++ ) {

    if( *p == '[' ) {
      int paren = 1;
      char *name_end = p + 1;

      while( *name_end != '\0' ) {

        if( *name_end == '[' ) {
          paren++;
        }
        else if( *name_end == ']' ) {

          if( --paren == 0 ) {
            int len = name_end - p + 1;
            char *name = strndup( p, len );
            char *value = var_value( name );

            if( strcmp( name, value ) != 0 ) {
              char *new_line = strndup( line, p - line );
              value = quote_string( value, 0 );
              new_line = mstr2cat( new_line, value, p + len );
              free( name );
              free( value );
              return new_line;
            }
            free( name );
            free( value );
            break;
          }
        }
        name_end++;
      }
    }
    else if( *p == '\'' ) {
      /* skip until end of quoted string */
      while( *++p != '\'' ) {
        if( *p == '\0' )
          return NULL;
      }
    }
  }
  return NULL;
}


/*
 * put quotes around string and duplicate quotes inside string
 * input string is supposed to be allocated
 * if quote flag is set the string is quoted anyway
 * otherwise it is quoted only if necessary
 */
char *quote_string( char *string,
                   int quote )
{
  if( string[0] == '\0' ) {
    free( string );
    return strdup( "''" );
  }

  else {
    int n;

    if( string[0] == '\'' && (n = strlen( string )) > 1
       && string[n-1] == '\'' )
      return string;
  }

  if( !quote )
    quote = (strpbrk( string, " '" ) != NULL);

  if( !quote )
    return string;

  else {
    char *line = strdup( "'" );
    char *p;

    for( p = string; *p != '\0'; p++ ) {
      if( *p == '\'' )
        line = mstrcat( line, "''" );
      else
        line = mstrccat( line, *p, 1 );
    }
    line = mstrcat( line, "'" );
    free( string );
    return line;
  }
}


enum SysFunCode {
  SysANUM,
  SysARGS,
  SysCPTIME,
  SysDATE,
  SysKEYNUM,
  SysKEYVAL,
  SysLAST,
  SysMACHINE,
  SysNUMVEC,
  SysOS,
  SysPID,
  SysRTIME,
  SysSTYLE,
  SysTIME
};

static struct sysfun {
  enum SysFunCode code;
  char *name;
} sysfun[] = {
  { SysANUM,      "ANUM"     },
  { SysARGS,      "ARGS"     },
  { SysCPTIME,    "CPTIME"   },
  { SysDATE,      "DATE"     },
  { SysKEYNUM,    "KEYNUM"   },
  { SysKEYVAL,    "KEYVAL"   },
  { SysLAST,      "LAST"     },
  { SysMACHINE,   "MACHINE"  },
  { SysNUMVEC,    "NUMVEC"   },
  { SysOS,        "OS"       },
  { SysPID,       "PID"      },
  { SysRTIME,     "RTIME"    },
  { SysSTYLE,     "STYLE"    },
  { SysTIME,      "TIME"     }
};

/*
 * substitute system function
 * if eval flag is set functions with arguments are sent to expression parser
 */
char *subst_sysfun( line, eval )
     char *line;
     int eval;
{
  char *p;
  if( (p = repl_sysfun( line, eval )) != NULL ) {
    char *new_line = p;

    while( (p = repl_sysfun( new_line, eval )) != NULL ) {
      free( new_line );
      new_line = p;
    }
    return new_line;
  }
  return NULL;
}


/*
 * return the number of leading characters which are valid for a function name
 * line does not include the leading '$'
 */
int len_sysfun( const char *line )
{
  int len = 0;

  if( !isdigit( line[0] ) ) {
    while( line[len] != '\0'
          && ( isalnum( line[len] )
              || strchr( "_", line[len] ) != NULL ) ) {
      len++;
    }
  }
  return len;
}


char *repl_sysfun( line, eval )
     char *line;
     int eval;
{
  char *p;

  for( p = line; *p != '\0'; p++ ) {

    if( *p == '$' ) {
      int len = len_sysfun( p + 1 );

      /* protect against DISK$USER:[dir] etc. */
      if( len > 0 && p[len+1] != ':' ) {
        int i;
        char *value = NULL;

        /* test for system functions without arguments */
        for( i = 0; i < (sizeof sysfun) / sizeof( struct sysfun ); i++ ) {

          if( len == strlen( sysfun[i].name )
             && strncasecmp( p + 1, sysfun[i].name, len ) == 0 ) {

            switch( sysfun[i].code ) {

            case SysANUM:       /* number of aliases */
              value = malloc( 16 );
              sprintf( value, "%d", hash_entries( kc_alias.arg_table ) );
              break;

            case SysARGS:       /* program arguments */
              value = strdup( k_getar() );
              break;

            case SysCPTIME:     /* CPU time since last call */
              value = sys_cptime();
              break;

            case SysDATE:       /* current date */
              value = sys_date();
              break;

            case SysKEYNUM:
              value = strdup( (kc_window.panel_keynum != NULL)
                             ? kc_window.panel_keynum : "0.00" );
              break;

            case SysKEYVAL:
              value = strdup( (kc_window.panel_keyval != NULL)
                             ? kc_window.panel_keyval : "' '" );
              break;

            case SysLAST:
              if( kc_flags.last_cmd == NULL )
                value = strdup( "" );
              else
                value = strdup( kc_flags.last_cmd );
              /* avoid infinite recursion if last command contains $LAST */
              value = quote_string( value, 1 );
              break;

            case SysMACHINE:
              value = strdup( MACHINE_NAME );
              break;

            case SysNUMVEC:
              value = malloc( 16 );
              sprintf( value, "%d", (int)Kcvect.NUMVEC - 1 );
              break;

            case SysOS:
              value = strdup( OS_NAME );
              break;

            case SysPID:
              value = malloc( 16 );
              sprintf( value, "%d", (int)getpid() );
              break;

            case SysRTIME:      /* real time since last call */
              value = sys_rtime();
              break;

            case SysSTYLE:
              value = strdup( style_name( kc_flags.style ) );
              break;

            case SysTIME:
              value = sys_time();
              break;
            }
          }
        }

        /* test for system functions with arguments */
        if( value == NULL && eval && p[len+1] == '(' ) {
          int arglen = match_paren( p );

          if( arglen > 0 ) {
            char *expr = strndup( p, arglen );
            int is_eval = (strncasecmp( p + 1, "EVAL", len ) == 0);
            int is_unquote = (strncasecmp( p + 1, "UNQUOTE", len ) == 0);

            if( is_eval || is_unquote ) {
              expr[arglen-1] = '\0';
              expr = strcpy( expr, expr + len + 2 );
            }
            value = ku_eval( expr );
            free( expr );
            if( value != NULL ) {
              if( !is_unquote )
                value = quote_string( value, 0 );
              len = arglen - 1;
            }
          }
        }

        if( value == NULL && kc_flags.try_getenv ) {
          /* try $var as environment variable */
          char *name = strndup( p + 1, len );
          char *env = getenv( name );
          /*
           * Note that on VMS getenv searches uppercase names first in the
           * logical names tables and then in the symbol table.
           * Lowercase names are searched for only in the symbol table.
           * Ignore value starting with $ or @ which are most probable
           * command abbreviations.
           */
          free( name );
#ifdef vms
          if( env != NULL && env[0] != '$' && env[0] != '@' )
            value = strdup( env );
#else
          if( env != NULL )
            value = strdup( env );
#endif
        }
        if( value != NULL ) {
          char *new_line = strndup( line, p - line );
          new_line = mstr2cat( new_line, value, p + len + 1 );
          free( value );
          return new_line;
        }
      }
    }
    else if( *p == '\'' ) {
      while( *++p != '\'' ) {
        if( *p == '\0' )
          return NULL;
      }
    }
  }
  return NULL;
}


/*
 * find the distance to the next balanced ')'
 */
int match_paren( const char *line )
{
  int len;
  int paren = 0;

  for( len = 0; line[len] != '\0'; len++ ) {

    switch( line[len] ) {

    case '(':
      paren++;
      break;

    case ')':
      paren--;
      if( paren == 0 )
        return len + 1;
      break;

    case '\'':
      while( line[++len] != '\'' ) {
        if( line[len] == '\0' )
          return 0;
      }
      break;

    }
  }
  return 0;
}


/*
 * return value for $CPTIME
 */
static
char *sys_cptime()
{
  static int initialized = 0;
  static clock_t time0;
  clock_t time_now;
  char *value = malloc( 16 );

  if( !initialized ) {
    initialized = 1;
    time0 = clock();
  }
  time_now = clock();
  sprintf( value, "%.3f", (double)(time_now - time0) / CLOCKS_PER_SEC );
  time0 = time_now;
  return value;
}


/*
 * return value for $RTIME
 */
static
char *sys_rtime()
{
  static int initialized = 0;
  static time_t time0;
  time_t time_now;
  char *value = malloc( 16 );

  if( !initialized ) {
    initialized = 1;
    time0 = time( NULL );
  }
  time_now = time( NULL );
  sprintf( value, "%d", (int)difftime( time_now, time0) );
  time0 = time_now;
  return value;
}


/*
 * return value of $DATE
 */
static
char *sys_date()
{
  time_t time_now = time( NULL );
  struct tm *tms = localtime( &time_now );
  char *value = malloc( 16 );

  sprintf( value, "%s/%s/%s",
          strfromi( tms->tm_mday, 2 ), strfromi( tms->tm_mon + 1, 2 ),
          strfromi( tms->tm_year+1900, 4 ) );
  return value;
}


/*
 * return value of $TIME
 */
static
char *sys_time()
{
  time_t time_now = time( NULL );
  struct tm *tms = localtime( &time_now );
  char *value = malloc( 16 );

  sprintf( value, "%s.%s.%s",
          strfromi( tms->tm_hour, 2), strfromi( tms->tm_min, 2 ),
          strfromi( tms->tm_sec, 2 ) );
  return value;
}


/*
 * return commands to restore current environment
 */
void F77_ENTRY_C(Kuqenv,chline)
/* { */
  static char **env = NULL;
  static int next = 0;
  char *line = fstr0dup( chline, len_chline );

  if( line == NULL ) {
    env = ku_qenv();
    next = 0;
  }
  else
    free( line );

  if( env == NULL || env[next] == NULL )
    fstrset( chline, len_chline, "" );
  else
    fstrset( chline, len_chline, env[next++] );
}


char **ku_qenv()
{
  static char **env = NULL;
  char numbers[64];
  int nenv = 0;
  int k;

  if( env != NULL ) {
    int n;
    for( n = 0; env[n] != NULL; n++ )
      free( env[n] );
    free( (char*)env );
  }
  env = (char**)malloc( 64 * sizeof(char*) );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/BREAK '",
                        kc_value.set_break, "'" );

  env[nenv++] = str2dup( "/KUIP/SET_SHOW/COLUMNS ",
                        kc_value.set_columns );

  if( kc_value.set_command != NULL ) {
    env[nenv++] = str3dup( "/KUIP/SET_SHOW/COMMAND '",
                          kc_value.set_command, "'" );
  }

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/DOLLAR '",
                        kc_value.set_dollar, "'" );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/FILECASE '",
                        kc_value.set_filecase, "'" );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/HOST_EDITOR '",
                        kc_value.set_host_editor, "'" );

  sprintf( numbers, "%d %d %d %d",
          kc_window.edit_pad.top,
          kc_window.edit_pad.left,
          kc_window.edit_pad.width,
          kc_window.edit_pad.height );
  env[nenv++] = str2dup( "/KUIP/SET_SHOW/HOST_EDITOR 'WINDOW' ", numbers );

  sprintf( numbers, "%d %d %d %d %d %d %d",
          kc_window.help_pad.top,
          kc_window.help_pad.left,
          kc_window.help_pad.width,
          kc_window.help_pad.height,
          kc_window.hoffset,
          kc_window.voffset,
          kc_window.shift_max );
  env[nenv++] = str2dup( "/KUIP/SET_SHOW/HOST_EDITOR 'PAD' ", numbers );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/HOST_PAGER '",
                        kc_value.set_host_pager, "'" );

  if( kc_value.set_print_cmds != NULL ) {
    int i;
    for( i = 0; kc_value.set_print_cmds[i] != NULL; i++ ) {
      env[nenv++] = str4dup( "/KUIP/SET_SHOW/HOST_PRINTER '",
                            kc_value.set_print_cmds[i], "' ",
                            kc_value.set_print_exts[i] );
    }
  }

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/HOST_PSVIEWER '",
                        kc_value.set_host_psviewer, "'" );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/HOST_SHELL '",
                        kc_value.set_host_shell, "'" );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/PROMPT '",
                        kc_value.set_prompt, "'" );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/RECALL_STYLE '",
                        kc_value.set_recall_style, "'" );

  env[nenv++] = str2dup( "/KUIP/SET_SHOW/RECORDING ",
                        kc_value.set_recording );

  env[nenv++] = str2dup( "/KUIP/SET_SHOW/ROOT ",
                        kc_value.set_root );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/STYLE '",
                        kc_value.set_style, "'" );

  env[nenv++] = str3dup( "/KUIP/SET_SHOW/TIMING '",
                        kc_value.set_timing, "'" );

  env[nenv++] = str2dup( "/KUIP/HELP ",
                        kc_value.help_edit );

  env[nenv++] = str4dup( "/MACRO/DEFAULTS '",
                        kc_value.defaults_path, "' ",
                        kc_value.defaults_order );

  for( k = 0; k < 2; k++ ) {
    HashArray *alias;
    int nalias;
    char *opt;
    int i;

    if( k == 0 ) {
      nalias = hash_entries( kc_alias.arg_table );
      alias = hash_array( kc_alias.arg_table );
      opt = "'";
    }
    else {
      nalias = hash_entries( kc_alias.cmd_table );
      alias = hash_array( kc_alias.cmd_table );
      opt = "' 'C'";
    }

    if( nalias > 0 ) {
      env = (char**)realloc( (char*)env, (nenv + nalias + 1) * sizeof(char*) );

      for( i = 0; i < nalias; i++, nenv++ ) {
        char *value = strdup( (char*)alias[i].value );
        char *p;

        env[nenv] = str3dup( "/KUIP/ALIAS/CREATE '", alias[i].name, "' '" );
        while( (p = strchr( value, '\'' )) != NULL ) {
          *p = '\0';
          env[nenv] = mstr2cat( env[nenv], value, "''" );
          strcpy( value, p + 1 );
        }
        env[nenv] = mstr2cat( env[nenv], value, opt );
        free( value );
      }
      free( (char*)alias );
    }
  }
  env[nenv] = NULL;
  return env;
}
