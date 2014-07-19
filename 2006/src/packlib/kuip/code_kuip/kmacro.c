/*
 * $Id: kmacro.c,v 1.2 1996/04/11 09:43:26 cernlib Exp $
 *
 * $Log: kmacro.c,v $
 * Revision 1.2  1996/04/11 09:43:26  cernlib
 * Use memmove to move a string in next_statement
 *
 * Revision 1.1.1.1  1996/03/08 15:32:57  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/06 14/06/95  15.33.23  by  Gunter Folger*/
/*-- Author :    Alfred Nathaniel   13/04/94*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"
#include "kuip/kmacro.h"

#define MAX_KUMAC_TABLE 97

static HashTable *kumac_table;
static HashTable *orphan_table;
static int        orphan_count;

#define MAX_VARIABLE_TABLE 97

#define MAX_EXEC_STACK 100

static ExecStack  exec_stack[MAX_EXEC_STACK];
static int        exec_stack_ptr;

static NestStack *nest_stack;
static int        nest_stack_max;
static int        nest_stack_ptr;

static char  *kumac_path;
static int    line_count;
static int    label_count;
static char  *full_line;
static char  *stmt_arg1;
static char  *stmt_arg2;
static char  *stmt_arg3;
static char  *stmt_arg4;


/*
 * return the number of leading characters which are valid for a variable name
 */
int len_variable( line )
     char *line;
{
  int len = 0;

  if( isdigit( line[0] ) ) {
    while( isdigit( line[len] ) ) {
      len++;
    }
  }
  else {
    while( isalnum( line[len] ) || line[len] == '_' || line[len] == '$' ) {
      len++;
    }
  }

  return len;
}


static
void internal_error( msg )
     char *msg;
{
  ku_alfa();
  printf( " *** Internal error: %s - Call KUIP maintenance\n", msg );
}


static
void print_error( msg, arg )
     char *msg;
     char *arg;
{
  ku_alfa();
  printf( " *** %s in %s at line %d:\n     ",
         msg[0] == '>' ? "Warning" : "Error",
         kumac_path, line_count );
  printf( msg[0] == '>' ? msg + 1 : msg, arg );
  printf( "\n" );
}


static
void nest_error( tos, found )
     NestStack *tos;
     char *found;
{
  char fmt[80];
  char *expect;

  switch( tos->stmt ) {

  case CASE_Statement:
  case CASE_Label:
    expect = "(label) or ENDCASE";
    break;

  case DO_Statement:
    expect = "ENDDO";
    break;

  case FOR_Statement:
    expect = "ENDFOR";
    break;

  case REPEAT_Statement:
    expect = "UNTIL";
    break;

  case WHILE_Statement:
    expect = "ENDWHILE";
    break;

  case IF_THEN_Statement:
    expect = "ELSEIF, ELSE, or ENDIF";
    break;

  case ELSE_Statement:
    expect = "ENDIF";
    break;

  default:
    internal_error( "Unexpected statement code on nest stack" );
    return;
    break;
  }

  sprintf( fmt, "%s found where %s was expected", found, expect );
  print_error( fmt, NULL );
}


static
void too_much( stmt )
     char *stmt;
{
  print_error( ">Additional text at end of %s statement ignored", stmt );
}


static
int op_label( tos, user_label, aux_label )
     ExecStack *tos;
     char *user_label;
     char *aux_label;
{
  if( kc_flags.trace_debug ) {
    ku_alfa();
    if( user_label != NULL ) {
      printf( " ###### %s:\n", user_label );
    }
    else {
      printf( " ###### $%s:\n", aux_label );
    }
  }

  return tos->pc + 1;
}


static
int op_source( tos, line_number, source_line )
     ExecStack *tos;
     char *line_number;
     char *source_line;
{
  if( kc_flags.trace_on ) {
    ku_alfa();
    printf( "%2d.%s: %s\n", exec_stack_ptr, line_number, source_line );
  }

  if( kc_flags.appl_stream != NULL ) {
    /* strip leading blanks from the APPLICATION terminator */
    char *p = source_line;
    while( p[0] == ' ' )
      p++;
    if( strcasecmp( p, kc_flags.appl_exit ) != 0 )
      p = source_line;
    fprintf( kc_flags.appl_stream, "%s\n", p );
  }

  tos->line_number = line_number;
  tos->source_line = source_line;

  return tos->pc + 1;
}


static
int find_label( macro, label, which )
     KumacMacro *macro;
     char *label;
     int which;
{
  KumacCode *code = macro->code;
  int n = which < 0 ? 0 : which;
  int pc;

  for( pc = 0; pc < macro->ncode; pc++ ) {
    if( code[pc].op == op_label && code[pc].arg[n] != NULL
       && strcasecmp( code[pc].arg[n], label ) == 0 ) {
      return pc;
    }
  }

  if( which >= 0 ) {
    ku_alfa();
    printf( " *** Label %s not found\n", label );
  }
  return -1;
}


static
void swap_source( macro )
     KumacMacro *macro;
{
  KumacCode source;
  int pc = macro->ncode;

  while( macro->code[pc-1].op != op_source ) {
    pc--;
  }
  source = macro->code[pc-1];

  while( pc < macro->ncode ) {
    macro->code[pc-1] = macro->code[pc];
    pc++;
  }
  macro->code[pc-1] = source;
}


static
int op_application( tos, application_name, null2 )
     ExecStack *tos;
     char *application_name;
     char *null2;
{
  fclose( kc_flags.appl_stream );
  kc_flags.appl_stream = NULL;

  /* Fortran OPEN the file and call application */
  ku_open( kc_flags.appl_luno, kc_flags.appl_file, "OLD" );
  kc_flags.appl_called = 1;
  ku_exel( application_name );
  kc_flags.appl_called = 0;
  ku_close( kc_flags.appl_luno );
  remove( kc_flags.appl_file );

  ku_sapp( "", "" );

  return tos->pc + 1;
}


static
int op_bool( tos, expression, null2 )
     ExecStack *tos;
     char *expression;
     char *null2;
{
  int value = ku_bool( expression );

  if( kc_flags.trace_debug ) {
    ku_alfa();
    printf( " ###### Bool %s --> %d\n",
           expression, value );
  }

  if( value == -1 ) {
    ku_alfa();
    printf( " *** Invalid boolean expression in file %s\n",
           tos->file->path );
    printf( "%s: %s\n", tos->line_number, tos->source_line );
    return -1;
  }

  if( value == 1 )
    return tos->pc + 2;

  return tos->pc + 1;
}


void store_variable( ExecStack* tos, const char* name, const char* value )
{
  if( tos == NULL && kc_flags.in_macro )
    tos = &exec_stack[exec_stack_ptr-1];

  if( tos != NULL ) {
    int is_global = 0;

    if( hash_lookup( tos->variables, name, &is_global ) != NULL
       && is_global ) {
      hash_insert( kc_alias.global_value, name, value, 0 );
    }
    hash_insert( tos->variables, name, value, is_global );
  }
  else
    hash_insert( kc_alias.global_value, name, value, 0 );
}


static
int op_command( tos, command_line, status_test )
     ExecStack *tos;
     char *command_line;
     char *status_test;
{
  int status = ku_exel( command_line );

  if( status == KUMAC_UNWIND ) {
    tos->status = KUMAC_UNWIND;
    return -1;
  }

  if( status_test == NULL ) {

    if( status != 0 ) {

      switch( tos->on_error_stmt ) {

      case ON_ERROR_CONTINUE:
        break;

      case ON_ERROR_GOTO:
        return find_label( tos->macro, tos->on_error_arg, 0 );
        break;

      case ON_ERROR_EXITM:
        if( tos->on_error_arg != NULL ) {
          char *value = ku_expr( tos->on_error_arg );
          store_variable( tos, "@", value );
          free( value );
        }
        return -1;
        break;

      case ON_ERROR_STOPM:
        tos->status = KUMAC_UNWIND;
        return -1;
        break;

      default:
        internal_error( "Unexpected ON ERROR statement" );
        tos->status = KUMAC_UNWIND;
        return -1;
        break;
      }
    }
  }
  else if( ( status_test[0] == '&' && status != 0 )
          || ( status_test[0] == '!' && status == 0 ) ) {
    /*
     * skip to next source line
     */
    KumacMacro *macro = tos->macro;
    int pc;

    for( pc = tos->pc + 1; pc < macro->ncode; pc++ ) {
      if( macro->code[pc].op == op_source ) {
        return pc;
      }
    }

    return -1;
  }

  return tos->pc + 1;
}


static
int op_expr( ExecStack *tos,
            const char *lhs,
            const char *expression )
{
  char *name = strdup( lhs );
  char *value = ku_expr( expression );
  char *p;

  if( (p = subst_var_alias( name )) != NULL ) {
    /*
     * array element can then be assign as "x_[i] = ..."
     */
    free( name );
    name = p;
  }

  if( kc_flags.trace_debug ) {
    ku_alfa();
    printf( " ###### Expr %s = %s --> %s\n",
           name, expression, value );
  }

  store_variable( tos, name, value );
  free( name );
  free( value );

  return tos->pc + 1;
}


static
int op_forlist( tos, var, list )
     ExecStack *tos;
     char *var;
     char *list;
{
  char *dlist = strdup( list );
  char *elist = strdup( "" );
  char *p;

  for( p = strqtok( dlist ); p != NULL; p = strqtok( NULL ) ) {

    if( strcmp( p, "[*]" ) == 0 ) {
      int n = atoi( hash_lookup( tos->variables, "#", NULL ) );
      int i;

      for( i = 0; i < n; i++ ) {
        char buf[8];
        sprintf( buf, " [%d]", i + 1 );
        elist = mstrcat( elist, buf );
      }
    }
    else {
      elist = mstr2cat( elist, " ", p );
    }
  }
  store_variable( tos, var, elist );
  free( dlist );
  free( elist );

  return tos->pc + 1;
}


static
int op_fornext( tos, loop_var, list_var )
     ExecStack *tos;
     char *loop_var;
     char *list_var;
{
  char *list = strdup( hash_lookup( tos->variables, list_var, NULL ) );
  char *head = strqtok( list );
  char *tail;
  char *value;
  char *p;

  if( head == NULL ) {
    free( list );
    return tos->pc + 1;
  }
  else {
    value = ku_expr( head );
    store_variable( tos, loop_var, value );
    free( value );

    tail = strdup( "" );
    while( (p = strqtok( NULL )) != NULL ) {
      tail = mstr2cat( tail, " ", p );
    }
    store_variable( tos, list_var, tail );
    free( tail );
  }
  return tos->pc + 2;
}


static
int op_goto( tos, user_label, aux_label )
     ExecStack *tos;
     char *user_label;
     char *aux_label;
{
  if( user_label != NULL ) {
    char *label = ku_expr( user_label );
    int pc = find_label( tos->macro, label, 0 );

    if( kc_flags.trace_debug ) {
      ku_alfa();
      printf( " ###### Goto %s --> %s\n", user_label, label );
    }

    free( label );
    return pc;
  }
  else {
    if( kc_flags.trace_debug ) {
      ku_alfa();
      printf( " ###### Goto $%s\n", aux_label );
    }

    return find_label( tos->macro, aux_label, 1 );
  }
}


static
char *arg_value( value, in_parm  )
     char *value;
     int in_parm;
{
  char *p;
  int n = strlen( value );

  if( value[0] == '\'' && value[n-1] == '\'' ) {
    int fixed = 0;
    int i;

    for( i = 1; i < n - 1; i++ ) {

      if( value[i] == '\'' ) {

        if( i < n - 2 && value[i+1] == '\'' ) {
          i++;
        }
        else {
          char *nvalue = strndup( value, i + 1 );
          nvalue = mstr2cat( nvalue, "'", value + i + 1 );
          free( value );
          value = nvalue;
          n++;
          i++;
          fixed = 1;
        }
      }
    }

    if( fixed ) {
      ExecStack *tos;
      if( in_parm ) {
        tos = &exec_stack[exec_stack_ptr-1];
      }
      else if( exec_stack_ptr > 1 ) {
        tos = &exec_stack[exec_stack_ptr-2];
      }
      else {
        tos = NULL;
      }
      if( tos == NULL ) {
        printf( " *** Warning in EXEC command:\n" );
      }
      else {
        printf( " *** Warning in %s at line %s:\n",
               tos->file->path, tos->line_number );
      }
      printf( "     Old style use of quotes in macro argument fixed to %s\n",
             value );
    }
  }

  kc_alias.substitutions = 100; /* protect against recursive aliases */

  if( (p = subst_var_alias( value )) != NULL ) {
    free( value );
    value = p;
  }

  if( (p = subst_arg_alias( value )) != NULL ) {
    free( value );
    value = p;
  }

  if( (p = subst_sysfun( value, 1 )) != NULL ) {
    free( value );
    value = p;
  }

  return clean_word( value );
}


static
int op_macro( tos, parameter_list, null2 )
     ExecStack *tos;
     char *parameter_list;
     char *null2;
{
  char **equiv = (char**)malloc( sizeof(char*) );
  char *line;
  char *tok;
  char buf[8];
  int npar = 0;
  int narg = 0;
  int i;

  if( parameter_list != NULL ) {
    /*
     * assign the variables defined in the macro header
     */
    line = strdup( parameter_list );

    for( tok = strqtok( line ); tok != NULL; tok = strqtok( NULL ) ) {
      char buf[8];
      char *name = tok;
      char *value;
      char *p;
      int n;

      if( (p = strchr( name, '=' )) != NULL ) {
        /* "var=value" */
        *p = '\0';
      }

      /* allow "[1]" instead of "1" */
      n = strlen( name );
      if( name[0] == '[' && name[n-1] == ']' ) {
        name[n-1] = '\0';
        strcpy( name, name + 1 );
      }

      /*
       * enter named and numbered variables and keep equivalence between them
       */
      if( strlen( name ) != len_variable( name ) ) {
        ku_alfa();
        printf( " *** Invalid name for macro parameter [%d] in file %s\n",
               npar + 1, tos->file->path );
        printf( "%s: %s\n", tos->line_number, tos->source_line );

        for( i = 0; i < npar; i++ ) {
          free( equiv[i] );
        }
        free( (char*)equiv );
        free( line );

        return -1;
      }

      if( p != NULL ) {
        /* "var=value" */
        value = arg_value( strdup( p + 1 ), 1 );
      }
      else {
        /* variable name with empty initial value */
        value = strdup( "" );
      }
      hash_insert( tos->variables, name, value, 0 );
      sprintf( buf, "%d", ++npar );
      hash_insert( tos->variables, buf, value, 0 );
      free( value );

      equiv = (char**)realloc( (char*)equiv, npar * sizeof(char*) );
      equiv[npar-1] = strdup( name );
    }

    free( line );
  }

  if( tos->argline != NULL ) {
    /*
     * assign variable from EXEC argument list
     */
    int named = 0;

    line = strdup( tos->argline );

    for( tok = strqtok( line ); tok != NULL; tok = strqtok( NULL ) ) {
      char *name;
      char *value;
      int n = len_variable( tok );

      if( n > 0 && tok[n] == '=' ) {
        /* "name=value" */
        named = 1;
        tok[n] = '\0';
        name = tok;

        /*
         * for "name=!" we don't want to overwrite the MACRO default
         */
        if( strcmp( tok + n + 1, "!" ) == 0 ) {
          value = hash_lookup( tos->variables, name, NULL );
          if( value == NULL )
            value = strdup( "!" );
          else
            value = strdup( value );
        }
        else {
          value = arg_value( strdup( tok + n + 1 ), 0 );
        }

        hash_insert( tos->variables, name, value, 0 );

        /*
         * If the name is defined as macro parameter we also have to set the
         * corresponding numbered variable.
         */
        for( i = 0; i < npar; i++ ) {
          if( strcasecmp( equiv[i], name ) == 0 ) {
            sprintf( buf, "%d", i + 1 );
            hash_insert( tos->variables, buf, value, 0 );
            break;
          }
        }
      }
      else {
        /* unnamed value */

        value = arg_value( strdup( tok ), 0 );

        if( named ) {
          /*
           * If we have unnamed values following named values then we assign
           * them to numbered variables beyond those defined in the macro
           * header.
           */
          if( narg < npar ) {
            narg = npar;
          }
        }
        else if( narg < npar ) {
          /*
           * We alse have to set the named variable corresponding to the
           * numbered variable.
           */
          name = equiv[narg];
          if( strcmp( tok, "!" ) == 0 ) {
            free( value );
            value = strdup( hash_lookup( tos->variables, name, NULL ) );
          }
          else {
            hash_insert( tos->variables, name, value, 0 );
          }
        }

        sprintf( buf, "%d", ++narg );
        hash_insert( tos->variables, buf, value, 0 );
      }
      free( value );
    }
    free( line );
  }

  for( i = 0; i < npar; i++ ) {
    free( equiv[i] );
  }
  free( (char*)equiv );

  if( narg < npar ) {
    narg = npar;
  }

  sprintf( buf, "%d", narg );
  hash_insert( tos->variables, "#", buf, 0 );

  if( narg > 0 ) {
    line = strdup( hash_lookup( tos->variables, "1", NULL ) );
    for( i = 1; i < narg; i++ ) {
      sprintf( buf, "%d", i + 1 );
      line = mstr2cat( line, " ", hash_lookup( tos->variables, buf, NULL ) );
    }
    hash_insert( tos->variables, "*", line, 0 );
    free( line );
  }
  else {
    hash_insert( tos->variables, "*", "", 0 );
  }

  hash_insert( tos->variables, "@", "0", 1 );
  hash_insert( kc_alias.global_value, "@", "0", 0 );

  line = str3dup( tos->file->path, "#", tos->macro->name );
  hash_insert( tos->variables, "0", line, 0 );
  free( line );

  return tos->pc + 1;
}


int ku_match( const char *string,
             const char *pattern,
             int ignore_case )
{
  if( pattern[0] == '*' ) {

    while( ku_match( string, pattern + 1, ignore_case ) == 0 ) {

      if( string[0] == '\0' )
        return 0;
      else
        string++;
    }
    return 1;
  }
  else {
    int p0 = pattern[0];
    int s0 = string[0];

    if( ignore_case ) {
      if( isupper( p0 ) )
        p0 = tolower( p0 );
      if( isupper( s0 ) )
        s0 = tolower( s0 );
    }

    if( p0 == s0 ) {

      if( pattern[0] == '\0' )
        return 1;

      return ku_match( string + 1, pattern + 1, ignore_case );
    }
  }
  return 0;
}


static
int op_match( tos, variable, pattern_list )
     ExecStack *tos;
     char *variable;
     char *pattern_list;
{
  char *value = hash_lookup( tos->variables, variable, NULL );
  char *pattern = strdup( pattern_list );
  char *p;

  for( p = strtok( pattern, "," ); p != NULL; p = strtok( NULL, "," ) ) {
    int match;
    int n;

    while( p[0] == ' ' )
      p++;

    n = strlen( p );
    while( n > 0 && p[n-1] == ' ' )
      p[--n] = '\0';

    match = ku_match( value, p, 0 );

    if( kc_flags.trace_debug ) {
      ku_alfa();
      printf( " ###### Match %s ~ %s --> %s\n",
             value, p, match ? "yes" : "no" );
    }

    if( match ) {
      free( pattern );
      return tos->pc + 2;
    }
  }

  free( pattern );
  return tos->pc + 1;
}


static
int op_math( tos, variable_name, expression )
     ExecStack *tos;
     char *variable_name;
     char *expression;
{
  double dval;
  int status = ku_math( expression, &dval );

  if( kc_flags.trace_debug ) {
    ku_alfa();
    printf( " ###### Math %s = %s --> %g%s\n",
           variable_name, expression,
           status == 0 ? dval : 0.0,
           status == 0 ? "" : " <Error>" );
  }

  if( status != 0 ) {
    ku_alfa();
    printf( " *** Invalid arithmetic expression in file %s\n",
           tos->file->path );
    printf( "%s: %s\n", tos->line_number, tos->source_line );
    return -1;
  }
  else {
    store_variable( tos, variable_name, strfromd( dval, 0 ) );
    return tos->pc + 1;
  }
}


static
int op_on_error( tos, keyword, argument )
     ExecStack *tos;
     char *keyword;
     char *argument;
{
  switch( keyword[0] ) {

  case '-':                     /* OFF */
    tos->off_error_stmt = tos->on_error_stmt;
    tos->off_error_arg = tos->on_error_arg;
    tos->on_error_stmt = ON_ERROR_CONTINUE;
    tos->on_error_arg = NULL;
    break;

  case '+':                     /* ON */
    tos->on_error_stmt = tos->off_error_stmt;
    tos->on_error_arg = tos->off_error_arg;
    break;

  case 'C':
    tos->on_error_stmt = ON_ERROR_CONTINUE;
    tos->on_error_arg = argument;
    break;

  case 'G':
    tos->on_error_stmt = ON_ERROR_GOTO;
    tos->on_error_arg = argument;
    break;

  case 'E':
    tos->on_error_stmt = ON_ERROR_EXITM;
    tos->on_error_arg = argument;
    break;

  case 'S':
    tos->on_error_stmt = ON_ERROR_STOPM;
    tos->on_error_arg = argument;
    break;

  }

  return tos->pc + 1;
}


static
int op_read( tos, variable, prompt_expr )
     ExecStack *tos;
     char *variable;
     char *prompt_expr;
{
  char *prompt;
  char *dfault;
  const char *value;

  if( prompt_expr == NULL ) {
    prompt = str5dup( "Macro ", tos->macro->name, ": ", variable, " ?" );
  }
  else {
    prompt = ku_expr( prompt_expr );
  }

  if( (value = hash_lookup( tos->variables, variable, NULL )) == NULL ) {
    value = "";
  }
  dfault = strdup( value );

  value = ku_pros( prompt, dfault );
  store_variable( tos, variable, value );

  free( prompt );
  free( dfault );

  return tos->pc + 1;
}


static
int op_return( tos, expression, stopm )
     ExecStack *tos;
     char *expression;
     char *stopm;
{
  if( expression != NULL ) {
    char *value = ku_expr( expression );

    if( kc_flags.trace_debug ) {
      ku_alfa();
      printf( " ###### Return %s --> %s\n",
             expression, value );
    }
    store_variable( tos, "@", value );
    free( value );
  }
  else if( kc_flags.trace_debug ) {
    ku_alfa();
    printf( " ###### Return\n" );
  }

  if( stopm != NULL )
    tos->status = KUMAC_UNWIND;

  return -1;
}


static
int op_shift( tos, null1, null2 )
     ExecStack *tos;
     char *null1;
     char *null2;
{
  char buf[8];
  int n = atoi( hash_lookup( tos->variables, "#", NULL ) );
  int i;

  for( i = 1; i < n; i++ ) {
    char *value;
    sprintf( buf, "%d", i + 1 );
    value = hash_lookup( tos->variables, buf, NULL );
    sprintf( buf, "%d", i );
    store_variable( tos, buf, value );
  }

  if( n > 0 ) {
    sprintf( buf, "%d", n );
    hash_remove( tos->variables, buf );
    sprintf( buf, "%d", n - 1 );
    store_variable( tos, "#", buf );
  }

  if( n > 1 ) {
    char *line = strdup( hash_lookup( tos->variables, "1", NULL ) );
    for( i = 1; i < n - 1; i++ ) {
      sprintf( buf, "%d", i + 1 );
      line = mstr2cat( line, " ", hash_lookup( tos->variables, buf, NULL ) );
    }
    store_variable( tos, "*", line );
    free( line );
  }
  else if( n == 1 ) {
    store_variable( tos, "*", "" );
  }

  return tos->pc + 1;
}


static
int op_trace( tos, label, variable )
     ExecStack *tos;
     char *label;
     char *variable;
{
  if( kc_flags.trace_on ) {
    char *value = hash_lookup( tos->variables, variable, NULL );

    ku_alfa();
    if( label[0] == '=' ) {
      printf( " >>>>>> %s%s = %s\n", label + 1, variable, value );
    }
    else {
      printf( " >>>>>> %s%s\n", label, value );
    }
  }

  return tos->pc + 1;
}


static
void add_code( macro, op, arg1, arg2 )
     KumacMacro *macro;
     int (*op)();
     char *arg1;
     char *arg2;
{
  KumacCode *code;

  macro->ncode++;
  if( macro->code == NULL ) {
    macro->code = (KumacCode*)malloc( sizeof(KumacCode) );
  }
  else {
    macro->code = (KumacCode*)realloc( (char*)macro->code,
                                      macro->ncode * sizeof(KumacCode) );
  }
  code = &macro->code[macro->ncode-1];

  code->op = op;
  code->arg[0] = arg1 == NULL ? NULL : strdup( arg1 );
  code->arg[1] = arg2 == NULL ? NULL : strdup( arg2 );
}


static
char *read_line( stream, macro )
     FILE *stream;
     KumacMacro *macro;
{
  char buf[1024];
  char *line;
  char *p;

  if( fgets( buf, (sizeof buf), stream ) == NULL ) {
    return NULL;
  }
  else {
    /*
     * allow @( and @) as square brackets missing on some IBM keyboards
     */
    while( (p = strstr( buf, "@(" )) != NULL ) {
      *p = '[';
      strcpy( p + 1, p + 2 );
    }
    while( (p = strstr( buf, "@)" )) != NULL ) {
      *p = ']';
      strcpy( p + 1, p + 2 );
    }

    /* remove tabs and trailing blanks */
    line = struntab( strdup( buf ) );

    sprintf( buf, "%3d", ++line_count );
    add_code( macro, op_source, buf, line );

    return line;
  }
}


static
KumacStatement parse_statement( line )
     char *line;
{
  KmCommand **cmd_list;
  KmCommand *cmd;
  KumacStatement stmt = SYNTAX_ERROR;
  char *tokline = strdup( line );
  char *tok1 = strqtok( tokline );
  char *tok2 = strqtok( NULL );
  char *p;
  int len1 = strlen( tok1 );
  int n;

  if( stmt_arg1 != NULL ) {
    free( stmt_arg1 );
    stmt_arg1 = NULL;
  }
  if( stmt_arg2 != NULL ) {
    free( stmt_arg2 );
    stmt_arg2 = NULL;
  }
  if( stmt_arg3 != NULL ) {
    free( stmt_arg3 );
    stmt_arg3 = NULL;
  }
  if( stmt_arg4 != NULL ) {
    free( stmt_arg4 );
    stmt_arg4 = NULL;
  }


  if( strcasecmp( tok1, "GO" ) == 0 && strcasecmp( tok2, "TO" ) == 0 ) {
    tok1 = "GOTO";
    tok2 = strqtok( NULL );
  }


  if( strcasecmp( tok1, "LET" ) == 0 ) {
    tok1 = tok2;
    tok2 = strqtok( NULL );
  }


  if( strchr( tok1 + 1, '=' ) != NULL
     || ( tok2 != NULL && tok2[0] == '=' ) ) {
    /* variable assignment */

    p = strchr( line, '=' );
    stmt = ASSIGNMENT_Statement;
    stmt_arg1 = strtrim( strndup( line, p - line ) );
    stmt_arg2 = strtrim( strdup( p + 1 ) );
  }


  else if( strcasecmp( tok1, "IF" ) == 0 ) {
    n = strlen( line );

    if( strcasecmp( line + n - 5, " THEN" ) == 0 ) {

      stmt_arg1 = strtrim( strndup( line + len1 + 1, n - len1 - 6 ) );

      if( stmt_arg1[0] == '\0' ) {
        print_error( "IF-THEN statement without expression", NULL );
        stmt = SYNTAX_ERROR;
      }
      else {
        stmt = IF_THEN_Statement;
      }
    }
    else if( strcasecmp( tok1, "IF" ) == 0 ) {
      /* check for "IF expr GOTO label" */

      strcpy( tokline, line );
      if( (p = strrchr( tokline, ' ' )) != NULL ) {

        stmt_arg2 = strdup( p + 1 ); /* label */

        while( p > tokline && p[-1] == ' ' )
          p--;
        *p = '\0';

        if( (p = strrchr( tokline, ' ' )) != NULL ) {

          if( strcasecmp( p + 1, "TO" ) == 0 ) {

            while( p > tokline && p[-1] == ' ' )
              p--;
            *p = '\0';

            if( (p = strrchr( tokline, ' ' )) != NULL ) {

              if( strcasecmp( p + 1, "GO" ) == 0 ) {

                while( p > tokline && p[-1] == ' ' )
                  p--;
                *p = '\0';
              }
              else {
                p = NULL;
              }
            }
          }
          else if( strcasecmp( p + 1, "GOTO" ) == 0 ) {

            while( p > tokline && p[-1] == ' ' )
              p--;
            *p = '\0';
          }
          else {
            p = NULL;
          }
        }
      }

      if( p == NULL ) {
        print_error( "Invalid IF statement (THEN missing?)", NULL );
        stmt = SYNTAX_ERROR;
      }
      else {
        stmt = IF_GOTO_Statement;
        stmt_arg1 = strtrim( strdup( tokline + 3 ) ); /* expr */
      }
    }
  }

  else if( strcasecmp( tok1, "ELSEIF" ) == 0 ) {

    n = strlen( line );

    if( strcasecmp( line + n - 5, " THEN" ) != 0 ) {
      print_error( ">Missing THEN at end of ELSEIF statement supplied", NULL );
      stmt_arg1 = strtrim( strdup( line + len1 + 1 ) );
    }
    else {
      stmt_arg1 = strtrim( strndup( line + len1 + 1, n - len1 - 6 ) );
    }

    if( stmt_arg1[0] == '\0' ) {
      print_error( "ELSEIF statement without expression", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = ELSEIF_Statement;
    }
  }

  else if( strcasecmp( tok1, "ELSE" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = ELSE_Statement;
  }

  else if( strcasecmp( tok1, "ENDIF" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = ENDIF_Statement;
  }


  else if( strcasecmp( tok1, "CASE" ) == 0 ) {

    n = strlen( line );

    if( strcasecmp( line + n - 3, " IN" ) != 0 ) {
      print_error( ">Missing IN at end of CASE statement supplied", NULL );
      stmt_arg1 = strtrim( strdup( line + len1 + 1 ) );
    }
    else {
      stmt_arg1 = strtrim( strndup( line + len1 + 1, n - len1 - 4 ) );
    }

    if( stmt_arg1[0] == '\0' ) {
      print_error( "CASE statement without expression", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = CASE_Statement;
    }
  }

  else if( tok1[0] == '(' ) {
    /* case label */

    stmt_arg1 = strdup( strchr( line, '(' ) + 1 );

    if( (p = strchr( stmt_arg1, ')' )) == NULL ) {
      print_error( "Invalid CASE label", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      *p = '\0';
      stmt = CASE_Label;
      stmt_arg2 = strtrim( strdup( p + 1 ) );
      if( stmt_arg2[0] == '\0' ) {
        free( stmt_arg2 );
        stmt_arg2 = NULL;
      }
    }
  }

  else if( strcasecmp( tok1, "ENDCASE" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = ENDCASE_Statement;
  }


  else if( strcasecmp( tok1, "DO" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "DO statement without loop variable", NULL );
      stmt = SYNTAX_ERROR;
    }
    else if( (p = strchr( line, '=' )) == NULL ) {
      print_error( "DO statement without init expression", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      int quoted = 0;
      int paren = 0;

      stmt = DO_Statement;
      stmt_arg1 = strtrim( strndup( line + len1 + 1, p - line - len1 - 1 ) );

      stmt_arg2 = strdup( p + 1 );
      for( n = 0; stmt_arg2[n] != '\0'; n++ ) {
        if( stmt_arg2[n] == '\'' )
          quoted = !quoted;
        else if( !quoted ) {
          if( stmt_arg2[n] == '(' )
            paren++;
          else if( stmt_arg2[n] == ')' )
            paren--;
          else if( paren == 0 && stmt_arg2[n] == ',' )
            break;
        }
      }

      if( !quoted && paren == 0 ) {

        if( stmt_arg2[n] != ',' ) {
          print_error( "DO statement without end expression", NULL );
          stmt = SYNTAX_ERROR;
        }
        else {
          stmt_arg2[n] = '\0';
          p += n + 1;
          stmt_arg3 = strdup( p + 1 );
          for( n = 0; stmt_arg3[n] != '\0'; n++ ) {
            if( stmt_arg3[n] == '\'' )
              quoted = !quoted;
            else if( !quoted ) {
              if( stmt_arg3[n] == '(' )
                paren++;
              else if( stmt_arg3[n] == ')' )
                paren--;
              else if( paren == 0 && stmt_arg3[n] == ',' )
                break;
            }
          }

          if( !quoted && paren == 0 ) {

            if( stmt_arg3[n] != ',' ) {
              stmt_arg4 = strdup( "1" );
            }
            else {
              stmt_arg3[n] = '\0';
              stmt_arg4 = strtrim( strdup( p + n + 2 ) );
            }
          }
        }
      }

      if( quoted ) {
        print_error( "DO statement with unbalanced quotes", NULL );
        stmt = SYNTAX_ERROR;
      }
      else if( paren != 0 ) {
        print_error( "DO statement with unbalanced parentheses", NULL );
        stmt = SYNTAX_ERROR;
      }
      else {
        strtrim( stmt_arg2 );
        strtrim( stmt_arg3 );
        if( stmt_arg2[0] == '\0' ) {
          print_error( "DO statement with empty start expression", NULL );
          stmt = SYNTAX_ERROR;
        }
        if( stmt_arg3[0] == '\0' ) {
          print_error( "DO statement with empty end expression", NULL );
          stmt = SYNTAX_ERROR;
        }
        if( stmt_arg4[0] == '\0' ) {
          print_error( ">Missing increment 1 at end of DO statement supplied",
                      NULL );
          stmt_arg4 = mstrcat( stmt_arg4, "1" );
        }
      }
    }
  }

  else if( strcasecmp( tok1, "ENDDO" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = ENDDO_Statement;
  }


  else if( strcasecmp( tok1, "FOR" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "FOR statement without loop variable", NULL );
      stmt = SYNTAX_ERROR;
    }
    else if( (p = strqtok( NULL )) == NULL
            || strcasecmp( p, "IN" ) != 0 ) {
      print_error( "Invalid FOR statement", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = FOR_Statement;
      stmt_arg1 = strdup( tok2 );
      stmt_arg2 = strdup( "" );

      while( (p = strqtok( NULL )) != NULL ) {
        if( stmt_arg2[0] != '\0' )
          stmt_arg2 = mstrcat( stmt_arg2, " " );
        stmt_arg2 = mstrcat( stmt_arg2, p );
      }
      if( stmt_arg2[0] == '\0' ) {
        print_error( ">FOR statement with empty expression list", NULL );
      }
    }
  }

  else if( strcasecmp( tok1, "ENDFOR" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = ENDFOR_Statement;
  }


  else if( strcasecmp( tok1, "REPEAT" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = REPEAT_Statement;
  }

  else if( strcasecmp( tok1, "UNTIL" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "UNTIL statement without expression", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = UNTIL_Statement;
      stmt_arg1 = strtrim( strdup( line + len1 + 1 ) );
    }
  }


  else if( strcasecmp( tok1, "WHILE" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "WHILE statement without expression", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = WHILE_Statement;
      n = strlen( line );
      if( strcasecmp( line + n - 3, " DO" ) != 0 ) {
        print_error( ">Missing DO at end of WHILE statement supplied", NULL );
      }
      else {
        n -= 3;
      }
      stmt_arg1 = strtrim( strndup( line + len1 + 1, n - len1 - 1 ) );
      if( stmt_arg1[0] == '\0' ) {
        print_error( "WHILE statement with empty expression", NULL );
        stmt = SYNTAX_ERROR;
      }
    }
  }

  else if( strcasecmp( tok1, "ENDWHILE" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = ENDWHILE_Statement;
  }


  else if( strcasecmp( tok1, "BREAKL" ) == 0 ) {

    stmt = BREAKL_Statement;
    if( tok2 == NULL ) {
      stmt_arg1 = strdup( "1" );
    }
    else {
      char *tail;
      n = fstrtoi( tok2, &tail );
      if( *tail != '\0' || n < 1 ) {
        print_error( "BREAKL level must be an integer constant >= 1", NULL );
        stmt = SYNTAX_ERROR;
      }
      else {
        stmt_arg1 = strdup( tok2 );
        if( strqtok( NULL ) != NULL ) {
          too_much( tok1 );
        }
      }
    }
  }

  else if( strcasecmp( tok1, "NEXTL" ) == 0 ) {

    stmt = NEXTL_Statement;
    if( tok2 == NULL ) {
      stmt_arg1 = strdup( "1" );
    }
    else {
      char *tail;
      n = fstrtoi( tok2, &tail );
      if( *tail != '\0' || n < 1 ) {
        print_error( "NEXTL level must be an integer constant >= 1", NULL );
        stmt = SYNTAX_ERROR;
      }
      else {
        stmt_arg1 = strdup( tok2 );
        if( strqtok( NULL ) != NULL ) {
          too_much( tok1 );
        }
      }
    }
  }


  else if( strcasecmp( tok1, "GOTO" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "GOTO statement without label", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = GOTO_Statement;
      stmt_arg1 = strdup( tok2 );
      if( strqtok( NULL ) != NULL ) {
        too_much( tok1 );
      }
    }
  }

  else if( tok1[len1-1] == ':' ) {
    /* target label */

    stmt = GOTO_Label;
    stmt_arg1 = strndup( tok1, len1 - 1 );
    if( tok2 != NULL )
      stmt_arg2 = strtrim( strdup( line + len1 ) );
  }


  else if( strcasecmp( tok1, "MACRO" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "MACRO statement without macro name", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = MACRO_Statement;
      stmt_arg1 = strdup( tok2 );
      p = strstr( line + len1 + 1, tok2 ) + strlen( tok2 );
      if( *p != '\0' )
        stmt_arg2 = strtrim( strdup( p + 1 ) );
    }
  }

  else if( ( strcasecmp( tok1, "ON" ) == 0
            || strcasecmp( tok1, "OF" ) == 0
            || strcasecmp( tok1, "OFF" ) == 0 )
          && strcasecmp( tok2, "ERROR" ) == 0 ) {

    if( strcasecmp( tok1, "ON" ) != 0 ) {
      stmt = OFF_ERROR_Statement;
    }
    else if( (p = strqtok( NULL )) == NULL ) {
      stmt = ON_ERROR_Statement;
    }
    else if( strcasecmp( p, "GOTO" ) == 0
            || ( strcasecmp( p, "GO" ) == 0 && (p = strqtok( NULL )) != NULL
                && strcasecmp( p, "TO" ) == 0 ) ) {

      if( (p = strqtok( NULL)) == NULL ) {
        print_error( "ON ERROR GOTO statement without label", NULL );
        stmt = SYNTAX_ERROR;
      }
      else {
        stmt = ON_ERROR_GOTO;
        stmt_arg1 = strdup( p );
      }
    }
    else if( strcasecmp( p, "CONTINUE" ) == 0 ) {
      stmt = ON_ERROR_CONTINUE;
    }
    else if( strcasecmp( p, "EXITM" ) == 0 ) {
      stmt = ON_ERROR_EXITM;
      if( (p = strqtok( NULL )) != NULL ) {
        stmt_arg1 = strdup( p );
      }
    }
    else if( strcasecmp( p, "STOPM" ) == 0 ) {
      stmt = ON_ERROR_STOPM;
    }
    else {
      print_error( "Invalid ON ERROR statement", NULL );
      stmt = SYNTAX_ERROR;
    }

    if( stmt != SYNTAX_ERROR && (p = strqtok( NULL )) != NULL ) {
      too_much( "ON ERROR" );
    }
  }

  else if( strcasecmp( tok1, "READ" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "READ statement without variable name", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = READ_Statement;
      stmt_arg1 = strdup( tok2 );
      p = strstr( line + len1 + 1, tok2 );
      n = strlen( tok2 );
      if( p[n] != '\0' ) {
        stmt_arg2 = strtrim( strdup( p + n ) );
      }
    }
  }

  else if( strcasecmp( tok1, "SHIFT" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = SHIFT_Statement;
  }


  else if( strcasecmp( tok1, "EXITM" ) == 0 ) {

    stmt = EXITM_Statement;
    if( tok2 != NULL ) {
      stmt_arg1 = strtrim( strdup( line + len1 + 1 ) );
    }
  }

  else if( strcasecmp( tok1, "RETURN" ) == 0 ) {

    stmt = RETURN_Statement;
    if( tok2 != NULL ) {
      stmt_arg1 = strtrim( strdup( line + len1 + 1 ) );
    }
  }

  else if( strcasecmp( tok1, "STOPM" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = STOPM_Statement;
  }

  else if( strcasecmp( tok1, "ENDKUMAC" ) == 0 ) {

    if( tok2 != NULL ) {
      too_much( tok1 );
    }
    stmt = EOF_REACHED;
  }


  else if( (cmd = search_command( tok1, &cmd_list )) != NULL
          && strcasecmp( cmd->path, "/KUIP/SET_SHOW/APPLICATION" ) == 0 ) {

    if( tok2 == NULL ) {
      print_error( "APPLICATION statement without application name", NULL );
      stmt = SYNTAX_ERROR;
    }
    else {
      stmt = APPLICATION_Statement;
      stmt_arg1 = strdup( tok2 );

      if( (p = strqtok( NULL )) == NULL ) {
        stmt_arg2 = strdup( "EXIT" );
      }
      else {
        stmt_arg2 = strdup( p );
        if( (p = strqtok( NULL )) != NULL ) {
          too_much( "APPLICATION" );
        }
      }
    }
  }

  else {
    /* command */
    stmt = COMMAND_Statement;
    stmt_arg1 = strdup( full_line );
  }

  free( tokline );

#ifdef DBMALLOC
  if( stmt_arg1 != NULL ) {
    malloc_mark( stmt_arg1 );
  }
  if( stmt_arg2 != NULL ) {
    malloc_mark( stmt_arg2 );
  }
  if( stmt_arg3 != NULL ) {
    malloc_mark( stmt_arg3 );
  }
  if( stmt_arg4 != NULL ) {
    malloc_mark( stmt_arg4 );
  }
#endif

  return stmt;
}


static
KumacStatement next_statement( stream, macro )
     FILE *stream;
     KumacMacro *macro;
{
  KumacStatement stmt;
  char *tail;
  char *p;
  int quoted = 0;
  int n = 0;

  if( full_line == NULL ) {
    if( (full_line = read_line( stream, macro )) == NULL ) {
      return EOF_REACHED;
    }
  }

  while( full_line[n] == ' ' )
    n++;
  if( n > 0 )
	memmove(full_line, full_line + n, strlen(full_line + n) + 1);
/* overlapping strings, use memmove strcpy( full_line, full_line + n ); */

  if( full_line[0] == '\0' || full_line[0] == '*' || full_line[0] == '|' ) {
    /* empty or comment line */
    free( full_line );
    full_line = NULL;
    return next_statement( stream, macro );
  }

  /* remove inline comments */
  for( p = full_line; *p != '\0'; p++ ) {

    if( *p == '\'' ) {
      quoted = !quoted;
    }
				/* p+1 is always valid, even if its \0 */
				/* This allows || to pass through */
    else if( !quoted && *p == '|' && *(p+1) != '|' ) {
      while( p > full_line && p[-1] == ' ' )
        p--;
      *p = '\0';
      quoted = -1;
      break;
    }
  }

  if( quoted != -1 ) {
    /* check for continuation lines */

    n = strlen( full_line );
    if( full_line[n-1] == '_' ) {
      /* continuation line */
      char *line = read_line( stream, macro );

      full_line[--n] = '\0';

      if( line == NULL ) {
        print_error( ">Line concatenation before EOF ignored", NULL );
        while( n > 0 && full_line[n-1] == ' ' )
          n--;
        full_line[n] = '\0';
      }
      else {
        /* keep only one of multiple leading blanks */
        p = line;
        while( *p == ' ' )
          p++;
        if( p > line )
          p--;
        full_line = mstrcat( full_line, p );
        free( line );
        return next_statement( stream, macro );
      }
    }
    if( quoted ) {
      print_error( ">Missing quote character supplied at end of line", NULL );
      full_line = mstrcat( full_line, "'" );
    }
  }

  tail = split_at_semicolon( full_line );

  if( (stmt = parse_statement( full_line )) == COMMAND_Statement ) {

    if( *tail == '&' || *tail == '!' ) {
      /* flag "cmd1 ;& cmd2" or "cmd1 ;! cmd2 */
      stmt_arg2 = strndup( tail++, 1 );
      while( *tail == ' ' )
        tail++;
    }
  }

  else if( stmt == CASE_Label || stmt == GOTO_Label ) {
    /*
     * If a label is followed by another statement on the same line
     * we have to push back the statement returned as stmt_arg2.
     */
    if( stmt_arg2 != NULL ) {

      if( *tail == '\0' ) {
        free( full_line );
        full_line = stmt_arg2;
      }
      else {
        strcpy( full_line, stmt_arg2 );
        strcat( full_line, "; " );
        strcat( full_line, tail );
        free( stmt_arg2 );
      }
      stmt_arg2 = NULL;
      return stmt;
    }
  }

  if( *tail == '\0' ) {
    free( full_line );
    full_line = NULL;
  }
  else {
    strcpy( full_line, tail );
  }

  return stmt;
}


static
KumacMacro *new_macro( file )
     KumacFile *file;
{
  KumacMacro *macro;

  if( file->macros == NULL ) {
    macro = file->macros = (KumacMacro*)malloc( sizeof(KumacMacro) );
  }
  else {
    macro = file->macros;
    while( macro->next != NULL )
      macro = macro->next;
    macro = macro->next = (KumacMacro*)malloc( sizeof(KumacMacro) );
  }
  macro->name = NULL;
  macro->next = NULL;
  macro->ncode = 0;
  macro->code = NULL;

  label_count = 0;

  return macro;
}


static
NestStack *push_stack( stmt, var )
     KumacStatement stmt;
     char *var;
{
  NestStack *tos;

  if( ++nest_stack_ptr > nest_stack_max ) {
    nest_stack_max = nest_stack_ptr;
    if( nest_stack == NULL ) {
      nest_stack = (NestStack*)malloc( sizeof(NestStack) );
    }
    else {
      nest_stack = (NestStack*)realloc( (char*)nest_stack,
                                       nest_stack_max * sizeof(NestStack) );
    }
#ifdef DBMALLOC
    malloc_mark( nest_stack );
#endif
  }

  tos = &nest_stack[nest_stack_ptr-1];

  tos->stmt = stmt;
  tos->var = str0dup( var );
  sprintf( tos->tmp1, "tmp1.%d", nest_stack_ptr );
  sprintf( tos->tmp2, "tmp2.%d", nest_stack_ptr );
  sprintf( tos->label1, "%d", ++label_count );
  sprintf( tos->label2, "%d", ++label_count );
  sprintf( tos->label3, "%d", ++label_count );

  return tos;
}


static
NestStack *pop_stack()
{

  if( nest_stack_ptr > 0  && nest_stack_ptr-- > 0 ) {
    NestStack *tos = &nest_stack[nest_stack_ptr];

    if( tos->var != NULL ) {
      free( tos->var );
    }
  }
  if( nest_stack_ptr > 0 ) {
    return &nest_stack[nest_stack_ptr-1];
  }
  else {
    return NULL;
  }
}


/*
 * free macro structure;
 */
static
void free_macro( macro )
     KumacMacro *macro;
{
  int i;

  if( macro->name != NULL )
    free( macro->name );

  for( i = 0; i < macro->ncode; i++ ) {
    KumacCode *code = &macro->code[i];

    if( code->arg[0] != NULL )
      free( code->arg[0] );
    if( code->arg[1] != NULL )
      free( code->arg[1] );
  }

  if( macro->code != NULL )
    free( (char*)macro->code );

  free( (char*)macro );
}


/*
 * free kumac structure;
 */
static
void free_kumac( file )
     KumacFile *file;
{
  KumacMacro *macro = file->macros;

  free( file->path );

  while( macro != NULL ) {
    KumacMacro *next = macro->next;
    free_macro( macro );
    macro = next;
  }

  free( file );
}


static
int compile_macros( file )
     KumacFile *file;
{
  FILE *stream = fopen( file->path, "r" );
  KumacStatement stmt;
  KumacMacro *macro = new_macro( file );
  NestStack *tos = NULL;
  char buf[16];
  char *p;
  int first = 1;
  int nerror = 0;
  int n;
  int i;

  while ( ( tos=pop_stack() ) != NULL ) ;

  kumac_path = file->path;
  line_count = 0;
  if( full_line != NULL ) {
    free( full_line );
    full_line = NULL;
  }

  do {
    stmt = next_statement( stream, macro );

    if( macro->name == NULL && stmt != MACRO_Statement ) {
      if( first ) {
        /* macro does not start with a MACRO header */
        macro->name = strdup( "" );
        add_code( macro, op_macro, NULL, NULL );
      }
      else if( stmt != EOF_REACHED ) {
        print_error( "MACRO statement missing", NULL );
        nerror++;
      }
    }
    first = 0;

    if( stmt != CASE_Label && tos != NULL && tos->stmt == CASE_Statement ) {
      print_error( ">Statements between CASE and first label are unreachable",
                  NULL );
      tos->stmt = CASE_Label;
    }


    switch( stmt ) {
      char label0[16];

    case APPLICATION_Statement:
      p = str4dup( "APPLICATION ", stmt_arg1, " ", stmt_arg2 );
      add_code( macro, op_command, p, NULL );
      free( p );

      while( (p = read_line( stream, macro )) != NULL ) {
        while( p[0] == ' ' )
          p++;
        if( strcasecmp( p, stmt_arg2 ) == 0 ) {
          break;
        }
      }
      if( p == NULL ) {
        print_error( ">EOF reached before APPLICATION text terminator", NULL );
        add_code( macro, op_source, "EOF", stmt_arg2 );
      }

      add_code( macro, op_application, stmt_arg1, NULL );
      break;

    case ASSIGNMENT_Statement:
      add_code( macro, op_expr, stmt_arg1, stmt_arg2 );
      add_code( macro, op_trace, "=", stmt_arg1 );
      break;

    case BREAKL_Statement:
    case NEXTL_Statement:
      for( i = nest_stack_ptr, n = atoi( stmt_arg1 ); i > 0 && n > 0; i-- ) {
        KumacStatement loop = nest_stack[i-1].stmt;
        if( loop == DO_Statement || loop == FOR_Statement
           || loop == REPEAT_Statement || loop == WHILE_Statement ) {
          n--;
        }
      }
      if( n > 0 ) {
        print_error( ">%s statement without enough nesting levels ignored",
                    stmt == BREAKL_Statement ? "BREAKL" : "NEXTL" );
      }
      else {
        add_code( macro, op_goto, NULL,
                 stmt == BREAKL_Statement ? nest_stack[i].label3
                                          : nest_stack[i].label2 );
      }
      break;


    case COMMAND_Statement:
      add_code( macro, op_command, stmt_arg1, stmt_arg2 );
      break;

    case EOF_REACHED:
      if( tos != NULL ) {
        nest_error( tos, "EOF" );
        nerror++;
      }
      else if( macro->name != NULL ) {
        add_code( macro, op_return, NULL, NULL );
        macro = new_macro( file );
      }
      break;

    case EXITM_Statement:
      add_code( macro, op_return, stmt_arg1, NULL );
      break;

    case GOTO_Statement:
      add_code( macro, op_goto, stmt_arg1, NULL );
      break;

    case GOTO_Label:
      if( find_label( macro, stmt_arg1, -1 ) >= 0 ) {
        print_error( ">Redefinition of label ignored", NULL );
      }
      add_code( macro, op_label, stmt_arg1, NULL );
      swap_source( macro );
      break;

    case IF_GOTO_Statement:
      add_code( macro, op_bool, stmt_arg1, NULL );
      sprintf( buf, "%d", ++label_count );
      add_code( macro, op_goto, NULL, buf );
      add_code( macro, op_goto, stmt_arg2, NULL );
      add_code( macro, op_label, NULL, buf );
      break;

    case MACRO_Statement:
      if( tos != NULL ) {
        nest_error( tos, "MACRO" );
        nerror++;
      }
      else {
        if( macro->name != NULL ) {
          print_error( ">Missing RETURN before MACRO statement supplied",
                      NULL );
          add_code( macro, op_return, NULL, NULL );
          macro = new_macro( file );
        }
        macro->name = strdup( stmt_arg1 );
        add_code( macro, op_macro, stmt_arg2, NULL );
      }
      break;

    case OFF_ERROR_Statement:
      add_code( macro, op_on_error, "-", NULL );
      break;

    case ON_ERROR_Statement:
      add_code( macro, op_on_error, "+", NULL );
      break;

    case ON_ERROR_CONTINUE:
      add_code( macro, op_on_error, "CONTINUE", NULL );
      break;

    case ON_ERROR_GOTO:
      add_code( macro, op_on_error, "GOTO", stmt_arg1 );
      break;

    case ON_ERROR_EXITM:
      add_code( macro, op_on_error, "EXITM", stmt_arg1 );
      break;

    case ON_ERROR_STOPM:
      add_code( macro, op_on_error, "STOPM", NULL );
      break;

    case READ_Statement:
      add_code( macro, op_read, stmt_arg1, stmt_arg2 );
      break;

    case SHIFT_Statement:
      add_code( macro, op_shift, NULL, NULL );
      break;

    case STOPM_Statement:
      add_code( macro, op_return, NULL, "STOPM" );
      break;

    case RETURN_Statement:
      if( tos != NULL ) {
        nest_error( tos, "RETURN" );
        nerror++;
      }
      else {
        add_code( macro, op_return, stmt_arg1, NULL );
        macro = new_macro( file );
      }
      break;


    case IF_THEN_Statement:
      tos = push_stack( IF_THEN_Statement, NULL );
      add_code( macro, op_bool, stmt_arg1, NULL );
      add_code( macro, op_goto, NULL, tos->label1 );
      break;

    case ELSEIF_Statement:
      if( tos == NULL ) {
        print_error( ">ELSEIF without matching IF statement", NULL );
      }
      else if( tos->stmt != IF_THEN_Statement ) {
        nest_error( tos, ">ELSEIF" );
      }
      else {
        add_code( macro, op_goto, NULL, tos->label3 );
        add_code( macro, op_label, NULL, tos->label1 );
        swap_source( macro );
        sprintf( tos->label1, "%d", ++label_count );
        add_code( macro, op_bool, stmt_arg1, NULL );
        add_code( macro, op_goto, NULL, tos->label1 );
      }
      break;

    case ELSE_Statement:
      if( tos == NULL ) {
        print_error( ">ELSE without matching IF statement", NULL );
      }
      else if( tos->stmt != IF_THEN_Statement ) {
        nest_error( tos, ">ELSE" );
      }
      else {
        tos->stmt = ELSE_Statement;
        add_code( macro, op_goto, NULL, tos->label3 );
        add_code( macro, op_label, NULL, tos->label1 );
        swap_source( macro );
        sprintf( tos->label1, "%d", ++label_count );
      }
      break;

    case ENDIF_Statement:
      if( tos == NULL ) {
        print_error( ">ENDIF without matching IF statement", NULL );
      }
      else if( tos->stmt != IF_THEN_Statement
              && tos->stmt != ELSE_Statement ) {
        nest_error( tos, ">ENDIF" );
      }
      else {
        add_code( macro, op_label, NULL, tos->label1 );
        add_code( macro, op_label, NULL, tos->label3 );
        swap_source( macro );
        tos = pop_stack();
      }
      break;


    case CASE_Statement:
      tos = push_stack( CASE_Statement, NULL );
      add_code( macro, op_expr, tos->tmp1, stmt_arg1 );
      add_code( macro, op_trace, "CASE: ", tos->tmp1 );
      add_code( macro, op_goto, NULL, tos->label1 );
      break;

    case CASE_Label:
      if( tos == NULL ) {
        print_error( ">(label) outside CASE-ENDCASE block ignored", NULL );
      }
      else if( tos->stmt != CASE_Statement && tos->stmt != CASE_Label ) {
        nest_error( tos, ">CASE label" );
      }
      else {
        tos->stmt = CASE_Label;
        add_code( macro, op_goto, NULL, tos->label3 );
        add_code( macro, op_label, NULL, tos->label1 );
        swap_source( macro );
        add_code( macro, op_match, tos->tmp1, stmt_arg1 );
        sprintf( tos->label1, "%d", ++label_count );
        add_code( macro, op_goto, NULL, tos->label1 );
      }
      break;

    case ENDCASE_Statement:
      if( tos == NULL ) {
        print_error( ">ENDCASE without matching CASE statement", NULL );
      }
      else if( tos->stmt != CASE_Statement && tos->stmt != CASE_Label ) {
        nest_error( tos, ">ENDCASE" );
      }
      else {
        if( tos->stmt == CASE_Statement ) {
          print_error( ">CASE construct without any labels", NULL );
        }
        add_code( macro, op_label, NULL, tos->label1 );
        add_code( macro, op_label, NULL, tos->label3 );
        swap_source( macro );
        tos = pop_stack();
      }
      break;


    case DO_Statement:
      /* DO var = start, finish, step */
      tos = push_stack( DO_Statement, stmt_arg1 );

      /* var = start */
      add_code( macro, op_math, tos->var, stmt_arg2 );

      /* tmp1 = finish */
      add_code( macro, op_math, tos->tmp1, stmt_arg3 );

      /* tmp2 = step */
      add_code( macro, op_math, tos->tmp2, stmt_arg4 );

      /* if step <> 0 then tmp1 = (finish-start)/step */
      p = str3dup( "[", tos->tmp2, "]=0" );
      add_code( macro, op_bool, p, NULL );
      free( p );
      p = str5dup( "([", tos->tmp1, "]-[", tos->var, "])/[" );
      p = mstr2cat( p, tos->tmp2, "]" );
      add_code( macro, op_math, tos->tmp1, p );
      free( p );

      /* label1: */
      add_code( macro, op_label, NULL, tos->label1 );
      add_code( macro, op_trace, "=DO:", tos->var );

      /* if step = 0 goto label0 */
      sprintf( label0, "%d", ++label_count );
      p = str3dup( "[", tos->tmp2, "]<>0" );
      add_code( macro, op_bool, p, NULL );
      free( p );
      add_code( macro, op_goto, NULL, label0 );

      /* if tmp1 < 0 goto label3 */
      p = str3dup( "[", tos->tmp1, "]>=0" );
      add_code( macro, op_bool, p, NULL );
      free( p );
      add_code( macro, op_goto, NULL, tos->label3 );

      /* tmp1 = tmp1-1 */
      p = str3dup( "[", tos->tmp1, "]-1" );
      add_code( macro, op_math, tos->tmp1, p );
      free( p );

      /* label0: */
      add_code( macro, op_label, NULL, label0 );
      break;

    case ENDDO_Statement:
      if( tos == NULL ) {
        print_error( ">ENDDO without matching DO statement", NULL );
      }
      else if( tos->stmt != DO_Statement ) {
        nest_error( tos, ">ENDDO" );
      }
      else {
        /* label2: */
        add_code( macro, op_label, NULL, tos->label2 );

        /* var = var+step */
        p = str5dup( "[", tos->var, "]+[", tos->tmp2, "]" );
        add_code( macro, op_math, tos->var, p );
        free( p );

        /* goto label1 */
        add_code( macro, op_goto, NULL, tos->label1 );

        /* label3: */
        add_code( macro, op_label, NULL, tos->label3 );
        swap_source( macro );

        tos = pop_stack();
      }
      break;


    case FOR_Statement:
      /* FOR var IN item_1 item_2 ... */
      tos = push_stack( FOR_Statement, stmt_arg1 );

      /* tmp1 = list */
      add_code( macro, op_forlist, tos->tmp1, stmt_arg2 );

      /* label1: */
      add_code( macro, op_label, NULL, tos->label1 );

      /* var = head(list) ; tmp1 = tail(list) */
      add_code( macro, op_fornext, tos->var, tos->tmp1 );

      /* if var empty goto label3 */
      add_code( macro, op_goto, NULL, tos->label3 );
      add_code( macro, op_trace, "=FOR:", tos->var );
      break;

    case ENDFOR_Statement:
      if( tos == NULL ) {
        print_error( ">ENDFOR without matching FOR statement", NULL );
      }
      else if( tos->stmt != FOR_Statement ) {
        nest_error( tos, ">ENDFOR" );
      }
      else {
        /* label2: */
        add_code( macro, op_label, NULL, tos->label2 );

        /* goto label1 */
        add_code( macro, op_goto, NULL, tos->label1 );

        /* label3: */
        add_code( macro, op_label, NULL, tos->label3 );
        swap_source( macro );

        tos = pop_stack();
      }
      break;


    case REPEAT_Statement:
      tos = push_stack( REPEAT_Statement, NULL );

      /* label1: */
      add_code( macro, op_label, NULL, tos->label1 );
      swap_source( macro );
      break;

    case UNTIL_Statement:
      if( tos == NULL ) {
        print_error( ">UNTIL without matching REPEAT statement", NULL );
      }
      else if( tos->stmt != REPEAT_Statement ) {
        nest_error( tos, ">UNTIL" );
      }
      else {
        /* label2: */
        add_code( macro, op_label, NULL, tos->label2 );

        /* if bool = false goto label1 */
        add_code( macro, op_bool, stmt_arg1, NULL );
        add_code( macro, op_goto, NULL, tos->label1 );

        /* label3: */
        add_code( macro, op_label, NULL, tos->label3 );
        swap_source( macro );

        tos = pop_stack();
      }
      break;


    case WHILE_Statement:
      tos = push_stack( WHILE_Statement, NULL );

      /* label1: */
      add_code( macro, op_label, NULL, tos->label1 );
      swap_source( macro );

      /* if bool = false goto label3 */
      add_code( macro, op_bool, stmt_arg1, NULL );
      add_code( macro, op_goto, NULL, tos->label3 );
      break;

    case ENDWHILE_Statement:
      if( tos == NULL ) {
        print_error( ">ENDWHILE without matching WHILE statement", NULL );
      }
      else if( tos->stmt != WHILE_Statement ) {
        nest_error( tos, ">ENDWHILE" );
      }
      else {
        /* label2: */
        add_code( macro, op_label, NULL, tos->label2 );

        /* goto label1: */
        add_code( macro, op_goto, NULL, tos->label1 );

        /* label3: */
        add_code( macro, op_label, NULL, tos->label3 );
        swap_source( macro );

        tos = pop_stack();
      }
      break;

    default:                    /* SYNTAX_ERROR */
      nerror++;
      break;
    }

  } while( nerror < 100 && stmt != EOF_REACHED );

  if( nerror ) {
    free_kumac( file );
  }
  else {
    /* remove the last macro which is always bogus */
    macro = file->macros;
    while( macro->next->next != NULL ) {
      macro = macro->next;
    }
    free_macro( macro->next );
    macro->next = NULL;
  }

  fclose( stream );

  return nerror;
}


/*
 * compile a kumac file
 */
static
KumacFile *compile_kumac( path )
     char *path;
{
  KumacFile *file = (KumacFile*)calloc( 1, sizeof(KumacFile) );

  file->path = strdup( path );
  file->macros = NULL;

  if( compile_macros( file ) == 0 ) {
    /* successful compilation */
    hash_insert( kumac_table, path, file, 0 );
    file->stamp = checksum( path );
    return file;
  }

  return NULL;
}


static
void free_stack( tos )
     ExecStack *tos;
{
  hash_destroy( tos->variables );

  if( tos->argline != NULL )
    free( tos->argline );
}


static
int exec_macro( file, macro, argline )
     KumacFile *file;
     KumacMacro *macro;
     char *argline;
{
  if( exec_stack_ptr < MAX_EXEC_STACK ) {
    ExecStack *tos;
    int status;

    exec_stack_ptr++;

    tos = &exec_stack[exec_stack_ptr-1];
    tos->file = file;
    tos->macro = macro;
    tos->argline = str0dup( argline );
    tos->variables = hash_create( MAX_VARIABLE_TABLE );
    tos->on_error_stmt = ON_ERROR_CONTINUE;
    tos->on_error_arg = NULL;
    tos->off_error_stmt = ON_ERROR_CONTINUE;
    tos->off_error_arg = NULL;
    tos->pc = 0;
    tos->status = 0;

    kc_alias.var_table = tos->variables;

    kc_flags.quit_macro = 0;

    do {
      KumacCode *code = &macro->code[tos->pc];
      tos->pc = (*code->op)( tos, code->arg[0], code->arg[1] );
      if( kc_flags.quit_macro ) {
        tos->status = KUMAC_UNWIND;
        break;
      }
    } while( tos->pc >= 0 );

    status = tos->status;

    if( --exec_stack_ptr > 0 ) {
      int n;

      tos = &exec_stack[exec_stack_ptr-1];

      if( (n = hash_entries( tos->variables )) > 0 ) {
        HashArray *values = hash_array( tos->variables );
        int i;

        for( i = 0; i < n; i++ ) {
          if( values[i].tag ) {
            char *name = values[i].name;
            int is_global = 0;

            if( hash_lookup( tos->variables, name, &is_global ) != NULL
               && is_global ) {
              char *value = (char*)hash_lookup( kc_alias.global_value,
                                               name, NULL );
              if( value == NULL ) {
                /* global has been deleted, delete local as well */
                hash_remove( tos->variables, name );
              }
              else {
                /* global may have been changed, update local */
                hash_insert( tos->variables, name, value, is_global );
              }
            }
          }
        }
        free( (char*)values );
      }
      kc_alias.var_table = tos->variables;

      if( kc_flags.trace_on ) {
        ku_alfa();
        printf( " >>>>>> back in %s#%s\n", tos->file->path, tos->macro->name );
      }
    }
    else {
      kc_alias.var_table = kc_alias.global_value;
    }
#if defined(STUPID_MALLOC) || defined(DBMALLOC)
    /*
     * On some systems, e.g. IBM C/370, malloc() operations become more and
     * more expensive with the number of allocated block.  Then, it is better
     * to clear the Kumac structure as soon as it is not needed any more.
     */
    {
      int needed = 0;
      int n;
      for( n = 0; n < exec_stack_ptr; n++ ) {
        if( exec_stack[n].file == exec_stack[exec_stack_ptr].file ) {
          needed = 1;
          break;
        }
      }
      if( !needed ) {
        hash_remove( kumac_table, exec_stack[exec_stack_ptr].file->path );
        free_kumac( exec_stack[exec_stack_ptr].file );
      }
    }
#endif
    free_stack( &exec_stack[exec_stack_ptr] );

    return status;
  }
  else {
    printf( " *** MACRO/EXEC: too many nested EXECs\n" );
    return KUMAC_UNWIND;
  }
}


/*
 * action routine for /MACRO/EXEC
 */
int kxmexec()
{
  char *kumac_spec = ku_gets();
  char *arg_list = ku_gete();
  char *kumac_name = strdup( kumac_spec );
  char *macro_name = strrchr( kumac_name, '#' );
  KumacFile *file = NULL;
  KumacMacro *macro = NULL;
  int i;

  ku_alfa();
  if( !kc_flags.in_macro ) {
    int n;

    if( kumac_table == NULL ) {
      kumac_table = hash_create( MAX_KUMAC_TABLE );
      hash_config( kumac_table, "struct" );
      orphan_table = hash_create( MAX_KUMAC_TABLE );
      hash_config( orphan_table, "struct" );
#ifdef DBMALLOC
      malloc_mark( kumac_table );
      malloc_mark( kumac_table->entries );
      malloc_mark( orphan_table );
      malloc_mark( orphan_table->entries );
#endif
    }
    else if( (n = hash_entries( kumac_table )) > 0 ) {
      HashArray *kumacs = hash_array( kumac_table );

      for( i = 0; i < n; i++ ) {
        file = kumacs[i].value;
        if( checksum( file->path ) != file->stamp ) {
          /* file was modified since the last compilation */
          hash_remove( kumac_table, kumacs[i].name );
          free_kumac( file );
        }
      }
      free( (char*)kumacs );
    }

    if( (n = hash_entries( orphan_table )) > 0 ) {
      HashArray *kumacs = hash_array( orphan_table );

      for( i = 0; i < n; i++ ) {
        file = kumacs[i].value;
        hash_remove( orphan_table, kumacs[i].name );
        free_kumac( file );
      }
      free( (char*)kumacs );
    }

    for( i = 0; i < exec_stack_ptr; i++ ) {
      free_stack( &exec_stack[i] );
    }
    exec_stack_ptr = 0;

    kc_flags.trace_wait = kc_flags.trace_wait_save;
  }

  if( kc_flags.in_macro && macro_name == NULL ) {
    /*
     * "EXEC name" inside a macro means to look first for macro name from
     * the same kumac file before looking for file name.
     */
    file = exec_stack[exec_stack_ptr-1].file;

    for( macro = file->macros; macro != NULL; macro = macro->next ) {
      if( strcasecmp( macro->name, kumac_name ) == 0 ) {
        break;
      }
    }
  }

  if( macro == NULL ) {
    char *path;

    if( macro_name != NULL ) {
      *macro_name++ = '\0';
    }

    if ( kumac_name[0] != '-' ) {
        path = fsearch( kumac_name, "kumac", kc_flags.macro_path );
    }
    else { path = fsearch( &kumac_name[1], "kumac", NULL );
    }

    if( path == NULL ) {
      printf( " *** MACRO/EXEC: macro file %s not found\n", kumac_name );
    }
    else {
#if defined(UNIX) && !defined(MSDOS)
      hash_config( kumac_table, kc_flags.keep_fcase ? "respect" : "ignore" );
#endif
      if( (file = hash_lookup( kumac_table, path, NULL )) != NULL ) {

        if( checksum( file->path ) != file->stamp ) {
          /* file was modified since the last compilation */
          int needed = 0;
          int n;
          for( n = 0; n < exec_stack_ptr; n++ ) {
            if( exec_stack[n].file == file ) {
              needed = 1;
              break;
            }
          }
          hash_remove( kumac_table, path );
          if( needed ) {
            /*
             * The KumacFile structure must be kept because it is still
             * in use in the EXEC stack.
             */
            char buf[16];
            sprintf( buf, "%d", ++orphan_count );
            hash_insert( orphan_table, buf, file, 0 );
          }
          else {
            free_kumac( file );
          }
          file = NULL;
        }
      }

      if( file != NULL
         || (file = compile_kumac( path )) != NULL ) {

        if( macro_name == NULL ) {
          macro = file->macros;
        }
        else {
          for( macro = file->macros; macro != NULL; macro = macro->next ) {
            if( strcasecmp( macro->name, macro_name ) == 0 ) {
              break;
            }
          }
        }
      }
    }
  }

  free( kumac_name );

  if( macro == NULL ) {
    printf( " *** MACRO/EXEC: cannot execute %s\n", kumac_spec );
    return KUMAC_UNWIND;
  }
  else {
    int old_in_macro = kc_flags.in_macro;
    int status;

    kc_flags.in_macro = 1;
    status = exec_macro( file, macro, arg_list );
    kc_flags.in_macro = old_in_macro;
    return status;
  }
}


/*
 * check if mname is a macro in the current .kumac file:
 *   return -1 if EXEC called from command line
 *   return  1 if mname is macro name
 *   return  0 if mname is kumac name
 *
 * (to be used by CMZ/EXEC overloading the MACRO/EXEC command in order that
 * "EXEC mname" looks first for "MACRO mname" before "mname.kumac")
 */
int ku_qmac( const char *mname )
{
  if( !kc_flags.in_macro ) {
    return -1;
  }
  else {
    KumacFile *file = exec_stack[exec_stack_ptr-1].file;
    KumacMacro *macro;

    for( macro = file->macros; macro != NULL; macro = macro->next ) {
      if( strcasecmp( macro->name, mname ) == 0 ) {
        return 1;
      }
    }
  }
  return 0;
}


/*
 * set a variable
 */
void F77_ENTRY_C(Kuvar,chline)
/* { */
  char *line = fstrdup( chline, len_chline );
  char *p;

  if( (p = strchr( line, '=' )) != NULL ) {
    char *value = clean_word( strdup( p + 1 ) );
    *p = '\0';
    hash_insert( kc_alias.var_table, line, value, 0 );
    free( value );
    IQUEST(1) = 0;
  }
  else {
    IQUEST(1) = 1;
  }

  free( line );
}


/*
 * return value of expression
 */
void F77_ENTRY_CC(Kuqvar,inpline,outline)
/* { */
  char *expr = fstrdup( inpline, len_inpline );
  char *value = ku_expr( expr );

  fstrset( outline, len_outline, value );

  free( value );
  free( expr );
}


/*
 * action routine for /MACRO/GLOBAL/CREATE
 */
int kxgcreate()
{
  char *name = ku_gets();
  char *value = ku_gets();
  char *text = ku_gete();

  hash_insert( kc_alias.global_value, name, value, 0 );
  hash_remove( kc_alias.global_text, name );
  if( text[0] != '\0' )
    hash_insert( kc_alias.global_text, name, text, 0 );

  if( kc_flags.in_macro ) {
    ExecStack *tos = &exec_stack[exec_stack_ptr-1];
    hash_insert( tos->variables, name, value, 1 );
  }

  return 0;
}


/*
 * action routine for /MACRO/GLOBAL/IMPORT
 */
int kxgimport()
{
  if( kc_flags.in_macro ) {
    int npar = ku_npar();

    while( npar-- > 0 ) {
      char *pattern = ku_gets();
      int n = hash_entries( kc_alias.global_value );

      if( n > 0 ) {
        ExecStack *tos = &exec_stack[exec_stack_ptr-1];
        HashArray *values = hash_array( kc_alias.global_value );
        int i;

        for( i = 0; i < n; i++ ) {
          char *name = values[i].name;
          char *value = (char*)values[i].value;

          if( ku_match( name, pattern, 1 ) ) {
            hash_insert( tos->variables, name, value, 1 );
          }
        }
        free( (char*)values );
      }
    }
  }

  return 0;
}


/*
 * action routine for /MACRO/GLOBAL/DELETE
 */
int kxgdelete()
{
  int npar = ku_npar();

  while( npar-- > 0 ) {
    char *pattern = ku_gets();
    int n = hash_entries( kc_alias.global_value );

    if( n > 0 ) {
      HashArray *values = hash_array( kc_alias.global_value );
      int i;

      for( i = 0; i < n; i++ ) {
        char *name = values[i].name;

        if( strcmp( name, "@" ) != 0 && ku_match( name, pattern, 1 ) ) {
          hash_remove( kc_alias.global_value, name );
          hash_remove( kc_alias.global_text, name );
        }
      }
      free( (char*)values );
    }

    if( kc_flags.in_macro ) {
      ExecStack *tos = &exec_stack[exec_stack_ptr-1];

      if( (n = hash_entries( tos->variables ) ) > 0 ) {
        HashArray *values = hash_array( tos->variables );
        int i;

        for( i = 0; i < n; i++ ) {
          char *name = values[i].name;

          if( strcmp( name, "@" ) != 0 && ku_match( name, pattern, 1 ) ) {
            hash_remove( tos->variables, name );
          }
        }
        free( (char*)values );
      }
    }
  }
  return 0;
}


/*
 * action routine for /MACRO/GLOBAL/LIST
 */
int kxglist()
{
  char *pattern = ku_gets();
  char *fname = ku_getf();
  char *path;
  FILE *stream = stdout;

  if( fname[0] != '\0' ) {
    char *path = ku_home( fname, "kumac" );
    if( (stream = fopen( path, "w" )) == NULL ) {
      printf( " *** Cannot write file %s\n", path );
    }
    else {
      fprintf( stream, "* GLOBAL/LIST %s %s\n", pattern, fname );
    }
    free( path );
  }

  if( stream != NULL ) {
    int n = hash_entries( kc_alias.global_value );

    if( n > 0 ) {
      HashArray *values = hash_array( kc_alias.global_value );
      int i;

      for( i = 0; i < n; i++ ) {
        char *name = values[i].name;

        if( ku_match( name, pattern, 1 ) ) {
          char *value = quote_string( strdup( values[i].value ), 0 );
          char *text = hash_lookup( kc_alias.global_text, name, NULL );

          if( stream == stdout ) {

            if( text != NULL )
              printf( " %s = %s | %s\n", name, value, text );
            else
              printf( " %s = %s\n", name, value );
          }
          else if( strcmp( name, "@" ) != 0 ) {
            text = quote_string( strdup( text == NULL ? "" : text ), 0 );
            fprintf( stream, "GLOBAL/CREATE %s %s %s\n", name, value, text );
            free( text );
          }
          free( value );
        }
      }
      free( (char*)values );
    }

    if( stream != stdout )
      fclose( stream );

    return 0;
  }

  return 1;
}
