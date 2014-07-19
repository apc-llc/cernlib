/*
 * $Id: kmath.y,v 1.2 1997/01/17 17:07:28 cremel Exp $
 *
 * $Log: kmath.y,v $
 * Revision 1.2  1997/01/17 17:07:28  cremel
 * Bug correction for $UNQUOTE: this is the original code, written in Yacc.
 * --> must generate file code_kuip/kmath.c after yacc processing.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:13  mclareni
 * Kuip
 *
 *
 *CMZ :  2.07/04 22/05/95  11.58.21  by  Gunter Folger
 *-- Author :    Alfred Nathaniel   29/06/93
 */
/* kmath.y: KUIP expression evaluation */

%{

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"
#include "kuip/klink.h"
#include "kuip/kmacro.h"

enum {
  IFUN_x,
  IFUN_xx,
  IFUN_Cx,
  IFUN_CC,
  IFUN_CCxx,
  IFUN_CCx3,
  IFUN_xCx,
  IFUN_xCC,
  IFUN_xCx8
};

typedef struct _user_fundef {
  char     *face_name;          /* function name, e.g. HEXIST */
  int       face_tok;
  char      face_type[16];
  int       face_const[16];
  char     *face_sval[16];
  double    face_dval[16];
  INT_FUNCPTR func;
  char     *call_name;
  int       call_tok;
  char      call_type[16];
  int       call_index[16];
  int       call_const[16];
  char     *call_sval[16];
  double    call_dval[16];
  char     *synopsis;
  char     *guidance;
} user_fundef;

static user_fundef user_functions[100]; /* should be plenty */


static char  *pline;
static int    kline;
static char  *result;
static int    iresult;
static double dresult;
static int    rsigma;
static int    wanted;

/*
 * The nonum flag is set in some mid-rule actions that kmathlex should
 * classify a numeric constant as STRING.
 */
static int    nonum;

static void  call_user_function( const char*, char**, double*, int*, int );
static int   count_words( const char*, const char* );
static char* extract_word( const char*, const char*, int );
static char* join_words( const char*, const char*, int, int );
static char* shell_line( const char*, int, const char* );
static char* zero_pad( char*, int );

extern int   kmathparse(void);

#ifdef ABS
#undef ABS
#endif

%}


%union {
  double dval;
  char  *sval;
}
%token <dval> NUMBER
%token <sval> STRING
%token <sval> VNAME

/* user defined functions */
%token <sval> S_USER
%token <sval> S_USER_S
%token <sval> S_USER_X
%token <sval> S_USER_X_S
%token <sval> X_USER_S
%token <sval> X_USER_X
%token <sval> X_USER_X_S

%token ABS
%token INT
%token MOD

%token ANAM
%token AVAL

%token QUOTE
%token UNQUOTE
%token ENV
%token FEXIST
%token FORMAT
%token INDEX
%token LEN
%token LOWER
%token UPPER
%token SUBSTRING
%token WORD
%token WORDS
%token SHELL

%token DEFINED
%token INLINE
%token iQUEST
%token SIGMA
%token RSIGMA

%token VEXIST
%token VDIM
%token VLEN

%token WANT_ANY
%token WANT_BOOL
%token WANT_MATH

/*
 * Note that we cannot use character constants in the grammer because
 * Yacc translates them to integer constants according to the ASCII code
 */
%token LT    '<'
%token EQ    '='
%token GT    '>'
%token PLUS  '+'
%token MINUS '-'
%token STAR  '*'
%token SLASH '/'
%token LPAR  '('
%token RPAR  ')'
%token COMMA ','

%left OR                        /* logical operators */
%left AND
%left NOT
%nonassoc LT EQ GT LE NE GE     /* relational operators */
%left PLUS MINUS
%left STAR SLASH
%left UPLUS UMINUS              /* unary plus/minus */
%left CONCAT

%type <sval> Line
%type <dval> Bool
%type <dval> Expr
%type <sval> Shell
%type <sval> String
%type <sval> Substring
%type <sval> Word
%type <sval> Words
%type <sval> Vdim
%type <sval> Vlen


%%

Line:
          WANT_BOOL Bool
        {
          iresult = $2;         /* 0 or 1 */
        }

        | WANT_BOOL Expr
        {
          iresult = ($2 != 0);
        }

        | WANT_MATH Expr
        {
          dresult = $2;
        }

        | WANT_ANY Expr
        {
          int maxint = 0x7FFFFFFF;
          if( $2 <= maxint && $2 >= -maxint && (int)$2 == $2 ) {
            result = malloc( 16 );
            sprintf( result, "%d", (int)$2 );
          }
          else {
            result = strdup( strfromd( $2, 0 ) );
          }
          if( rsigma && strpbrk( result, ".eE" ) == NULL ) {
            /* append a dot to make an integer look like a float */
            result = mstrcat( result, "." );
          }
        }

        | WANT_ANY String
        {
          result = $2;
        }
;


Expr:     NUMBER                   { $$ = $1;       }
        | LPAR  Expr RPAR          { $$ = $2;       }
        | PLUS  Expr  %prec UPLUS  { $$ = $2;      }
        | MINUS Expr  %prec UMINUS { $$ = -$2;      }
        | Expr PLUS Expr           { $$ = $1 + $3;  }
        | Expr MINUS Expr          { $$ = $1 - $3;  }
        | Expr STAR Expr           { $$ = $1 * $3;  }
        | ABS Expr RPAR            { $$ = $2 < 0 ? -$2 : $2; }
        | INT Expr RPAR            { $$ = (int)$2;  }
        | iQUEST Expr RPAR         { $$ = IQUEST((int)$2); }

        | Expr SLASH Expr
        {
          if( $3 != 0 )
            $$ = $1 / $3;
          else
            YYABORT;
        }
        | MOD Expr COMMA Expr RPAR
        {
          if( $4 != 0 )
/*            $$ = $2 / $4 - (int)($2 / $4); */
	    $$ = (int) $2 % (int) $4;
          else
            YYABORT;
        }

/* Without the nonum flag we get problems for strings looking like numbers */
        | LEN {nonum=1;} String RPAR
        {
          $$ = strlen( $3 );
          free( $3 );
          nonum = 0;
        }

        | INDEX {nonum=1;} String COMMA String RPAR
        {
          char *p = strstr( $3, $5 );
          if( p == NULL )
            $$ = 0;
          else
            $$ = p - $3 + 1;
          free( $3 );
          free( $5 );
          nonum = 0;
        }

        | Words RPAR
        {
          $$ = count_words( $1, " " );
          nonum = 0;
        }

        | Words COMMA String RPAR
        {
          $$ = count_words( $1, $3 );
          free( $3 );
          nonum = 0;
        }

        | FEXIST {nonum=1;} String RPAR
        {
          FILE *stream = fopen( $3, "r" );
          if( stream == NULL ) {
            $$ = 0;
          }
          else {
            $$ = 1;
            fclose( stream );
          }
          free( $3 );
          nonum = 0;
        }

        | VEXIST {nonum=1;} String RPAR
        {
          $$ = ku_vtype( $3 );
          free( $3 );
          nonum = 0;
        }

        | Vdim RPAR
        {
          if( ku_vtype( $1 ) != 0 )
            $$ = IQUEST( 31 );
          else
            $$ = 0;
          free( $1 );
        }
        | Vdim COMMA Expr RPAR
        {
          int n = $3;
          if( n >= 1 && n <= 3 && ku_vtype( $1 ) != 0 )
            $$ = IQUEST(30+n);
          else
            $$ = 0;
          free( $1 );
        }

        | Vlen RPAR
        {
          if( ku_vtype( $1 ) != 0 ) {
            int k = IQUEST(31);
            if( IQUEST(32) == 1 && IQUEST(33) == 1 ) {
              int low = ku_vqaddr( $1 );
              while( k > 0 && IQ(low+k-1) == 0 )
                k--;
            }
            $$ = k;
          }
          else
            $$ = 0;
          free( $1 );
        }
        | Vlen COMMA Expr RPAR
        {
          int n = $3;
          if( n >= 1 && n <= 3 && ku_vtype( $1 ) != 0 ) {
            int k = IQUEST(30+n);
            if( n == 1 && IQUEST(32) == 1 && IQUEST(33) == 1 ) {
              int low = ku_vqaddr( $1 );
              while( k > 0 && IQ(low+k-1) == 0 )
                k--;
            }
            $$ = k;
          }
          else
            $$ = 0;
          free( $1 );
        }

        | VNAME Expr RPAR
        {
          int n = strlen( $1 );
          $1 = realloc( $1, n + 16 );
          sprintf( &$1[n], "(%d)", (int)$2 );
          n = ku_vvalue( $1, &$$ );
          free( $1 );
          if( n != 0 )
            YYABORT;
        }
        | VNAME Expr COMMA Expr RPAR
        {
          int n = strlen( $1 );
          $1 = realloc( $1, n + 32 );
          sprintf( &$1[n], "(%d,%d)", (int)$2, (int)$4 );
          n = ku_vvalue( $1, &$$ );
          free( $1 );
          if( n != 0 )
            YYABORT;
        }
        | VNAME Expr COMMA Expr COMMA Expr RPAR
        {
          int n = strlen( $1 );
          $1 = realloc( $1, n + 48 );
          sprintf( &$1[n], "(%d,%d,%d)", (int)$2, (int)$4, (int)$6 );
          n = ku_vvalue( $1, &$$ );
          free( $1 );
          if( n != 0 )
            YYABORT;
        }

        | X_USER_S {nonum=1;} String RPAR
        {
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 2;
          slist[1] = $3;
          which[1] = 1;
          call_user_function( $1, slist, dlist, which, 1 );
          $$ = dlist[0];
          nonum = 0;
        }

        | X_USER_X Expr RPAR
        {
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 2;
          dlist[1] = $2;
          which[1] = 2;
          call_user_function( $1, slist, dlist, which, 1 );
          $$ = dlist[0];
        }

        | X_USER_X_S Expr COMMA {nonum=1;} String RPAR
        {
          char  *slist[3];
          double dlist[3];
          int    which[3];
          which[0] = 2;
          dlist[1] = $2;
          which[1] = 2;
          slist[2] = $5;
          which[2] = 1;
          call_user_function( $1, slist, dlist, which, 2 );
          $$ = dlist[0];
          nonum = 0;
        }

;


Bool:     LPAR Bool RPAR           { $$ = $2;       }
        | NOT Bool                 { $$ = ! $2; }
        | Bool AND Bool            { $$ = $1 && $3; }
        | Bool OR  Bool            { $$ = $1 || $3; }
        | Expr LT Expr             { $$ = $1 <  $3; }
        | Expr GT Expr             { $$ = $1 >  $3; }
        | Expr LE Expr             { $$ = $1 <= $3; }
        | Expr GE Expr             { $$ = $1 >= $3; }

/*
 * Round to machine precision for comparison of two expressions
 */
        | Expr EQ Expr
        {
          char *s1 = strdup( strfromd( $1, 0 ) );
          $$ = (strcmp( s1, strfromd( $3, 0 ) ) == 0);
          free( s1 );
        }
        | Expr NE Expr
        {
          char *s1 = strdup( strfromd( $1, 0 ) );
          $$ = (strcmp( s1, strfromd( $3, 0 ) ) != 0);
          free( s1 );
        }

/*
 * If either side is a String "=" and "<>" become string comparions.
 * The lexical tie-in avoids the rules "String EQ Expr" etc.
 */
        | String EQ {nonum=1;} String
        {
          $$ = (strcmp( $1, $4 ) == 0);
          free( $1 );
          free( $4 );
          nonum = 0;
        }
        | Expr EQ String
        {
          $$ = (strcmp( strfromd( $1,0 ), $3 ) == 0);
          free( $3 );
        }

        | String NE {nonum=1;} String
        {
          $$ = (strcmp( $1, $4 ) != 0);
          free( $1 );
          free( $4 );
          nonum = 0;
        }
        | Expr NE String
        {
          $$ = (strcmp( strfromd( $1,0 ), $3 ) != 0);
          free( $3 );
        }
;

String:   STRING                    { $$ = $1; }

        | String CONCAT {nonum=1;} String
        {
          $$ = mstrcat( $1, $4 );
          free( $4 );
          nonum = 0;
        }
        | Expr CONCAT {nonum=1;} String
        {
          $$ = strdup( strfromd( $1, 0 ) );
          $$ = mstrcat( $$, $4);
          free( $4 );
          nonum = 0;
        }

        | S_USER RPAR
        {
          char  *slist[1];
          double dlist[1];
          int    which[1];
          which[0] = 1;
          call_user_function( $1, slist, dlist, which, 0 );
          $$ = slist[0];
        }

        | S_USER_S {nonum=1;} String RPAR
        {
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 1;
          slist[1] = $3;
          which[1] = 1;
          call_user_function( $1, slist, dlist, which, 1 );
          $$ = slist[0];
          nonum = 0;
        }

        | S_USER_X Expr RPAR
        {
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 1;
          dlist[1] = $2;
          which[1] = 2;
          call_user_function( $1, slist, dlist, which, 1 );
          $$ = slist[0];
        }

        | S_USER_X_S Expr COMMA {nonum=1;} String RPAR
        {
          char  *slist[3];
          double dlist[3];
          int    which[3];
          which[0] = 1;
          dlist[1] = $2;
          which[1] = 2;
          slist[2] = $5;
          which[2] = 1;
          call_user_function( $1, slist, dlist, which, 2 );
          $$ = slist[0];
          nonum = 0;
        }

        | ANAM Expr RPAR
        {
          int n = $2;
          if( n > 0 && n <= hash_entries( kc_alias.arg_table ) ) {
            HashArray *alias = hash_array( kc_alias.arg_table );
            $$ = strdup( alias[n-1].name );
            free( (char*)alias );
          }
          else
            YYABORT;
        }

        | AVAL Expr RPAR
        {
          int n = $2;
          if( n > 0 && n <= hash_entries( kc_alias.arg_table ) ) {
            HashArray *alias = hash_array( kc_alias.arg_table );
            $$ = strdup( (char*)alias[n-1].value );
            free( (char*)alias );
          }
          else
            YYABORT;
        }

        | ENV {nonum=1;} String RPAR
        {
          if( ($$ = getenv( $3 )) == NULL )
            $$ = "";
          free( $3 );
          $$ = strdup( $$ );
          nonum = 0;
        }

        | DEFINED {nonum=1;} String RPAR
        {
          int n = hash_entries( kc_alias.var_table );
          $$ = strdup( "" );
          if( n > 0 ) {
            HashArray *values = hash_array( kc_alias.var_table );
            int i;
            for( i = 0; i < n; i++ ) {
              char *name = values[i].name;

              if( ku_match( name, $3, 1 ) ) {
                if( $$[0] != '\0' )
                  $$ = mstrcat( $$, " " );
                $$ = mstrcat( $$, name );
              }
            }
            free( (char*)values );
          }
          free( $3 );
          nonum = 0;
        }


        | FORMAT Expr COMMA String RPAR
        {
          char format[16];
          char *dot;
          int n = fstrtoi( $4 + 1, &dot );
          int m = 0;

          if( *dot == '.' ) {
            char *tail;
            m = fstrtoi( dot + 1, &tail );
          }

          $$ = realloc( $4, 256 ); /* some safety for too small n */

          switch( $$[0] ) {

          case 'I': case 'i':
            sprintf( format, "%%%dd", n );
            sprintf( $$, format, (int)$2 );
            if( m > 0 )
              $$ = zero_pad( $$, m );
            break;

          case 'Z': case 'z':     /* hexadecimal */
            sprintf( format, "%%%dX", n );
            sprintf( $$, format, (int)$2 );
            if( m > 0 )
              $$ = zero_pad( $$, m );
            break;

          case 'F': case 'f':
            sprintf( format, "%%%d.%df", n, m );
            sprintf( $$, format, $2 );
            /* force a decimal point */
            if( strchr( $$, '.' ) == NULL )
              $$ = mstrcat( $$, "." );
            break;

          case 'E': case 'e':
            sprintf( format, "%%%d.%de", n, m );
            sprintf( $$, format, $2 );
            strupper( $$ );
            break;

          case 'G': case 'g':
            sprintf( format, "%%%d.%dg", n, m );
            sprintf( $$, format, $2 );
            strupper( $$ );
            break;

          default:                /* error */
            free( $$ );
            YYABORT;
            break;
          }
        }


        | QUOTE {nonum=1;} String RPAR
        {
          $$ = quote_string( $3, 1 );
          nonum = 0;
        }

        | UNQUOTE {nonum=1;} String RPAR
        {
          /* int n = strlen( $$ ); - NC - */
          int n = strlen( $3 );
          $$ = $3;
          if( n >= 2 && $$[0] == '\'' && $$[n-1] == '\'' ) {
            char *p = $$;
            $$[n-1] = '\0';
            strcpy( $$, $$ + 1 );
            while( (p = strstr( p, "''" )) != NULL ) {
              strcpy( p, p + 1 );
              p++;
            }
          }
          nonum = 0;
        }

        | Word RPAR
        {
          $$ = extract_word( $1, " ", 1 );
          free( $1 );
          nonum = 0;
        }
        | Word COMMA Expr RPAR
        {
          $$ = extract_word( $1, " ", (int)$3 );
          free( $1 );
        }
        | Word COMMA Expr COMMA Expr RPAR
        {
          char *sep = " ";
          $$ = join_words( $1, sep, (int)$3, (int)$5 );
          free( $1 );
        }
        | Word COMMA Expr COMMA Expr COMMA {nonum=1;} String RPAR
        {
          $$ = join_words( $1, $8, (int)$3, (int)$5 );
          free( $1 );
          free( $8 );
          nonum = 0;
        }

        | LOWER {nonum=1;} String RPAR
        {
          $$ = strlower( $3 );
          nonum = 0;
        }

        | UPPER {nonum=1;} String RPAR
        {
          $$ = strupper( $3 );
          nonum = 0;
        }

/* SUBSTRING(s,k) */
        | Substring Expr RPAR
        {
          int len = strlen( $1 );
          int k = $2;
          $$ = $1;
          if( k <= 0 )
            k += len + 1;
          if( k > len )
            $$[0] = '\0';
          else if( k < 1 )
            YYABORT;
          else
            strcpy( $$, $$ + k - 1 );
        }

/* SUBSTRING(s,k,n) */
        | Substring Expr COMMA Expr RPAR
        {
          int len = strlen( $1 );
          int k = $2;
          int n = $4;
          $$ = $1;
          if( k <= 0 )
            k += len + 1;
          if( k > len )
            $$[0] = '\0';
          else if( k < 1 )
            YYABORT;
          else {
            strcpy( $$, $$ + k - 1 );
            if( k + n <= len )
              $$[n] = '\0';
          }
        }

/* SHELL(cmd) */
        | Shell RPAR
        {
          $$ = shell_line( $1, 0, " " );
          free( $1 );
        }

/* SHELL(cmd,sep) */
        | Shell COMMA String RPAR
        {
          $$ = shell_line( $1, 0, $3 );
          free( $1 );
          free( $3 );
        }

/* SHELL(cmd,n) */
        | Shell COMMA Expr RPAR
        {
          int n = $3;
          $$ = shell_line( $1, n, NULL );
          free( $1 );
        }
;


/*
 * This rule allows to reset nonum before the parses has to decide between
 * SUBSTRING(s,k) and SUBSTRING(s,k,n).
 */
Substring:
          SUBSTRING {nonum=1;} String COMMA
        {
          $$ = $3;
          nonum = 0;
        }

Words:    WORDS {nonum=1;} String
        {
          $$ = $3;
        }

Word:    WORD {nonum=1;} String
        {
          $$ = $3;
          nonum = 0;
        }

Shell:   SHELL {nonum=1;} String
        {
          $$ = $3;
          nonum = 0;
        }


/*
 * For the vector functions we want to get the vector name as a STRING.
 * That avoids that single element vectors would come back as a NUMBER.
 */
Vdim:    VDIM {nonum=1;} String
        {
          $$ = $3;
          nonum = 0;
        }

Vlen:    VLEN {nonum=1;} String
        {
          $$ = $3;
          nonum = 0;
        }

%%


/*
 * execute a shell command and return the n'th line of its output
 * if n<=0 return complete output with newlines replaced by blanks
 */
static
char *shell_line( const char *cmd,
                 int n,
                 const char *sep )
{
#ifdef UNIX
  FILE *stream;
  char *shell_cmd;
  int status;

#ifdef MSDOS
  shell_cmd = str3dup( cmd, " >", kc_flags.appl_file );
#else
  /* protect against noclobber option */
  shell_cmd = str5dup( "cp /dev/null ", kc_flags.appl_file, " ; rm ",
                      kc_flags.appl_file, " ; ( " );
  shell_cmd = mstr3cat( shell_cmd, cmd, " ) > ", kc_flags.appl_file );
#endif

  status = shsystem( kc_value.set_host_shell, shell_cmd );
  free( shell_cmd );
  if( status == -1 ) {
    perror( "$SHELL" );
  }
  else if( (stream = fopen( kc_flags.appl_file, "r" )) != NULL ) {
    char line[1024];
    char *value = NULL;

    if( sep == NULL ) {
      while( --n > 0 ) {
        fgets( line, (sizeof line), stream );
      }
      if( fgets( line, (sizeof line), stream ) != NULL ) {
        int len = strlen( line );
        line[len-1] = '\0';
        value = strdup( line );
      }
    }
    else {
      while( fgets( line, (sizeof line), stream ) != NULL ) {
        int len = strlen( line );
        line[len-1] = '\0';
        if( value == NULL ) {
          value = strdup( line );
        }
        else {
          value = mstr2cat( value, sep, line );
        }
      }
    }
    fclose( stream );
    remove( kc_flags.appl_file );
    return value;
  }
#endif

  return strdup( "" );
}


static
int count_words( const char *line,
                const char *sep )
{
  int n = 0;
  while( 1 ) {
    char *p = extract_word( line, sep, n + 1 );
    if( *p != '\0' ) {
      free( p );
      n++;
    }
    else {
      free( p );
      break;
    }
  }
  return n;
}


static
char *extract_word( const char *line,
                   const char *sep,
                   int k )
{
  const char *p = line;
  int n = 0;
  if( *sep != '\0' ) {
    while( k-- > 0 ) {
      p += n;
      while( *p != '\0' && strchr( sep, *p ) != NULL )
        p++;
      n = 0;
      while( strchr( sep, p[n] ) == NULL )
        n++;
    }
  }
  return strndup( p, n );
}


static
char *join_words( const char *line,
                 const char *sep,
                 int k,
                 int n )
{
  char *words = strdup( "" );
  while( n-- > 0 ) {
    char *p = extract_word( line, sep, k++ );
    if( p[0] != '\0' ) {
      if( words[0] != '\0' )
        words = mstrcat( words, sep );
      words = mstrcat( words, p );
      free( p );
    }
    else {
      free( p );
      break;
    }
  }
  return words;
}


/*
 * VAX C printf() does not understand "%5.3d" to do zero padding
 * and we have to do it by hand.
 */
static
char* zero_pad( char *format,
               int m )
{
  int n = strlen( format );
  int minus = 0;

  while( --n >= 0 && --m >= 0 ) {
    if( format[n] == '-' ) {
      minus = 1;
      format[n] = '0';
    }
    else if( format[n] == ' ' )
      format[n] = '0';
  }

  if( minus ) {
    if( n >= 0 )
      format[n] = '-';
    else {
      char *p = str2dup( "-", format );
      free( format );
      return p;
    }
  }
  return format;
}


/*
 * enter a user define system function
 * chname is of the form "<T>=<name>(<T><T>...)"
 */
void F77_ENTRY_C4E(Kufdef,chsyn,chguid,chface,chcall,fun)
/* { */
  char *face = fstrdup( chface, len_chface );
  char *call = fstrdup( chcall, len_chcall );
  int i;

  for( i = 0; i < sizeof(user_functions) / sizeof(user_fundef) - 1; i++ ) {

    if( user_functions[i].face_tok == 0 ) {
      char face_class[16];
      char call_class[16];
      int argc;
      char *p;
      int n;

      user_functions[i].face_name = face + 2;
      user_functions[i].face_type[0] = face[0];
      user_functions[i].face_sval[0] = NULL;
      p = strchr( face, ')' );
      *p = ',';
      p = strchr( face, '(' );
      *p++ = '\0';

      if( *p == ',' ) {
        n = 1;                  /* no arguments */
      }
      else {
        for( n = 1; *p != '\0'; n++ ) {
          char *argv = NULL;

          user_functions[i].face_type[n] = *p;
          if( p[1] == '=' ) {
            char *s = strchr( p, ',' );
            *s = '\0';
            argv = p + 2;
            p = s;
          }
          else {
            p += 2;
          }
          user_functions[i].face_sval[n] = argv;
        }
      }
      user_functions[i].face_type[n] = '\0';
      argc = n;

      for( n = 0; n < argc; n++ ) {
        char *argv = user_functions[i].face_sval[n];

        if( strchr( "stuS", user_functions[i].face_type[n] ) != NULL ) {
          face_class[n] = 'S';
          if( argv != NULL )
            user_functions[i].face_const[n] = 1;
        }
        else {
          face_class[n] = 'X';
          if( argv != NULL ) {
            user_functions[i].face_const[n] = 2;
            user_functions[i].face_dval[n] = atof( argv );
          }
        }
      }
      face_class[n] = '\0';

      if( strcmp( face_class, "S" ) == 0 ) {
        user_functions[i].face_tok = S_USER;
      }
      else if( strcmp( face_class, "SS" ) == 0 ) {
        user_functions[i].face_tok = S_USER_S;
      }
      else if( strcmp( face_class, "SX" ) == 0 ) {
        user_functions[i].face_tok = S_USER_X;
      }
      else if( strcmp( face_class, "SXS" ) == 0 ) {
        user_functions[i].face_tok = S_USER_X_S;
      }
      else if( strcmp( face_class, "XS" ) == 0 ) {
        user_functions[i].face_tok = X_USER_S;
      }
      else if( strcmp( face_class, "XX" ) == 0 ) {
        user_functions[i].face_tok = X_USER_X;
      }
      else if( strcmp( face_class, "XXS" ) == 0 ) {
        user_functions[i].face_tok = X_USER_X_S;
      }
      else {
        printf( " *** KUFDEF: %s face class %s not implemented\n",
               user_functions[i].face_name, face_class );
        return;
      }

      user_functions[i].func = (INT_FUNCPTR)fun;
      user_functions[i].call_name = call + 2;
      user_functions[i].call_type[0] = call[0];
      user_functions[i].call_sval[0] = NULL;

      p = strchr( call, ')' );
      *p = ',';
      p = strchr( call, '(' );
      *p++ = '\0';
      for( n = 1; *p != '\0'; n++ ) {
        char *argv = NULL;

        user_functions[i].call_type[n] = *p;
        if( isdigit( p[1] ) ) {
          user_functions[i].call_index[n] = p[1] - '0';
          p++;
        }

        if( p[1] == '=' ) {
          char *s = strchr( p, ',' );
          *s = '\0';
          argv = p + 2;
          p = s;
        }
        else {
          p += 2;
        }
        user_functions[i].call_sval[n] = argv;
      }
      user_functions[i].call_type[n] = '\0';
      argc = n;

      for( n = 0; n < argc; n++ ) {
        char *argv = user_functions[i].call_sval[n];

        if( strchr( "stuC$", user_functions[i].call_type[n] ) != NULL ) {
          call_class[n] = 'S';
          if( argv != NULL )
            user_functions[i].call_const[n] = 1;
        }
        else {
          call_class[n] = 'X';
          if( argv != NULL ) {
            user_functions[i].call_const[n] = 2;
            user_functions[i].call_dval[n] = atof( argv );
          }
        }
      }
      call_class[n] = '\0';

      if( strcmp( call_class, "XX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_x;
      }
      else if( strcmp( call_class, "XXX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_xx;
      }
      else if( strcmp( call_class, "XSX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_Cx;
      }
      else if( strcmp( call_class, "XSS" ) == 0 ) {
        user_functions[i].call_tok = IFUN_CC;
      }
      else if( strcmp( call_class, "XSSXX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_CCxx;
      }
      else if( strcmp( call_class, "XSSXXX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_CCx3;
      }
      else if( strcmp( call_class, "XXSX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_xCx;
      }
      else if( strcmp( call_class, "XXSS" ) == 0 ) {
        user_functions[i].call_tok = IFUN_xCC;
      }
      else if( strcmp( call_class, "XXSXXXXXXXX" ) == 0 ) {
        user_functions[i].call_tok = IFUN_xCx8;
      }
      else {
        printf( " *** KUFDEF: %s call class %s not implemented\n",
               user_functions[i].call_name, call_class );
        return;
      }
      user_functions[i].synopsis = fstrdup( chsyn, len_chsyn );
      user_functions[i].guidance = fstrdup( chguid, len_chguid );
      return;
    }
  }
  printf( " *** KUFDEF: too many definitions --- %s ignored\n", face );
}


/*
 * User help routine for /KUIP/FUNCTIONS
 */
char **kxfunclist()
{
  char **text = (char**)malloc( sizeof(char*) );
  int k = 0;
  int n;

  for( n = 0; user_functions[n].face_tok != 0; n++ ) {

    if( user_functions[n].synopsis[0] != '\0'
       || user_functions[n].guidance[0] != '\0' ) {
      int len = strlen( user_functions[n].synopsis );

      text[k] = str3dup( " ", user_functions[n].synopsis, "  " );
      text[k] = mstrccat( text[k], '.', 28 - len );
      text[k] = mstr2cat( text[k], "  ", user_functions[n].guidance );
      text = (char**)realloc( (char*)text, (k+2) * sizeof( char* ) );
      k++;
    }
  }
  text[k] = NULL;

  return text;
}


static
void call_user_function( const char *name,
                        char **slist,
                        double *dlist,
                        int *which,
                        int npar )
{
  user_fundef *ufun = NULL;
  char   *sval[16];
  double  dval[16];
  EQUIV_INT_REAL xval[16][10];
  char      cval[16][256];
  char *p;
  char c;
  int i;
  int j;

  for( i = 0; user_functions[i].face_tok != 0; i++ ) {

    if( strcasecmp( user_functions[i].face_name, name ) == 0 ) {

      ufun = &user_functions[i];
      for( j = 1; j <= npar; j++ ) {
        if( ufun->face_const[j] == 1
           && strcasecmp( ufun->face_sval[j], slist[j] ) != 0 ) {
          ufun = NULL;
          break;
        }
        if( ufun->face_const[j] == 2
           && ufun->face_dval[j] != dlist[j] ) {
          ufun = NULL;
          break;
        }
      }

      if( ufun != NULL )
        break;
    }
  }

  if( ufun == NULL ) {
    char *comma = "";
    printf( " *** Invalid argument in function %s(", name );
    for( i = 1; i <= npar; i++ ) {
      if( which[i] == 1 )
        printf( "%s%s", comma, slist[i] );
      else
        printf( "%s%s", comma, strfromd( dlist[i], 0 ) );
      comma = ",";
    }
    printf( ")\n" );

    if( which[0] == 1 )
      slist[0] = str2dup( name, "(?)" );
    else
      dlist[0] = 1.2345e6;
    return;
  }

  for( i = 1; ufun->call_type[i] != '\0'; i++ ) {
    sval[i] = ufun->call_sval[i];
    dval[i] = ufun->call_dval[i];
  }

  for( i = 1; (c = ufun->face_type[i]) != '\0'; i++ ) {

    if( (p = strchr( ufun->call_type, c )) != NULL ) {
      int n = p - ufun->call_type;

      sval[n] = slist[i];
      dval[n] = dlist[i];
    }
  }

  for( i = 1; (c = ufun->call_type[i]) != '\0'; i++ ) {

    if( strchr( "stuC$", c ) != NULL ) {
      char *s = (sval[i] != NULL) ? sval[i] : "";
      if( c == '$' )
        s = ufun->call_name;
      else if( c == 'u' )
        strupper( s );
      fstrset( cval[i], 256, s );
    }
    else if( strchr( "ijkI", c ) != NULL ) {
      xval[i][0].i = dval[i];
    }
    else if( strchr( "lmnL", c ) != NULL ) {
      xval[i][0].l = ku_true( dval[i] != 0 );
    }
    else if( strchr( "xyzR", c ) != NULL ) {
      xval[i][0].r = dval[i];
    }
  }

  switch( ufun->call_tok ) {

  case IFUN_x:
    F77_IFUN_x(xval[0][0].i,ufun->func,xval[1]);
    break;

  case IFUN_xx:
    F77_IFUN_xx(xval[0][0].i,ufun->func,xval[1],xval[2]);
    break;

  case IFUN_Cx:
    F77_IFUN_Cx(xval[0][0].i,ufun->func,cval[1],256,xval[2]);
    break;

  case IFUN_CC:
    F77_IFUN_CC(xval[0][0].i,ufun->func,cval[1],256,cval[2],256);
    break;

  case IFUN_CCxx:
    F77_IFUN_CCxx(xval[0][0].i,ufun->func,cval[1],256,cval[2],256,
                  xval[3],xval[4]);
    break;

  case IFUN_CCx3:
    F77_IFUN_CCx3(xval[0][0].i,ufun->func,cval[1],256,cval[2],256,
                  xval[3],xval[4],xval[5]);
    break;

  case IFUN_xCx:
    F77_IFUN_xCx(xval[0][0].i,ufun->func,xval[1],cval[2],256,xval[3]);
    break;

  case IFUN_xCC:
    F77_IFUN_xCC(xval[0][0].i,ufun->func,xval[1],cval[2],256,cval[3],256);
    break;

  case IFUN_xCx8:
    F77_IFUN_xCx8(xval[0][0].i,ufun->func,xval[1],cval[2],256,
                  xval[3],xval[4],xval[5],xval[6],
                  xval[7],xval[8],xval[9],xval[10]);
    break;
  }

  c = ufun->face_type[0];
  p = strchr( ufun->call_type, c );
  i = p - ufun->call_type;
  j = ufun->call_index[i];

  if( strchr( "stu", c ) != NULL ) {
    slist[0] = fstrdup( cval[i], 256 );
  }
  else if( strchr( "ijk", c ) != NULL ) {
    dlist[0] = xval[i][j].i;
  }
  else if( strchr( "lmn", c ) != NULL ) {
    dlist[0] = (xval[i][j].l != ku_true( 0 ));
  }
  else if( strchr( "xyz", c ) != NULL ) {
    dlist[0] = xval[i][j].r;
  }
  else if( strchr( "def", c ) != NULL ) {
    DBLPREC dbl;
    memcpy( (void*)&dbl, (void*)xval[i], sizeof(DBLPREC) );
    dlist[0] = dbl;
  }
}


/*
 * Replace $EXEC functions which we can't leave to the non-reentrent parser.
 */
char *repl_exec( const char *line )
{
  int quoted = 0;
  const char *p;

  for( p = line; *p != '\0'; p++ ) {
    int n;

    if( *p == '\'' ) {
      quoted = !quoted;
    }
    else if( !quoted && strncasecmp( p, "$EXEC(", 6 ) == 0
            && (n = match_paren( p )) > 0 ) {

      char *args = clean_word( strndup( p + 6, n - 7 ) );
      char *cmd = str2dup( "EXEC ", args );
      char *new_line = strndup( line, p - line );
      char *old_value = strdup( hash_lookup( kc_alias.var_table, "@", NULL ) );
      char *value;

      ku_exel( cmd );
      free( args );
      free( cmd );

      value = quote_string( strdup( hash_lookup( kc_alias.var_table,
                                                "@", NULL ) ), 0 );
      store_variable( NULL, "@", old_value );
      free( old_value );

      new_line = mstr2cat( new_line, value, p + n );
      free( value );

      return new_line;
    }
  }
  return NULL;
}


char *subst_exec( const char *line )
{
  char *ldup = strdup( line );
  char *p;

  while( (p = repl_exec( ldup )) != NULL ) {
    free( ldup );
    ldup = p;
  }
  return ldup;
}


/*
 * Evaluate an expression and return the result as an allocated string.
 * If the expression cannot be parsed a NULL string is returned.
 */
char *ku_eval( const char *line )
{
  char *val = NULL;

  /* pline must be the first of these static assignments !!! */
  pline = subst_exec( line );
  kline = 0;
  nonum = 0;
  rsigma = 0;
  wanted = WANT_ANY;

  if( kmathparse() == 0 )
    val = result;

  free( pline );
  return val;
}


/*
 * Evaluate numerical expressions
 */
int ku_math( const char *line, double *dval )
{
  int status = -1;

  /* pline must be the first of these static assignments !!! */
  pline = subst_exec( line );
  kline = 0;
  nonum = 0;
  rsigma = 0;
  wanted = WANT_MATH;

  if( kmathparse() == 0 ) {
    status = 0;
    *dval = dresult;
  }

  free( pline );
  return status;
}


/*
 * Evaluate boolean expressions
 * Return -1 if invalid expression
 */
int ku_bool( const char *line )
{
  int bool = -1;

  /* pline must be the first of these static assignments !!! */
  pline = subst_exec( line );
  kline = 0;
  nonum = 0;
  rsigma = 0;
  wanted = WANT_BOOL;

  if( kmathparse() == 0 )
    bool = iresult;

  free( pline );
  return bool;
}


/*
 * Evaluate expression.
 * For invalid expressions variable, alias, and system function substitution.
 */
char *ku_expr( const char *line )
{
  char *value = ku_eval( line );

  if( value == NULL ) {
    char *p;
    value = strdup( line );

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
  }
  return value;
}


static
int kmathlex()
{
  static struct tokens {
    char *tok;
    int   val;
  } op[] = {
    { "//",      CONCAT},
    { ".NOT.",   NOT   },
    { ".AND.",   AND   },
    { ".OR.",    OR    },
    { ".LE.",    LE    },  { "<=", LE  },
    { ".GE.",    GE    },  { ">=", GE  },
    { ".NE.",    NE    },  { "<>", NE  },
    { ".EQ.",    EQ    },
    { ".LT.",    LT    },
    { ".GT.",    GT    }
  }, fun[] = {
    { "ABS",        ABS        },
    { "INT",        INT        },
    { "MOD",        MOD        },
    { "$ANAM",      ANAM       },
    { "$AVAL",      AVAL       },
    { "$DEFINED",   DEFINED    },
    { "$ENV",       ENV        },
    { "$FEXIST",    FEXIST     },
    { "$FORMAT",    FORMAT     },
    { "$INDEX",     INDEX      },
    { "$INLINE",    INLINE     },
    { "$IQUEST",    iQUEST     },
    { "$LEN",       LEN        },
    { "$LOWER",     LOWER      },
    { "$QUOTE",     QUOTE      },
    { "$RSIGMA",    RSIGMA     },
    { "$SHELL",     SHELL      },
    { "$SIGMA",     SIGMA      },
    { "$SUBSTRING", SUBSTRING  },
    { "$UNQUOTE",   UNQUOTE    },
    { "$UPPER",     UPPER      },
    { "$VDIM",      VDIM       },
    { "$VEXIST",    VEXIST     },
    { "$VLEN",      VLEN       },
    { "$WORD",      WORD       },
    { "$WORDS",     WORDS      },
  }, single[] = {
     { "<",      LT    },
     { "=",      EQ    },
     { ">",      GT    },
     { "+",      PLUS  },
     { "-",      MINUS },
     { "*",      STAR  },
     { "/",      SLASH },
     { "(",      LPAR  },
     { ")",      RPAR  },
     { ",",      COMMA }
  };
  char *str;
  char *p;
  int c;
  int i;
  int n;

  if( wanted ) {
    int w  = wanted;
    wanted = 0;
    return w;
  }

  while( (c = pline[kline]) == ' ' || c == '\t' )
    kline++;                    /* ignore spaces */

  if( c == '\0' )
    return 0;

  if( c == '\'' ) {             /* quoted string */
    int quoted = 1;
    str = NULL;

    for( n = kline + 1; pline[n] != '\0'; n++ ) {
      if( pline[n] == '\'' ) {
        quoted = !quoted;
        if( !quoted && pline[n+1] != '\'' ) {
          str = strndup( &pline[kline+1], n - kline - 1 );
          break;
        }
      }
    }
    if( str != NULL ) {         /* remove quotes */
      char *s;
      for( p = str; (s = strstr( p, "''" )) != NULL; p = s + 1 )
        strcpy( s, s + 1 );
      kmathlval.sval = str;
      kline = n + 1;
      return STRING;
    }
  }

  /* check for macro variable */
  if( c == '[' ) {
    /*
     * Isolate the variable name and classify the translation
     * as number or string.
     */
    int paren = 1;

    for( p = pline + kline + 1; *p != '\0'; p++ ) {

      if( *p == '[' ) {
        paren++;
      }
      else if( *p == ']' ) {

        if( --paren == 0 ) {
          int len = p - &pline[kline] + 1;
          char *name = strndup( &pline[kline], len );
          char *value = var_value( name );
          free( name );

          kline += len;

          /* e.g. for $UPPER([s]) do not try classify [s] as a number */
          if( !nonum ) {
            char *tail;
            kmathlval.dval = fstrtod( value, &tail );
            if( *tail == '\0' ) {
              free( value );
              return NUMBER;
            }
            /* allow "[vname]([i])" */
            if( ku_vtype( value ) != 0 ) {
              /*
               * test if name is followed by '('
               * otherwise it must be a single element vector for which we
               * return the immediate value
               */
              p = &pline[kline];
              while( *p == ' ' )
                p++;
              if( *p == '(' ) {
                kline = p - pline + 1;
                kmathlval.sval = value;
                return VNAME;
              }
              else if( ku_vvalue( value, &kmathlval.dval ) == 0 ) {
                kline = p - pline;
                free( value );
                return NUMBER;
              }
            }
          }
          kmathlval.sval = value;
          return STRING;
        }
      }
    }
  }

  /* numbers */
  if( !nonum && ( isdigit( c )
                 || ( c == '.' && isdigit( pline[kline+1] ) ) ) ) {
    char *tail;
    str = pline + kline;
    kmathlval.dval = strtod( str, &tail );
    if( tail != str ) {
      kline += tail - str;
      return NUMBER;
    }
  }

  /* multiple character operators */
  for( i = 0; i < (sizeof op) / sizeof( struct tokens ); i++ ) {
    n = strlen( op[i].tok );

    if( strncasecmp( &pline[kline], op[i].tok, n ) == 0 ) {
      kline += n;
      return op[i].val;
    }
  }

  /* single character operators */
  for( i = 0; i < (sizeof single) / sizeof( struct tokens ); i++ ) {
    if( single[i].tok[0] == c ) {
      kline++;
      return single[i].val;
    }
  }

  /* alias names */
  n = len_alias( pline + kline, (kline > 0) ? pline[kline-1] : '\0' );
  if( n > 0 ) {
    char *name = strndup( &pline[kline], n );
    if( (p = subst_arg_alias( name )) != NULL ) {
      free( name );
      kline += n;
      if( !nonum ) {
        char *tail;
        kmathlval.dval = fstrtod( p, &tail );
        if( *tail == '\0' ) {
          free( p );
          return NUMBER;
        }
      }
      kmathlval.sval = p;
      return STRING;
    }
    free( name );
  }

  /* vector names */
  if( !nonum ) {
    n = len_vector( pline + kline );
    if( n > 0 ) {
      char *name = strndup( &pline[kline], n );
      if( ku_vtype( name ) != 0 ) {
        /*
         * test if name is followed by '('
         * otherwise it must be a single element vector for which we
         * return the immediate value
         */
        p = &pline[kline+n];
        while( *p == ' ' )
          p++;
        if( *p == '(' ) {
          kline = p - pline + 1;
          kmathlval.sval = name;
          return VNAME;
        }
        else if( ku_vvalue( name, &kmathlval.dval ) == 0 ) {
          kline = p - pline;
          free( name );
          return NUMBER;
        }
      }
      free( name );
    }
  }

  /* user defined functions */
  for( i = 0; user_functions[i].face_tok != 0; i++ ) {
    n = strlen( user_functions[i].face_name );

    if( strncasecmp( pline + kline, user_functions[i].face_name, n ) == 0 ) {

      while( pline[kline+n] == ' ' )
        n++;

      if( pline[kline+n] == '(' ) {
        kline += n + 1;
        kmathlval.sval = user_functions[i].face_name;
        return user_functions[i].face_tok;
      }
    }
  }

  /* system functions with arguments */
  for( i = 0; i < (sizeof fun) / sizeof( struct tokens ); i++ ) {
    int n = strlen( fun[i].tok );

    if( strncasecmp( &pline[kline], fun[i].tok, n ) == 0 ) {
      int k = kline + n;

      while( pline[k] == ' ' || pline[k] == '\t' )
        k++;

      if( pline[k] == '(' ) {

        if( fun[i].val == SIGMA || fun[i].val == RSIGMA
           || fun[i].val == INLINE ) {
          /*
           * SIGMA expressions do not follow our syntax rules.
           * We isolate the SIGMA expression by looking for the matching ")".
           * Before calling SIGMA we have to substitute macro variables and
           * system functions without arguments.  We cannot substitute
           * system functions with arguments because this is done by the
           * expression parser which is not reentrant.
           * The result is returned as a vector name.
           *
           * INLINE allows textual substitution in an expression.
           * The argument is alias and variable expanded and replaced in the
           * expression line.
           */
          int len = match_paren( pline + k );

          if( len > 0 ) {
            char *expr = strndup( pline + k + 1, len - 2 );

            if( (p = subst_arg_alias( expr )) != NULL ) {
              free( expr );
              expr = p;
            }
            if( (p = subst_var_alias( expr )) != NULL ) {
              free( expr );
              expr = p;
            }
            if( (p = subst_sysfun( expr, 0 )) != NULL ) {
              free( expr );
              expr = p;
            }

            if( fun[i].val == INLINE ) {
              p = strndup( pline, kline );
              p = mstrcat( p, expr );
              free( expr );
              p = mstrcat( p, pline + k + len );
              free( pline );
              pline = p;
              return kmathlex();
            }
            else if( kjmpaddr.user_sigma_F != NULL ) {
              char vname[16]; /* temporary sigma vector */

              sprintf( vname, "?SIG%d", ++kc_flags.temp_vectors );
              p = str3dup( vname, "=", expr );
              free( expr );
              fstrset( Sikuip.CHSIGM, (sizeof Sikuip.CHSIGM), p );
              free( p );

              (*kjmpaddr.user_sigma_F)();
              if( IQUEST(1) != 0 ) {
                kc_flags.temp_vectors--;
              }
              else {
                rsigma = (fun[i].val == RSIGMA);
                kline = k + len;
                if( ku_vvalue( vname, &kmathlval.dval ) == 0 )
                  return NUMBER;
                kmathlval.sval = strdup( vname );
                return STRING;
              }
            }
          }
        }
        else {
          kline = k + 1;
          return fun[i].val;
        }
      }
    }
  }

  /* system functions without arguments */
  if( c == '$' ) {
    if( (n = len_sysfun( pline + kline + 1 )) > 0 ) {
      char *name = strndup( pline + kline, n + 1 );
      char *value = repl_sysfun( name, 0 );
      free( name );
      if( value != NULL ) {
        kline += n + 1;
        if( !nonum ) {
          char *tail;
          kmathlval.dval = fstrtod( value, &tail );
          if( *tail == '\0' ) {
            free( value );
            return NUMBER;
          }
        }
        kmathlval.sval = value;
        return STRING;
      }
    }
  }

  /* classify other stuff as unquoted string */
  n = 1;
  while( strchr( " =,)[", pline[kline+n] ) == NULL )
    n++;

  str = strupper( strndup( pline + kline, n ) );

  /* check that there is no operator before the end of the string */
  for( i = 0; i < (sizeof op) / sizeof( struct tokens ); i++ ) {
    if( (p = strstr( str, op[i].tok )) != NULL ) {
      *p = '\0';
    }
  }
  n = strlen( str );
  free( str );

  kmathlval.sval = strndup( &pline[kline], n );
  kline += n;
  return STRING;
}


static
void kmatherror()
{
  /* ignore */
}
