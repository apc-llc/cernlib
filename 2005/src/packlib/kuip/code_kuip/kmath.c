
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

typedef union  {
  double dval;
  char  *sval;
} YYSTYPE;
#ifdef __cplusplus
#  include <stdio.h>
#  include <yacc.h>
#endif	/* __cplusplus */ 
# define yyparse kmathparse
# define yylex kmathlex
# define yyerror kmatherror
# define yylval kmathlval
# define yychar kmathchar
# define yydebug kmathdebug
# define yymaxdepth kmathmaxdepth
# define yynerrs kmathnerrs
# define NUMBER 257
# define STRING 258
# define VNAME 259
# define S_USER 260
# define S_USER_S 261
# define S_USER_X 262
# define S_USER_X_S 263
# define X_USER_S 264
# define X_USER_X 265
# define X_USER_X_S 266
# define ABS 267
# define INT 268
# define MOD 269
# define ANAM 270
# define AVAL 271
# define QUOTE 272
# define UNQUOTE 273
# define ENV 274
# define FEXIST 275
# define FORMAT 276
# define INDEX 277
# define LEN 278
# define LOWER 279
# define UPPER 280
# define SUBSTRING 281
# define WORD 282
# define WORDS 283
# define SHELL 284
# define DEFINED 285
# define INLINE 286
# define iQUEST 287
# define SIGMA 288
# define RSIGMA 289
# define VEXIST 290
# define VDIM 291
# define VLEN 292
# define WANT_ANY 293
# define WANT_BOOL 294
# define WANT_MATH 295
# define LT 296
# define EQ 297
# define GT 298
# define PLUS 299
# define MINUS 300
# define STAR 301
# define SLASH 302
# define LPAR 303
# define RPAR 304
# define COMMA 305
# define OR 306
# define AND 307
# define NOT 308
# define LE 309
# define NE 310
# define GE 311
# define UPLUS 312
# define UMINUS 313
# define CONCAT 314
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif

/* __YYSCLASS defines the scoping/storage class for global objects
 * that are NOT renamed by the -p option.  By default these names
 * are going to be 'static' so that multi-definition errors
 * will not occur with multiple parsers.
 * If you want (unsupported) access to internal names you need
 * to define this to be null so it implies 'extern' scope.
 * This should not be used in conjunction with -p.
 */
#ifndef __YYSCLASS
# define __YYSCLASS static
#endif
YYSTYPE yylval;
__YYSCLASS YYSTYPE yyval;
typedef int yytabelem;
# define YYERRCODE 256



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
__YYSCLASS yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 104
# define YYLAST 700
__YYSCLASS yytabelem yyact[]={

    10,    28,    24,    29,    30,    31,    32,    25,    26,    27,
    13,    14,    16,    33,    34,    38,    39,    35,    20,    37,
    18,    17,    41,    42,    49,    48,    45,    50,    36,   228,
    15,    72,   209,    21,    46,    47,   186,   222,    74,    74,
   221,    74,    11,    12,    73,    74,     7,    74,    74,    55,
    74,     8,    10,    28,    24,    29,    30,    31,    32,    25,
    26,    27,    13,    14,    16,    33,    34,    38,    39,    35,
    20,    37,    18,    17,    41,    42,    49,    48,    45,    50,
    36,   219,    15,   216,    95,    21,    46,    47,    56,    55,
   115,    74,   114,    74,    11,    12,   207,   118,    52,    61,
    65,    62,    57,    58,    59,    60,    74,   135,   136,   116,
    56,    55,    63,    66,    64,   111,   112,    67,    61,    65,
    62,    57,    58,    59,    60,    57,    58,    59,    60,   205,
   208,    63,    66,    64,   113,    10,    67,    24,   117,    74,
    67,   109,    25,    26,    27,    13,    14,    16,    57,    58,
    59,    60,   204,    20,   201,    18,    17,   200,   106,   107,
   108,    45,    74,    67,    74,    15,   198,    74,    21,    46,
    47,   226,   197,   195,   193,   189,    74,    11,    12,   188,
   187,    52,    74,    74,    74,    74,   185,    89,    90,    74,
    74,   105,    57,    58,    59,    60,    74,   223,   224,    57,
    58,    59,    60,   104,   212,   213,    57,    58,    59,    60,
   102,   202,   203,    57,    58,    59,    60,   101,   170,   171,
    57,    58,    59,    60,   196,   152,   153,    57,    58,    59,
    60,    87,    88,   164,    57,    58,    59,    60,    83,    84,
   159,    57,    58,    59,    60,    59,    60,   156,    57,    58,
    59,    60,    96,   134,   143,    57,    58,    59,    60,   139,
   225,    57,    58,    59,    60,   138,   218,    57,    58,    59,
    60,   137,   210,    57,    58,    59,    60,   194,   191,    57,
    58,    59,    60,    92,   190,    57,    58,    59,    60,    86,
   135,    57,    58,    59,    60,    85,   161,    57,    58,    59,
    60,    82,   160,    57,    58,    59,    60,    81,   158,    57,
    58,    59,    60,    23,   155,    57,    58,    59,    60,    22,
   142,    57,    58,    59,    60,    19,   141,    57,    58,    59,
    60,    40,   140,    57,    58,    59,    60,     4,     2,     3,
     9,     5,    43,    44,     1,    54,     0,     0,     0,    69,
    70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   120,   121,     0,
     0,     0,     0,     0,     0,     0,   131,   133,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   144,   146,     0,   147,   148,   149,     0,     0,
     0,     0,     0,   154,     0,     0,     0,   157,     0,     0,
     0,     0,   162,   163,     0,   165,   166,     0,     0,   168,
   169,     0,     0,   172,   174,   175,   176,   177,   178,   179,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   180,     0,     0,   181,   182,
   183,     0,     0,     0,     0,   145,     0,     0,     6,    51,
    53,     0,     0,    68,    71,     0,     0,    75,    76,    77,
    78,    79,    80,     0,     0,   199,     0,     0,     0,     0,
    91,     0,    93,    94,     0,     0,     0,    97,    98,    99,
   100,     0,     0,   103,     0,     0,     0,   211,     0,   110,
     0,     0,     0,     0,     0,   214,     0,   215,   119,     0,
     0,    71,    71,   122,   123,   124,   125,   126,   127,   128,
   129,   130,   132,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   227,     0,     0,
     0,     0,     0,     0,   150,     0,   151,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   167,     0,     0,     0,     0,   173,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   184,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   206,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   217,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   220 };
__YYSCLASS yytabelem yypact[]={

    44, -3000,  -257,  -122,  -205,  -218,  -178,  -257,  -257,  -266,
 -3000,  -122,  -122,  -122,  -122,  -122,  -122, -3000, -3000,   -66,
 -3000, -3000,   -73,  -117,  -122, -3000,  -122,  -122, -3000,  -220,
 -3000,  -122,  -122,  -122,  -122, -3000, -3000,  -122, -3000, -3000,
  -146, -3000, -3000,  -122,  -189, -3000, -3000, -3000, -3000, -3000,
 -3000,    34,  -122,  -151,  -276,  -257,  -257,  -122,  -122,  -122,
  -122,  -122,  -122,  -122,  -122,  -205,  -205, -3000,  -197,  -196,
 -3000,  -178, -3000, -3000, -3000, -3000, -3000,    28,    22,    16,
   -51,  -205,  -205, -3000,  -205,  -205,  -205, -3000,  -122, -3000,
  -122,   -79,  -205,    10,   -58, -3000,  -205,     4,   -65,    -2,
    -8,  -205,  -205,   -72,  -205,  -205, -3000,  -122,  -205,  -205,
   -86, -3000,  -205,  -205,  -205,  -205,  -205,  -205,  -205,   -14,
 -3000,  -258,   -56,   -56, -3000, -3000,    34,    34,    34,    34,
  -151,  -276,  -151,  -276,  -205, -3000, -3000,  -205,  -205,  -205,
 -3000, -3000, -3000,  -122,  -118,  -151,  -269,  -124,  -125,  -129,
   -20,   -26, -3000,  -122,  -130, -3000, -3000,  -131, -3000, -3000,
 -3000, -3000,  -132,  -138,  -205,  -147,  -150,   -93,  -152,  -175,
 -3000,  -122,  -208,  -174,  -276,  -276,  -276,  -276,  -273,  -276,
 -3000,  -276,  -276, -3000,   -32, -3000,  -205, -3000, -3000, -3000,
 -3000, -3000,  -100, -3000,  -205, -3000,  -205, -3000, -3000,  -221,
 -3000, -3000, -3000,  -122, -3000, -3000,   -38, -3000, -3000, -3000,
 -3000,  -223, -3000,  -122,  -264,  -267, -3000,  -107, -3000, -3000,
   -44, -3000, -3000, -3000, -3000, -3000,  -205,  -275, -3000 };
__YYSCLASS yytabelem yypgo[]={

     0,   344,   341,   485,   343,   340,   342,   331,   325,   319,
   313,   307,   301,   295,   289,   283,   277,   271,   265,   259,
   253,   252,   224,   217,   210,   203,   191,   171,   160,   141,
   138,   134,   109,    97,    92,    90 };
__YYSCLASS yytabelem yyr1[]={

     0,     1,     1,     1,     1,     1,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,    11,     3,
    12,     3,     3,     3,    13,     3,    14,     3,     3,     3,
     3,     3,     3,     3,     3,    15,     3,     3,    16,     3,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    17,     2,     2,    18,     2,     2,     5,    19,     5,    20,
     5,     5,    21,     5,     5,    22,     5,     5,     5,    23,
     5,    24,     5,     5,    25,     5,    26,     5,     5,     5,
     5,    27,     5,    28,     5,    29,     5,     5,     5,     5,
     5,     5,    30,     6,    31,     8,    32,     7,    33,     4,
    34,     9,    35,    10 };
__YYSCLASS yytabelem yyr2[]={

     0,     5,     5,     5,     5,     5,     3,     7,     5,     5,
     7,     7,     7,     7,     7,     7,     7,    11,     1,     9,
     1,    13,     5,     9,     1,     9,     1,     9,     5,     9,
     5,     9,     7,    11,    15,     1,     9,     7,     1,    13,
     7,     5,     7,     7,     7,     7,     7,     7,     7,     7,
     1,     9,     7,     1,     9,     7,     3,     1,     9,     1,
     9,     5,     1,     9,     7,     1,    13,     7,     7,     1,
     9,     1,     9,    11,     1,     9,     1,     9,     5,     9,
    13,     1,    19,     1,     9,     1,     9,     7,    11,     5,
     9,     9,     1,     9,     1,     7,     1,     7,     1,     7,
     1,     7,     1,     7 };
__YYSCLASS yytabelem yychk[]={

 -3000,    -1,   294,   295,   293,    -2,    -3,   303,   308,    -5,
   257,   299,   300,   267,   268,   287,   269,   278,   277,    -8,
   275,   290,    -9,   -10,   259,   264,   265,   266,   258,   260,
   261,   262,   263,   270,   271,   274,   285,   276,   272,   273,
    -7,   279,   280,    -6,    -4,   283,   291,   292,   282,   281,
   284,    -3,   303,    -3,    -5,   307,   306,   299,   300,   301,
   302,   296,   298,   309,   311,   297,   310,   314,    -3,    -2,
    -2,    -3,   297,   310,   314,    -3,    -3,    -3,    -3,    -3,
    -3,   -11,   -12,   304,   305,   -13,   -14,   304,   305,   304,
   305,    -3,   -15,    -3,    -3,   304,   -21,    -3,    -3,    -3,
    -3,   -23,   -24,    -3,   -25,   -26,   304,   305,   -28,   -29,
    -3,   304,   305,   -31,   -34,   -35,   -32,   -30,   -33,    -3,
    -2,    -2,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,
    -3,    -5,    -3,    -5,   -20,   304,   304,   -17,   -18,   -19,
   304,   304,   304,   305,    -5,    -3,    -5,    -5,    -5,    -5,
    -3,    -3,   304,   305,    -5,   304,   305,    -5,   304,   305,
   304,   304,    -5,    -5,   305,    -5,    -5,    -3,    -5,    -5,
   304,   305,    -5,    -3,    -5,    -5,    -5,    -5,    -5,    -5,
    -5,    -5,    -5,    -5,    -3,   304,   305,   304,   304,   304,
   304,   304,    -3,   304,   -16,   304,   -22,   304,   304,    -5,
   304,   304,   304,   305,   304,   304,    -3,   304,   304,   305,
   304,    -5,   304,   305,    -5,    -5,   304,    -3,   304,   304,
    -3,   304,   304,   304,   305,   304,   -27,    -5,   304 };
__YYSCLASS yytabelem yydef[]={

     0,    -2,     0,     0,     0,     1,     2,     0,     0,     0,
     6,     0,     0,     0,     0,     0,     0,    18,    20,     0,
    24,    26,     0,     0,     0,    35,     0,     0,    56,     0,
    62,     0,     0,     0,     0,    69,    71,     0,    74,    76,
     0,    83,    85,     0,     0,    94,   100,   102,    96,    92,
    98,     3,     0,     4,     5,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,    59,     0,     0,
    41,     0,    50,    53,    57,     8,     9,     0,     0,     0,
     0,     0,     0,    22,     0,     0,     0,    28,     0,    30,
     0,     0,     0,     0,     0,    61,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    78,     0,     0,     0,
     0,    89,     0,     0,     0,     0,     0,     0,     0,     0,
    42,    43,    10,    11,    12,    16,    44,    45,    46,    47,
    48,    52,    49,    55,     0,     7,    40,     0,     0,     0,
    13,    14,    15,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    32,     0,     0,    37,    38,     0,    64,    65,
    67,    68,     0,     0,     0,     0,     0,     0,     0,     0,
    87,     0,     0,     0,    95,   101,   103,    97,     0,    99,
    60,    51,    54,    58,     0,    19,     0,    23,    25,    27,
    29,    31,     0,    36,     0,    63,     0,    70,    72,     0,
    75,    77,    79,     0,    84,    86,     0,    90,    91,    93,
    17,     0,    33,     0,     0,     0,    73,     0,    88,    21,
     0,    39,    66,    80,    81,    34,     0,     0,    82 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

__YYSCLASS yytoktype yytoks[] =
{
	"NUMBER",	257,
	"STRING",	258,
	"VNAME",	259,
	"S_USER",	260,
	"S_USER_S",	261,
	"S_USER_X",	262,
	"S_USER_X_S",	263,
	"X_USER_S",	264,
	"X_USER_X",	265,
	"X_USER_X_S",	266,
	"ABS",	267,
	"INT",	268,
	"MOD",	269,
	"ANAM",	270,
	"AVAL",	271,
	"QUOTE",	272,
	"UNQUOTE",	273,
	"ENV",	274,
	"FEXIST",	275,
	"FORMAT",	276,
	"INDEX",	277,
	"LEN",	278,
	"LOWER",	279,
	"UPPER",	280,
	"SUBSTRING",	281,
	"WORD",	282,
	"WORDS",	283,
	"SHELL",	284,
	"DEFINED",	285,
	"INLINE",	286,
	"iQUEST",	287,
	"SIGMA",	288,
	"RSIGMA",	289,
	"VEXIST",	290,
	"VDIM",	291,
	"VLEN",	292,
	"WANT_ANY",	293,
	"WANT_BOOL",	294,
	"WANT_MATH",	295,
	"LT",	296,
	"<",	60,
	"EQ",	297,
	"=",	61,
	"GT",	298,
	">",	62,
	"PLUS",	299,
	"+",	43,
	"MINUS",	300,
	"-",	45,
	"STAR",	301,
	"*",	42,
	"SLASH",	302,
	"/",	47,
	"LPAR",	303,
	"(",	40,
	"RPAR",	304,
	")",	41,
	"COMMA",	305,
	",",	44,
	"OR",	306,
	"AND",	307,
	"NOT",	308,
	"LE",	309,
	"NE",	310,
	"GE",	311,
	"UPLUS",	312,
	"UMINUS",	313,
	"CONCAT",	314,
	"-unknown-",	-1	/* ends search */
};

__YYSCLASS char * yyreds[] =
{
	"-no such reduction-",
	"Line : WANT_BOOL Bool",
	"Line : WANT_BOOL Expr",
	"Line : WANT_MATH Expr",
	"Line : WANT_ANY Expr",
	"Line : WANT_ANY String",
	"Expr : NUMBER",
	"Expr : LPAR Expr RPAR",
	"Expr : PLUS Expr",
	"Expr : MINUS Expr",
	"Expr : Expr PLUS Expr",
	"Expr : Expr MINUS Expr",
	"Expr : Expr STAR Expr",
	"Expr : ABS Expr RPAR",
	"Expr : INT Expr RPAR",
	"Expr : iQUEST Expr RPAR",
	"Expr : Expr SLASH Expr",
	"Expr : MOD Expr COMMA Expr RPAR",
	"Expr : LEN",
	"Expr : LEN String RPAR",
	"Expr : INDEX",
	"Expr : INDEX String COMMA String RPAR",
	"Expr : Words RPAR",
	"Expr : Words COMMA String RPAR",
	"Expr : FEXIST",
	"Expr : FEXIST String RPAR",
	"Expr : VEXIST",
	"Expr : VEXIST String RPAR",
	"Expr : Vdim RPAR",
	"Expr : Vdim COMMA Expr RPAR",
	"Expr : Vlen RPAR",
	"Expr : Vlen COMMA Expr RPAR",
	"Expr : VNAME Expr RPAR",
	"Expr : VNAME Expr COMMA Expr RPAR",
	"Expr : VNAME Expr COMMA Expr COMMA Expr RPAR",
	"Expr : X_USER_S",
	"Expr : X_USER_S String RPAR",
	"Expr : X_USER_X Expr RPAR",
	"Expr : X_USER_X_S Expr COMMA",
	"Expr : X_USER_X_S Expr COMMA String RPAR",
	"Bool : LPAR Bool RPAR",
	"Bool : NOT Bool",
	"Bool : Bool AND Bool",
	"Bool : Bool OR Bool",
	"Bool : Expr LT Expr",
	"Bool : Expr GT Expr",
	"Bool : Expr LE Expr",
	"Bool : Expr GE Expr",
	"Bool : Expr EQ Expr",
	"Bool : Expr NE Expr",
	"Bool : String EQ",
	"Bool : String EQ String",
	"Bool : Expr EQ String",
	"Bool : String NE",
	"Bool : String NE String",
	"Bool : Expr NE String",
	"String : STRING",
	"String : String CONCAT",
	"String : String CONCAT String",
	"String : Expr CONCAT",
	"String : Expr CONCAT String",
	"String : S_USER RPAR",
	"String : S_USER_S",
	"String : S_USER_S String RPAR",
	"String : S_USER_X Expr RPAR",
	"String : S_USER_X_S Expr COMMA",
	"String : S_USER_X_S Expr COMMA String RPAR",
	"String : ANAM Expr RPAR",
	"String : AVAL Expr RPAR",
	"String : ENV",
	"String : ENV String RPAR",
	"String : DEFINED",
	"String : DEFINED String RPAR",
	"String : FORMAT Expr COMMA String RPAR",
	"String : QUOTE",
	"String : QUOTE String RPAR",
	"String : UNQUOTE",
	"String : UNQUOTE String RPAR",
	"String : Word RPAR",
	"String : Word COMMA Expr RPAR",
	"String : Word COMMA Expr COMMA Expr RPAR",
	"String : Word COMMA Expr COMMA Expr COMMA",
	"String : Word COMMA Expr COMMA Expr COMMA String RPAR",
	"String : LOWER",
	"String : LOWER String RPAR",
	"String : UPPER",
	"String : UPPER String RPAR",
	"String : Substring Expr RPAR",
	"String : Substring Expr COMMA Expr RPAR",
	"String : Shell RPAR",
	"String : Shell COMMA String RPAR",
	"String : Shell COMMA Expr RPAR",
	"Substring : SUBSTRING",
	"Substring : SUBSTRING String COMMA",
	"Words : WORDS",
	"Words : WORDS String",
	"Word : WORD",
	"Word : WORD String",
	"Shell : SHELL",
	"Shell : SHELL String",
	"Vdim : VDIM",
	"Vdim : VDIM String",
	"Vlen : VLEN",
	"Vlen : VLEN String",
};
#endif /* YYDEBUG */
#define YYFLAG  (-3000)
/* @(#) $Revision: 1.3 $ */    

/*
** Skeleton parser driver for yacc output
*/

#if defined(NLS) && !defined(NL_SETN)
#include <msgbuf.h>
#endif

#ifndef nl_msg
#define nl_msg(i,s) (s)
#endif

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab

#ifndef __RUNTIME_YYMAXDEPTH
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#else
#define YYACCEPT	{free_stacks(); return(0);}
#define YYABORT		{free_stacks(); return(1);}
#endif

#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( (nl_msg(30001,"syntax error - cannot backup")) );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
/* define for YYFLAG now generated by yacc program. */
/*#define YYFLAG		(FLAGVAL)*/

/*
** global variables used by the parser
*/
# ifndef __RUNTIME_YYMAXDEPTH
__YYSCLASS YYSTYPE yyv[ YYMAXDEPTH ];	/* value stack */
__YYSCLASS int yys[ YYMAXDEPTH ];		/* state stack */
# else
__YYSCLASS YYSTYPE *yyv;			/* pointer to malloc'ed value stack */
__YYSCLASS int *yys;			/* pointer to malloc'ed stack stack */

#if defined(__STDC__) || defined (__cplusplus)
#include <stdlib.h>
#else
	extern char *malloc();
	extern char *realloc();
	extern void free();
#endif /* __STDC__ or __cplusplus */


static int allocate_stacks(); 
static void free_stacks();
# ifndef YYINCREMENT
# define YYINCREMENT (YYMAXDEPTH/2) + 10
# endif
# endif	/* __RUNTIME_YYMAXDEPTH */
long  yymaxdepth = YYMAXDEPTH;

__YYSCLASS YYSTYPE *yypv;			/* top of value stack */
__YYSCLASS int *yyps;			/* top of state stack */

__YYSCLASS int yystate;			/* current state */
__YYSCLASS int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
__YYSCLASS int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

	/*
	** Initialize externals - yyparse may be called more than once
	*/
# ifdef __RUNTIME_YYMAXDEPTH
	if (allocate_stacks()) YYABORT;
# endif
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
# ifndef __RUNTIME_YYMAXDEPTH
			yyerror( (nl_msg(30002,"yacc stack overflow")) );
			YYABORT;
# else
			/* save old stack bases to recalculate pointers */
			YYSTYPE * yyv_old = yyv;
			int * yys_old = yys;
			yymaxdepth += YYINCREMENT;
			yys = (int *) realloc(yys, yymaxdepth * sizeof(int));
			yyv = (YYSTYPE *) realloc(yyv, yymaxdepth * sizeof(YYSTYPE));
			if (yys==0 || yyv==0) {
			    yyerror( (nl_msg(30002,"yacc stack overflow")) );
			    YYABORT;
			    }
			/* Reset pointers into stack */
			yy_ps = (yy_ps - yys_old) + yys;
			yyps = (yyps - yys_old) + yys;
			yy_pv = (yy_pv - yyv_old) + yyv;
			yypv = (yypv - yyv_old) + yyv;
# endif

		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( (nl_msg(30003,"syntax error")) );
				yynerrs++;
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:{
          iresult = yypvt[-0].dval;         /* 0 or 1 */
        } break;
case 2:{
          iresult = (yypvt[-0].dval != 0);
        } break;
case 3:{
          dresult = yypvt[-0].dval;
        } break;
case 4:{
          int maxint = 0x7FFFFFFF;
          if( yypvt[-0].dval <= maxint && yypvt[-0].dval >= -maxint && (int)yypvt[-0].dval == yypvt[-0].dval ) {
            result = malloc( 16 );
            sprintf( result, "%d", (int)yypvt[-0].dval );
          }
          else {
            result = strdup( strfromd( yypvt[-0].dval, 0 ) );
          }
          if( rsigma && strpbrk( result, ".eE" ) == NULL ) {
            /* append a dot to make an integer look like a float */
            result = mstrcat( result, "." );
          }
        } break;
case 5:{
          result = yypvt[-0].sval;
        } break;
case 6:{ yyval.dval = yypvt[-0].dval;       } break;
case 7:{ yyval.dval = yypvt[-1].dval;       } break;
case 8:{ yyval.dval = yypvt[-0].dval;      } break;
case 9:{ yyval.dval = -yypvt[-0].dval;      } break;
case 10:{ yyval.dval = yypvt[-2].dval + yypvt[-0].dval;  } break;
case 11:{ yyval.dval = yypvt[-2].dval - yypvt[-0].dval;  } break;
case 12:{ yyval.dval = yypvt[-2].dval * yypvt[-0].dval;  } break;
case 13:{ yyval.dval = yypvt[-1].dval < 0 ? -yypvt[-1].dval : yypvt[-1].dval; } break;
case 14:{ yyval.dval = (int)yypvt[-1].dval;  } break;
case 15:{ yyval.dval = IQUEST((int)yypvt[-1].dval); } break;
case 16:{
          if( yypvt[-0].dval != 0 )
            yyval.dval = yypvt[-2].dval / yypvt[-0].dval;
          else
            YYABORT;
        } break;
case 17:{
          if( yypvt[-1].dval != 0 )
/*            $$ = $2 / $4 - (int)($2 / $4); */
	    yyval.dval = (int) yypvt[-3].dval % (int) yypvt[-1].dval;
          else
            YYABORT;
        } break;
case 18:{nonum=1;} break;
case 19:{
          yyval.dval = strlen( yypvt[-1].sval );
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 20:{nonum=1;} break;
case 21:{
          char *p = strstr( yypvt[-3].sval, yypvt[-1].sval );
          if( p == NULL )
            yyval.dval = 0;
          else
            yyval.dval = p - yypvt[-3].sval + 1;
          free( yypvt[-3].sval );
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 22:{
          yyval.dval = count_words( yypvt[-1].sval, " " );
          nonum = 0;
        } break;
case 23:{
          yyval.dval = count_words( yypvt[-3].sval, yypvt[-1].sval );
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 24:{nonum=1;} break;
case 25:{
          FILE *stream;
	  char* fname = fexpand(yypvt[-1].sval,NULL);
	  stream = fopen( fname, "r" );
          if( stream == NULL ) {
            yyval.dval = 0;
          }
          else {
            yyval.dval = 1;
            fclose( stream );
          }
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 26:{nonum=1;} break;
case 27:{
          yyval.dval = ku_vtype( yypvt[-1].sval );
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 28:{
          if( ku_vtype( yypvt[-1].sval ) != 0 )
            yyval.dval = IQUEST( 31 );
          else
            yyval.dval = 0;
          free( yypvt[-1].sval );
        } break;
case 29:{
          int n = yypvt[-1].dval;
          if( n >= 1 && n <= 3 && ku_vtype( yypvt[-3].sval ) != 0 )
            yyval.dval = IQUEST(30+n);
          else
            yyval.dval = 0;
          free( yypvt[-3].sval );
        } break;
case 30:{
          if( ku_vtype( yypvt[-1].sval ) != 0 ) {
            int k = IQUEST(31);
            if( IQUEST(32) == 1 && IQUEST(33) == 1 ) {
              int low = ku_vqaddr( yypvt[-1].sval );
              while( k > 0 && IQ(low+k-1) == 0 )
                k--;
            }
            yyval.dval = k;
          }
          else
            yyval.dval = 0;
          free( yypvt[-1].sval );
        } break;
case 31:{
          int n = yypvt[-1].dval;
          if( n >= 1 && n <= 3 && ku_vtype( yypvt[-3].sval ) != 0 ) {
            int k = IQUEST(30+n);
            if( n == 1 && IQUEST(32) == 1 && IQUEST(33) == 1 ) {
              int low = ku_vqaddr( yypvt[-3].sval );
              while( k > 0 && IQ(low+k-1) == 0 )
                k--;
            }
            yyval.dval = k;
          }
          else
            yyval.dval = 0;
          free( yypvt[-3].sval );
        } break;
case 32:{
          int n = strlen( yypvt[-2].sval );
          yypvt[-2].sval = realloc( yypvt[-2].sval, n + 16 );
          sprintf( &yypvt[-2].sval[n], "(%d)", (int)yypvt[-1].dval );
          n = ku_vvalue( yypvt[-2].sval, &yyval.dval );
          free( yypvt[-2].sval );
          if( n != 0 )
            YYABORT;
        } break;
case 33:{
          int n = strlen( yypvt[-4].sval );
          yypvt[-4].sval = realloc( yypvt[-4].sval, n + 32 );
          sprintf( &yypvt[-4].sval[n], "(%d,%d)", (int)yypvt[-3].dval, (int)yypvt[-1].dval );
          n = ku_vvalue( yypvt[-4].sval, &yyval.dval );
          free( yypvt[-4].sval );
          if( n != 0 )
            YYABORT;
        } break;
case 34:{
          int n = strlen( yypvt[-6].sval );
          yypvt[-6].sval = realloc( yypvt[-6].sval, n + 48 );
          sprintf( &yypvt[-6].sval[n], "(%d,%d,%d)", (int)yypvt[-5].dval, (int)yypvt[-3].dval, (int)yypvt[-1].dval );
          n = ku_vvalue( yypvt[-6].sval, &yyval.dval );
          free( yypvt[-6].sval );
          if( n != 0 )
            YYABORT;
        } break;
case 35:{nonum=1;} break;
case 36:{
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 2;
          slist[1] = yypvt[-1].sval;
          which[1] = 1;
          call_user_function( yypvt[-3].sval, slist, dlist, which, 1 );
          yyval.dval = dlist[0];
          nonum = 0;
        } break;
case 37:{
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 2;
          dlist[1] = yypvt[-1].dval;
          which[1] = 2;
          call_user_function( yypvt[-2].sval, slist, dlist, which, 1 );
          yyval.dval = dlist[0];
        } break;
case 38:{nonum=1;} break;
case 39:{
          char  *slist[3];
          double dlist[3];
          int    which[3];
          which[0] = 2;
          dlist[1] = yypvt[-4].dval;
          which[1] = 2;
          slist[2] = yypvt[-1].sval;
          which[2] = 1;
          call_user_function( yypvt[-5].sval, slist, dlist, which, 2 );
          yyval.dval = dlist[0];
          nonum = 0;
        } break;
case 40:{ yyval.dval = yypvt[-1].dval;       } break;
case 41:{ yyval.dval = ! yypvt[-0].dval; } break;
case 42:{ yyval.dval = yypvt[-2].dval && yypvt[-0].dval; } break;
case 43:{ yyval.dval = yypvt[-2].dval || yypvt[-0].dval; } break;
case 44:{ yyval.dval = yypvt[-2].dval <  yypvt[-0].dval; } break;
case 45:{ yyval.dval = yypvt[-2].dval >  yypvt[-0].dval; } break;
case 46:{ yyval.dval = yypvt[-2].dval <= yypvt[-0].dval; } break;
case 47:{ yyval.dval = yypvt[-2].dval >= yypvt[-0].dval; } break;
case 48:{
          char *s1 = strdup( strfromd( yypvt[-2].dval, 0 ) );
          yyval.dval = (strcmp( s1, strfromd( yypvt[-0].dval, 0 ) ) == 0);
          free( s1 );
        } break;
case 49:{
          char *s1 = strdup( strfromd( yypvt[-2].dval, 0 ) );
          yyval.dval = (strcmp( s1, strfromd( yypvt[-0].dval, 0 ) ) != 0);
          free( s1 );
        } break;
case 50:{nonum=1;} break;
case 51:{
          yyval.dval = (strcmp( yypvt[-3].sval, yypvt[-0].sval ) == 0);
          free( yypvt[-3].sval );
          free( yypvt[-0].sval );
          nonum = 0;
        } break;
case 52:{
          yyval.dval = (strcmp( strfromd( yypvt[-2].dval,0 ), yypvt[-0].sval ) == 0);
          free( yypvt[-0].sval );
        } break;
case 53:{nonum=1;} break;
case 54:{
          yyval.dval = (strcmp( yypvt[-3].sval, yypvt[-0].sval ) != 0);
          free( yypvt[-3].sval );
          free( yypvt[-0].sval );
          nonum = 0;
        } break;
case 55:{
          yyval.dval = (strcmp( strfromd( yypvt[-2].dval,0 ), yypvt[-0].sval ) != 0);
          free( yypvt[-0].sval );
        } break;
case 56:{ yyval.sval = yypvt[-0].sval; } break;
case 57:{nonum=1;} break;
case 58:{
          yyval.sval = mstrcat( yypvt[-3].sval, yypvt[-0].sval );
          free( yypvt[-0].sval );
          nonum = 0;
        } break;
case 59:{nonum=1;} break;
case 60:{
          yyval.sval = strdup( strfromd( yypvt[-3].dval, 0 ) );
          yyval.sval = mstrcat( yyval.sval, yypvt[-0].sval);
          free( yypvt[-0].sval );
          nonum = 0;
        } break;
case 61:{
          char  *slist[1];
          double dlist[1];
          int    which[1];
          which[0] = 1;
          call_user_function( yypvt[-1].sval, slist, dlist, which, 0 );
          yyval.sval = slist[0];
        } break;
case 62:{nonum=1;} break;
case 63:{
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 1;
          slist[1] = yypvt[-1].sval;
          which[1] = 1;
          call_user_function( yypvt[-3].sval, slist, dlist, which, 1 );
          yyval.sval = slist[0];
          nonum = 0;
        } break;
case 64:{
          char  *slist[2];
          double dlist[2];
          int    which[2];
          which[0] = 1;
          dlist[1] = yypvt[-1].dval;
          which[1] = 2;
          call_user_function( yypvt[-2].sval, slist, dlist, which, 1 );
          yyval.sval = slist[0];
        } break;
case 65:{nonum=1;} break;
case 66:{
          char  *slist[3];
          double dlist[3];
          int    which[3];
          which[0] = 1;
          dlist[1] = yypvt[-4].dval;
          which[1] = 2;
          slist[2] = yypvt[-1].sval;
          which[2] = 1;
          call_user_function( yypvt[-5].sval, slist, dlist, which, 2 );
          yyval.sval = slist[0];
          nonum = 0;
        } break;
case 67:{
          int n = yypvt[-1].dval;
          if( n > 0 && n <= hash_entries( kc_alias.arg_table ) ) {
            HashArray *alias = hash_array( kc_alias.arg_table );
            yyval.sval = strdup( alias[n-1].name );
            free( (char*)alias );
          }
          else
            YYABORT;
        } break;
case 68:{
          int n = yypvt[-1].dval;
          if( n > 0 && n <= hash_entries( kc_alias.arg_table ) ) {
            HashArray *alias = hash_array( kc_alias.arg_table );
            yyval.sval = strdup( (char*)alias[n-1].value );
            free( (char*)alias );
          }
          else
            YYABORT;
        } break;
case 69:{nonum=1;} break;
case 70:{
          if( (yyval.sval = getenv( yypvt[-1].sval )) == NULL )
            yyval.sval = "";
          free( yypvt[-1].sval );
          yyval.sval = strdup( yyval.sval );
          nonum = 0;
        } break;
case 71:{nonum=1;} break;
case 72:{
          int n = hash_entries( kc_alias.var_table );
          yyval.sval = strdup( "" );
          if( n > 0 ) {
            HashArray *values = hash_array( kc_alias.var_table );
            int i;
            for( i = 0; i < n; i++ ) {
              char *name = values[i].name;

              if( ku_match( name, yypvt[-1].sval, 1 ) ) {
                if( yyval.sval[0] != '\0' )
                  yyval.sval = mstrcat( yyval.sval, " " );
                yyval.sval = mstrcat( yyval.sval, name );
              }
            }
            free( (char*)values );
          }
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 73:{
          char format[16];
          char *dot;
          int n = fstrtoi( yypvt[-1].sval + 1, &dot );
          int m = 0;

          if( *dot == '.' ) {
            char *tail;
            m = fstrtoi( dot + 1, &tail );
          }

          yyval.sval = realloc( yypvt[-1].sval, 256 ); /* some safety for too small n */

          switch( yyval.sval[0] ) {

          case 'I': case 'i':
            sprintf( format, "%%%dd", n );
            sprintf( yyval.sval, format, (int)yypvt[-3].dval );
            if( m > 0 )
              yyval.sval = zero_pad( yyval.sval, m );
            break;

          case 'Z': case 'z':     /* hexadecimal */
            sprintf( format, "%%%dX", n );
            sprintf( yyval.sval, format, (int)yypvt[-3].dval );
            if( m > 0 )
              yyval.sval = zero_pad( yyval.sval, m );
            break;

          case 'F': case 'f':
            sprintf( format, "%%%d.%df", n, m );
            sprintf( yyval.sval, format, yypvt[-3].dval );
            /* force a decimal point */
            if( strchr( yyval.sval, '.' ) == NULL )
              yyval.sval = mstrcat( yyval.sval, "." );
            break;

          case 'E': case 'e':
            sprintf( format, "%%%d.%de", n, m );
            sprintf( yyval.sval, format, yypvt[-3].dval );
            strupper( yyval.sval );
            break;

          case 'G': case 'g':
            sprintf( format, "%%%d.%dg", n, m );
            sprintf( yyval.sval, format, yypvt[-3].dval );
            strupper( yyval.sval );
            break;

          default:                /* error */
            free( yyval.sval );
            YYABORT;
            break;
          }
        } break;
case 74:{nonum=1;} break;
case 75:{
          yyval.sval = quote_string( yypvt[-1].sval, 1 );
          nonum = 0;
        } break;
case 76:{nonum=1;} break;
case 77:{
          /* int n = strlen( $$ ); - NC - */
          int n = strlen( yypvt[-1].sval );
          yyval.sval = yypvt[-1].sval;
          if( n >= 2 && yyval.sval[0] == '\'' && yyval.sval[n-1] == '\'' ) {
            char *p = yyval.sval;
            yyval.sval[n-1] = '\0';
            strcpy( yyval.sval, yyval.sval + 1 );
            while( (p = strstr( p, "''" )) != NULL ) {
              strcpy( p, p + 1 );
              p++;
            }
          }
          nonum = 0;
        } break;
case 78:{
          yyval.sval = extract_word( yypvt[-1].sval, " ", 1 );
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 79:{
          yyval.sval = extract_word( yypvt[-3].sval, " ", (int)yypvt[-1].dval );
          free( yypvt[-3].sval );
        } break;
case 80:{
          char *sep = " ";
          yyval.sval = join_words( yypvt[-5].sval, sep, (int)yypvt[-3].dval, (int)yypvt[-1].dval );
          free( yypvt[-5].sval );
        } break;
case 81:{nonum=1;} break;
case 82:{
          yyval.sval = join_words( yypvt[-8].sval, yypvt[-1].sval, (int)yypvt[-6].dval, (int)yypvt[-4].dval );
          free( yypvt[-8].sval );
          free( yypvt[-1].sval );
          nonum = 0;
        } break;
case 83:{nonum=1;} break;
case 84:{
          yyval.sval = strlower( yypvt[-1].sval );
          nonum = 0;
        } break;
case 85:{nonum=1;} break;
case 86:{
          yyval.sval = strupper( yypvt[-1].sval );
          nonum = 0;
        } break;
case 87:{
          int len = strlen( yypvt[-2].sval );
          int k = yypvt[-1].dval;
          yyval.sval = yypvt[-2].sval;
          if( k <= 0 )
            k += len + 1;
          if( k > len )
            yyval.sval[0] = '\0';
          else if( k < 1 )
            YYABORT;
          else
            strcpy( yyval.sval, yyval.sval + k - 1 );
        } break;
case 88:{
          int len = strlen( yypvt[-4].sval );
          int k = yypvt[-3].dval;
          int n = yypvt[-1].dval;
          yyval.sval = yypvt[-4].sval;
          if( k <= 0 )
            k += len + 1;
          if( k > len )
            yyval.sval[0] = '\0';
          else if( k < 1 )
            YYABORT;
          else {
            strcpy( yyval.sval, yyval.sval + k - 1 );
            if( k + n <= len )
              yyval.sval[n] = '\0';
          }
        } break;
case 89:{
          yyval.sval = shell_line( yypvt[-1].sval, 0, " " );
          free( yypvt[-1].sval );
        } break;
case 90:{
          yyval.sval = shell_line( yypvt[-3].sval, 0, yypvt[-1].sval );
          free( yypvt[-3].sval );
          free( yypvt[-1].sval );
        } break;
case 91:{
          int n = yypvt[-1].dval;
          yyval.sval = shell_line( yypvt[-3].sval, n, NULL );
          free( yypvt[-3].sval );
        } break;
case 92:{nonum=1;} break;
case 93:{
          yyval.sval = yypvt[-1].sval;
          nonum = 0;
        } break;
case 94:{nonum=1;} break;
case 95:{
          yyval.sval = yypvt[-0].sval;
        } break;
case 96:{nonum=1;} break;
case 97:{
          yyval.sval = yypvt[-0].sval;
          nonum = 0;
        } break;
case 98:{nonum=1;} break;
case 99:{
          yyval.sval = yypvt[-0].sval;
          nonum = 0;
        } break;
case 100:{nonum=1;} break;
case 101:{
          yyval.sval = yypvt[-0].sval;
          nonum = 0;
        } break;
case 102:{nonum=1;} break;
case 103:{
          yyval.sval = yypvt[-0].sval;
          nonum = 0;
        } break;
	}
	goto yystack;		/* reset registers in driver code */
}

# ifdef __RUNTIME_YYMAXDEPTH

static int allocate_stacks() {
	/* allocate the yys and yyv stacks */
	yys = (int *) malloc(yymaxdepth * sizeof(int));
	yyv = (YYSTYPE *) malloc(yymaxdepth * sizeof(YYSTYPE));

	if (yys==0 || yyv==0) {
	   yyerror( (nl_msg(30004,"unable to allocate space for yacc stacks")) );
	   return(1);
	   }
	else return(0);

}


static void free_stacks() {
	if (yys!=0) free((char *) yys);
	if (yyv!=0) free((char *) yyv);
}

# endif  /* defined(__RUNTIME_YYMAXDEPTH) */

