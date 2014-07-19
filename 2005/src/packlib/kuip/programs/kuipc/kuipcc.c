/*
 * $Id: kuipcc.c,v 1.7 2000/03/27 14:13:17 couet Exp $
 *
 * $Log: kuipcc.c,v $
 * Revision 1.7  2000/03/27 14:13:17  couet
 * - Mod to suppress a warning on  Solaris 2.7
 *
 * Revision 1.6  1998/02/12 10:31:28  gunter
 * Remove lines at start of .c output files setting wrong defaults for name
 * (upper/lower/underscore); this required a CERNLIB_ to be set compiling the
 * C code.
 *
 *
 *
 * CVS:----------------------------------------------------------------------
 * CVS:----------------------------------------------------------------------
 *
 * Revision 1.5  1997/11/19 16:42:54  mclareni
 * Correct typo
 *
 * Revision 1.4  1997/09/02 14:50:03  mclareni
 * WINNT corrections
 *
 * Revision 1.3  1997/03/14 17:15:56  mclareni
 * WNT mods
 *
 * Revision 1.2.2.1  1997/01/21 11:33:19  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/04/16 13:29:49  gunter
 * Mods to compile kuipc.
 *  The header files can be deleted and remade from the .m4 files. Use gnu's m4,
 *  ie.:
 *  gmake headers M4=/usr/local/bin/m4
 *  And then
 *  gmake kuipc
 *
 * Revision 1.1.1.1  1996/03/08 15:33:03  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/09 27/02/95  16.54.36  by  N.Cremel*/
/*-- Author :    Alfred Nathaniel   17/08/92*/
/* kuipcc.c: KUIP CDF Compiler */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kbrow.h"
#include "kuip/kmenu.h"

#ifdef IBM370
#define F77_EXTENSION ".fortran"
#endif

#ifdef vms
#define F77_EXTENSION ".for"
#endif

#ifdef CERNLIB_WINNT
#define F77_EXTENSION ".for"
#endif

#ifndef F77_EXTENSION
#define F77_EXTENSION ".f"
#endif

#define MAXLINE  512            /* maximum length of lines */
#define MAXNLEN   32            /* maximum length of names */

enum { ACT_ION, ACT_USER };
enum { BR_OWSE, BR_CLASS };
enum { COM_MAND, COM_HELP };

typedef struct {
  int used;
  int allocated;
  char *buffer;
} Buffer;

static Buffer cdef_menu;
static Buffer cdef_body;
static Buffer cdef_head;
static Buffer cdef_tail;

static Buffer fdef_body;
static Buffer fdef_head;

typedef struct {
  int      groups;
  int     *count;
  char  ***value;
} Group;

typedef struct {
  char    *name;                /* parameter name */
  int      abbrev;              /* minimum length that name is recognized */
  char    *prompt;              /* prompt string  */
  char    *dfault;              /* default value  */
  char    *width;               /* width of input field */
  int      range_count;         /* number of range values */
  char   **range_value;
  char   **option_text;         /* explanation text for options */
  Group    mutex;
  int     *mutex_index;
  Group    radio;
  int     *radio_index;
  int      select_count;        /* number of selection values */
  char   **select_value;
  KmParFlag flags;              /* special flags */
  KmParType type;               /* parameter type */
  char    *filter_default;      /* file name filter */
  char    *range_lower;         /* lower range value */
  char    *range_upper;         /* upper range value */
  char    *slider_lower;        /* slider lower range */
  char    *slider_upper;        /* slider upper range */
} Parameter;

typedef struct _Command {
  struct _Command *next;        /* link to next command structure */
  char    *name;                /* command name */
  int      total;               /* total number of parameters */
  int      mandatory;           /* number of mandatory parameters */
  int      list_par;            /* index+1 for parameter of list type */
  Parameter **par;              /* parameter descriptions */
  char    *action;              /* name of action routine */
  char    *user_help;           /* name of user help routine */
  int     nguidance;            /* number of lines in guidance text */
  char   **guidance;            /* guidance text */
  int     nkeyword;             /* number of "keyword" commands/menus */
  char   **keyword;             /* "keyword" commands/menus list */
  int     nhlink;                /* number of "links" (for HTML) */
  char   **hlink;                /* "link" list */
  int     nsource;              /* number of lines in source text */
  char   **source;              /* source text */
} Command;

typedef struct _Menu {
  struct _Menu *next;           /* link to next menu */
  struct _Menu *down;           /* link to submenu */
  struct _Menu *up;             /* link to parent menu */
  char     *path;               /* menu path */
  char     *name;               /* menu name */
  int       level;              /* depth of submenus */
  int      ncmds;               /* number of commands */
  Command **cmds;               /* list of command */
  int      nguidance;           /* number of lines in guidance text */
  char    **guidance;           /* guidance text */
  int     nkeyword;             /* number of "keyword" commands/menus */
  char   **keyword;             /* "keyword" commands/menus list */
  int     nhlink;                /* number of "links" (for HTML) */
  char   **hlink;                /* "link" list */
} Menu;

typedef struct {
  char    *text;                /* menu text */
  char    *accel;               /* accelerator definition */
  char    *exec;                /* action commands */
  char    *call;                /* action routine */
  BrActFlag flags;              /* flags from menu text and accelerator */
  BrActTag tag;                 /* for which window the action is defined */
} Action;

typedef struct {
  char    *name;                /* class name */
  char    *title;               /* class title */
  char    *scan_km;             /* user function scanning the directory */
  char    *scan_br;             /* user function scanning for browsables */
  int     nroot;                /* number of entries in root actions */
  Action **root;                /* action bindings in root menu */
  int     nopen;                /* number of entries in open actions */
  Action **open;                /* action bindings in open menu */
} Browsable;

typedef struct {
  char    *name;                /* class name */
  char    *title;               /* class title */
  char    *big_icon;            /* big icon name */
  char    *sm_icon;             /* small icon name */
  char    *user_icon;           /* name of user icon routine */
  int     ncont;                /* number of entries in cont actions */
  Action **cont;                /* action bindings in content menu */
  int     ngraf;                /* number of entries in graf actions */
  Action **graf;                /* action bindings in graphics menu */
} Class;

typedef struct {
  char    *name;                /* icon name */
  int      width;               /* width of the pixmap */
  int      height;              /* height of the pixmap */
  int     nbitmap;              /* length of bitmap */
  char    *bitmap;              /* bitmap data */
} Icon;

typedef struct {
  char    *action;              /* user routine */
  char    *menu;                /* menu title or NULL for a button */
  char    *label;               /* button label or menu item */
  char    *mnemo;               /* button mnemonic */
  char    *accel;               /* button accelerator */
  char    *accel_text;          /* accelerator text */
  KmButtFlag flags;            /* sensitivity type */
} Button;

static char *motif_FallBk = NULL;
static char *motif_TopWid = NULL;

static char line[MAXLINE];

static Menu       root_menu = { (Menu*)0, (Menu*)0, &root_menu,
                                  "", "", 0, 0, (Command**)0, 0, (char**)0 };
static Menu      *curr_menu = &root_menu;
static Command   *curr_cmd;

static Browsable **browsables;
static int        nbrowsables;
static Class     **classes;
static int        nclasses;
static Icon      **icons;
static int        nicons;
static Button    **buttons;
static int        nbuttons;
static int         have_graphics;

static int  line_count;
static char full_line[MAXLINE]; /* line as read from CDF */
static char **word;             /* pointers to start of words in parsed line */
static int   nword;             /* number of words on line */
static int   mword;             /* length of **word */

static char **external_F;       /* list of external Fortran routines */
static int   nexternal_F;       /* number of external Fortran routines */
static char **external_C;       /* list of external C functions */
static int   nexternal_C;       /* number of external C functions */
static int  *texternal_C;       /* type of external C functions */
enum { FUNC_SUBR, FUNC_INT, FUNC_CHAR, FUNC_pCHAR };

static int   is_menu;           /* flag if guidance belongs to menu */

static char *curr_name;         /* name of the C definition routine */

static int cmode;               /* flag if C code should be generated */
static int fmode;               /* flag if Fortran code should be generated */
static int split;               /* flag if code should go to separate files */
static char ifile[256];         /* input file name */
static char ofile[256];         /* output file name */

#define Malloc(type,size)      (type*)malloc( size*sizeof(type) )
#define Malloc1(type)          Malloc( type, 1 )
#define Realloc(type,ptr,size) ( ptr == NULL ? Malloc( type, size ) \
                            : (type*)realloc( (char*)ptr, size*sizeof(type) ) )

static void  appchr( Buffer*, int );
static void  append( Buffer*, const char* );
static void  appext( Buffer*, const char*, int );
static void  appflt( Buffer*, const char*, int );
static void  appint( Buffer*, int, int );
static void  applit( Buffer*, const char*, int );
static void  appnum( Buffer*, const char*, int, int );
static void  appstr( Buffer*, const char*, int );
static void  assign_external( const char*, const char*, int );
static void  cat_buffer( Buffer*, Buffer* );
static void  copy_buffer( Buffer*, Buffer* );
static void  check_abbrev( const char*, const char* );
static void  check_args( int, int );
static void  fatal( const char* );
static Menu* find_menu( Menu*, const char* );
static void  finish_name(void);
static void  flush_buffer( Buffer* );
static char* format_name( const char*, const char* );
static void  fredirect( const char* );
static void  init_buffer( Buffer* );
static char* lowercase( const char* );
static void  new_action( Action*, const char*, const char*, const char*,
                        const char*, BrActTag );
static char* new_external_C( const char*, int );
static char* new_external_F( const char* );
static void  only_cmode( const char* );
static void  only_fmode( const char* );
static char* output_action( const char*, const char*, int, Action** );
static void  output_browsables(void);
static void  output_buttons(void);
static void  output_classes(void);
static void  output_cmd_list( Menu* );
static void  output_external(void);
static void  output_icons(void);
static void  output_kupval( const char*, const char*, const char*,
                           const char*, const char*, const char* );
static void  output_menu_list( Menu* );
static int   parse_line(void);
static char** parse_range( int*, const char* );
static void  proc_action( int );
static void  proc_browse(void);
static void  proc_button(void);
static void  proc_class(void);
static void  proc_command( int );
static void  proc_comment(void);
static void  proc_fortran(void);
static void  proc_graphics(void);
static void  proc_guidance(void);
static void  proc_keyword(void);
static void  proc_hlink(void);
static void  proc_icon_bitmaps(void);
static void  proc_menu(void);
static void  proc_motif_customize(void);
static void  proc_name(void);
static void  proc_parameters(void);
static void  quit(void);
static int   read_line(void);
static int*  set_groups( Parameter*, Group* );
static void  unread_line(void);
static char* uppercase( const char* );
static void  warning( const char* );


int main( argc, argv )
     int argc;
     char **argv;
{
  char *p;
  int iarg = 1;
  int explicit = 0;

  cmode = 0;

  while( iarg < argc && argv[iarg][0] == '-' && argv[iarg][1] != '\0' ) {
    if( strcasecmp( argv[iarg], "-split" ) == 0 ) {
      split = 1;
    }
    else if( strcasecmp( argv[iarg], "-c" ) == 0 ) {
      cmode = 1;
      explicit = 1;
    }
    else if( strcasecmp( argv[iarg], "-f" ) == 0 ) {
      cmode = 0;
      explicit = 1;
    }
    else {
      fprintf( stderr, "Invalid option %s\n", argv[iarg] );
      quit();
    }
    iarg++;
  }

  if( argc > iarg ) {
    strcpy( ifile, argv[iarg] );
    iarg++;
    if( argc > iarg )
      strcpy( ofile, argv[iarg] );
    else {
      /* use input file name and chop off extension */
      strcpy( ofile, ifile );
      if( (p = strrchr( ofile, '.' )) != NULL && strpbrk( p, "/]" ) == NULL )
        *p = '\0';
    }
  }
  else {
    fprintf( stderr, "Enter CDF input file name: " );
    gets( ifile );
    fprintf( stderr, "Enter definition output file name: " );
    gets( ofile );
  }

  if( !explicit ) {
    /* derive output mode from file extension */
    int len = strlen( ofile );
    cmode = (strcasecmp( &ofile[len-2], ".c" ) == 0);
  }
  fmode = !cmode;

  if( (p = strrchr( ifile, '.' )) == NULL || strpbrk( p, "/]" ) != NULL )
    strcat( ifile, ".cdf" );

  if( (p = strrchr( ofile, '.' )) == NULL || strpbrk( p, "/]" ) != NULL )
    strcat( ofile, cmode ? ".c" : F77_EXTENSION );

  if( strcmp( ifile, "-" ) != 0 && freopen( ifile, "r", stdin ) == NULL ) {
    fprintf( stderr, "Cannot open input file '%s'\n", ifile );
    quit();
  }
  if( strcmp( ofile, "-" ) != 0 ) {
    fredirect( ofile );
  }

  while( read_line() > 0 ) {

    if( full_line[0] == '*' )
      continue;

    parse_line();

    if( strncasecmp( word[0], ">A", 2 ) == 0 ) {
      check_abbrev( word[0], ">Action" );
      proc_action( ACT_ION );
    }
    else if( strcasecmp( word[0], ">Browse" ) == 0 ) {
      proc_browse();
    }
    else if( strcasecmp( word[0], ">Button" ) == 0 ) {
      proc_button();
    }
    else if( strcasecmp( word[0], ">Class" ) == 0 ) {
      proc_class();
    }
    else if( strncasecmp( word[0], ">C", 2 ) == 0 ) {
      check_abbrev( word[0], ">Command" );
      proc_command( COM_MAND );
    }
    else if( strcasecmp( word[0], ">Fortran" ) == 0 ) {
      proc_fortran();
    }
    else if( strcasecmp( word[0], ">Graphics" ) == 0 ) {
      proc_graphics();
    }
    else if( strncasecmp( word[0], ">G", 2 ) == 0 ) {
      check_abbrev( word[0], ">Guidance" );
      proc_guidance();
    }
    else if( strncasecmp( word[0], ">K", 2 ) == 0 ) {
      check_abbrev( word[0], ">Keyword" );
      proc_keyword();
    }
    else if( strncasecmp( word[0], ">L", 2 ) == 0 ) {
      check_abbrev( word[0], ">Link" );
      proc_hlink();
    }
    else if( strncasecmp( word[0], ">H", 2 ) == 0 ) {
      check_abbrev( word[0], ">Help_item" );
      proc_command( COM_HELP );
    }
    else if( strcasecmp( word[0], ">Icon_bitmaps" ) == 0 ) {
      proc_icon_bitmaps();
    }
    else if( strcasecmp( word[0], ">Motif_customize" ) == 0 ) {
      proc_motif_customize();
    }
    else if( strncasecmp( word[0], ">M", 2 ) == 0 ) {
      check_abbrev( word[0], ">Menu" );
      proc_menu();
    }
    else if( strncasecmp( word[0], ">N", 2 ) == 0 ) {
      check_abbrev( word[0], ">Name" );
      proc_name();
    }
    else if( strncasecmp( word[0], ">P", 2 ) == 0 ) {
      check_abbrev( word[0], ">Parameters" );
      proc_parameters();
    }
    else if( strncasecmp( word[0], ">U", 2 ) == 0 ) {
      check_abbrev( word[0], ">User_help" );
      proc_action( ACT_USER );
    }
    else if( strncmp( word[0], ">*", 2 ) == 0 ) {
      proc_comment();
    }
    else {
      warning( "Invalid directive ignored" );
      proc_comment();
    }
  }

  finish_name();
  exit( 0 );
}


static
void warning( const char *text )
{
  fprintf( stderr, "Line %d - %s:\n%s\n", line_count, text, full_line );
}


static
void fatal( const char *text )
{
  warning( text );
  fprintf( stderr, "Fatal error in CDF\n" );
  quit();
}


static
void quit()
{
  exit( 1 );
}


static
void only_cmode( const char *keyw )
{
  if( !cmode ) {
    char text[80];
    sprintf( text, "%s only supported in C output mode", keyw );
    warning( text );
  }
}


static
void only_fmode( const char *keyw )
{
  if( !fmode ) {
    char text[80];
    sprintf( text, "%s only supported in Fortran output mode", keyw );
    warning( text );
  }
}


/*
 * redirect stdout into a file
 */
static
void fredirect( const char *file )
{
  FILE *stream;
#if 0
  char *format = fmode ? "rfm=var" : "rfm=stmlf";
  stream = freopen( file, "w", stdout, format );
#else
  stream = freopen( file, "w", stdout );
#endif
  if( stream == NULL ) {
    fprintf( stderr, "Cannot open output file '%s'\n", ofile );
    quit();
  }
}


/*****************************************************************************
 * routines for processing directives
 *****************************************************************************/

/*
 * output code for >Action and >User_help
 */
static
void proc_action( int what )
{

  check_args( 2, 2 );

  switch( what ) {
  case ACT_ION:
    curr_cmd->action = strdup( word[1] );
    break;
  case ACT_USER:
    curr_cmd->user_help = strdup( word[1] );
    break;
  }
}


/*
 * process comment lines >*
 */
static
void proc_comment()
{
  while( read_line() > 0 ) {
    /* skip lines until next CDF directive */
    if( full_line[0] == '>' ) {
      unread_line();
      return;
    }
  }
}


/*
 * output code for >Browse
 */
static
void proc_browse()
{
  Browsable *brw;
  BrActTag tag = BRACT_ROOT;
  Action *act;

  check_args( 4, 5 );
  only_cmode( ">Browse" );

  browsables = Realloc( Browsable*, browsables, ++nbrowsables );
  brw = browsables[nbrowsables-1] = Malloc1( Browsable );

  brw->name = strdup( word[1] );
  brw->title = str0dup( word[2] );
  brw->scan_km = str0dup( word[3] );
  brw->scan_br = str0dup( word[4] );
  brw->nroot = 0;
  brw->root = NULL;
  brw->nopen = 0;
  brw->open = NULL;

  while( read_line() > 0 ) {

    if( full_line[0] == '*' )
      continue;

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      return;
    }

    parse_line();
    if( strcmp( word[0], "+" ) == 0 ) {
      tag = BRACT_OPEN;
      continue;
    }
    if( tag == BRACT_ROOT ) {
      brw->root = Realloc( Action*, brw->root, ++brw->nroot );
      act = brw->root[brw->nroot-1] = Malloc1( Action );
    }
    else {
      brw->open = Realloc( Action*, brw->open, ++brw->nopen );
      act = brw->open[brw->nopen-1] = Malloc1( Action );
    }
    new_action( act, word[0], word[1], word[2], word[3], tag );
  }
}


/*
 * output code for >Button action b_text m_text chopt
 * output code for >Button ...
 * ... menu-title [/]button-label callb_routine [mnem.] [acc.] [acct.] [chopt]
 */
static
void proc_button()
{
  Button *but;

  check_args( 4, 8 );
  only_cmode( ">Button" );

  buttons = Realloc( Button*, buttons, ++nbuttons );
  but = buttons[nbuttons-1] = Malloc1( Button );

  but->menu = strdup( word[1] );
  but->label = strdup( word[2] );
  but->action = strdup( word[3] );
  but->mnemo = str0dup( word[4] );
  but->accel = str0dup( word[5] );
  but->accel_text = str0dup( word[6] );
  but->flags = KmButtSensitive;
  if( word[7] != NULL ) {
      if( strcasecmp( word[7], "NS" ) == 0 )
          but->flags = KmButtNonSensitive;
      else if( strcasecmp( word[7], "TS" ) == 0 )
          but->flags = KmButtToggleSensitive;
      else if( strcasecmp( word[7], "BR" ) == 0 )
          but->flags |= KmButtBrowser;
      else {
          warning( "Invalid button definition ignored" );
          nbuttons--;
      }
  }
  if( but->label[0] == '/' ) {
    but->label++;
    but->flags |= KmButtSeparator;
  }
}


/*
 * output code for >Class
 */
static
void proc_class()
{
  Class *cls;
  BrActTag tag = BRACT_CONT;
  Action *act;

  check_args( 2, 5 );
  only_cmode( ">Class" );

  classes = Realloc( Class*, classes, ++nclasses );
  cls = classes[nclasses-1] = Malloc1( Class );

  cls->name = strdup( word[1] );
  cls->title = strdup( word[2] );
  cls->big_icon = str0dup( word[3] );
  cls->sm_icon = str0dup( word[4] );
  cls->user_icon = str0dup( word[5] );
  cls->ncont = 0;
  cls->cont = NULL;
  cls->ngraf = 0;
  cls->graf = NULL;

  while( read_line() > 0 ) {

    if( full_line[0] == '*' )
      continue;

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      return;
    }

    parse_line();
    if( strcmp( word[0], "+" ) == 0 ) {
      tag = BRACT_GRAF;
      continue;
    }
    if( tag == BRACT_CONT ) {
      if( cls->ncont == 0 ) {
        if( ( cls->big_icon == NULL || cls->sm_icon == NULL )
           && cls->user_icon == NULL ) {
          warning( "Missing icon definition in >Class directive" );
        }
        if( cls->big_icon == NULL )
          cls->big_icon = "big_menu";
        if( cls->sm_icon == NULL )
          cls->sm_icon = "sm_menu";
      }
      cls->cont = Realloc( Action*, cls->cont, ++cls->ncont );
      act = cls->cont[cls->ncont-1] = Malloc1( Action );
    }
    else {
      cls->graf = Realloc( Action*, cls->graf, ++cls->ngraf );
      act = cls->graf[cls->ngraf-1] = Malloc1( Action );
    }
    new_action( act, word[0], word[1], word[2], word[3], tag );
  }
}


/*
 * output code for >Command and >Help_item
 */
static
void proc_command( int what )
{
  check_args( 2, 2 );

  curr_menu->cmds = Realloc( Command*, curr_menu->cmds, ++curr_menu->ncmds );
  curr_cmd = curr_menu->cmds[curr_menu->ncmds-1] = Malloc1( Command );

  curr_cmd->name = strdup( word[1] );
  curr_cmd->total = 0;
  curr_cmd->mandatory = 0;
  curr_cmd->list_par = 0;
  curr_cmd->par = NULL;
  curr_cmd->action = NULL;
  curr_cmd->user_help = NULL;
  curr_cmd->nguidance = 0;
  curr_cmd->guidance = NULL;
  curr_cmd->nkeyword = 0;
  curr_cmd->keyword = NULL;
  curr_cmd->nhlink = 0;
  curr_cmd->hlink = NULL;
  curr_cmd->nsource = 0;
  curr_cmd->source = NULL;

  is_menu = 0;
}


/*
 * output code for >Fortran
 */
static
void proc_fortran()
{
  int   nsource = 0;
  char **source = NULL;

  check_args( 1, 1 );
  only_fmode( ">Fortran" );

  while( read_line() >= 0 ) {

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      break;
    }

    if( !fmode )
      continue;

    source = Realloc( char*, source, ++nsource );
    source[nsource-1] = strdup( full_line );
  }

  curr_cmd->nsource = nsource;
  curr_cmd->source = source;
}


/*
 * output code for >Guidance
 */
static
void proc_guidance()
{
  int nguidance = 0;
  char **guidance = NULL;

  check_args( 1, 1 );

  while( read_line() >= 0 ) {

    if( full_line[0] == '*' )
      continue;

    if( full_line[0] == '\0' )
      continue;                 /* ignore empty lines and comments */

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      break;
    }
    if( strcmp( full_line, "." ) == 0 )
      full_line[0] = '\0';      /* make empty line */
    guidance = Realloc( char*, guidance, ++nguidance );
    guidance[nguidance-1] = strdup( full_line );
  }
  if( is_menu ) {
    curr_menu->nguidance = nguidance;
    curr_menu->guidance = guidance;
  }
  else {
    curr_cmd->nguidance = nguidance;
    curr_cmd->guidance = guidance;
  }
}



/*
 * output code for >Keyword
 */
static
void proc_keyword()
{
  int nkeyword = 0;
  char **keyword = NULL;

  check_args( 1, 1 );

  while( read_line() >= 0 ) {
    int i;

    if( full_line[0] == '*' )
      continue;

    if( full_line[0] == '\0' )
      continue;                 /* ignore empty lines and comments */

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      break; }

    parse_line();
    for (i = 0; i < nword; i++) {
         keyword = Realloc( char*, keyword, ++nkeyword );
         keyword[nkeyword-1] = strdup( word[i] );
    }

  } /* end ... while( read_line() >= 0 ) */

  if( is_menu ) {
    curr_menu->nkeyword = nkeyword;
    curr_menu->keyword = keyword;
  }
  else {
    curr_cmd->nkeyword = nkeyword;
    curr_cmd->keyword = keyword;
  }

}


/*
 * output code for >Link
 */
static
void proc_hlink()
{
  int nhlink = 0;
  char **hlink = NULL;

  check_args( 1, 1 );

  while( read_line() >= 0 ) {
    int i;

    if( full_line[0] == '*' )
      continue;

    if( full_line[0] == '\0' )
      continue;                 /* ignore empty lines and comments */

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      break; }

    parse_line();
    for (i = 0; i < nword; i++) {
         hlink = Realloc( char*, hlink, ++nhlink );
         hlink[nhlink-1] = strdup( word[i] );
    }

  } /* end ... while( read_line() >= 0 ) */

  if( is_menu ) {
    curr_menu->nhlink = nhlink;
    curr_menu->hlink = hlink;
  }
  else {
    curr_cmd->nhlink = nhlink;
    curr_cmd->hlink = hlink;
  }

}


/*
 * output code for >Icon_bitmaps
 */
static
void proc_icon_bitmaps()
{
  check_args( 1, 1 );
  only_cmode( ">Icon" );

  while( read_line() > 0 ) {
    Icon *icn;
    char icon_name[32];
    char *p;
    int width;
    int height;
    int nbytes;
    int n;

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      break;
    }

    parse_line();
    check_args( 3, 3 );
    if( strcmp( word[0], "#define" ) != 0 )
      check_args( 99, 99 );

    strcpy( icon_name, word[1] );
    if( (p = strrchr( icon_name, '_' )) == NULL )
      check_args( 99, 99 );

    *p++ = '\0';
    if( strcmp( p, "width" ) != 0 )
      check_args( 99, 99 );

    width = atoi( word[2] );

    if( read_line() <= 0 )
      check_args( 99, 99 );
    parse_line();
    check_args( 3, 3 );
    if( strcmp( word[0], "#define" ) != 0 )
      check_args( 99, 99 );

    if( strcmp( strrchr( word[1], '_' ), "_height" ) != 0 )
      check_args( 99, 99 );

    height = atoi( word[2] );

    if( read_line() <= 0 )
      check_args( 99, 99 );
    parse_line();
    check_args( 5, 5 );
    if( strcmp( word[0], "static" ) != 0 )
      check_args( 99, 99 );

    nbytes = ((width-1)/8+1) * height;

    icons = Realloc( Icon*, icons, ++nicons );
    icn = icons[nicons-1] = Malloc1( Icon );
    icn->name = strdup( icon_name );
    icn->width = width;
    icn->height = height;
    icn->nbitmap = nbytes;
    icn->bitmap = malloc( nbytes );

    for( n = 0; n < nbytes && read_line() > 0; n += 12 ) {
      int m = nbytes - n < 12 ? nbytes - n : 12;
      int i;

      parse_line();
      check_args( m, m );

      for( i = 0; i < m; i++ )
        icn->bitmap[n+i] = strtol( word[i], NULL, 0 );
    }
  }
}


/*
 * output code for >Menu
 */
static
void proc_menu()
{
  check_args( 2, 2 );

  curr_menu = find_menu( curr_menu, word[1] );
  is_menu = 1;
}


/*
 * output code for >Motif_customize [user_res] [top_widget]
 */
static
void proc_motif_customize()
{
  check_args( 1, 3 );
  only_cmode( ">Motif" );

  motif_FallBk = str0dup( word[1] );
  motif_TopWid = str0dup( word[2] );
}


/*
 * output code for >Name
 */
static
void proc_name()
{
  check_args( 2, 2 );

  if( curr_name != NULL )
    finish_name();
  curr_name = lowercase( word[1] );

  curr_menu = &root_menu;
}


/*
 * output code for >Parameters
 */
static
void proc_parameters()
{
  int optional = 0;
  int constant = 0;

  check_args( 1, 1 );

  while( read_line() > 0 ) {
    Parameter *par;
    char *star;
    int i;

    if( full_line[0] == '*' )
      continue;

    if( full_line[0] == '\0' )  /* ignore empty lines */
      continue;

    if( full_line[0] == '>' ) { /* loop until next CDF directive */
      unread_line();
      break;
    }
    if( strcmp( full_line, "+" ) == 0 ) { /* enter optional parameters */
      optional = 1;
      if( constant ) {
        warning( "Cannot have more optional parameters following constants" );
      }
      continue;
    }
    if( strcmp( full_line, "++" ) == 0 ) { /* enter constant parameters */
      optional = 1;
      constant = KmFLAG_CONST;
      continue;
    }
    parse_line();
    check_args( 3, 99 );

    if( !optional )
      curr_cmd->mandatory++;

    curr_cmd->par = Realloc( Parameter*, curr_cmd->par, ++curr_cmd->total );
    par = curr_cmd->par[curr_cmd->total-1] = Malloc1( Parameter );

    par->name = strdup( word[0] );
    star = strchr( par->name, '*' );
    if( star == NULL )
      par->abbrev = strlen( par->name );
    else {
      par->abbrev = star - par->name;
      strcpy( star, star + 1 );
    }
    par->prompt = strdup( word[1] );
    par->dfault = NULL;
    par->width = NULL;
    par->range_count = 0;
    par->range_value = NULL;
    par->option_text = NULL;
    par->mutex.groups = 0;
    par->mutex_index = NULL;
    par->radio.groups = 0;
    par->radio_index = NULL;
    par->select_count = 0;
    par->select_value = NULL;
    par->flags = constant;
    par->filter_default = NULL;
    par->range_lower = NULL;
    par->range_upper = NULL;
    par->slider_lower = NULL;
    par->slider_upper = NULL;

    if( strcasecmp( word[2], "C" ) == 0 )
      par->type = KmTYPE_CHAR;
    else if( strcasecmp( word[2], "I" ) == 0 )
      par->type = KmTYPE_INT;
    else if( strcasecmp( word[2], "R" ) == 0 )
      par->type = KmTYPE_REAL;
    else {
      warning( "Invalid parameter type; 'C' assumed" );
      par->type = KmTYPE_CHAR;
    }

    if( strncasecmp( word[0], "CHOPT", 5 ) == 0
       || strncasecmp( word[0], "OPTION", 6 ) == 0 )
      par->type = KmTYPE_OPTION;

    for( i = 3; i < nword; i++ ) {
      char *tag = word[i];
      char *value = strchr( tag, '=' );
      if( value != NULL ) {
        *value++ = '\0';
        if( *value == '\0' ) {
          if( i == nword - 1 ) {
            sprintf( line, "Value after %s= missing", tag );
            fatal( line );
          }
          value = word[++i];
        }
      }

      if( strcasecmp( tag, "D" ) == 0 ) { /* default */
        if( value == NULL )
          warning( "Default value missing" );
        else
          par->dfault = strdup( value );
      }
      else if( strcasecmp( tag, "R" ) == 0 ) { /* range */
        if( value == NULL )
          warning( "Range value missing" );
        else {
          char *colon = strchr( value, ':' );
          if( colon != NULL ) {   /* R=lo:hi */
            if( colon != value ) {
              *colon = '\0';
              par->range_lower = strdup( value );
              value = colon + 1;
            }
            if( *value != '\0' ) {
              par->range_upper = strdup( value );
            }
          }
          else {                  /* range of discrete values */
            if( par->type == KmTYPE_CHAR )
              par->type = KmTYPE_OPTION;
            par->range_value = parse_range( &par->range_count, value );
          }
        }
      }
      else if( strcasecmp( tag, "FILE" ) == 0 ) {
        par->type = KmTYPE_FILE;
        par->filter_default = str0dup( value );
      }
      else if( strcasecmp( tag, "FORGET" ) == 0 ) {
        if( value != NULL )
          warning( "Value in FORGET definition ignored" );
        par->flags |= KmFLAG_FORGET;
      }
      else if( strcasecmp( tag, "HIDDEN" ) == 0 ) {
        if( value != NULL )
          warning( "Value in HIDDEN definition ignored" );
        if( !optional )
          warning( "Cannot use HIDDEN for mandatory parameter" );
        else
          par->flags |= KmFLAG_HIDDEN;
      }
      else if( strcasecmp( tag, "LOOP" ) == 0 ) {
        if( value != NULL )
          warning( "Value in LOOP definition ignored" );
        curr_cmd->list_par = curr_cmd->total;
      }
      else if( strcasecmp( tag, "MINUS" ) == 0 ) {
        if( value != NULL )
          warning( "Value in MINUS definition ignored" );
        par->flags |= KmFLAG_MINUS;
      }
      else if( strcasecmp( tag, "MUTEX" ) == 0 ) {
        only_cmode( "MUTEX" );
        if( value == NULL )
          warning( "MUTEX value missing" );
        else {
          int n;
          n = ++par->mutex.groups;
          if( par->mutex.groups == 1 ) {
            par->mutex.count = (int*)malloc( sizeof(int) );
            par->mutex.value = (char***)malloc( sizeof(char**) );
          }
          else {
            par->mutex.count = (int*)realloc( (char*)par->mutex.count,
                                             n * sizeof(int) );
            par->mutex.value = (char***)realloc( (char*)par->mutex.value,
                                                n * sizeof(char**) );
          }
          par->mutex.value[n-1] = parse_range( &par->mutex.count[n-1], value );
        }
      }
      else if( strcasecmp( tag, "OPTION" ) == 0 ) {
        if( value != NULL )
          warning( "Value in OPTION definition ignored" );
        par->type = KmTYPE_OPTION;
      }
      else if( strcasecmp( tag, "QUOTE" ) == 0 ) {
        if( value != NULL )
          warning( "Value in QUOTE definition ignored" );
        par->flags |= KmFLAG_QUOTE;
      }
      else if( strcasecmp( tag, "RADIO" ) == 0 ) {
        only_cmode( "RADIO" );
        if( value == NULL )
          warning( "RADIO value missing" );
        else {
          int n;
          n = ++par->radio.groups;
          if( par->radio.groups == 1 ) {
            par->radio.count = (int*)malloc( sizeof(int) );
            par->radio.value = (char***)malloc( sizeof(char**) );
          }
          else {
            par->radio.count = (int*)realloc( (char*)par->radio.count,
                                             n * sizeof(int) );
            par->radio.value = (char***)realloc( (char*)par->radio.value,
                                                n * sizeof(char**) );
          }
          par->radio.value[n-1] = parse_range( &par->radio.count[n-1], value );
        }
      }
      else if( strcasecmp( tag, "SELECT" ) == 0 ) {
        only_cmode( "SELECT" );
        if( value == NULL )
          warning( "SELECT value missing" );
        else
          par->select_value = parse_range( &par->select_count, value );
      }
      else if( strcasecmp( tag, "SEPARATE" ) == 0 ) {
        if( value != NULL )
          warning( "Value in SEPARATE definition ignored" );
        par->flags |= KmFLAG_SEPARATE;
      }
      else if( strcasecmp( tag, "SLIDER" ) == 0 ) {
        only_cmode( "SLIDER" );
        if( value == NULL )
          warning( "SLIDER value missing" );
        else {
          char *colon = strchr( value, ':' );
          if( colon != NULL ) {
            if( colon != value ) {
              *colon = '\0';
              par->slider_lower = strdup( value );
              value = colon + 1;
            }
            if( *value != '\0' ) {
              par->slider_upper = strdup( value );
            }
          }
          else
            warning( "Invalid SLIDER definition ignored" );
        }
      }
      else if( strcasecmp( tag, "VARARG" ) == 0 ) {
        if( value != NULL )
          warning( "Value in VARARG definition ignored" );
        par->flags |= KmFLAG_VARARG;
      }
      else if( strcasecmp( tag, "WIDTH" ) == 0 ) {
        only_cmode( "WIDTH" );
        if( value == NULL )
          warning( "WIDTH value missing" );
        else
          par->width = strdup( value );
      }
      else
        warning( "Invalid parameter definition ignored" );
    }

    if( par->type == KmTYPE_OPTION ) {
      /* read option explanations:  "-X text" */

      if( par->range_count != 0 )
        par->option_text = (char**)calloc( par->range_count, sizeof(char*) );

      while( read_line() > 0 ) {

        if( full_line[0] == '*' ) /* ignore comments */
          continue;

        if( full_line[0] == '\0' ) /* ignore empty lines */
          continue;

        if( full_line[0] != '-' ) {
          unread_line();
          break;
        }
        else {
          char *value = strdup( full_line );
          char *text = strchr( value, ' ' );
          int n;

          if( text != NULL ) {
            char *p;
            *text++ = '\0';
            while( *text == ' ' ) {
              /* skip leading blanks */
              text++;
            }
            while( (p = strstr( text, "  " )) != NULL ) {
              /* remove multiple blanks */
              strcpy( p, p + 1 );
            }
          }
          value++;              /* skip the minus */

          /* look if option is already defined by R='...' */
          for( n = 0; n < par->range_count; n++ ) {
            if( strcasecmp( par->range_value[n], value ) == 0 )
              break;
          }
          if( n == par->range_count ) {
            par->range_count++;
            if( par->range_value == NULL ) {
              par->range_value = (char**)malloc( sizeof(char*) );
              par->option_text = (char**)malloc( sizeof(char*) );
            }
            else {
              par->range_value = (char**)realloc( (char*)par->range_value,
                                                 (n+1) * sizeof(char*) );
              par->option_text = (char**)realloc( (char*)par->option_text,
                                                 (n+1) * sizeof(char*) );
            }
            par->range_value[n] = value;
          }
          par->option_text[n] = text;
        }
      }
      if( par->dfault != NULL && par->range_count > 0
         && par->type != KmTYPE_OPTION ) {
        int n;
        /* look if default is already contained on value list */
        for( n = 0; n < par->range_count; n++ ) {
          if( strcasecmp( par->range_value[n], par->dfault ) == 0 )
            break;
          if( par->range_value[n][0] == '\0'
             && strcmp( par->dfault, " " ) == 0 )
            break;
        }
        if( n == par->range_count ) {
          par->range_count++;
          par->range_value = (char**)realloc( (char*)par->range_value,
                                             (n+1) * sizeof(char*) );
          par->option_text = (char**)realloc( (char*)par->option_text,
                                             (n+1) * sizeof(char*) );
          par->range_value[n] = par->dfault;
          par->option_text[n] = NULL;
        }
      }
      par->mutex_index = set_groups( par, &par->mutex );
      par->radio_index = set_groups( par, &par->radio );
    }
  }
}


/*
 * output code for >Graphics
 */
static
void proc_graphics()
{
  check_args( 1, 1 );
  only_cmode( ">Graphics" );

  have_graphics = 1;
}


/*
 * output the epilog for the definition routine
 */
static
void finish_name()
{
  static int first = 1;

  if( split ) {
    char fname[MAXNLEN];
    if( cmode )
      sprintf( fname, "%s.c", curr_name );
    if( fmode )
      sprintf( fname, "%s%s", curr_name, F77_EXTENSION );
    fredirect( fname );
  }
  if( cmode && (split || first) ) {
    time_t ltime;
    first = 0;
    time( &ltime );

    append( &cdef_head, "\
/*\n\
 * This file has been generated by the KUIP compiler.  Do NOT change it!\n\
 *\n" );
    sprintf( line, " * KUIP header: %d      Generation date: %s *\n",
            KUIP_VERSION, asctime( localtime( &ltime ) ) );
    append( &cdef_head, line );
    sprintf( line, " * Input file: %s\n */\n\n", ifile );
    append( &cdef_head, line );

    append( &cdef_head, "\
#if !defined(F77_LCASE) && !defined(F77_UCASE) && !defined(F77_USCORE)\n\
#  if defined(__EXTENDED__) && !defined(IBM370) && !defined(_IBMR2)\n\
#    define IBM370\n\
#  endif\n\
#  if defined(CRAY) || defined(IBM370) || defined(vms)\n\
#    define F77_UCASE\n\
#  else\n\
#    if ( defined(apollo) || defined(__apollo) ) && defined(APOFTN)\n\
#      define F77_LCASE\n\
#    else\n\
#      define F77_USCORE\n\
#    endif\n\
#  endif\n\
#endif\n\
\n\
#if (defined(WIN32)||defined(CERNLIB_WINNT)) && defined(CERNLIB_MSSTDCALL)\n\
#ifndef F77_UCASE\n\
#define F77_UCASE\n\
#endif\n\
#define type_of_call _stdcall\n\
#endif\n\
#ifndef type_of_call\n\
#  define type_of_call\n\
#endif\n\
\n\
typedef int     IntFunc();\n\
typedef char*   CharFunc();\n\
typedef char** pCharFunc();\n\
typedef void  type_of_call  SUBROUTINE();\n\
#ifdef IBM370\n\
#  pragma linkage(SUBROUTINE,FORTRAN)\n\
#endif\n\
\n\
extern void klnkmenu();\n\
extern void klnkbrcl();\n\
extern void klnkkmcl();\n\
extern void klnkicon();\n\
extern void klnkbutt();\n\
\n" );
    append( &cdef_head,
#include "kbrow_q1.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kbrow_q2.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kbrow_q3.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kbrow_q4.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "klink_q1.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kmenu_q1.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kmenu_q2.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kmenu_q3.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kmenu_q4.h"
           );
    append( &cdef_head, "\n\n" );
    append( &cdef_head,
#include "kmenu_q5.h"
           );
    append( &cdef_head, "\n\n" );
  }

  if( cmode ) {
    sprintf( line, "void type_of_call %s()\n{\n\n", new_external_F( curr_name ) );
    append( &cdef_body, line );
  }

  if( fmode ) {
    sprintf( line, "\
      SUBROUTINE %s\n\
      CHARACTER*1 BS1\n\
      PARAMETER (BS1='\\\\')\n\
      PARAMETER (MGUIDL=199)\n\
      CHARACTER*80 GUID\n\
      COMMON /KCGUID/ GUID(MGUIDL)\n", uppercase( curr_name ) );
    append( &fdef_head, line );
  }

  if( root_menu.down != NULL ) {
    output_menu_list( &root_menu );
    sprintf( line, "  klnkmenu( &_%s, %d );\n",
            &root_menu.down->path[1], KUIP_VERSION );
    append( &cdef_tail, line );
    root_menu.down = NULL;
  }
  if( nbrowsables > 0 ) {
    output_browsables();
    sprintf( line, "  klnkbrcl( &brclass_1, %d );\n", KUIP_VERSION );
    append( &cdef_tail, line );
    nbrowsables = 0;
    browsables = NULL;
  }
  if( nclasses > 0 ) {
    output_classes();
    sprintf( line, "  klnkkmcl( &kmclass_1, %d );\n", KUIP_VERSION );
    append( &cdef_tail, line );
    nclasses = 0;
    classes = NULL;
  }
  if( nicons > 0 ) {
    output_icons();
    sprintf( line, "  klnkicon( &kmicons_1, %d );\n", KUIP_VERSION );
    append( &cdef_tail, line );
    nicons = 0;
    icons = NULL;
  }
  if( nbuttons > 0 ) {
    output_buttons();
    sprintf( line, "  klnkbutt( &kmbuttons_1, %d );\n", KUIP_VERSION );
    append( &cdef_tail, line );
    nbuttons = 0;
    buttons = NULL;
  }
  if( have_graphics ) {
    assign_external( "graf_info", "ixmotif", FUNC_INT );
    assign_external( "graf_size", "IGRSIZ", FUNC_SUBR );
    assign_external( "graf_pick", "IGOBJ", FUNC_SUBR );
    assign_external( "graf_attr", "IGSET", FUNC_SUBR );
    assign_external( "graf_close", "ICLWK", FUNC_SUBR );
    have_graphics = 0;
  }
  if( motif_FallBk != NULL ) {
    assign_external( "user_FallBk", motif_FallBk, FUNC_pCHAR );
    motif_FallBk = NULL;
  }
  if( motif_TopWid != NULL ) {
    assign_external( "user_TopWid", motif_TopWid, FUNC_INT );
    motif_TopWid = NULL;
  }
  output_external();

  if( cmode ) {
    append( &cdef_tail, "}\n\n\n" );
    flush_buffer( &cdef_head );
    flush_buffer( &cdef_body );
    flush_buffer( &cdef_tail );
  }

  if( fmode ) {
    append( &fdef_body, "\
\n\
      CALL KUCMD('/',' ','SW')\n\
\n\
      END\n" );
    flush_buffer( &fdef_head );
    flush_buffer( &fdef_body );
  }
  is_menu = 0;
}


/*
 * change menu to the specified path
 * if menu does not exist then create it first
 */
static
Menu *find_menu( Menu *menu,
                const char *path )
{
  Menu *m;
  char *name;
  char *tail;

  if( menu == NULL || path == NULL || path[0] == '\0' )
    fatal( "Invalid menu path" );

  if( strcmp( path, "/" ) == 0 )
    return &root_menu;

  if( strcmp( path, "\\" ) == 0 || strcmp( path, ".." ) == 0 )
    return menu->up;

  if( path[0] == '/' )
    return find_menu( &root_menu, &path[1] );

  if( path[0] == '\\' )
    return find_menu( menu->up, &path[1] );

  if( strncmp( path, "../", 3 ) == 0 )
    return find_menu( menu->up, &path[3] );

  name = strdup( path );
  if( (tail = strchr( name, '/' )) != NULL ) {
    *tail++ = '\0';
    menu = find_menu( menu, name );
    return find_menu( menu, tail );
  }

  if( menu->down == NULL )
    m = menu->down = Malloc1( Menu );
  else {
    Menu *prev = NULL;
    for( m = menu->down; m != NULL; m = m->next ) {
      if( strcasecmp( m->name, name ) == 0 )
        return m;
      prev = m;
    }
    m = prev->next = Malloc1( Menu );
  }
  m->next = NULL;
  m->down = NULL;
  m->up = menu;
  m->path = malloc( strlen( menu->path ) + strlen( name ) + 2 );
  sprintf( m->path, "%s/%s", menu->path, name );
  m->name = strdup( name );
  m->level = menu->level + 1;
  m->ncmds = 0;
  m->cmds = NULL;
  m->nguidance = 0;
  m->guidance = NULL;
  m->nkeyword = 0;
  m->keyword = NULL;
  m->nhlink = 0;
  m->hlink = NULL;
  return m;
}


/*
 * output code for the complete command tree
 */
static
void output_menu_list( Menu *menu )
{
  if( menu == NULL || menu->down == NULL )
    return;

  if( fmode ) {
    for( menu = menu->down; menu != NULL; menu = menu->next ) {

      append( &fdef_body, "\n      CALL KUCMD(' '" );
      appstr( &fdef_body, menu->name, 1 );
      append( &fdef_body, ",'C')\n" );

      if( menu->nguidance > 0 ) {
        int i;
        for( i = 0; i < menu->nguidance; i++ ) {
          sprintf( line, "      GUID(%d)=", i + 1 );
          append( &fdef_body, line );
          appstr( &fdef_body, menu->guidance[i], 0 );
          append( &fdef_body, "\n" );
        }
        append( &fdef_body, "      CALL KUGUID(" );
        appstr( &fdef_body, menu->name, 0 );
        sprintf( line, ",GUID,%d,'S')\n", menu->nguidance );
        append( &fdef_body, line );
      }
      append( &fdef_body, "\n      CALL KUCMD(" );
      appstr( &fdef_body, menu->name, 0 );
      append( &fdef_body, ",' ','SW')\n" );

      output_cmd_list( menu );
      output_menu_list( menu );

      append( &fdef_body, "\n      CALL KUCMD('..',' ','SW')\n" );
    }
    return;
  }

  for( menu = menu->down; menu != NULL; menu = menu->next ) {
    char *mname = format_name( menu->path, NULL );
    char *mnext = menu->next == NULL ? " (KmMenu*)0"
                : format_name( menu->next->path, NULL );
    char *mdown = menu->down == NULL ? " (KmMenu*)0"
                : format_name( menu->down->path, NULL );
    char *cname = menu->ncmds == 0 ? " (KmCommand*)0"
                : format_name( menu->path, menu->cmds[0]->name );
    char *gname = menu->nguidance == 0 ? " (char**)0"
                : format_name( menu->path, "guidance" );
    char *kname = menu->nkeyword  == 0 ? " (char**)0"
                : format_name( menu->path, "keyword" );
    char *lname = menu->nhlink  == 0 ? " (char**)0"
                : format_name( menu->path, "hlink" );
    Buffer cdef_temp;

    init_buffer( &cdef_temp );
    copy_buffer( &cdef_temp, &cdef_menu );

    output_menu_list( menu );
    output_cmd_list( menu );

    if( menu->nguidance > 0 ) {
      int i;
      sprintf( line, "static char *%s[] = { ", &gname[1] );
      append( &cdef_menu, line );
      for( i = 0; i < menu->nguidance; i++ )
        appstr( &cdef_menu, menu->guidance[i], i > 0 );
      append( &cdef_menu, " };\n" );
    }
    if( menu->nkeyword > 0 ) {
      int i;
      sprintf( line, "static char *%s[] = { ", &kname[1] );
      append( &cdef_menu, line );
      for( i = 0; i < menu->nkeyword; i++ )
        appstr( &cdef_menu, menu->keyword[i], i > 0 );
      append( &cdef_menu, " };\n" );
    }
    if( menu->nhlink > 0 ) {
      int i;
      sprintf( line, "static char *%s[] = { ", &lname[1] );
      append( &cdef_menu, line );
      for( i = 0; i < menu->nhlink; i++ )
        appstr( &cdef_menu, menu->hlink[i], i > 0 );
      append( &cdef_menu, " };\n" );
    }
    sprintf( line, "static KmMenu %s = { %s, %s", &mname[1], mnext, mdown );
    append( &cdef_menu, line );
    appstr( &cdef_menu, menu->path, 1 );
    appstr( &cdef_menu, menu->name, 1 );
    appint( &cdef_menu, menu->level, 1 );
    applit( &cdef_menu, cname, 1 );
    appint( &cdef_menu, menu->nguidance, 1 );
    applit( &cdef_menu, &gname[1], 1 );
    appint( &cdef_menu, menu->nkeyword, 1 );
    applit( &cdef_menu, &kname[1], 1 );
    appint( &cdef_menu, menu->nhlink, 1 );
    applit( &cdef_menu, &lname[1], 1 );
    append( &cdef_menu, " };\n\n" );
    cat_buffer( &cdef_menu, &cdef_temp );
  }
  cat_buffer( &cdef_body, &cdef_menu );
}


/*
 * output code for CALL KUPVAL
 */
static
void output_kupval( const char *cmd_name,
                   const char *par_name,
                   const char *ival,
                   const char *rval,
                   const char *cval,
                   const char *tag )
{
  append( &fdef_body, "      CALL KUPVAL(" );
  appstr( &fdef_body, cmd_name, 0 );
  appstr( &fdef_body, par_name, 1 );
  applit( &fdef_body, ival, 1 );
  appflt( &fdef_body, rval, 1 );
  appstr( &fdef_body, cval, 1 );
  appstr( &fdef_body, tag, 1 );
  append( &fdef_body, ")\n" );
}


/*
 * output code for all commands linked to the menu
 */
static
void output_cmd_list( Menu *menu )
{
  int ncmd;

  if( menu == NULL )
    return;

  if( fmode ) {
    for( ncmd = 0; ncmd < menu->ncmds; ncmd++ ) {
      Command *cmd = menu->cmds[ncmd];
      int n;
      int i;

      append( &fdef_body, "\n      CALL KUCMD(' '" );
      appstr( &fdef_body, cmd->name, 1 );
      append( &fdef_body, ",'C')\n" );

      for( i = 0; i < cmd->total; i++ ) {
        Parameter *par = cmd->par[i];
        char *ptype;
        char  popt[8];
        char *pdefc;
        char *pdefi;
        char *pdefr;
        char *prloi = NULL;
        char *prlor = NULL;
        char *prhii = NULL;
        char *prhir = NULL;

        popt[0] = '\0';
        if( i >= cmd->mandatory )
          strcat( popt, "O" );
        if( (par->flags & KmFLAG_CONST) != 0 )
          strcat( popt, "C" );
        if( (par->flags & KmFLAG_FORGET) != 0 )
          strcat( popt, "F" );
        if( (par->flags & KmFLAG_HIDDEN) != 0 )
          strcat( popt, "H" );
        if( (par->flags & KmFLAG_MINUS) != 0 )
          strcat( popt, "M" );
        if( (par->flags & KmFLAG_QUOTE) != 0 )
          strcat( popt, "Q" );
        if( (par->flags & KmFLAG_SEPARATE) != 0 )
          strcat( popt, "S" );
        if( (par->flags & KmFLAG_VARARG) != 0 )
          strcat( popt, "V" );

        switch( par->type ) {
        case KmTYPE_INT:
          ptype = "I";
          pdefc = " ";
          pdefi = par->dfault;
          pdefr = "0.";
          prloi = par->range_lower;
          prlor = "0.";
          prhii = par->range_upper;
          prhir = "0.";
          break;
        case KmTYPE_REAL:
          ptype = "R";
          pdefc = " ";
          pdefi = "0";
          pdefr = par->dfault;
          prloi = "0";
          prlor = par->range_lower;
          prhii = "0";
          prhir = par->range_upper;
          break;
        case KmTYPE_OPTION:
          ptype = "O";
          pdefc = par->dfault;
          pdefi = "0";
          pdefr = "0.";
          break;
        default:
          ptype = "C";
          pdefc = par->dfault;
          pdefi = "0";
          pdefr = "0.";
          break;
        }

        append( &fdef_body, "      CALL KUPAR(" );
        appstr( &fdef_body, cmd->name, 0 );
        appstr( &fdef_body, par->name, 1 );
        appstr( &fdef_body, par->prompt, 1 );
        sprintf( line, ",'%s%s','S')\n", ptype, popt );
        append( &fdef_body, line );

        if( par->dfault != NULL )
          output_kupval( cmd->name, par->name, pdefi, pdefr, pdefc, "D" );

        if( par->range_lower != NULL )
          output_kupval( cmd->name, par->name, prloi, prlor, NULL, "L" );

        if( par->range_upper != NULL )
          output_kupval( cmd->name, par->name, prhii, prhir, NULL, "H" );

        if( par->range_count > 0 ) {
          if( par->type == KmTYPE_INT || par->type == KmTYPE_REAL ) {
            for( n = 0; n < par->range_count; n++ ) {
              if( par->type == KmTYPE_INT )
                output_kupval( cmd->name, par->name,
                              par->range_value[n], "0.", NULL, "V" );
              else
                output_kupval( cmd->name, par->name,
                              "0", par->range_value[n], NULL, "V" );
            }
          }
          else {
            if( par->range_value[0][0] == '\0' )
              strcpy( line, " " );
            else
              strcpy( line, par->range_value[0] );

            for( n = 1; n < par->range_count; n++ ) {
              if( par->range_value[n][0] == '\0' )
                strcat( line, ", " );
              else {
                strcat( line, "," );
                strcat( line, par->range_value[n] );
              }
            }
            output_kupval( cmd->name, par->name,
                          "0", "0.", line, "V" );

            if( par->type == KmTYPE_OPTION ) {
              for( n = 0; n < par->range_count; n++ ) {
                if( par->option_text[n] != NULL ) {
                  /* explanation text for "-option" */
                  line[0] = '-';
                  strcpy( &line[1], par->range_value[n] );
                  output_kupval( cmd->name, par->name,
                                "0", "0.", par->option_text[n], line );
                }
              }
            }
          }
        }

        if( par->abbrev != strlen( par->name ) ) {
          char pmlen[8];
          sprintf( pmlen, "%d", par->abbrev );
          output_kupval( cmd->name, par->name, pmlen, "0.", NULL, "ABBREV" );
        }

        if( par->flags != 0 ) {
          char pmflags[8];
          sprintf( pmflags, "%d", par->flags );
          output_kupval( cmd->name, par->name, pmflags, "0.", NULL, "FLAGS" );
        }

        if( cmd->list_par == i + 1 ) {
          output_kupval( cmd->name, par->name, "1", "0.", NULL, "LOOP" );
        }
      }

      if( cmd->nguidance > 0 ) {
        for( n = 0; n < cmd->nguidance; n++ ) {
          sprintf( line, "      GUID(%d)=", n + 1 );
          append( &fdef_body, line );
          appstr( &fdef_body, cmd->guidance[n], 0 );
          append( &fdef_body, "\n" );
        }
        sprintf( line, "\
      CALL KUGUID('%s',GUID,%d,'S')\n",
                cmd->name, cmd->nguidance );
        append( &fdef_body, line );
      }

      if( cmd->user_help != NULL ) {
        sprintf( line, "\
      CALL KUACH('%s',%s)\n",
                cmd->name, cmd->user_help );
        append( &fdef_body, line );
        new_external_F( cmd->user_help );
      }

      if( cmd->action != NULL ) {
        sprintf( line, "\
      CALL KUACT('%s',%s)\n",
                cmd->name, cmd->action );
        append( &fdef_body, line );
        new_external_F( cmd->action );
      }

      for( n = 0; n < cmd->nsource; n++ ) {
        append( &fdef_body, cmd->source[n] );
      }
    }
    return;
  }

  for( ncmd = menu->ncmds; ncmd > 0; ncmd-- ) {
    Command *cmd = menu->cmds[ncmd-1];
    char *cname = format_name( menu->path, cmd->name );
    char *cnext = ncmd == menu->ncmds ? " (KmCommand*)0"
                : format_name( menu->path, menu->cmds[ncmd]->name );
    char *gname, *kname, *lname;
    char *plist;

    if( cmd->total == 0 ) {
      plist = " (KmParameter**)0";
    }
    else {
      int i;
      for( i = 0; i < cmd->total; i++ ) {
        Parameter *par = cmd->par[i];
        char *pname = format_name( &cname[1], par->name );
        char *prange;
        char *ptext = " (char**)0";
        char *pmutex = " (int*)0";
        char *pradio = " (int*)0";
        char *pselect;
        char *ptype = " (void*)0";
        char *ktype = NULL;
        int width = 0;
        int n;

        switch( par->type ) {
        case KmTYPE_CHAR:
          ktype = "KmTYPE_CHAR";
          width = 20;
          break;
        case KmTYPE_FILE:
          ktype = "KmTYPE_FILE";
          width = 20;
          break;
        case KmTYPE_INT:
          ktype = "KmTYPE_INT";
          width = 8;
          break;
        case KmTYPE_OPTION:
          ktype = "KmTYPE_OPTION";
          width = 20;
          break;
        case KmTYPE_REAL:
          ktype = "KmTYPE_REAL";
          width = 12;
          break;
        }

        /* discrete range values */
        if( par->range_count == 0 ) {
          prange = " (char**)0";
        }
        else {
          prange = format_name( &pname[1], "range" );
          sprintf( line, "static char *%s[] = { ", &prange[1] );
          append( &cdef_body, line );
          for( n = 0; n < par->range_count; n++ ) {
            appstr( &cdef_body, par->range_value[n], n > 0 );
            if( par->range_value[n][0] == '-' )
              par->flags |= KmFLAG_MINUS;
          }
          append( &cdef_body, " };\n" );
          width = 8;
        }

        /* values for selection box */
        if( par->select_count == 0 ) {
          par->select_count = par->range_count;
          pselect = prange;
        }
        else {
          pselect = format_name( &pname[1], "select" );
          sprintf( line, "static char *%s[] = { ", &pselect[1] );
          append( &cdef_body, line );
          for( n = 0; n < par->select_count; n++ )
            appstr( &cdef_body, par->select_value[n], n > 0 );
          append( &cdef_body, " };\n" );
        }

        /* type dependent substructures */
        switch( par->type ) {
          int decimals;

        case KmTYPE_CHAR:
          break;

        case KmTYPE_FILE:
          ptype = format_name( &pname[1], "type" );
          if( par->filter_default == NULL || par->filter_default[0] == '\0' )
            par->filter_default = "*.*";
          sprintf( line, "static KmParFile   %s = { ", &ptype[1] );
          append( &cdef_body, line );
          appstr( &cdef_body, par->filter_default, 0 );
          append( &cdef_body, ", (char*)0 };\n" );
          break;

        case KmTYPE_OPTION:
          if( par->option_text != NULL ) {
            ptext = format_name( &pname[1], "text" );
            sprintf( line, "static char *%s[] = { ", &ptext[1] );
            append( &cdef_body, line );
            for( n = 0; n < par->range_count; n++ )
              appstr( &cdef_body, par->option_text[n], n > 0 );
            append( &cdef_body, " };\n" );
          }
          if( par->mutex_index != NULL ) {
            pmutex = format_name( &pname[1], "mutex" );
            sprintf( line, "static int %s[] = { ", &pmutex[1] );
            append( &cdef_body, line );
            for( n = 0; n < par->range_count; n++ )
              appint( &cdef_body, par->mutex_index[n], n > 0 );
            append( &cdef_body, " };\n" );
          }
          if( par->radio_index != NULL ) {
            pradio = format_name( &pname[1], "radio" );
            sprintf( line, "static int %s[] = { ", &pradio[1] );
            append( &cdef_body, line );
            for( n = 0; n < par->range_count; n++ )
              appint( &cdef_body, par->radio_index[n], n > 0 );
            append( &cdef_body, " };\n" );
          }
          ptype = format_name( &pname[1], "type" );
          sprintf( line, "static KmParOption %s = { ", &ptype[1] );
          append( &cdef_body, line );
          applit( &cdef_body, &ptext[1], 0 );
          applit( &cdef_body, &pmutex[1], 1 );
          applit( &cdef_body, &pradio[1], 1 );
          append( &cdef_body, " };\n" );
          break;

        case KmTYPE_INT:
          if( par->range_lower != NULL && par->range_upper != NULL ) {
            int lower = atoi( par->range_lower );
            int upper = atoi( par->range_upper );
            int max;
            if( lower < 0 )
              lower = -lower;
            if( upper < 0 )
              upper = -upper;
            max = lower > upper ? lower : upper;
            for( width = 1; max > 0; max /= 10 )
              width++;
          }
          /* fallthrough */
        case KmTYPE_REAL:
          ptype = format_name( &pname[1], "type" );
          if( par->slider_lower == NULL )
            par->slider_lower = par->range_lower;
          if( par->slider_upper == NULL )
            par->slider_upper = par->range_upper;
          decimals = 0;
          if( par->slider_lower != NULL ) {
            char *point = strchr( par->slider_lower, '.' );
            if( point != NULL )
              decimals = strlen( point + 1 );
          }
          if( par->slider_upper != NULL ) {
            char *point = strchr( par->slider_upper, '.' );
            if( point != NULL ) {
              int n = strlen( point + 1 );
              if( n > decimals )
                decimals = n;
            }
          }
          sprintf( line, "static KmPar%s   %s = { ",
                  par->type == KmTYPE_INT ? "Int " : "Real", &ptype[1] );
          append( &cdef_body, line );
          appstr( &cdef_body, par->range_lower, 0 );
          appstr( &cdef_body, par->range_upper, 1 );
          appstr( &cdef_body, par->slider_lower, 1 );
          appstr( &cdef_body, par->slider_upper, 1 );
          appint( &cdef_body, decimals, 1 );
          append( &cdef_body, " };\n" );

          break;
        }

        if( par->width != NULL )
          width = atoi( par->width );

        sprintf( line, "static KmParameter %s = { ", &pname[1] );
        append( &cdef_body, line);
        appstr( &cdef_body, par->name, 0 );
        appint( &cdef_body, par->abbrev, 1 );
        appstr( &cdef_body, par->prompt, 1 );
        appstr( &cdef_body, par->dfault, 1 );
        appstr( &cdef_body, NULL, 1 ); /* last */
        appint( &cdef_body, width, 1 );
        appint( &cdef_body, par->range_count, 1 );
        applit( &cdef_body, &prange[1], 1 );
        appint( &cdef_body, par->select_count, 1 );
        applit( &cdef_body, &pselect[1], 1 );
        appnum( &cdef_body, "(KmParFlag)", par->flags, 1 );
        applit( &cdef_body, ktype, 1 );
        applit( &cdef_body, ptype, 1 );
        append( &cdef_body, " };\n" );
      }

      plist = format_name( &cname[1], "parameters" );
      sprintf( line, "static KmParameter *%s[] = { ", &plist[1] );
      append( &cdef_body, line );
      for( i = 0; i < cmd->total; i++ ) {
        Parameter *par = cmd->par[i];
        char *pname = format_name( &cname[1], par->name );
        applit( &cdef_body, pname, i > 0 );
      }
      append( &cdef_body, " };\n" );
    }

    if( cmd->nguidance == 0 ) {
      gname = " (char**)0";
    }
    else {
      int i;
      gname = format_name( &cname[1], "guidance" );
      sprintf( line, "static char *%s[] = { ", &gname[1] );
      append( &cdef_body, line );
      for( i = 0; i < cmd->nguidance; i++ )
        appstr( &cdef_body, cmd->guidance[i], i > 0 );
      append( &cdef_body, " };\n" );
    }

    if( cmd->nkeyword == 0 ) {
      kname = " (char**)0";
    }
    else {
      int i;
      kname = format_name( &cname[1], "keyword" );
      sprintf( line, "static char *%s[] = { ", &kname[1] );
      append( &cdef_body, line );
      for( i = 0; i < cmd->nkeyword; i++ )
        appstr( &cdef_body, cmd->keyword[i], i > 0 );
      append( &cdef_body, " };\n" );
    }

    if( cmd->nhlink == 0 ) {
      lname = " (char**)0";
    }
    else {
      int i;
      lname = format_name( &cname[1], "hlink" );
      sprintf( line, "static char *%s[] = { ", &lname[1] );
      append( &cdef_body, line );
      for( i = 0; i < cmd->nhlink; i++ )
        appstr( &cdef_body, cmd->hlink[i], i > 0 );
      append( &cdef_body, " };\n" );
    }


    sprintf( line, "static KmCommand %s = { %s", &cname[1], cnext );
    append( &cdef_body, line );
    sprintf( line, "%s/%s", menu->path, cmd->name );
    appstr( &cdef_body, line, 1 );
    appstr( &cdef_body, cmd->name, 1 );
    append( &cdef_body, ", 0" ); /* hidden */
    appint( &cdef_body, menu->level, 1 );
    appint( &cdef_body, cmd->total, 1 );
    appint( &cdef_body, cmd->mandatory, 1 );
    applit( &cdef_body, &plist[1], 1 );
    appint( &cdef_body, cmd->list_par, 1 );
    append( &cdef_body, ", 0" ); /* xcount */
    appext( &cdef_body, cmd->action, FUNC_INT );
    appext( &cdef_body, cmd->user_help, FUNC_pCHAR );
    appint( &cdef_body, cmd->nguidance, 1 );
    applit( &cdef_body, &gname[1], 1 );
    appint( &cdef_body, cmd->nkeyword, 1 );
    applit( &cdef_body, &kname[1], 1 );
    appint( &cdef_body, cmd->nhlink, 1 );
    applit( &cdef_body, &lname[1], 1 );
    append( &cdef_body, ", 0, (char**)0, (char*)0, (int*)0 };\n\n" );
  }
}


/*
 * output code for linkage defines
 */
static
void output_external()
{
  if( fmode ) {
    int i;
    for( i = 0; i < nexternal_F; i++ ) {
      append( &fdef_head, "      EXTERNAL " );
      append( &fdef_head, uppercase( external_F[i] ) );
      append( &fdef_head, "\n" );
    }
    nexternal_F = 0;
    external_F = NULL;
    return;
  }

  if( nexternal_F > 0 ) {
    int i;

    append( &cdef_head, "#ifdef F77_LCASE\n" );
    for( i = 0; i < nexternal_F; i++ ) {
      char *name = external_F[i];
      sprintf( line, "#  define %s_ %s\n", name, name );
      append( &cdef_head, line );
    }
    append( &cdef_head, "#endif\n\n" );

    append( &cdef_head, "#ifdef F77_UCASE\n" );
    for( i = 0; i < nexternal_F; i++ ) {
      char *name = external_F[i];
      sprintf( line, "#  define %s_ %s\n", name, uppercase( name ) );
      append( &cdef_head, line );
    }
    append( &cdef_head, "#endif\n\n" );

    append( &cdef_head, "#ifdef IBM370\n" );
    for( i = 0; i < nexternal_F; i++ ) {
      char *name = external_F[i];
      sprintf( line, "#  pragma linkage(%s,FORTRAN)\n", uppercase( name ) );
      append( &cdef_head, line );
    }
    append( &cdef_head, "#endif\n\n" );

    for( i = 0; i < nexternal_F; i++ ) {
      char *name = external_F[i];
      sprintf( line, "extern void type_of_call %s_();\n", name );
      append( &cdef_head, line );
    }
    append( &cdef_head, "\n" );
    nexternal_F = 0;
    external_F = NULL;
  }

  if( nexternal_C > 0 ) {
    int i;
    for( i = 0; i < nexternal_C; i++ ) {
      char *name = external_C[i];
      char *type = NULL;
      switch( texternal_C[i] ) {
      case FUNC_INT:
        type = "int ";
        break;
      case FUNC_CHAR:
        type = "char *";
        break;
      case FUNC_pCHAR:
        type = "char **";
        break;
      }
      sprintf( line, "extern %s%s();\n", type, name );
      append( &cdef_head, line );
    }
    append( &cdef_head, "\n" );
    nexternal_C = 0;
    external_C = NULL;
    texternal_C = NULL;
  }
}


/*
 * output code for external definitions
 */
static
void appext( Buffer *buf,
            const char *name,
            int type )
{
  char *ffun = "(SUBROUTINE*)0";
  char *cfun = NULL;

  switch( type ) {
  case FUNC_INT:
    cfun = "(IntFunc*)0";
    break;
  case FUNC_CHAR:
    cfun = "(CharFunc*)0";
    break;
  case FUNC_pCHAR:
    cfun = "(pCharFunc*)0";
    break;
  }

  if( name != NULL ) {
    char *fun = strdup( name );
    char *lan = strchr( fun, '%' );
    int f77 = 1;

    if( lan != NULL ) {
      *lan++ = '\0';
      f77 = toupper( lan[0] ) != 'C';
    }
    if( f77 )
      ffun = new_external_F( fun );
    else
      cfun = new_external_C( fun, type );
  }
  applit( buf, ffun, 1 );
  applit( buf, cfun, 1 );
}


static
void assign_external( const char *name,
                     const char *fun,
                     int type )
{
  if( type == FUNC_SUBR )
    sprintf( line, "  klnkaddr.%s_F = %s;\n",
            name, new_external_F( fun ) );
  else
    sprintf( line, "  klnkaddr.%s_C = %s;\n",
            name, new_external_C( fun, type ) );
  append( &cdef_tail, line );
}


static
char *new_external_F( const char *name )
{
  int i;
  char *lname = lowercase( name );
  char *uname = malloc( 64 );
  sprintf( uname, "%s_", lname );

  for( i = 0; i < nexternal_F; i++ ) {
    if( strcasecmp( external_F[i], name ) == 0 )
      return uname;
  }

  external_F = Realloc( char*, external_F, ++nexternal_F );
  external_F[nexternal_F-1] = lname;
  return uname;
}


static
char *new_external_C( const char *fun,
                     int type )
{
  char *name = strdup( fun );
  char *lan = strchr( name, '%' );
  int i;

  if( lan != NULL )
    *lan = '\0';

  for( i = 0; i < nexternal_C; i++ ) {
    if( strcasecmp( external_C[i], name ) == 0 )
      return name;
  }

  external_C = Realloc( char*, external_C, ++nexternal_C );
  external_C[nexternal_C-1] = strdup( name );
  texternal_C = Realloc( int, texternal_C, nexternal_C );
  texternal_C[nexternal_C-1] = type;
  return name;
}


/*
 * output code for browsable definitions
 */
static
void output_browsables()
{
  int n;
  for( n = nbrowsables; n > 0; n-- ) {
    Browsable *brw = browsables[n-1];
    char *proot;
    char *popen;
    char bname[16];

    sprintf( bname, "brclass_%d", n );
    proot = output_action( bname, "root", brw->nroot, brw->root );
    popen = output_action( bname, "open", brw->nopen, brw->open );

    sprintf( line, "static BrClass %s = { ", bname );
    append( &cdef_body, line );
    if( n == nbrowsables )
      append( &cdef_body, "(BrClass*)0" );
    else {
      sprintf( line, "&brclass_%d", n + 1 );
      append( &cdef_body, line );
    }
    appstr( &cdef_body, brw->name, 1 );
    appstr( &cdef_body, brw->title, 1 );
    appext( &cdef_body, brw->scan_km, FUNC_pCHAR );
    appext( &cdef_body, brw->scan_br, FUNC_pCHAR );
    applit( &cdef_body, proot, 1 );
    applit( &cdef_body, popen, 1 );
    append( &cdef_body, " };\n\n" );
  }
}


/*
 * output code for class definitions
 */
static
void output_classes()
{
  int n;
  for( n = nclasses; n > 0; n-- ) {
    Class *cls = classes[n-1];
    char *name = cls->name;
    int is_dir = name[0] == '/';
    char *pcont;
    char *pgraf;
    char cname[16];

    sprintf( cname, "kmclass_%d", n );
    pcont = output_action( cname, "cont", cls->ncont, cls->cont );
    pgraf = output_action( cname, "graf", cls->ngraf, cls->graf );

    sprintf( line, "static KmClass %s = { ", cname );
    append( &cdef_body, line );
    if( n == nclasses )
      append( &cdef_body, "(KmClass*)0" );
    else {
      sprintf( line, "&kmclass_%d", n + 1 );
      append( &cdef_body, line );
    }
    appint( &cdef_body, is_dir, 1 );
    appstr( &cdef_body, &name[is_dir], 1 );
    appstr( &cdef_body, cls->title, 1 );
    appstr( &cdef_body, cls->big_icon, 1 );
    append( &cdef_body, ", (KmIcon*)0" ); /* bicon */
    appstr( &cdef_body, cls->sm_icon, 1 );
    append( &cdef_body, ", (KmIcon*)0" ); /* sicon */
    appext( &cdef_body, cls->user_icon, FUNC_INT );
    applit( &cdef_body, pcont, 1 );
    applit( &cdef_body, pgraf, 1 );
    appint( &cdef_body, 0, 1 ); /* obj_count */
    append( &cdef_body, " };\n\n" );
  }
}


/*
 * output code for action definitions
 */
static
char *output_action( const char *cname,
                    const char *aname,
                    int n,
                    Action **act_list )
{
  char *pact = "(BrAction*)0";
  int i;

  for( i = n; i > 0; i-- ) {
    Action *act = act_list[i-1];
    char caname[32];
    char *tag = NULL;

    sprintf( caname, "&%s_%s_%d", cname, aname, i );
    if( i == 1 )
      pact = strdup( caname );
    sprintf( line, "static BrAction %s = { ", &caname[1] );
    append( &cdef_body, line );
    if( i == n )
      append( &cdef_body, "(BrAction*)0" );
    else {
      sprintf( line, "&%s_%s_%d", cname, aname, i + 1 );
      append( &cdef_body, line );
    }
    appnum( &cdef_body, "(BrActFlag)", act->flags, 1 );
    appstr( &cdef_body, act->text, 1 );
    appstr( &cdef_body, NULL, 1 ); /* user_text */
    appstr( &cdef_body, act->accel, 1 );
    appstr( &cdef_body, act->exec, 1 );
    appext( &cdef_body, act->call, FUNC_INT );
    switch( act->tag ) {
    case BRACT_OPEN:
      tag = "BRACT_OPEN";
      break;
    case BRACT_ROOT:
      tag = "BRACT_ROOT";
      break;
    case BRACT_CONT:
      tag = "BRACT_CONT";
      break;
    case BRACT_GRAF:
      tag = "BRACT_GRAF";
      break;
    }
    applit( &cdef_body, tag, 1 );
    append( &cdef_body, ", (BrClass*)0 };\n" );
  }
  return pact;
}


/*
 * output code for icon definitions
 */
static
void output_icons()
{
  int n;
  for( n = nicons; n > 0; n-- ) {
    Icon *icn = icons[n-1];
    char bname[32];
    int i;

    sprintf( bname, "(char*)kmicons_%d_bitmap", n );
    sprintf( line, "static unsigned char %s[] = { ", bname + 7 );
    append( &cdef_body, line );
    for( i = 0; i < icn->nbitmap; i++ )
      appint( &cdef_body, icn->bitmap[i] & 0xFF, i > 0 );
    append( &cdef_body, " };\n" );
    sprintf( line, "static KmIcon kmicons_%d = { ", n );
    append( &cdef_body, line );
    if( n == nicons )
      append( &cdef_body, "(KmIcon*)0" );
    else {
      sprintf( line, "&kmicons_%d", n + 1 );
      append( &cdef_body, line );
    }
    appstr( &cdef_body, icn->name, 1 );
    appint( &cdef_body, icn->width, 1 );
    appint( &cdef_body, icn->height, 1 );
    applit( &cdef_body, bname, 1 );
    append( &cdef_body, ", (KmPixmap)0, (KmPixmap)0 };\n\n" );
  }
}


/*
 * output code for button definitions
 */
static
void output_buttons()
{
  int n;
  char *name, *lan;
  for( n = nbuttons; n > 0; n-- ) {
    Button *but = buttons[n-1];

    sprintf( line, "static KmButton kmbuttons_%d = { ", n );
    append( &cdef_body, line );
    if( n == nbuttons )
      append( &cdef_body, "(KmButton*)0" );
    else {
      sprintf( line, "&kmbuttons_%d", n + 1 );
      append( &cdef_body, line );
    }
    appstr( &cdef_body, but->menu, 1 );
    appstr( &cdef_body, but->label, 1 );
    name = strdup (but->action);
    lan = strchr( name, '%' );
    if( lan != NULL ) {
      /* callback is a routine */
        *lan = '\0';
        appext( &cdef_body, but->action, FUNC_INT );
        appstr( &cdef_body, name, 1 );
    } else {
      /* callback is a command */
        char *p, *com;
        int il, i, j;

        while( (p = strstr( name, "  " )) != NULL ) {
          /* remove multiple blanks */
          strcpy( p, p + 1 );
        }
        /* remove ending blanks */
        il = strlen(name);
        for (i = il-1; i >=0; i--) {
             if (name[i] == ' ')
                 name[i] = '\0';
             else
                  break;
        }

        applit (&cdef_body, "(SUBROUTINE*)0", 1);
        applit (&cdef_body, "(IntFunc*)0",  1);
        il = strlen(name);
        com = malloc(il + 10);
        strcpy (com, "BUTTON(");
        for (j=0; j < il; j++) {
             if (name[j] == ' ')
                 com [j+7] = ',';
             else
                 com [j+7] = name[j];
        }
        com [j+7] = ')';
        com [j+8] = '\0';
        appstr( &cdef_body, com, 1 );
        free (com);
    }
    free (name);
    appstr( &cdef_body, but->mnemo, 1 );
    appstr( &cdef_body, but->accel, 1 );
    appstr( &cdef_body, but->accel_text, 1 );
    appnum( &cdef_body, "(KmButtFlag)", but->flags, 1 );
    append( &cdef_body, ", (KmWidget)0 };\n\n" );
  }
}


/*****************************************************************************
 * routines for reading and parsing
 *****************************************************************************/

static int line_in_buffer;      /* flag used for unread_line() */

/*
 * read next line from stdin into full_line and return length or -1 for EOF
 */
static
int read_line()
{
  int n;

  if( line_in_buffer ) {
    line_in_buffer = 0;
    return strlen( full_line );
  }

  n = 0;
  while( 1 ) {
    line_count++;
    if( fgets( &full_line[n], (sizeof full_line) - n, stdin ) == NULL )
      return -1;
    n = strlen( full_line );
    while( n > 0 && strchr( " \t\n", full_line[n-1] ) != NULL )
      n--;
    if( n > 0 && full_line[n-1] == '_' )
      n--;
    else {
      full_line[n] = '\0';
      break;
    }
  }

  if( n == 0 )
    return read_line();

  else if( n > 0 && strchr( full_line, '\t' ) != NULL ) {
    char *line = struntab( strdup( full_line ) );
    strcpy( full_line, line );
    free( line );
    n = strlen( full_line );
  }

  return n;
}


/*
 * next read_line() will return the same line again
 */
static
void unread_line()
{
  line_in_buffer = 1;
}


/*
 * parse the line into blank separated words
 */
static
int parse_line()
{
  static char copy_line[MAXLINE];
  char *p;
  char *s;
  int n = 0;
  int quoted = 0;
  int i = 0;

  while( 1 ) {
    char c = full_line[i++];

    /* protect blanks inside quoted strings by replacing them with \n */
    if( c == ' ' && quoted )
      c = '\n';
    else if( c == '\'' ) {
      if( quoted && full_line[i] == '\'' ) {
        i++;                    /* keep one quote */
      }
      else {
        quoted = !quoted;
        continue;               /* remove quotes */
      }
    }
    copy_line[n++] = c;
    if( c == '\0' )
      break;
  }

  if( mword < 10 ) {
    mword = 10;
    word = Malloc( char*, mword );
  }

  for( nword = 0; nword < mword; nword++ )
    word[nword] = NULL;

  for( nword = 0, s = copy_line; (p = strtok( s, " " )) != NULL; nword++ ) {
    if( nword == mword ) {
      mword += 10;
      word = Realloc( char*, word, mword );
    }
    if( p[0] == '.' && p[1] == '\0' )
      word[nword] = NULL;
    else
      word[nword] = p;
    s = NULL;
  }

  while( --n >= 0 ) {
    if( copy_line[n] == '\n' )
      copy_line[n] = ' ';
  }

  return nword;
}


/*
 * check that the directive is not abbreviated
 */
static
void check_abbrev( const char *is,
                  const char *should )
{
#if 0
  if( strcasecmp( is, should ) != 0 ) {
    sprintf( line, "Abbreviation recognized as '%s'", should );
    warning( line );
  }
#endif
}


/*
 * check that the number of blank separated fields is between nmin and nmax
 */
static
void check_args( int nmin,
                int nmax )
{
  if( nword < nmin )
    fatal( "Too few arguments" );
  else if( nword > nmax )
    warning( "Too many arguments" );
}


/*****************************************************************************
 * routines for buffer management
 *****************************************************************************/

/*
 * append text at end of buffer
 */
static
void append( Buffer *buf,
            const char *text )
{
  int len = strlen( text );
  if( len == 0 )
    return;
  if( buf->buffer == NULL ) {
    buf->allocated = len * 2 + 1;
    buf->buffer = malloc( buf->allocated );
  }
  else if( buf->allocated < buf->used + len + 1 ) {
    buf->allocated += len * 2 + 1;
    buf->buffer = realloc( buf->buffer, buf->allocated );
  }
  strcpy( &buf->buffer[buf->used], text );
  buf->used += len;
}


/*
 * append a single character
 */
static
void appchr( Buffer *buf,
            int chr )
{
  char str[2];
  str[0] = chr;
  str[1] = '\0';
  append( buf, str );
}


/*
 * append float constant encoded in text
 */
static
void appflt( Buffer *buf,
            const char *text,
            int comma )
{
  if( comma )
    append( buf, "," );
  append( buf, text );
  if( strchr( text, '.' ) == NULL )
    append( buf, "." );
}


/*
 * append integer constant
 */
static
void appint( Buffer *buf,
            int n,
            int comma )
{
  char text[16];

  if( comma )
    append( buf, ", " );

  sprintf( text, "%d", n );
  append( buf, text );
}


/*
 * append integer constant with type cast
 */
static
void appnum( Buffer *buf,
            const char *type,
            int n,
            int comma )
{
  char text[16];

  if( comma )
    append( buf, ", " );

  append( buf, type );
  sprintf( text, "%d", n );
  append( buf, text );
}


/*
 * append text as literal
 */
static
void applit( Buffer *buf,
            const char *text,
            int comma )
{
  if( fmode ) {
    if( comma )
      append( buf, "," );

    append( buf, text );
    return;
  }

  if( comma )
    append( buf, ", " );

  if( text == NULL )
    append( buf, "NULL" );
  else
    append( buf, text );
}


/*
 * append text as string protecting \ and "
 */
static
void appstr( Buffer *buf,
            const char *str,
            int comma )
{
  char *text = (str == NULL) ? NULL : strdup( str );
  char *p;

  if( fmode ) {

    if( comma )
      append( buf, "," );

    if( text == NULL || text[0] == '\0' )
      append( buf, "' '" );
    else {
      int quoted = 0;
      int len = strlen( text );
      if( len > 1 && text[len-1] == ' ' ) {
        while( len > 1 && text[len-1] == ' ' )
          len--;
        text[len] = '\0';
      }
      for( p = text; *p != '\0'; p++ ) {
        if( *p == '\\' ) {
          if( quoted ) {
            append( buf, "'" );
            quoted = 0;
          }
          if( p != text )
            append( buf, "//" );
          append( buf, "BS1" );
        }
        else {
          if( !quoted ) {
            if( p != text )
              append( buf, "//" );
            append( buf, "'" );
            quoted = 1;
          }
          if( *p == '\'' )
            append( buf, "''" );
          else
            appchr( buf, *p );
        }
      }
      if( quoted )
        append( buf, "'" );
    }

    return;
  }

  if( comma )
    append( buf, ", " );

  if( text == NULL )
    append( buf, "(char*)0" );
  else {
    append( buf, "\"" );
    while( (p = strpbrk( text, "\\\"" )) != NULL ) {
      static char esc[] = "\\?";
      esc[1] = *p;
      *p++ = '\0';
      append( buf, text );
      append( buf, esc );
      text = p;
    }
    append( buf, text );
    append( buf, "\"" );
  }
}


/*
 * initialize a buffer
 */
static
void init_buffer( Buffer *buf )
{
  buf->used = 0;
  buf->allocated = 0;
  buf->buffer = NULL;
}


/*
 * concatenate buf2 to buf1 and reset buf2
 */
static
void cat_buffer( Buffer *buf1,
                Buffer *buf2 )
{
  if( buf2->used == 0 )
    return;
  if( buf1->used + buf2->used + 1 > buf1->allocated ) {
    buf1->allocated += buf2->allocated;
    buf1->buffer = realloc( buf1->buffer, buf1->allocated );
  }
  strcpy( &buf1->buffer[buf1->used], buf2->buffer );
  buf1->used += buf2->used;
  buf2->used = 0;
  buf2->allocated = 0;
  free( buf2->buffer );
  buf2->buffer = NULL;
}


/*
 * copy buf2 to buf1 and reset buf2
 */
static
void copy_buffer( Buffer *buf1,
                 Buffer *buf2 )
{
  if( buf1->buffer != NULL )
    free( buf1->buffer );
  buf1->used = buf2->used;
  buf1->allocated = buf2->allocated;
  buf1->buffer = buf2->buffer;
  buf2->used = 0;
  buf2->allocated = 0;
  buf2->buffer = NULL;
}


/*
 * output the buffer content taking care of continuation lines
 */
static
void flush_buffer( Buffer *buf )
{
  int i;

  if( buf->used == 0 )
    return;

  if( fmode ) {
    int quoted = 0;
    int n0 = 0;
    for( i = 0; i < buf->used; ) {
      int nbreak = 0;
      int n = 0;
      char *p;
      for( p = &buf->buffer[i]; n + n0 < 72 && *p != '\0' && *p != '\n';
          p++, n++ ) {
        if( *p == '\'' )
          quoted = !quoted;
        else if( !quoted && *p == ',' )
          nbreak = n + 1;       /* good break outside quotes */
      }
      if( *p == '\0' ) {
        fputs( &buf->buffer[i], stdout );
        break;
      }
      else if( *p == '\n' ) {
        strncpy( line, &buf->buffer[i], ++n );
        line[n] = '\0';
        n0 = 0;
      }
      else if( nbreak != 0 ) {
        n = nbreak;
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "\n     +" );
        n0 = 6;
        quoted = 0;
      }
      else if( p[-1] == '\'' && !quoted && p[0] != '\'' ) {
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "\n     +" );
        n0 = 6;
      }
      else if( p[-1] != ' ' && p[-1] != '\'' ) {
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "\n     +" );
        n0 = 6;
      }
      else {
        if( p[-1] == '\'' ) {
          while( p[-1] == '\'' ) {
            p--;
            n--;
            quoted = !quoted;
          }
        }
        else {
          n--;
        }
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "'\n     +//'" );
        n0 = 9;
      }
      fputs( line, stdout );
      i += n;
    }
  }

  if( cmode ) {
    int squoted = 0;
    int dquoted = 0;
    int bslashd = 0;
    for( i = 0; i < buf->used; ) {
      int nbreak = 0;
      int nquote = 0;
      int n = 0;
      char *p;
      for( p = &buf->buffer[i]; n < 79 && *p != '\0' && *p != '\n';
          p++, n++ ) {
        if( bslashd ) {
          bslashd = 0;          /* ignore any character after \ */
        }
        else if( *p == '\\' ) {
          bslashd = 1;
        }
        else if( *p == '"' ) {
          if( !squoted )
            dquoted = !dquoted; /* " except inside ' */
        }
        else if( *p == '\'' ) {
          if( !dquoted )
            squoted = !squoted; /* ' except inside " */
        }
        else if( *p == ' ' ) {
          if( !squoted && !dquoted )
            nbreak = n;         /* good break outside quotes */
          if( dquoted )
            nquote = n;         /* good break inside quotes */
        }
      }
      if( *p == '\0' ) {
        fputs( &buf->buffer[i], stdout );
        break;
      }
      if( *p == '\n' ) {
        strncpy( line, &buf->buffer[i], ++n );
        line[n] = '\0';
      }
      else if( nbreak != 0 ) {
        n = nbreak;
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "\n" );
        squoted = bslashd = 0;
        dquoted = 0;
      }
      else if( nquote != 0 ) {
        n = nquote;
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "\\\n" );
        squoted = bslashd = 0;
        dquoted = 1;
      }
      else {
        strncpy( line, &buf->buffer[i], n );
        strcpy( &line[n], "\n" );
      }
      fputs( line, stdout );
      i += n;
    }
  }
  buf->used = 0;
}


/*
 * concatenate head and tail
 * translate each occurence of '/' into '_'
 */
static
char *format_name( const char *head,
                  const char *tail )
{
  char *name;
  int len = strlen( head ) + 1;
  int i;

  if( tail != NULL )
    len += strlen( tail ) + 1;

  name = malloc( len + 1 );
  strcpy( name, "&" );
  strcat( name, head );
  if( tail != NULL ) {
    strcat( name, "_" );
    strcat( name, tail );
  }

  for( i = 0; i < len; i++ ) {
    if( name[i] == '/' )
      name[i] = '_';
  }
  return name;
}


/*
 * return string converted to lowercase
 */
static
char *lowercase( const char *str )
{
  return strlower( strdup( str ) );
}


/*
 * return string converted to uppercase
 */
static
char *uppercase( const char *str )
{
  return strupper( strdup( str ) );
}


/*
 * parse comma separated list of range values
 */
static
char **parse_range( int *prange_count,
                   const char *range )
{
  int range_count = 0;
  char **range_value = NULL;
  char *s = strdup( range );
  char *p;

  for( range_count = 0; (p = strtok( s, "," )) != NULL; ) {
    range_value = Realloc( char*, range_value, ++range_count );
    range_value[range_count-1] = p;
    s = NULL;
  }
  *prange_count = range_count;
  return range_value;
}


/*
 * set index array for mutex and radio groups
 */
static
int *set_groups( Parameter *par,
                Group *group )
{
  int *pindex = NULL;

  if( group != NULL && group->groups > 0 ) {
    int i;

    pindex = (int*)calloc( par->range_count, sizeof(int) );

    for( i = 0; i < group->groups; i++ ) {
      char **value = group->value[i];
      int j;

      for( j = 0; j < group->count[i]; j++ ) {
        int k;

        for( k = 0; k < par->range_count; k++ ) {
          if( strcasecmp( par->range_value[k], value[j] ) == 0 ) {
            if( pindex[k] != 0 ) {
              sprintf( line, "Redefining MUTEX or RADIO group for option '%s'",
                      value[j] );
              warning( line );
            }
            pindex[k] = i + 1;
            break;
          }
        }
        if( k == par->range_count ) {
          sprintf( line, "Undefined MUTEX or RADIO option '%s'", value[j] );
          warning( line );
        }
      }
    }
  }
  return pindex;
}


static
void new_action( Action *act,
                const char *text,
                const char *accel,
                const char *exec,
                const char *call,
                BrActTag tag )
{
  act->flags = 0;
  if( text[0] == '/' ) {
    act->flags |= BrActSeparator;
    text++;
  }
  if( text[0] == '!' ) {
    act->flags |= BrActUpdate;
    text++;
  }
  act->text = strdup( text );

  if( accel != NULL ) {
    if( accel[0] == '/' ) {
      act->flags |= BrActSeparator;
      accel++;
    }
    if( accel[0] == '!' ) {
      act->flags |= BrActUpdate;
      accel++;
    }
    if( strncasecmp( accel, "T=", 2 ) == 0 ) {
      act->flags |= BrActToggle;
      if( accel[2] != '0' )
        act->flags |= BrActToggleOn;
      accel += 3;
    }
  }
  if( accel != NULL && strncasecmp( accel, "S=0", 3 ) == 0 ) {
    accel += 3;
  }
  else {
    act->flags |= BrActSensitive;
  }
  act->accel = str0dup( accel );
  act->exec = str0dup( exec );
  act->call = str0dup( call );
  act->tag = tag;
}
