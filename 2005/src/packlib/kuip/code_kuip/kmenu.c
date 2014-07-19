/*
 * $Id: kmenu.c,v 1.5 1999/06/04 09:42:02 couet Exp $
 *
 * $Log: kmenu.c,v $
 * Revision 1.5  1999/06/04 09:42:02  couet
 * - mod for latex
 *
 * Revision 1.4  1997/03/17 16:54:58  mclareni
 * WNT mods
 *
 * Revision 1.3  1997/01/28 14:17:49  gunter
 * In realloc_find, avoid to double free memory; this caused a bad crash
 * on Linux.
 *
 * In interactive find, fflush stdout before calling input_line(..); this
 * preserves the correct order of lines on stdout on Linux
 *
 * Revision 1.2.2.1  1997/01/21 11:32:49  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/03/15 14:57:41  cremel
 * kuipcdf.c --> generated with new kuipc
 *
 * kmenu.c : re-implement the possibility to type "\" (one level back)
 * in command line for HELP.
 *
 * Revision 1.1.1.1  1996/03/08 15:32:57  mclareni
 * Kuip
 *
 */
/*-- Author :    Alfred Nathaniel   07/08/92                                  */
/* kmenu.c: command menus                                                     */

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"
#include "kuip/mkutfu.h"

EXTERN char cdf_def[80], cdf_value[80];

enum { DOCSYS_HELP = 0, DOCSYS_USAGE, DOCSYS_LATEX };

static KmMenu  real_root = { (KmMenu*)0, (KmMenu*)0, "", "", 0,
                               (KmCommand*)0, 0, (char**)0 };
static KmMenu *curr_root = &real_root;
static KmMenu *curr_menu = &real_root;
static KmCommand *curr_cmd = NULL;
static KmCommand *prev_cmd = NULL;
static KmCommand **last_cmd_list = NULL;

static char **flis_path = NULL;
static char **flis_name = NULL;
static char **flis_cmd = NULL;
static char **flis_guidance = NULL;
static int *flis_flag = NULL;
static int *fprl_flag = NULL;

static int see_hidden = 0;      /* flag if hidden command should be seen */
static int edit_mode = 1;       /* flag for "edit" option in HELP/CHELP */

static char *user_help_name;    /* path name reported by KUHELP */

static char *alpha_label[] = { /* Q is excluded */
  " A", " B", " C", " D", " E", " F", " G", " H",
  " I", " J", " K", " L", " M", " N", " O", " P",
  " R", " S", " T", " U", " V", " W", " X", " Y", " Z", " 0" };
static int nalpha_label = (sizeof alpha_label) / sizeof(char*);


static KmCommand* find_command( KmMenu*, const char* );
static KmCommand** find_cmd_list( KmMenu*, const char* );
static KmMenu** find_menu_list( KmMenu**, KmMenu*, const char* );
static char* fmt_filter( const char*, int );
static char* fmt_guidance( char**, int, int );
static char* fmt_menu_help( KmMenu*, int );
static char* fmt_paragraph( char*, const char*, int, int );
static char* fmt_usage( KmCommand*, int );
static int   help_index( const char* );
static char* help_label( int );
static void  interactive_help_motif( KmMenu* );
static void  interactive_help( KmMenu* );
static void  interactive_find( char*, int );
static void realloc_find( int );
static KmCommand* lookup_cmd( const char*, const char* );
static void  merge_menus( KmMenu*, KmMenu* );
static void  patch_defaults( KmMenu* );
static char* get_guidance( char**, int );
static void  print_guidance( char**, int );
static void  print_help( char* );
static void  print_help_line ( int, char*, char*, int );


/*
 * action routine for /KUIP/SET_SHOW/ROOT
 */
int kxroot()
{
  int status = 0;
  char *path = ku_getc();

  if( strcmp( path, "?" ) == 0 ) {
    ku_alfa();
    printf( " Current root: %s\n",
         (curr_root == &real_root) ? "/" : curr_root->path );
  }
  else if( strcmp( path, "/" ) == 0 ) {
    curr_root = &real_root;
    kc_value.set_root = "/";
    if( kc_flags.curr_prompt != NULL )
      free( kc_flags.curr_prompt );
    kc_flags.curr_prompt = strdup( kc_value.set_prompt );
  }
  else {
    KmMenu **menu_list = find_menu_list( NULL, &real_root, path );
    if( menu_list == NULL ) {
      ku_alfa();
      printf( " *** Unknown command path\n" );
      status = -1;
    }
    else {
      if( menu_list[1] != NULL ) {
        ku_alfa();
        printf( " *** Ambiguous command path\n" );
        status = -1;
      }
      else {
        curr_root = menu_list[0];
        kc_value.set_root = curr_root->path;
        if( kc_flags.curr_prompt != NULL )
          free( kc_flags.curr_prompt );
        if( strstr( kc_value.set_prompt, "[]" ) != NULL )
          kc_flags.curr_prompt = str2dup( curr_root->name, " []" );
        else
          kc_flags.curr_prompt = str2dup( curr_root->name, " >" );
      }
      free( menu_list );
    }
  }
  return status;
}


/*
 * action routine for /KUIP/SET_SHOW/VISIBILITY
 */
int kxvisibility()
{
  int status = 0;
  char *path = ku_getc();
  KmCommand *cmd;

  see_hidden = 1;
  cmd = search_command( path, NULL );
  see_hidden = 0;

  if( cmd == NULL )
    status = -1;
  else {
    char *opt = ku_getc();
    if( strcmp( opt, "ON" ) == 0 ) {
      cmd->hidden = 0;
    }
    else if( strcmp( opt, "OFF" ) == 0 ) {
      cmd->hidden = 1;
    }
    else {
      printf( " %s is %s\n", cmd->path, cmd->hidden ? "OFF" : "ON" );
    }
  }
  return status;
}


/*
 * find the command matching the given path using the search order
 * return NULL if no match found or ambiguous
 */
KmCommand *search_command( const char *path,
                          KmCommand ***cmd_list )
{
  KmCommand *cmd;
  if( path[0] == '/' )
    cmd = find_command( &real_root, path );
  else {
    cmd = find_command( curr_root, path );
    if( cmd == NULL && last_cmd_list == NULL && curr_root != &real_root )
      cmd = find_command( &real_root, path );
  }
  if( cmd_list != NULL )
    *cmd_list = last_cmd_list;
  else if( cmd == NULL )
    print_cmd_list( last_cmd_list, path );
  return cmd;
}


/*
 * find the command matching the given path
 * return NULL if no match found or ambiguous
 * the last list is kept in last_cmd_list to allow checks for ambiguity
 */
static
KmCommand *find_command( KmMenu *menu,
                        const char *cmd_path )
{
  KmCommand **cmd_list = find_cmd_list( menu, cmd_path );
  KmCommand *cmd = NULL;
  const char *cmd_name;
  int ncmd = 0;
  int exact = 0;
  int min_level = 99;
  int i;

  if( last_cmd_list != NULL )
    free( last_cmd_list );
  last_cmd_list = cmd_list;

  if( cmd_list == NULL ) {
    /* no command found */
    return NULL;
  }
#ifdef DBMALLOC
  malloc_mark( cmd_list );
#endif

  /* count the number of commands */
  while( cmd_list[ncmd] != NULL )
    ncmd++;

  if( !see_hidden ) {
    /* remove hidden commands */
    for( i = 0; cmd_list[i] != NULL; i++ ) {
      if( cmd_list[i]->hidden ) {
        int j;
        for( j = i; j < ncmd; j++ )
          cmd_list[j] = cmd_list[j+1];
        ncmd--;
        i--;
      }
    }
    if( ncmd == 0 ) {
      /* no visible command left */
      free( last_cmd_list );
      last_cmd_list = NULL;
      return NULL;
    }
  }

  if( ncmd == 1 ) {
    /* unique command */
    return cmd_list[0];
  }

  cmd_name = strrchr( cmd_path, '/' );
  if( cmd_name != NULL )
    cmd_name++;
  else
    cmd_name = cmd_path;

  /* look for exact match */
  for( i = 0; i < ncmd; i++ ) {
    if( strcasecmp( cmd_list[i]->path, cmd_path ) == 0 ) {
      return cmd_list[i];
    }
    if( strcasecmp( cmd_list[i]->name, cmd_name ) == 0 ) {
      cmd_list[exact++] = cmd_list[i];
    }
  }

  if( exact > 0 ) {
    /* remove all inexact matches */
    ncmd = exact;
    cmd_list[ncmd] = NULL;
    if( ncmd == 1 ) {
      /* unique command */
      return cmd_list[0];
    }
  }

  /* check if unique on minimal depth of submenus */
  for( i = 0; i < ncmd; i++ ) {
    int level = cmd_list[i]->level;
    if( level < min_level ) {
      min_level = level;
      cmd = cmd_list[i];
    }
    else if( level == min_level ) {
      /* two commands on same level */
      cmd = NULL;
    }
  }
  return cmd;
}


/*
 * find all commands matching the given path
 */
static
KmCommand **find_cmd_list( KmMenu *top_menu,
                          const char *cmd_path )
{
  KmCommand **cmd_list = NULL;
  KmMenu **menu_list;
  char *full_path = strdup( cmd_path );
  char *menu_path = full_path;
  char *cmd_name = strrchr( full_path, '/' );

  if( cmd_name == NULL ) {
    /* path is only a command name ==> find all menus */
    cmd_name = full_path;
    menu_path = "";
  }
  else if( cmd_name == full_path ) {
      /* special case "/CMD" */
    cmd_name = &full_path[1];
    menu_path = "/";
  }
  else {
    /* split menu path and command name */
    *cmd_name++ = '\0';
  }

  if( (menu_list = find_menu_list( NULL, top_menu, menu_path )) != NULL ) {
    int cmd_len = strlen( cmd_name );
    int i;

    for( i = 0; menu_list[i] != NULL; i++ ) {
      KmCommand *cmd;

      for( cmd = menu_list[i]->cmds; cmd != NULL; cmd = cmd->next ) {

        if( strncasecmp( cmd->name, cmd_name, cmd_len ) == 0 ) {
          /* add command to list */
          int n = 0;

          if( cmd_list == NULL ) {
            /* allocate a new list */
            cmd_list = (KmCommand**)malloc( 2 * sizeof(KmCommand*) );
          }
          else {
            /* determine length of list and increase size */
            while( cmd_list[n] != NULL )
              n++;
            cmd_list = (KmCommand**)realloc( (char*)cmd_list,
                                            ( n + 2 ) * sizeof(KmCommand*) );
          }
          cmd_list[n] = cmd;
          cmd_list[n+1] = NULL;
        }
      }
    }
    free( menu_list );
  }
  free( full_path );
  return cmd_list;
}


/*
 * print command list
 */
void print_cmd_list( KmCommand **cmd_list,
                    const char *path )
{
  ku_alfa();

  if( cmd_list == NULL ) {
    KmMenu **menu_list = root_menu_list( path );
    if( menu_list != NULL ) {
      int i;
      printf( " *** %s is a menu:\n", path );
      for( i = 0; menu_list[i] != NULL; i++ ) {
        printf( " %s\n", menu_list[i]->path );
      }
      free( (char*)menu_list );
    }
    else {
      printf( " *** Unknown command: %s\n", path );
    }
  }
  else {
    int i;
    printf( " *** Ambiguous command %s. Possible commands are :\n\n", path );
    for( i = 0; cmd_list[i] != NULL; i++ )
      printf( " %s\n", cmd_list[i]->path );
    putchar( '\n' );
  }
}


/*
 * return pointer to menu with given path
 */
KmMenu *find_submenu( const char *path )
{
  KmMenu *menu = NULL;
  if( path[0] == '\0' )
    menu = &real_root;
  else {
    KmMenu **menu_list = root_menu_list( path );
    if( menu_list != NULL ) {
      menu = menu_list[0];
      free( (char*)menu_list );
    }
  }
  return menu;
}


/*
 * return the list of all menus matching the given path
 * the pointer returned should be free'ed
 */
KmMenu **root_menu_list( const char *path )
{
  return find_menu_list( NULL, &real_root, path );
}


/*
 * find all menus matching the given path
 * the pointer returned should be free'ed
 */
static
KmMenu **find_menu_list( KmMenu **menu_list,
                        KmMenu  *top_menu,
                        const char *menu_path )
{
  const char *head = (menu_path[0] == '/') ? menu_path + 1 : menu_path;
  const char *tail = strchr( head, '/' );
  int   head_len = (tail == NULL) ? strlen( head ) : tail - head;

  if( strncasecmp( top_menu->name, head, head_len ) == 0 ) {

    if( tail == NULL ) {
      /* add menu to list */

      if( top_menu != &real_root ) {
        int n = 0;
        if( menu_list == NULL ) {
          menu_list = (KmMenu**)malloc( 2 * sizeof(KmMenu*) );
        }
        else {
          while( menu_list[n] != NULL )
            n++;
          menu_list = (KmMenu**)realloc( (char*)menu_list,
                                        ( n + 2 ) * sizeof(KmMenu*) );
        }
        menu_list[n] = top_menu;
        menu_list[n+1] = NULL;
      }
    }
    else {
      /* look for submenus matching the rest of the path */
      KmMenu *menu;
      for( menu = top_menu->down; menu != NULL; menu = menu->next )
        menu_list = find_menu_list( menu_list, menu, tail );
    }
  }

  if( menu_path[0] != '/' || top_menu == &real_root ) {
    /* look for submenus matching a path not starting at root */
    KmMenu *menu;
    for( menu = top_menu->down; menu != NULL; menu = menu->next )
      menu_list = find_menu_list( menu_list, menu, menu_path );
  }

  return menu_list;
}


/*
 * merge menu into global tree
 */
void klnkmenu( menu, version )
     KmMenu *menu;
     int version;
{
  strcpy ( cdf_def, "Menu" );
  strcpy ( cdf_value, menu->path );
  check_version( version, KUIP_VERSION );

  patch_defaults( menu );

  if( real_root.down == NULL )
    real_root.down = menu;
  else
    merge_menus( real_root.down, menu );
}


/*
 * avoid the writable string problem when doing case conversion
 * on the default value by copying to allocated memory
 */
static
void patch_defaults( KmMenu *menu )
{
  while( menu != NULL ) {
    KmCommand *cmd;
    for( cmd = menu->cmds; cmd != NULL; cmd = cmd->next ) {
      int i;
      for( i = 0; i < cmd->total; i++ ) {
        KmParameter *par = cmd->par[i];
        if( par->dfault != NULL )
          par->dfault = strdup( par->dfault );
      }
    }
    patch_defaults( menu->down );
    menu = menu->next;
  }
}


/*
 * check version numbers
 */
void check_version( int cdf_version,
                   int lib_version )
{
  if( cdf_version != lib_version ) {
    printf( "\
!!!!!!!!!!!!!!   W A R N I N G    !!!!!!!!!!!!!\n\n\
The internal data structures have been changed.\n\
Please recompile the CDF which contains the \n\
       \'%s\' definition \'%s\'\n\
using the KUIPC compiler version %d.\n\n\
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n",
            cdf_def, cdf_value, lib_version );
    exit( 1 );
  }
}


/*
 * merge two menu trees
 */
static
void merge_menus( KmMenu *menu1,
                 KmMenu *menu2 )
{
  KmMenu *m1;
  KmMenu *last;

  for( m1 = menu1; m1 != NULL; last = m1, m1 = m1->next ) {

    if( strcmp( m1->name, menu2->name ) == 0 ) {

      /* merge commands of existing menus */
      if( m1->cmds == NULL )
        m1->cmds = menu2->cmds;
      else {
        KmCommand *cmd = m1->cmds;
        while( cmd->next != NULL )
          cmd = cmd->next;
        cmd->next = menu2->cmds;
      }

      /* merge submenus */
      if( m1->down == NULL )
        m1->down = menu2->down;
      else if( menu2->down != NULL )
        merge_menus( m1->down, menu2->down );

      /* handle next menu if any */
      if( menu2->next != NULL )
        merge_menus( menu1, menu2->next );

      return;
    }
  }

  /* add a new menu */
  last->next = menu2;
  if( menu2->next != NULL ) {
    KmMenu *m2 = menu2->next;
    menu2->next = NULL;
    merge_menus( menu1, m2 );
  }
}


/*
 * action routine for /KUIP/MANUAL
 */
int kxmanual()
{
  char *path = ku_getc();
  KmMenu **menu_list = NULL;
  KmMenu *menu;
  KmCommand **cmd_list;
  KmCommand *cmd = NULL;
  char *fname = ku_getf();
  FILE *stream = NULL;
  char *docsys_name = ku_getc();
  char *text;
  int docsys;
  int encapsulate = 1;

  if( strcmp( docsys_name, "SGML" ) == 0 ) {
    printf( " *** SGML not supported anymore. Please use LaTeX instead.\n" );
    return -1;
  }

  else if( strcmp( docsys_name, "TEX" ) == 0 ) {
    docsys = DOCSYS_LATEX;      /* LaTeX without encapsulation */
    encapsulate = 0;
  }

  else if( strcmp( docsys_name, "LATEX" ) == 0 ) {
    docsys = DOCSYS_LATEX;
  }

  else if( strcmp( docsys_name, "USAGE" ) == 0 ) {
    docsys = DOCSYS_USAGE;
  }

  else {
    docsys = DOCSYS_HELP;
  }

  if( strcmp( path, "/" ) == 0 ) {
    menu_list = root_menu_list( "" );
  }
  else {
    cmd = search_command( path, &cmd_list );
    if( cmd == NULL ) {
      menu = find_submenu( path );
      if( menu != NULL ) {
        /* recursive descend to all submenus */
        menu_list = find_menu_list( NULL, menu, "" );
      }
    }
    if( cmd == NULL && menu_list == NULL ) {
      /* try if it is a alias like "A/CRE LS DIR C" */
      char *value = (char*)hash_lookup( kc_alias.cmd_table, path, NULL );
      if( value == NULL ) {
        print_cmd_list( cmd_list, path );
        return -1;
      }
      if( (cmd = search_command( value, NULL )) == NULL )
        return -1;
    }
  }

  if( fname[0] != '\0' ) {
    fname = fexpand( fname, NULL );
    remove( fname );
    stream = fopen( fname, "w" );
    free( fname );
  }
  if( stream == NULL )
    stream = stdout;

  switch( docsys ) {
  case DOCSYS_LATEX:
     if( encapsulate ) {
      fputs( "\
\\documentclass[twoside]{report}\n\
\\usepackage{a4wide}\n\
%\\makeindex\n\
\\parskip=\\medskipamount\n\
\\textwidth=16cm\n\
\\oddsidemargin=0cm\n\
\\evensidemargin=\\oddsidemargin\n\
\n\
\\newif\\ifMENUtext \\MENUtexttrue\n\
\\newcommand{\\DEFMENU}[3]{% {level}{name}{path}\n\
\\chapter{#3}}\n\
\n\
\\newcommand{\\INDEX}[1]{% protect underscores\n\
{\\def\\_{\\char'137}\\index{#1@{\\tt #1}}}}\n\
\n\
\\newcommand{\\DEFCMD}[5]{% {menulabel}{cmdlabel}{menupath}{cmdname}{args}\n\
\\par\\begin{minipage}{\\textwidth}\n\
\\subsection{#4 #5 \\label{#1:#2}\\INDEX{#3/#4}\\INDEX{#4}}}\n\
\\newcommand{\\ENDCMD}{\\end{minipage}\\par}\n\
\n\
\\newcommand{\\DEFCBIG}[5]{% DEFCMD with long guidance text\n\
\\subsection{#4 #5 \\label{#1:#2}\\INDEX{#3/#4}\\INDEX{#4}}}\n\
\\newcommand{\\ENDCBIG}{\\par}\n\
\n\
\\newcommand{\\BEGARG}{\n\
\\par\\begin{tabular}{lcp{.75\\textwidth}}}\n\
\\newcommand{\\DEFARG}[4]{% {parname}{partype}{prompt}{default}\n\
{\\tt #1} & #2 & ``#3'' {\\tt #4} \\\\}\n\
\\newcommand{\\ENDARG}{\n\
\\end{tabular}\\par}\n\
\n\
\\newcommand{\\BEGOPT}[1]{% {parname}\n\
\\par\\noindent Possible {\\tt #1} values are:\n\
\\par\\noindent \\begin{tabular}{lp{.85\\textwidth}}}\n\
\\newcommand{\\DEFOPT}[2]{% {option}{text}\n\
{\\tt #1} & #2 \\\\[1ex]}\n\
\\newcommand{\\ENDOPT}{\\end{tabular}\\par}\n\
\n\
\\newcommand{\\BEGTEXT}{\\par}\n\
\\newcommand{\\ENDTEXT}{}\n\
\\newcommand{\\ENDVERB}{\\par}\n\
\\newcommand{\\EMPTY}{{\\tt '\\char`\\ '}}% empty string\n\
\\newcommand{\\BRA}{$\\langle$}% left angle <\n\
\\newcommand{\\KET}{$\\rangle$}% right angle >\n\
\\newcommand{\\PIPE}{$|$}% vertical bar |\n\
\\newcommand{\\DQUOTE}{{\\tt \"}}% double quote \"\n\
\n\
\\begin{document}\n\
", stream );
    }
    break;
  }

  if( menu_list != NULL ) {
    int i;
    for( i = 0; (menu = menu_list[i]) != NULL; i++ ) {

      if( docsys != DOCSYS_USAGE ) {
        text = fmt_menu_help( menu, docsys );
        fputs( text, stream );
        free( text );
      }

      for( cmd = menu->cmds; cmd != NULL; cmd = cmd->next ) {
        if( !cmd->hidden ) {
          text = fmt_cmd_help( cmd, docsys );
          fputs( text, stream );
          free( text );
        }
      }
    }
    free( menu_list );
  }

  else {
    text = fmt_cmd_help( cmd, docsys );
    if( kc_flags.in_motif && stream == stdout && docsys != DOCSYS_USAGE ) {
      /* display help text in Motif window */
      (*kjmpaddr.disp_text_C)( cmd->path, text, 0, 0, 1 );
    }
    else {
      fputs( text, stream );
    }
    free( text );
  }

  switch( docsys ) {

  case DOCSYS_LATEX:
    if( encapsulate ) {
      fputs( "\n\\end{document}\n", stream );
    }
    break;
  }

  if( stream != stdout )
    fclose( stream );

  return 0;
}


/*
 * action routine for /KUIP/CHELP (Command HELP)
 */
int kxhelp()
{
  char *path = ku_getc();
  int npar = ku_npar();
  KmMenu *menu = NULL;
  char *line;
  int status;

  if( npar >= 2 ) {
    char *view = ku_getc();

    if( strcmp( view, "E" ) == 0 || strcmp( view, "EDIT" ) == 0 )
      edit_mode = 1;
    else
      edit_mode = 0;

    kc_value.help_edit = edit_mode ? "-EDIT" : "-NOEDIT";

    if( path[0] == '\0' )
      return 0;
  }

  if( path[0] == '\0' )
    menu = &real_root;
  else if( strcmp( path, "/" ) != 0 ) {
    KmCommand **cmd_list;
    KmCommand *cmd = search_command( path, &cmd_list );
    if( cmd == NULL )
      menu = find_submenu( path );
  }

  if( menu != NULL ) {
    if( kc_flags.in_motif ) {
      interactive_help_motif( menu );
    }
    else {
      interactive_help( menu );
    }
    return 0;
  }

  line = str2dup( "/KUIP/MANUAL ", path );
  if( edit_mode )
    line = mstr3cat( line, " '", kc_flags.help_file, "'" );

  status = ku_exel( line );
  free( line );

  if( status == 0 && edit_mode ) {
    ku_pad( kc_flags.help_file, 1 );
  }

  return status;
}


/*
 * action routine for /KUIP/FIND
 */
int kxfind()
{
  char *keyw_expr = NULL;
  int npar = ku_npar();
  int key_mode = 0;

  if( npar == 1 ) {
    keyw_expr = ku_getc();
  } else if( npar > 1 ) {
    char *view;
    int i;

    for (i=0; i<npar-1; i++) {
        char *par = ku_getc();
        keyw_expr = mstr2cat( keyw_expr, " ", par);
    }
    /* last parameter : special case for EDIT, NOEDIT or KEYWORD (options) */
    view = ku_getc();

    if( strcmp( view, "E" ) == 0 || strcmp( view, "EDIT" ) == 0 ||
        strcmp( view, "-E" ) == 0 || strcmp( view, "-EDIT" ) == 0 ||
        strcmp( view, "N" ) == 0 || strcmp( view, "NOEDIT" ) == 0 ||
        strcmp( view, "-N" ) == 0 || strcmp( view, "-NOEDIT" ) == 0 ) {
        if( strcmp( view, "E" ) == 0 || strcmp( view, "EDIT" ) == 0 ||
            strcmp( view, "-E" ) == 0 || strcmp( view, "-EDIT" ) == 0 )
          edit_mode = 1;
        else
          edit_mode = 0;
        kc_value.help_edit = edit_mode ? "-EDIT" : "-NOEDIT";
    } else if( strcmp( view, "K" ) == 0 || strcmp( view, "KEYWORD" ) == 0 ||
        strcmp( view, "-K" ) == 0 || strcmp( view, "-KEYWORD" ) == 0 ) {
        key_mode = 1;
    } else {
        keyw_expr = mstr2cat( keyw_expr, " ", view);
    }

    if ( keyw_expr == NULL || km_strempty( keyw_expr ) )
      return 0;

  } /* end ... else if( npar > 1 ) */

  interactive_find( keyw_expr, key_mode );

  return 0;
}


/*
 * action routine for /KUIP/USAGE
 */
int kxusage()
{
  char *path = ku_getc();
  char *line;
  int status;

  line = str3dup( "/KUIP/MANUAL ", path, " ! 'USAGE'" );
  status = ku_exel( line );
  free( line );
  printf( "\n" );

  return status;
}


/*
 * print first sentence of guidance
 */
static char *get_guidance( char **guidance,
                    int nguidance )
{
  char *buf = strdup( "" );

  if( guidance != NULL ) {
    char *guid = strdup( guidance[0] );
    int len = strlen( guid );
    int n = 1;

    /* concatenate following lines until '.' at end or next paragraph */
    while( guid[len-1] != '.' && n < nguidance
          && guidance[n][0] != '\0' && guidance[n][0] != ' ' ) {
      int lenn = strlen( guidance[n] ) + 1;
      guid = realloc( guid, len + lenn + 1 );
      guid[len] = ' ';
      strcpy( &guid[len+1], guidance[n] );
      len += lenn;
      n++;
    }
    buf = fmt_paragraph( buf, guid, 20, DOCSYS_HELP );
    free( guid );
  }
  else
    buf = mstrcat( buf, "\n" );

  return buf;
}

static
void print_guidance( char **guidance,
                    int nguidance )
{
  if( guidance != NULL ) {
    char *buf = get_guidance( guidance, nguidance );

    fputs( buf, stdout );
    free( buf );
  }
  else
    printf( "\n" );
}


/*
 * called for style AL and AN
 */
void menu_style()
{
  interactive_help( &real_root );
  ku_exel( "/KUIP/SET_SHOW/STYLE 'C'" );
}

/*
 * interactive traversal of the help menus
 * specific code for the Motif interface
 */
static
void interactive_help_motif( KmMenu *top_menu )
{
    KmMenu *menu;
    KmCommand *cmd;
    int nmenus;
    int ncmds = 0;

    char **list_name = (char **) malloc ( sizeof (char *) );
    char **list_guidance = (char **) malloc ( sizeof (char *) );

    for( cmd = top_menu->cmds; cmd != NULL; cmd = cmd->next ) {
         list_name = (char **) realloc( (char*)list_name,
                                        (ncmds+1) * sizeof (char *) );
         list_guidance = (char **) realloc( (char*)list_guidance,
                                        (ncmds+1) * sizeof (char *) );
         list_name[ncmds] = strdup( cmd->name );
         list_guidance[ncmds++] = get_guidance(
                                       cmd->guidance, cmd->nguidance );
    }
    nmenus = ncmds;
    for( menu = top_menu->down; menu != NULL; menu = menu->next ) {
         list_name = (char **) realloc( (char*)list_name,
                                        (nmenus+1) * sizeof (char *) );
         list_guidance = (char **) realloc( (char*)list_guidance,
                                        (nmenus+1) * sizeof (char *) );
         list_name[nmenus] = strdup( menu->name );
         list_guidance[nmenus++] = get_guidance(
                                       menu->guidance, menu->nguidance );

    }
    /* display list of possible commands */
    list_name = (char **) realloc( (char*)list_name,
                                   (nmenus+1) * sizeof (char *) );
    list_guidance = (char **) realloc( (char*)list_guidance,
                                   (nmenus+1) * sizeof (char *) );
    list_name[nmenus] = NULL;
    list_guidance[nmenus] = NULL;
    if (kjmpaddr.disp_list_C != NULL) {
        static char *listFrom;
        ListData data;

        listFrom = strdup ( top_menu->path);
        data.listLabel = strdup ("From  ");
        data.listLabel = mstr2cat (data.listLabel, listFrom,  "/...");
        data.label = strdup("Select Menu");
        data.help = strdup("You did not specify a command.\n\
Select a menu in the List");
        data.OKcallback_data = listFrom;
        if (kjmpaddr.callb_kmenu != NULL)
            data.OKcallback = (*kjmpaddr.callb_kmenu);

        if (*kjmpaddr.disp_list_C != NULL)
            (*kjmpaddr.disp_list_C)( list_name, list_guidance, data);
    }
}


/*
 * interactive traversal of the menus and commands tree to check keyword
 * specific code for the Motif interface
 */
static
void interactive_find( char *expr, int keym )
{
    KmMenu *menu;
    char *p, *kval;
    int i, j, k;

    KmMenu **menu_list = root_menu_list("");
    int nkey = 0, nval = 0, opt = 0;
    int ncommand = 0, ncmd = 0;

    char **keyw = (char **) malloc ( sizeof (char *) );

    flis_path = (char **) malloc ( sizeof (char *) );
    flis_name = (char **) malloc ( sizeof (char *) );
    flis_cmd = (char **) malloc ( sizeof (char *) );
    flis_guidance = (char **) malloc ( sizeof (char *) );
    flis_flag = (int *) malloc ( sizeof (int) );
    fprl_flag = (int *) malloc ( sizeof (int) );

    /* First case : find without any keyword */
    if ( expr == NULL || km_strempty( expr ) ) {

       KmMenu *top_menu = &real_root;
       for( menu = top_menu->down; menu != NULL; menu = menu->next ) {
            /* Top level menu (``HELP'') */
            realloc_find ( nval );
            flis_name[nval] = strdup( menu->name );
            flis_path[nval] = strdup( menu->path );
/* printf ("*** Top menu : %s\n", flis_name[nval]); */
            flis_flag[nval] = 3;
            fprl_flag[nval] = 0;
            flis_guidance[nval++] = get_guidance(
                                          menu->guidance, menu->nguidance );
       }

    } else { /* list of keywords is not empty ... (nkey >= 1) */
    char *sav = strdup (expr);

    /* Decode expr and process each keyword independantly */
    while( (p = strstr( sav, " " )) != NULL) {
           keyw = (char **) realloc( (char*)keyw, (nkey+1) * sizeof (char *) );
           *p = '\0';
           if ( !km_strempty( sav ) ) keyw[nkey++] = strdup( sav );
           strcpy ( sav, p + 1 );
    }
    keyw = (char **) realloc( (char*)keyw, (nkey+1) * sizeof (char *) );
    if ( !km_strempty( sav ) ) keyw[nkey++] = strdup( sav );
    free (sav);

    /* Look first for possible menus & commands corresponding to that keyword */
    for (j = 0; j < nkey; j++) {
        KmMenu *menu0= NULL;
        KmMenu **menul;
        KmCommand **cmd_list;
        KmCommand *cmd0, *cmd;
        char *path;
        short take_it;
        int l;

        path = str0dup(keyw[j]);
        strupper (path);

        /* Look for valid commands */
        cmd0 = search_command( path, &cmd_list );

        if( cmd_list != NULL ) {
        /* this is a valid command: check other keywords if any in the list */
        int nc;

        for (nc = 0; cmd_list[nc] != NULL; nc++) {
        /* Loop on all commands from cmd_list */

           take_it = 0;
           cmd = cmd_list[nc];

           if (nkey == 1) {
              take_it = 1;
           } else { /* nkey > 1 --> check if other keywords are in that cmd */
             for (i = 0; i < cmd->nkeyword; i++) {
                 char *kval = str0dup(cmd->keyword[i]);
                 strupper (kval);
                 for (k = 0; k < nkey; k++) {
                   char *val = str0dup(keyw[k]);
                   strupper (val);
                   if ( strcmp(val, kval) == 0 ) {
                        take_it = 1;
                        free (val);
                        break;
                   } else
                        take_it = 0;
                   free (val);
                 }
                 free (kval);
                 if (!take_it) break;
             } /* end ... for (i = 0; i < cmd->nkeyword; i++) */
           } /* end ... else (nkey > 1) */
           if (take_it) {
              /* Real command (HELP for a valid command name) */
              realloc_find ( nval );
              flis_name[nval] = strdup( cmd->path );
/* printf ("*** Real command : %s\n", flis_name[nval]); */
              flis_cmd[nval] = strdup( cmd->name );
              flis_flag[nval] = 0;
              fprl_flag[nval] = 1;
              ncmd = nval;
              /* For "Help_item" do not increment the command counter */
              if( cmd->action_F != NULL || cmd->action_C != NULL )
                  ncommand++;
              else
                  flis_flag[nval] = -1;
              flis_guidance[nval++] = get_guidance(
                                          cmd->guidance, cmd->nguidance );
           }

        } /* end ... for (nc = 0; cmd_list[nc] != NULL; nc++) */
        } /* end ... if( cmd_list != NULL ) */

        /* Look for valid sub-menus */

        menul = root_menu_list( path );
        for (l = 0; menul != NULL && (menu0 = menul[l]) != NULL; l++) {

           if (menu0->nguidance > 0) {
           /* Help for the menu is available */

            take_it = 0;
            if (nkey == 1) {
              /* only one keyword : take it (do not check for others) */
              take_it = 1;
            } else { /* nkey > 1 : check if other keywords are OK */
              for (i = j+1; i < nkey; i++) {
                   char *kval = str0dup(keyw[i]);

                   strupper (kval);
                   /* if keyword is part of menu path take this cmd */
                   if ( strstr(menu0->path, kval) != NULL) {
                        take_it = 1;
                   } else {
                        /* Check list of keywords for that menu */
                       for (k = 0; k < menu0->nkeyword; k++) {
                            char *val = str0dup(menu0->keyword[k]);
                            strupper (val);
                            if ( strcmp(val, kval) == 0 ) {
                                 take_it = 1;
                                 free (val);
                                 break;
                            } else
                                 take_it = 0;
                            free (val);
                       } /* end ... for (k = 0; k < menu0->nkeyword; k++) */
                   } /* end ... else ... if ( strstr(menu0->path, kval) ... */
              } /* end ... for (i = j+1; i < nkey; i++) */
            } /* end ... else ... if (nkey == 1) */

            if (take_it) {
               /* Menu-root (help on a menu or sub-menu) */
               realloc_find ( nval );
               flis_name[nval] = strdup( menu0->path );
/* printf ("*** Menu0 : %s\n", flis_name[nval]); */
               flis_flag[nval] = 2;
               fprl_flag[nval] = 0;
               flis_path[nval] = strdup( menu0->name );
               flis_guidance[nval++] = get_guidance(
                                       menu0->guidance, menu0->nguidance );
            }
           } /* end ... if (menu0->nguidance > 0) */

           /* Loop on menu commands */
           for( cmd = menu0->cmds; cmd != NULL; cmd = cmd->next ) {
               /* all these are valid commands: check other keywords */

               take_it = 0;
               if (nkey == 1) {
                /* only one keyword : take it (do not check for others) */
                take_it = 1;
               } else { /* nkey > 1 : check if other keywords are OK */
                 for (i = j+1; i < nkey; i++) {
                   char *kval = str0dup(keyw[i]);

                   strupper (kval);
                   /* if keyword is part of command path take this cmd */
                   if ( strstr(cmd->path, kval) != NULL) {
                        take_it = 1;
                   } else {
                        /* Check list of keywords for that command */
                        for (k = 0; k < cmd->nkeyword; k++) {
                            char *val = str0dup(cmd->keyword[k]);
                            strupper (val);
                            if ( strcmp(val, kval) == 0 ) {
                                 take_it = 1;
                                 free (val);
                                 break;
                            } else
                                 take_it = 0;
                            free (val);
                        } /* end ... for (k = 0; k < cmd->nkeyword; k++) */
                 } /* end ... else ( strstr(cmd->path, kval) != NULL) */
               } /* end ... else (nkey > 1) */
           } /* end ... for (i = j+1; i < nkey; i++) */

           if (take_it) {
                /* Menu-command (help on all commands from menu-root) */
                realloc_find ( nval );
                flis_name[nval] = strdup( cmd->path );
                flis_path[nval] = strdup( menu0->path );
                flis_cmd[nval] = strdup( cmd->name );
/* printf ("*** Menu : %s\n", flis_name[nval]); */
                flis_flag[nval] = 1;
                fprl_flag[nval] = 1;
                flis_guidance[nval++] = get_guidance(
                                          cmd->guidance, cmd->nguidance );
           }

           } /* end ... for( cmd = menu0->cmds; cmd != NULL; ... */

           /* Loop now an all submenus associated to that menu ...   */
           for( menu = menu0->down; menu != NULL; menu = menu->next ) {
             /* all these are valid submenus: check other keywords */

             take_it = 0;
             if (nkey == 1) {
              /* only one keyword : take it (do not check for others) */
              take_it = 1;
             } else { /* nkey > 1 : check if other keywords are OK */
               for (i = j+1; i < nkey; i++) {
                 char *kval = str0dup(keyw[i]);

                 strupper (kval);
                 /* if keyword is part of menu path take this cmd */
                 if ( strstr(menu->path, kval) != NULL) {
                      take_it = 1;
                 } else {
                      /* Check list of keywords for that menu */
                     for (k = 0; k < menu->nkeyword; k++) {
                          char *val = str0dup(menu->keyword[k]);
                          strupper (val);
                          if ( strcmp(val, kval) == 0 ) {
                               take_it = 1;
                               free (val);
                               break;
                          } else
                               take_it = 0;
                          free (val);
                     } /* end ... for (k = 0; k < menu->nkeyword; k++) */
                 } /* end ... else ( strstr(menu->path, kval) != NULL) */
               } /* end ... for (i = j+1; i < nkey; i++) */
             } /* end ... else (nkey > 1) */

             if (take_it) {
                /* Sub-menu (help on all sub-menus from menu-root) */
                realloc_find ( nval );
                flis_name[nval] = strdup( menu->path );
                flis_path[nval] = strdup( menu0->path );
                flis_cmd[nval] = strdup( menu->name );
/* printf ("*** Sub-Menu : %s\n", flis_name[nval]); */
                flis_flag[nval] = 1;
                fprl_flag[nval] = 0;
                flis_guidance[nval++] = get_guidance(
                                          menu->guidance, menu->nguidance );
             }

           } /* end ... for( menu = menu0->down; menu != NULL; ... */

        } /* end ... for (l = 0; (menu0 = menul[l]) != NULL; l++) */
        free( menul );

        free (path);
    } /* end ... for (j = 0; j < nkey; j++) */

    /* Look now explicitely for possible list of keywords */
    for (i = 0; (menu = menu_list[i]) != NULL; i++) {
          KmCommand *cmd;
          short take_it;
          int nk;

          if (menu->nkeyword > 0) {

             take_it = 0;
             for (j = 0; j < nkey; j++) {
                  kval = str0dup(keyw[j]);
                  strupper (kval);
                  for (nk = 0; nk < menu->nkeyword; nk++) {
                       char *val = str0dup(menu->keyword[nk]);
                       strupper (val);
                       if ( strcmp(val, kval) == 0 ) {
                            take_it = 1;
                            free (val);
                            break;
                       } else
                            take_it = 0;
                       free (val);
                  }
                  free (kval );
                  if (!take_it) break;
             }

             if (take_it) {
                /* Keyword attached to a complete menu tree */
                realloc_find ( nval );
                flis_name[nval] = strdup( menu->path );
/* printf ("*** Keyword for menu : %s\n", flis_name[nval]); */
                flis_flag[nval] = 11;
                fprl_flag[nval] = 1;
                flis_path[nval] = strdup( menu->name );
                flis_guidance[nval++] = get_guidance(
                                          menu->guidance, menu->nguidance );
             }

          } /* end ... if (menu->nkeyword > 0) */

          for (cmd = menu->cmds; cmd != NULL; cmd = cmd->next) {

              if (cmd->nkeyword > 0) {

                  take_it = 0;
                  for (j = 0; j < nkey; j++) {
                       kval = str0dup(keyw[j]);
                       strupper (kval);
                       for (nk = 0; nk < cmd->nkeyword; nk++) {
                            char *val = str0dup(cmd->keyword[nk]);
                            strupper (val);
                            if ( strcmp(val, kval) == 0 ) {
                                 take_it = 1;
                                 free (val);
                                 break;
                            } else
                                 take_it = 0;
                            free (val);
                       }
                       free (kval );
                       if (!take_it) break;
                  }

                  if (take_it) {
                      /* Keyword attached to specific command */
                      realloc_find ( nval );
                      flis_name[nval] = strdup( cmd->path );
/* printf ("*** Keyword for command : %s\n", flis_name[nval]); */
                      flis_flag[nval] = 10;
                      fprl_flag[nval] = 1;
                      flis_guidance[nval++] = get_guidance(
                                               cmd->guidance, cmd->nguidance );
                  }

              } /* end ... if (cmd->nkeyword > 0) */

          } /* end ... for (cmd = menu->cmds; cmd != NULL; cmd = cmd->next) */

    } /* end ... for (i = 0; (menu = menu_list[i]) != NULL; i++) */

    } /* end ... else (list of keywords is not empty ...) */

    realloc_find ( nval );
    flis_name[nval] = NULL;
    flis_path[nval] = NULL;
    flis_cmd[nval] = NULL;
    flis_guidance[nval] = NULL;


    /* display list of possible commands */
    switch (nval) {

    case 0:  /* No command associated to that keyword(s) */
      if (!opt || nkey > 0)
          printf ("*** Cannot find any command associated to keyword(s): %s\n",
                  expr);
      break;

    case 1:  /* Only one command associated to that keyword(s) */
             /* Execute "command help" directly */
      if (flis_flag[0] == 0)
           printf ("\nCommand \"%s\" :\n", flis_name[0]);
      else if (flis_flag[0] == 10 || flis_flag[0] == 11)
           printf ("\nKeyword(s) \"%s\" found in commands: %s\n\n",
                    expr, flis_name[0]);
      else if (flis_flag[0] >= 1)
           printf ("\nMenu \"%s\" :\n", flis_path[0]);

      print_help (flis_name[0]);
      break;

    default: /* There is a list of commands associated to that keyword(s) */

      if (!keym && ncommand == 1) {
      /* Option -KEYWORD is not explicitely set and                */
      /* there is only 1 "true command" associated to that keyword */
         int i;
         print_help (flis_name[ncmd]);
         printf ("See also:\n    ");
         for (i = 0; i < nval; i++) {
             if (i == ncmd) continue;
             printf (flis_name[i]);
             if (i < nval-1)
                printf (", ");
             else
                printf ("\n");
         }
      } else { /* ncommand <> 1 (0 or more than 1 ) */

      if( kc_flags.in_motif && kjmpaddr.disp_list_C != NULL) {
      /* Motif interface */
        ListData data;

        if ( expr == NULL || km_strempty( expr ) ) {
           data.listLabel = strdup ( "List of all menus ");
           data.help = strdup("This is the list of commands\n\
associated to the keyword(s)\n\n       ");
        } else {
           data.listLabel = str2dup ( "Find keyword ", expr);
           data.help = str2dup("This is the list of commands\n\
associated to the keyword(s)\n\n       ", expr);
        }
        data.label = strdup("Select Command");
        data.OKcallback_data = NULL;
        if (kjmpaddr.callb_kmenu != NULL)
            data.OKcallback = (*kjmpaddr.callb_kmenu);

        if (*kjmpaddr.disp_list_C != NULL)
            (*kjmpaddr.disp_list_C)( flis_name, flis_guidance, data);

      } else if (!kc_flags.in_motif) {
      /* Command line interface */
        char menu_prmpt[80];
        static char *listFrom;
        char *line;
        int nl;

        if ( expr == NULL || km_strempty( expr ) ) {

           printf ("\nFrom  /...\n\n");
           listFrom = strdup("/...");
           for (i=0; i<nval; i++) {
               char *flag;

               if( fprl_flag[i] == 0)
                   flag = "   ";
               else if( fprl_flag[i] == 1)
                   flag = " * ";
               printf( "%s:%s%-12s  ", help_label( i+1 ), flag, flis_name[i]);
               if( strlen( flis_name[i] ) > 12 )
                 printf( "\n%20s", "" );
               fputs( flis_guidance[i], stdout );
           }

        } else {

           for (i=0; i<nval; ) {
               if (flis_flag[i] == 2) { /* top_menu : print help */
                   printf( "\nMenu %s \n\n", flis_name[i] );
                   print_help_line ( i+1 , flis_name[i], flis_guidance[i],
                                     fprl_flag[i] );
                   i++;
               } else if (flis_flag[i] == 1) {
                   printf( "\nFrom  %s/...\n\n", flis_path[i] );
                   listFrom = strdup(flis_path[i]);
                   while (flis_flag[i] == 1) {
                      if (i >= nval) break;
                      print_help_line ( i+1, flis_cmd[i], flis_guidance[i],
                                        fprl_flag[i] );
                      i++;
                   } /* end ... while (flis_flag[i] == 1) */
               } else if (flis_flag[i] == 10 || flis_flag[i] == 11) {
                   printf ("\nKeyword \"%s\" found in commands:\n\n", expr);
                   while (flis_flag[i] == 10 || flis_flag[i] == 11) {
                      if (i >= nval) break;
                      print_help_line ( i+1, flis_name[i], flis_guidance[i],
                                        fprl_flag[i] );
                      i++;
                   } /* end ... while (flis_flag[i] == 10) ... */
               } else {
                   int fbr = 0;
                   if (flis_flag[i] == 0) { /* real command */
                        if ( strstr (expr, "/" ) != NULL )
                           /* full branch of commands (from a sub-menu) */
                           fbr = 1;
                        printf ("\nList of command(s) for %s ...\n\n", expr);
                   } else if (flis_flag[i] == -1) /* "help_item" */
                        printf ("\nGeneral HELP information \n\n");
                   while (flis_flag[i] == 0 || flis_flag[i] == -1) {
                      if (i >= nval) break;
                      if (fbr)
                          print_help_line ( i+1,flis_cmd[i],flis_guidance[i],
                                            fprl_flag[i] );
                      else
                          print_help_line ( i+1,flis_name[i],flis_guidance[i],
                                            fprl_flag[i] );
                      i++;
                   } /* end ... while (flis_flag[i] == 0) */
               }
           } /* end ... for (i=0; i<nval; ) */
        } /* end ... else  ... if ( expr == NULL || km_strempty( expr ) ) */

        if (nkey > 0) {
           printf( "______\n%s:   %-12s  ", help_label( 0 ), "");
           fputs( "Top level menu", stdout );
	   fflush( stdout ); /* or the line is delayed on linux?! */
        }

        line = NULL;
        while (line == NULL || line[0] == '\0' ) {
         sprintf( menu_prmpt, 
         "\nEnter a number ('0'=Top, '\\'=one level back, 'Q'=command mode): ");
         line = input_line( menu_prmpt, 0 );
        }
        if ( strcasecmp( line, "\\" ) == 0 ) {
           /* one level back --> cut last "/..." */
           char *p;
           char cmd[512];
           p = strrstr( listFrom, "/" );
           if (p != NULL) {
               *p = '\0';
               sprintf (cmd, "/KUIP/HELP %s", listFrom);
	       realloc_find(-1);
               ku_exel (cmd);
           } else {
               realloc_find(-1);
               ku_exel ("/KUIP/HELP");
           }

        } else if ( strcasecmp( line, "Q" ) != 0 ) {
           nl = atoi( line );
           if (nl > 0) {
               char cmd[512];

               if (flis_flag[nl-1] == 2) { /* Menu */
                 KmMenu *menu = find_submenu( flis_path[nl-1] );
                 if( menu != NULL ) {
                     char *text = fmt_menu_help( menu, DOCSYS_HELP );
                     fputs( text, stdout );
                     free( text );
                 }
               } else {
                 sprintf (cmd, "/KUIP/HELP %s", flis_name[nl-1]);
		 realloc_find(-1);
                 ku_exel (cmd);
               }
           } else {
	       realloc_find(-1);
               ku_exel ("/KUIP/HELP");
           }
        }

      } /* end ... if( kc_flags.in_motif && kjmpaddr.disp_list_C != NULL) */

      } /* end ... else ... ncommand <> 1 (0 or more than 1 ) */
      break;

    } /* end ... switch (nval) */

    free (keyw);
    realloc_find ( -1 ); 
}


static
void realloc_find( int nval )
{
    if (nval < 0 ) {
      free (flis_path); flis_path = NULL;
      free (flis_name); flis_name = NULL;
      free (flis_cmd);  flis_cmd  = NULL;
      free (flis_guidance); flis_guidance = NULL;
      free (flis_flag); flis_flag = NULL;
      free (fprl_flag); fprl_flag = NULL;
    } else {
      flis_path = (char **) realloc( (char*)flis_path,
                              (nval+1) * sizeof (char *) );
      flis_name = (char **) realloc( (char*)flis_name,
                              (nval+1) * sizeof (char *) );
      flis_cmd = (char **) realloc( (char*)flis_cmd,
                              (nval+1) * sizeof (char *) );
      flis_guidance = (char **) realloc( (char*)flis_guidance,
                              (nval+1) * sizeof (char *) );
      flis_flag = (int *) realloc( flis_flag,
                              (nval+1) * sizeof (int) );
      fprl_flag = (int *) realloc( fprl_flag,
                              (nval+1) * sizeof (int) );
    }
}

static
void print_help( char *command )
{
    int status;
    char cmd[512];

    if( edit_mode )
        sprintf (cmd, "/KUIP/MANUAL %s \'%s\'", command, kc_flags.help_file);
    else
        sprintf (cmd, "/KUIP/MANUAL %s", command);

    status = ku_exel( cmd );

    if( status == 0 && edit_mode ) {
        ku_pad( kc_flags.help_file, 1 );
    }
}

static
void print_help_line( int nl, char *command, char *message, int fl )
{

    char *flag;

    if( fl == 0)
        flag = "   ";
    else if( fl == 1)
        flag = " * ";

    printf( "%s:%s%-12s  ", help_label( nl ), flag, command);
    if( strlen( command ) > 12 )
        printf( "\n%20s", "" );
    fputs( message, stdout );
}

/*
 * interactive traversal of the help menus
 */
static
void interactive_help( KmMenu *top_menu )
{
 loop:
  while( 1 ) {
    char *text = fmt_menu_help( top_menu, DOCSYS_HELP );

    KmMenu *menu;
    KmCommand *cmd;
    char menu_prmpt[80];
    int nmenus;
    int ncmds = 0;

    fputs( text, stdout );
    free( text );

    printf( "\nFrom  %s/...\n\n", top_menu->path );
    for( cmd = top_menu->cmds; cmd != NULL; cmd = cmd->next ) {
      char *flag;
      if( cmd->action_F == NULL && cmd->action_C == NULL )
        flag = "   ";
      else if( cmd->hidden )
        flag = "(*)";
      else
        flag = " * ";
      printf( "%s:%s%-12s  ", help_label( ++ncmds ), flag, cmd->name );
      if( strlen( cmd->name ) > 12 )
        printf( "\n%20s", "" );
      print_guidance( cmd->guidance, cmd->nguidance );
    }

    nmenus = ncmds;
    for( menu = top_menu->down; menu != NULL; menu = menu->next ) {
      printf( "%s:   %-12s  ", help_label( ++nmenus ), menu->name );
      if( strlen( menu->name ) > 12 )
        printf( "\n%20s", "" );
      print_guidance( menu->guidance, menu->nguidance );
    }

    sprintf( menu_prmpt, "\nEnter a %s (%s'Q'=command mode): ",
            (kc_flags.style & KmSTYLE_xL) ? "letter" : "number",
            (top_menu->level > 0) ? "'\\'=one level back, " : "" );

    while( 1 ) {
      char *line = input_line( menu_prmpt, 0 );
      if( line == NULL )
        return;
      if( strcasecmp( line, "Q" ) == 0 )
        return;
      if( strcmp( line, "\\" ) == 0 ) {
        if( top_menu->level > 0 ) {
          char *up_path = strdup( top_menu->path );
          char *last_slash = strrchr( up_path, '/' );
          *last_slash = '\0';
          if( up_path[0] == '\0' )
            top_menu = &real_root;
          else {
            KmMenu **menu_list = root_menu_list( up_path );
            top_menu = menu_list[0];
            free( menu_list );
          }
          free( up_path );
          break;
        }
      }
      else {
        int n = help_index( line );
        if( n > 0 ) {
          if( n <= ncmds ) {
            char cmd_prmpt[80];
            int  hidden;
            for( cmd = top_menu->cmds; n > 1; n-- )
              cmd = cmd->next;

            text = fmt_cmd_help( cmd, DOCSYS_HELP );
            fputs( text, stdout );
            free( text );

            hidden = cmd->hidden
              || (cmd->action_F == NULL && cmd->action_C == NULL);
            sprintf( cmd_prmpt, "<CR>=continue, 'Q'=command mode%s: ",
                    hidden ? "" : ", 'X'=execute" );

            while( 1 ) {

              if( (kc_flags.style & KmSTYLE_A) != 0 ) {
                if( hidden )
                  goto loop;
                line = "X";
              }
              else {
                line = input_line( cmd_prmpt, 0 );
                if( line == NULL )
                  return;
                if( strcasecmp( line, "Q" ) == 0 )
                  return;
                if( line[0] == '\0' )
                  goto loop;
              }

              if( !hidden && strcasecmp( line, "X" ) == 0 ) {
                char arg_prmpt[80];
                printf( "Add parameters or just <CR> to the command line\n" );
                printf( "('#'=cancel execution, '?'=help) :\n\n" );
                sprintf( arg_prmpt, "%s ", cmd->path );
                while( 1 ) {
                  line = input_line( arg_prmpt, 0 );
                  if( line == NULL )
                    return;
                  if( strcmp( line, "#" ) == 0 )
                    goto loop;
                  if( strcmp( line, "?" ) != 0 ) {
                    char *cmd_line = str3dup( cmd->path, " ", line );
                    exec_cmd_string( cmd_line, 1, NULL );
                    free( cmd_line );
                    goto loop;
                  }
                  text = fmt_cmd_help( cmd, DOCSYS_HELP );
                  fputs( text, stdout );
                  free( text );
                }
              }
            }
          }
          else if( n <= nmenus ) {
            n -= ncmds;
            for( menu = top_menu->down; n > 1; n-- )
              menu = menu->next;
            top_menu = menu;
            break;
          }
        }
      }
    }
  }
}


/*
 * convert the help label into an index
 */
static
int help_index( const char *label )
{

  if( kc_flags.style & KmSTYLE_xL ) {
    int uplabel;
    int n;

    while( *label == ' ' )
      label++;

    uplabel = *label;
    if( islower( uplabel ) )
       uplabel = toupper( uplabel );

    for( n = 0; n < nalpha_label; n++ ) {
      if( uplabel == alpha_label[n][1] )
        return n + 1;
    }
    return atoi( label ) + nalpha_label;
  }
  return atoi( label );
}


/*
 * return the label for the items in the help menu
 */
static
char *help_label( int n )
{
  static char number_label[4];

  if( kc_flags.style & KmSTYLE_xL ) {
    if( n <= nalpha_label )
      return alpha_label[n-1];
    n -= nalpha_label;
  }
  sprintf( number_label, "%2d", n );
  return number_label;
}


/*
 * format the usage line for a command
 */
static
char *fmt_usage( KmCommand *cmd,
                int docsys )
{
  char *buf = strdup( "\n" );
  char *pbuf = strdup( "" );
  char *path;
  char *p;
  int i;

  for( i = 0; i < cmd->total; i++ ) {
    KmParameter *par = cmd->par[i];
    if( i == cmd->mandatory )
      pbuf = mstrcat( pbuf, " [" );
    if( (par->flags & KmFLAG_HIDDEN) == 0 )
      pbuf = mstr2cat( pbuf, " ", par->name );
  }
  if( cmd->total > cmd->mandatory )
    pbuf = mstrcat( pbuf, " ]" );

  switch( docsys ) {

  case DOCSYS_LATEX:
    buf = mstrcat( buf, (cmd->nguidance <= 50) ? "\\DEFCMD{" : "\\DEFCBIG{" );
    for( p = cmd->path, i = 0; i < cmd->level; i++ ) {
      p = strchr( p, '/' );
      p++;
      buf = mstrccat( buf, *p, 1 );
    }
    buf = mstrcat( buf, "}{" );
    for( p = cmd->name, i = 0; i < 6 && *p != '\0'; p++ ) {
      if( *p != '_' ) {
        i++;
        buf = mstrccat( buf, *p, 1 );
      }
    }
    path = strdup( cmd->path + 1 );
    p = strrchr( path, '/' );
    *p = '\0';
    buf = mstr2cat( buf, "}{", fmt_filter( path, docsys ) );
    free( path );
    buf = mstr2cat( buf, "}{", fmt_filter( cmd->name, docsys ) );
    buf = mstr3cat( buf, "}{", fmt_filter( strlower( pbuf ), docsys ), "}\n" );
    break;

  default:
    if( cmd->action_F != NULL || cmd->action_C != NULL )
      buf = mstr4cat( buf, " * ", cmd->path + 1, pbuf, "\n" );
  }
  free( pbuf );

  return buf;
}


/*
 * format the help text for a command
 */
char *fmt_cmd_help( KmCommand *cmd,
                   int docsys )
{
  char *buf = strdup( "" );
  char *p = fmt_usage( cmd, docsys );

  buf = mstrcat( buf, p );
  free( p );
  if( docsys == DOCSYS_USAGE )
    return buf;

  buf = mstrcat( buf, "\n" );

  if( cmd->total > 0 ) {
    int has_option = 0;
    int i;

    switch( docsys ) {

    case DOCSYS_LATEX:
      buf = mstrcat( buf, "\\BEGARG\n" );
      break;
    }

    for( i = 0; i < cmd->total; i++ ) {
      KmParameter *par = cmd->par[i];
      KmParInt  *pint  = par->ptype;
      KmParReal *preal = par->ptype;
      int lname = strlen( par->name );
      char *pbuf;
      char *type;
      char *quote = "'";
      char *lower = NULL;
      char *upper = NULL;

      if( (par->flags & KmFLAG_HIDDEN) != 0 )
        continue;

      switch( par->type ) {
      case KmTYPE_INT:
        type = "I";
        quote = "";
        lower = pint->range_lower;
        upper = pint->range_upper;
        break;
      case KmTYPE_REAL:
        type = "R";
        quote = "";
        lower = preal->range_lower;
        upper = preal->range_upper;
        break;
      case KmTYPE_OPTION:
        if( par->range_count > 0 )
          has_option = 1;
        /* FALLTHRU */
      default:
        type = "C";
      }

      pbuf = strndup( par->name, par->abbrev );
      if( par->abbrev != lname ) {
        pbuf = mstr2cat( pbuf, "*", par->name + par->abbrev );
        lname++;
      }

      switch( docsys ) {

      case DOCSYS_LATEX:
        buf = mstr2cat( buf, "\\DEFARG{", fmt_filter( pbuf, docsys ) );
        buf = mstr2cat( buf, "}{", type );
        buf = mstr3cat( buf, "}{", fmt_filter( par->prompt, docsys ), "}{" );
        break;

      default:
        buf = mstr2cat( buf, "   ", pbuf );
        buf = mstrccat( buf, ' ', 11 - lname );
        buf = mstr4cat( buf, type, " '", par->prompt, "'" );
      }

      free( pbuf );

      if( par->dfault != NULL ) {
        if( (par->flags & KmFLAG_CONST) != 0 )
          buf = mstrcat( buf, " Const=" );
        else
          buf = mstrcat( buf, " D=" );
        if( docsys == DOCSYS_LATEX && strcmp( par->dfault, " " ) == 0 )
          buf = mstrcat( buf, "\\EMPTY{}" );
        else
          buf = mstr3cat( buf,
                         quote, fmt_filter( par->dfault, docsys ), quote );
      }

      if( par->type != KmTYPE_OPTION && par->range_count != 0 ) {
        int n;
        buf = mstr3cat( buf, " R=", quote, par->range_value[0] );
        for( n = 1; n < par->range_count; n++ )
          buf = mstr2cat( buf, ",", par->range_value[n] );
        buf = mstrcat( buf, quote );
      }
      if( lower != NULL || upper != NULL ) {
        if( lower == NULL )
          lower = "";
        if( upper == NULL )
          upper = "";
        buf = mstr4cat( buf, " R=", lower, ":", upper );
      }

      if( (par->flags & KmFLAG_FORGET) != 0 )
        buf = mstrcat( buf, " Forget" );

      if( i + 1 == cmd->list_par )
        buf = mstrcat( buf, " Loop" );

      if( (par->flags & KmFLAG_MINUS) != 0 )
        buf = mstrcat( buf, " Minus" );

      if( (par->flags & KmFLAG_QUOTE) != 0 )
        buf = mstrcat( buf, " Quote" );

      if( (par->flags & KmFLAG_SEPARATE) != 0 )
        buf = mstrcat( buf, " Separate" );

      if( (par->flags & KmFLAG_VARARG) != 0 )
        buf = mstrcat( buf, " Vararg" );

      if( docsys == DOCSYS_LATEX )
        buf = mstrcat( buf, "}" );
      buf = mstrcat( buf, "\n" );
    }

    if( docsys == DOCSYS_LATEX )
      buf = mstrcat( buf, "\\ENDARG\n" );

    if( has_option ) {
      for( i = 0; i < cmd->total; i++ ) {
        KmParameter *par = cmd->par[i];
        if( par->type == KmTYPE_OPTION && par->range_count > 0 ) {
          KmParOption *ptype = par->ptype;
          int maxlen = mstrlen( par->range_value, par->range_count );
          int j;

          switch( docsys ) {

          case DOCSYS_LATEX:
            buf = mstr3cat( buf, "\\BEGOPT{",
                          fmt_filter( par->name, docsys ), "}\n" );
            break;

          default:
            buf = mstr3cat( buf, "\n   Possible ", par->name,
                           " values are:\n\n" );
          }

          for( j = 0; j < par->range_count; j++ ) {
            char *val = par->range_value[j];
            char *vbuf;

            switch( docsys ) {

            case DOCSYS_LATEX:
              buf = mstrcat( buf, "\\DEFOPT{" );
              if( val[0] == '\0' || strcmp( val, " " ) == 0 )
                buf = mstrcat( buf, "\\EMPTY" );
              else
                buf = mstrcat( buf, fmt_filter( val, docsys ) );
              buf = mstrcat( buf, "}{" );
              if( ptype->text[j] != NULL )
                buf = fmt_paragraph( buf, ptype->text[j], 0, docsys );
              buf = mstrcat( buf, "}\n" );
              break;

            default:
              if( val[0] == '\0' || strcmp( val, " " ) == 0 )
                vbuf = strdup( "   ' '" );
              else
                vbuf = str3dup( "    ", val, " " );
              buf = mstrcat( buf, vbuf );
              buf = mstrccat( buf, ' ', maxlen + 6 - strlen( vbuf ) );
              free( vbuf );
              if( ptype->text[j] != NULL )
                buf = fmt_paragraph( buf, ptype->text[j],
                                    maxlen + 6, DOCSYS_HELP );
              else
                buf = mstrcat( buf, "\n" );
            }
          }

          switch( docsys ) {

          case DOCSYS_LATEX:
            buf = mstrcat( buf, "\\ENDOPT\n" );
            break;
          }
        }
      }
    }
    buf = mstrcat( buf, "\n" );
  }

  p = fmt_guidance( cmd->guidance, cmd->nguidance, docsys );
  buf = mstrcat( buf, p );
  free( p );

  if( cmd->user_help_C != NULL ) {
    /* user help routine has to return an allocated buffer */
    char **user_guidance = (*cmd->user_help_C)( cmd->name );
    int n = 0;
    while( user_guidance[n] != NULL )
      n++;
    p = fmt_guidance( user_guidance, n, docsys );
    while( n > 0 )
      free( user_guidance[--n] );
    free( (char*)user_guidance );
    buf = mstrcat( buf, p );
    free( p );
  }
  else if( cmd->user_help_F != NULL ) {
    /* Fortran OPEN the file and call user help routine */
    FILE *stream;
    char *fname = ku_fcase( kc_flags.uhlp_file );
    int luno = kc_flags.uhlp_luno;

    ku_open( luno, fname, "VERYNEW" );
    user_help_name = cmd->name; /* for KUHELP */
    (*cmd->user_help_F)();
    ku_close( luno );

    if( (stream = fopen( fname, "r" )) == NULL )
      perror( fname );
    else {
      char line[1024];

      if( docsys == DOCSYS_LATEX )
        buf = mstrcat( buf, "\\begin{verbatim}\n" );

      while( fgets( line, (sizeof line), stream ) != NULL )
        buf = mstrcat( buf, line );

      if( docsys == DOCSYS_LATEX )
        buf = mstrcat( buf, "\\end{verbatim}\n" );

      if( fclose( stream ) != 0 )
        perror( fname );
      if( remove( fname ) != 0 )
        perror( fname );
    }
  }

  switch( docsys ) {

  case DOCSYS_LATEX:
    buf = mstrcat( buf,
                  (cmd->nguidance <= 50) ? "\\ENDCMD\n" : "\\ENDCBIG\n" );
    break;
  }

  return buf;
}


/*
 * format the guidance text
 */
static
char *fmt_guidance( char **guidance,
                   int nguidance,
                   int docsys )
{
  char *buf = strdup( "" );

  if( guidance != NULL ) {
    char *para = strdup( "" );
    int indent = 3;
    int verbatim = 0;
    int i;

    for( i = 0; i < nguidance; i++ ) {

      if( guidance[i][0] == ' ' ) {
        /* verbatim output of indented lines */

        if( !verbatim ) {
          /* first verbatim line */
          verbatim = 1;
          if( para[0] != '\0' ) {
            buf = mstrccat( buf, ' ', indent );
            buf = fmt_paragraph( buf, para, indent, docsys );
            para[0] = '\0';
          }

          switch( docsys ) {

          case DOCSYS_LATEX:
            buf = mstrcat( buf, "\\begin{verbatim}\n" );
            break;

          default:
            if( i > 0 && guidance[i-1][0] != '\0' ) {
              /* verbatim not at beginning of paragraph */
              buf = mstrcat( buf, "\n" );
            }
          }
        }
      }

      else if( verbatim ) {
        /* leave verbatim mode */
        verbatim = 0;

        switch( docsys ) {

        case DOCSYS_LATEX:
          buf = mstrcat( buf, "\\end{verbatim}\n" );
          if( guidance[i][0] == '\0'
             && i+1 < nguidance && guidance[i+1][0] != ' ' ) {
            /* verbatim followed by a new paragraph which is not verbatim */
            buf = mstrcat( buf, "\\ENDVERB\n" );
          }
          break;

        default:
          buf = mstrcat( buf, "\n" );
        }
      }

      if( verbatim ) {
        buf = mstrccat( buf, ' ', indent );
        buf = fmt_paragraph( buf, guidance[i], indent, DOCSYS_HELP );
      }
      else if( guidance[i][0] == '\0' ) {
        if( para[0] != '\0' ) {
          buf = mstrccat( buf, ' ', indent );
          buf = fmt_paragraph( buf, para, indent, docsys );
          buf = mstrcat( buf, "\n" );
          para[0] = '\0';
        }
      }
      else {
        int len = strlen( guidance[i] );
        para = mstrcat( para, guidance[i] );
        if( guidance[i][len-1] == '.' || guidance[i][len-1] == ':' )
          para = mstrcat( para, "  " );
        else
          para = mstrcat( para, " " );
      }
    }

    if( verbatim ) {
      switch( docsys ) {

      case DOCSYS_LATEX:
        buf = mstrcat( buf, "\\end{verbatim}\n" );
        break;

      default:
        buf = mstrcat( buf, "\n" );
      }
    }
    else if( para[0] != '\0' ) {
      buf = mstrccat( buf, ' ', indent );
      buf = fmt_paragraph( buf, para, indent, docsys );
    }
    free( para );
    buf = mstrcat( buf, "\n" );
  }

  return buf;
}


/*
 * format the help text for a menu
 */
static
char *fmt_menu_help( KmMenu *menu,
                    int docsys )
{
  char *buf = strdup( "" );

  if( menu->path[0] != '\0' ) {
    char *guid = fmt_guidance( menu->guidance, menu->nguidance, docsys );

    switch( docsys ) {

    case DOCSYS_LATEX:
      buf = mstrcat( buf, "\\DEFMENU{" );
      buf = mstricat( buf, menu->level - 1 );
      buf = mstr2cat( buf, "}{", fmt_filter( menu->name, docsys ) );
      buf = mstr2cat( buf, "}{", fmt_filter( menu->path + 1, docsys ) );
      buf = mstr3cat( buf, "}\n\\ifMENUtext\n", guid, "\n\\fi\n" );
      break;

    default:
      buf = mstr4cat( buf, "\n   ", menu->path, "\n\n", guid );
    }

    free( guid );

  }

  return buf;
}


/*
 * break a paragraph into lines and append it to the buffer
 */
static
char *fmt_paragraph( char *buf,
                    const char *para,
                    int indent,
                    int docsys )
{
  char *p = fmt_filter( para, docsys );
  int len = strlen( p );
  int ncol = kc_window.term_width - indent - 1;

  switch( docsys  ) {

  case DOCSYS_LATEX:
    buf = mstrcat( buf, "\\par\n" );
    break;
  }

  while( len > 0 ) {
    int n = len;
    if( n > ncol ) {
      n = ncol;
      while( n > 1 && p[n-1] != ' ' )
        n--;
    }
    buf = mstrncat( buf, p, n );
    buf = mstrcat( buf, "\n" );
    p += n;
    len -= n;
    while( *p == ' ' ) {
      p++;
      len--;
    }
    if( len > 0 )
      buf = mstrccat( buf, ' ', indent );
  }

  return buf;
}


/*
 * protect all special characters of text formatter
 */
static
char *fmt_filter( const char *line,
                 int docsys )
{
  static char *buf = NULL;
  int i;

  if( buf == NULL )
    buf = strdup( "" );
  else
    buf[0] = '\0';

  for( i = 0; line[i] != '\0'; i++ ) {
    char esc[16];
    esc[0] = line[i];
    esc[1] = '\0';

    switch( docsys ) {

    case DOCSYS_LATEX:

      switch( line[i] ) {
      case '\\':
      case '$':
      case '&':
      case '%':
      case '#':
      case '{':
      case '}':
      case '_':
        esc[0] = '\\';
        esc[1] = line[i];
        esc[2] = '\0';
        break;

      case '<':
        strcpy( esc, "\\BRA{}" );
        break;

      case '>':
        strcpy( esc, "\\KET{}" );
        break;

      case '|':
        strcpy( esc, "\\PIPE{}" );
        break;

      case '"':
        strcpy( esc, "\\DQUOTE{}" );
        break;
      }
    }

    buf = mstrcat( buf, esc );
  }

  return buf;
}


void F77_ENTRY_I(Kulun,lun )
/* { */
  int base = *lun;
  if( base <= 0 )
    base = 11;
  kc_flags.vread_luno = base + 1;
  kc_flags.vwrite_luno = base + 2;
  kc_flags.appl_luno = base + 4;
  kc_flags.uhlp_luno = base + 4;
  Kilun( lun );                 /* kludge */
}


void F77_ENTRY_IC(Kuhelp,lun,chpath)
/* { */
  *lun = kc_flags.uhlp_luno;
  fstrset( chpath, len_chpath, user_help_name );
}


char **kscncmds( brobj_name, brcls_name, path, n )
     char *brobj_name;
     char *brcls_name;
     char *path;
     int n;
{
  static char *obj_desc[4];
  static char  ltext[128];
  static KmMenu    *menu;
  static KmMenu *submenu;
  static KmCommand *cmd;

  obj_desc[0] = NULL;           /* name */
  obj_desc[1] = NULL;           /* class */
  obj_desc[2] = NULL;           /* alias */
  obj_desc[3] = NULL;           /* text */

  if( n == 0 ) {
    /* initialize to return first command */
    if( path[0] != '\0' ) {
      KmMenu **menu_list = find_menu_list( NULL, &real_root, path );
      menu = menu_list[0];
      free( menu_list );
    }
    else
      menu = &real_root;
    if( menu == NULL ) return obj_desc;
    submenu = menu->down;
    cmd = menu->cmds;
  }

  if( submenu != NULL ) {
    /* return next submenu */
    obj_desc[0] = submenu->name;
    obj_desc[1] = "Menu";
    obj_desc[2] = submenu->path;
    submenu = submenu->next;
  }
  else if( cmd != NULL ) {
    /* return next command */
    int len = 0;
    int i;
    ltext[0] = '\0';
    for( i = 0; i < cmd->mandatory; i++ ) {
      KmParameter *par = cmd->par[i];
      sprintf( &ltext[len], " %s", par->name );
      len += strlen( &ltext[len] );
    }
    if( cmd->total > cmd->mandatory ) {
      sprintf( &ltext[len], " [" );
      len += strlen( &ltext[len] );
      for( i = cmd->mandatory; i < cmd->total; i++ ) {
        KmParameter *par = cmd->par[i];
        sprintf( &ltext[len], " %s", par->name );
        len += strlen( &ltext[len] );
      }
      sprintf( &ltext[len], " ]" );
    }
    obj_desc[0] = cmd->name;
    obj_desc[1] = cmd->hidden ? "InvCmd" : "Cmd";
    obj_desc[2] = cmd->path;
    obj_desc[3] = ltext;
    cmd = cmd->next;
  }
  return obj_desc;
}


/*
 *
 */
static
KmCommand *lookup_cmd( const char *cmd_name,
                      const char *called_by )
{
  KmCommand *cmd = curr_cmd;

  if( cmd != NULL && strcasecmp( cmd->name, cmd_name ) == 0 )
    return cmd;

  if( curr_menu != NULL ) {
    for( cmd = curr_menu->cmds; cmd != NULL; cmd = cmd->next ) {
      if( strcasecmp( cmd->name, cmd_name ) == 0 )
        return cmd;
    }
  }

  cmd = search_command( cmd_name, NULL );
  if( cmd == NULL )
    printf( " *** %s: invalid command '%s'\n", called_by, cmd_name );

  return cmd;
}


/*
 * set amount of words needed to store command definition
 */
void F77_ENTRY_I(Kunwg,nwg )
/* { */
  /* OBSOLETE */
}


/*
 * create a new command
 */
void F77_ENTRY_C3(Kucmd,refpat,item,chopt)
/* { */
  KmMenu **menu_list;
  char *opt = fstrdup( chopt, len_chopt );

  if( strcasecmp( opt, "C" ) == 0 ) {
    /* create a new menu or command */
    if( curr_menu->cmds == NULL ) {
      prev_cmd = NULL;
      curr_cmd = curr_menu->cmds = (KmCommand*)calloc( 1, sizeof(KmCommand) );
    }
    else {
      for( curr_cmd = curr_menu->cmds; curr_cmd->next != NULL; )
        curr_cmd = curr_cmd->next;
      prev_cmd = curr_cmd;
      curr_cmd = curr_cmd->next = (KmCommand*)calloc( 1, sizeof(KmCommand) );
    }
    curr_cmd->name = fstrdup( item, len_item );
    curr_cmd->path = str3dup( curr_menu->path, "/", curr_cmd->name );
    curr_cmd->level = curr_menu->level;
  }

  else if( strcasecmp( opt, "SW" ) == 0 ) {
    /* switch to menu */
    char *menu_name = fstrdup( refpat, len_refpat );

    if( curr_cmd != NULL && strcasecmp( menu_name, curr_cmd->name ) == 0 ) {
      /*
       * only now we know that KUCMD(' ',name,'C') should have created
       * a menu instead of a command
       */
      if( prev_cmd == NULL ) {
        curr_menu->cmds = NULL;
      }
      else {
        prev_cmd->next = NULL;
      }
      if( curr_menu->down == NULL ) {
        curr_menu = curr_menu->down = (KmMenu*)calloc( 1, sizeof(KmMenu) );
      }
      else {
        curr_menu = curr_menu->down;
        while( 1 ) {
          if( strcasecmp( curr_menu->name, curr_cmd->name ) == 0 ) {
            break;
          }
          else if( curr_menu->next != NULL ) {
            curr_menu = curr_menu->next;
          }
          else {
            curr_menu = curr_menu->next = (KmMenu*)calloc( 1, sizeof(KmMenu) );
            break;
          }
        }
      }
      curr_menu->path = curr_cmd->path;
      curr_menu->name = curr_cmd->name;
      curr_menu->level = curr_cmd->level + 1;
      if( curr_cmd->nguidance ) {
        curr_menu->nguidance = curr_cmd->nguidance;
        curr_menu->guidance = curr_cmd->guidance;
      }
      free( curr_cmd );
      curr_cmd = NULL;
    }
    else if( strcmp( menu_name, "/" ) == 0 ) {
      curr_menu = &real_root;
    }
    else if( strcmp( menu_name, ".." ) == 0
          || strcmp( menu_name, "\\" ) == 0 ) {
      char *path = strdup( curr_menu->path );
      char *slash = strrchr( path, '/' );
      *slash = '\0';
      if( slash == path )
        curr_menu = &real_root;
      else {
        menu_list = find_menu_list( NULL, &real_root, path );
        curr_menu = menu_list[0];
        free( menu_list );
      }
      free( path );
    }
    else if( strncmp( menu_name, "../", 3 ) == 0 || menu_name[0] == '\\' ) {
      char *path = strdup( curr_menu->path );
      char *slash = strrchr( path, '/' );
      if( menu_name[0] == '\\' )
        menu_name[0] = '/';
      else
        strcpy( menu_name, menu_name + 2 );
      *slash = '\0';
      path = realloc( path, strlen( path ) + strlen( menu_name ) + 1 );
      strcat( path, menu_name );
      menu_list = find_menu_list( NULL, &real_root, path );
      curr_menu = menu_list[0];
      free( menu_list );
      free( path );
    }
    else {
      menu_list = find_menu_list( NULL, curr_menu, menu_name );
      curr_menu = menu_list[0];
      free( menu_list );
    }
    free( menu_name );
  }

  else if( strcasecmp( opt, "D" ) == 0 ) {
    /* delete command */
    KmMenu *menu;
    KmCommand *cmd;
    char *menu_path = fstrdup( refpat, len_refpat );
    char *cmd_name = strrchr( menu_path, '/' );
    *cmd_name++ = '\0';
    menu_list = find_menu_list( NULL, &real_root, menu_path );
    menu = menu_list[0];
    free( menu_list );
    for( cmd = menu->cmds; cmd->next != NULL; cmd = cmd->next ) {
      if( strcasecmp( cmd->next->name, cmd_name ) == 0 ) {
        cmd->next = cmd->next->next;
        break;
      }
    }
  }
  else if( strcasecmp( opt, "E" ) == 0 ) {
    /* end definition */
  }
  else {
    printf( " *** KUCMD: invalid option '%s'\n", opt );
  }
  free( opt );
}


/*
 * store guidance text for command
 */
void F77_ENTRY_CCIC(Kuguid,refpat,gguid,nlines,chopt)
/* { */
  char *path = fstrdup( refpat, len_refpat );
  KmCommand *cmd = lookup_cmd( path, "KUGUID" );

  if( cmd != NULL ) {
    char *opt = fstrdup( chopt, len_chopt );
    if( strcasecmp( opt, "S" ) == 0 ) {
      /* set guidance */
      int i;
      cmd->nguidance = *nlines;
      cmd->guidance = (char**)malloc( *nlines * sizeof(char*) );
      for( i = 0; i < cmd->nguidance; i++ ) {
        cmd->guidance[i] = fstrdup( &gguid[len_gguid*i], len_gguid );
        if( strcmp( cmd->guidance[i], "." ) == 0 )
          cmd->guidance[i][0] = '\0';
      }
    }
    else if( strcasecmp( opt, "Q" ) == 0 ) {
      /* return the guidance text */
      int i;
      if( *nlines > cmd->nguidance ) {
        int n = ( *nlines - cmd->nguidance ) * len_gguid;
        memset( &gguid[len_gguid*cmd->nguidance], ' ', n );
        *nlines = cmd->nguidance;
      }
      for( i = 0; i < *nlines; i++ ) {
        fstrset( &gguid[len_gguid*i], len_gguid, cmd->guidance[i] );
      }
      *nlines = cmd->nguidance;
    }
    else {
      printf( " *** KUGUID: invalid option '%s'\n", opt );
    }
    free( opt );
  }
  free( path );
}


/*
 * store user help routine
 */
void F77_ENTRY_CE(Kuach,refpat,actrou)
/* { */
  char *path = fstrdup( refpat, len_refpat );
  KmCommand *cmd = lookup_cmd( path, "KUACH" );

  if( cmd != NULL )
    cmd->user_help_F = actrou;

  free( path );
}


/*
 * store action routine
 */
void F77_ENTRY_CE(Kuact,refpat,actrou)
/* { */
  char *path = fstrdup( refpat, len_refpat );
  KmCommand *cmd = lookup_cmd( path, "KUACT" );

  if( cmd != NULL )
    cmd->action_F = actrou;

  free( path );
}


/*
 * set amount of words needed to store parameter definition
 */
void Kundpv(  nwdef,  nwlow,  nwhig,  npval,  nwpv)
     INTEGER *nwdef, *nwlow, *nwhig, *npval, *nwpv;
{
  /* OBSOLETE */
}


/*
 * store parameter definition
 */
void F77_ENTRY_C5(Kupar,celpat,pname,prompt,type,chopt)
/* { */
  char *path = fstrdup( celpat, len_celpat );
  KmCommand *cmd = lookup_cmd( path, "KUPAR" );

  if( cmd != NULL ) {
    char *opt = fstrdup( chopt, len_chopt );
    if( strcasecmp( opt, "S" ) == 0 ) {
      /* define a new parameter */
      KmParameter *par;
      int optional = 0;
      int i;

      cmd->total++;

      if( cmd->par == NULL )
        cmd->par = (KmParameter**)malloc( sizeof(KmParameter*) );
      else
        cmd->par = (KmParameter**)realloc( (char*)cmd->par,
                                          cmd->total * sizeof(KmParameter*) );
      par = cmd->par[cmd->total-1] =
        (KmParameter*)calloc( 1, sizeof(KmParameter) );
      par->name = fstrdup( pname, len_pname );
      par->abbrev = strlen( par->name );
      par->prompt = fstrdup( prompt, len_prompt );

      switch( type[0] ) {
      case 'I':
        par->type = KmTYPE_INT;
        par->ptype = (KmParInt*)calloc( 1, sizeof(KmParInt) );
        break;
      case 'R':
        par->type = KmTYPE_REAL;
        par->ptype = (KmParReal*)calloc( 1, sizeof(KmParReal) );
        break;
      case 'O':
        par->type = KmTYPE_OPTION;
        par->ptype = (KmParOption*)calloc( 1, sizeof(KmParOption) );
        break;
      default:
        par->type = KmTYPE_CHAR;
      }

      par->flags = 0;
      for( i = 1; i < len_type; i++ ) {
        switch( type[i] ) {
        case 'O':
          optional = 1;
          break;
        case 'C':
          par->flags |= KmFLAG_CONST;
          break;
        case 'F':
          par->flags |= KmFLAG_FORGET;
          break;
        case 'H':
          par->flags |= KmFLAG_HIDDEN;
          break;
        case 'M':
          par->flags |= KmFLAG_MINUS;
          break;
        case 'Q':
          par->flags |= KmFLAG_QUOTE;
          break;
        case 'S':
          par->flags |= KmFLAG_SEPARATE;
          break;
        case 'V':
          par->flags |= KmFLAG_VARARG;
          break;
        case ' ':
          /* ignore */
          break;
        default:
          printf( " *** KUPAR: invalid modifier '%c'\n", type[i] );
        }
      }

      if( !optional )
        cmd->mandatory++;
    }
    else {
      printf( " *** KUPAR: invalid option '%s'\n", opt );
    }
    free( opt );
  }
  free( path );
}


/*
 * set parameter values
 */
void F77_ENTRY_CCIRCC(Kupval,celpat,pname,ival,rval,cval,chopt)
/* { */
  char *path = fstrdup( celpat, len_celpat );
  KmCommand *cmd = lookup_cmd( path, "KUPVAL" );

  if( cmd != NULL ) {
    char *par_name = fstrdup( pname, len_pname );
    KmParameter *par = NULL;
    int i;

    for( i = 0; i < cmd->total; i++ ) {
      if( strcasecmp( cmd->par[i]->name, par_name ) == 0 ) {
        par = cmd->par[i];
        break;
      }
    }
    if( par == NULL ) {
      printf( " *** KUPVAL: command %s has no parameter named %s\n",
             cmd->path, par_name );
    }
    else {
      char *opt = fstrdup( chopt, len_chopt );
      int wrong = 0;

      if( opt[0] == '-' ) {
        wrong = 1;
        if( par->type == KmTYPE_OPTION ) {
          KmParOption *popt = par->ptype;
          int n;
          for( n = 0; n < par->range_count; n++ ) {
            if( strcasecmp( par->range_value[n], &opt[1] ) == 0 ) {
              popt->text[n] = fstrdup( cval, len_cval );
              wrong = 0;
              break;
            }
          }
        }
      }
      else if( strcasecmp( opt, "ABBREV" ) == 0 ) {
        par->abbrev = *ival;
      }
      else if( strcasecmp( opt, "FLAGS" ) == 0 ) {
        par->flags |= *ival;
      }
      else if( strcasecmp( opt, "LOOP" ) == 0 ) {
        cmd->list_par = i + 1;
      }
      else if( par->type == KmTYPE_INT || par->type == KmTYPE_REAL ) {
        KmParInt *pint = par->ptype;
        KmParReal *preal = par->ptype;
        char buf[32];

        if( par->type == KmTYPE_INT ) {
          int lval = *ival;
          sprintf( buf, "%d", lval );
        }
        else {
          double dval = *rval;
          strcpy( buf, strfromd( dval, 0 ) );
        }

        if( strcasecmp( opt, "D" ) == 0 ) {
          par->dfault = strdup( buf );
        }
        else if( strcasecmp( opt, "H" ) == 0 ) {
          if( par->type == KmTYPE_INT )
            pint->range_upper = strdup( buf );
          else
            preal->range_upper = strdup( buf );
        }
        else if( strcasecmp( opt, "L" ) == 0 ) {
          if( par->type == KmTYPE_INT )
            pint->range_lower = strdup( buf );
          else
            preal->range_lower = strdup( buf );
        }
        else if( strcasecmp( opt, "V" ) == 0 ) {
          if( par->range_count++ == 0 )
            par->range_value = (char**)malloc( sizeof(char*) );
          else
            par->range_value =
              (char**)realloc( (char*)par->range_value,
                              par->range_count * sizeof(char*) );
          par->range_value[par->range_count-1] = strdup( buf );
        }
        else {
          wrong = 1;
        }
      }
      else if( par->type == KmTYPE_CHAR || par->type == KmTYPE_OPTION ) {
        char *sval = fstrdup( cval, len_cval );
        if( *sval == '\0' )
          sval = mstrcat( sval, " " );
        if( strcasecmp( opt, "D" ) == 0 ) {
          par->dfault = sval;
        }
        else if( strcasecmp( opt, "V" ) == 0 ) {
          char *p;
          for( p = strtok( sval, "," ); p != NULL; p = strtok( NULL, "," ) ) {
            if( par->range_count++ == 0 )
              par->range_value = (char**)malloc( sizeof(char*) );
            else
              par->range_value =
                (char**)realloc( (char*)par->range_value,
                                par->range_count * sizeof(char*) );
            if( strcmp( p, " " ) == 0 )
              p = "";
            par->range_value[par->range_count-1] = p;
          }
          if( par->range_count > 0 ) {
            KmParOption *popt;
            if( par->type != KmTYPE_OPTION ) {
              par->type = KmTYPE_OPTION;
              par->ptype = (KmParOption*)calloc( 1, sizeof(KmParOption) );
            }
            popt = par->ptype;
            popt->text = (char**)calloc( par->range_count, sizeof(char*) );
            popt->mutex = (int*)calloc( par->range_count, sizeof(int) );
            popt->radio = (int*)calloc( par->range_count, sizeof(int) );
          }
        }
        else {
          wrong = 1;
          free( sval );
        }
      }
      else {
        if( strcasecmp( opt, "D" ) == 0 ) {
          par->dfault = fstrdup( cval, len_cval );
        }
        else {
          wrong = 1;
        }
      }

      if( wrong )
        printf( " *** KUPVAL: invalid option '%s' for %s %s\n",
               opt, cmd->path, par->name );
      free( opt );
    }
    free( par_name );
  }
  free( path );
}


