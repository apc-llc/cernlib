/*
 * $Id: kuwhag.c,v 1.2 1997/03/17 16:55:02 mclareni Exp $
 *
 * $Log: kuwhag.c,v $
 * Revision 1.2  1997/03/17 16:55:02  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:32:53  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:32:58  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/03 09/05/95  11.54.42  by  N.Cremel*/
/*-- Author :*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"


#define MAX_MENU_LEVEL 16
static int redraw_menu_level = 0;
static int last_menu_choice[MAX_MENU_LEVEL];


static int   higz_init( int );
static void  input_styleG(void);
static char* disp_all_lines(void);
static char* disp_all_menus( const char* );
static char* disp_one_line( int, double, double, double, double, const char* );
static char* disp_one_menu( KmMenu*, float*, const char*, int* );
static char* disp_cmd_menu( KmCommand*, double, const char* );
static char* request_input( char*, double );
static int   disp_menu( const char*, double, double, double, double, int,
                       const char*, int, int, const char*, int,
                       const char*, int, const char*, int, const char*, int );


/*
 * command loop with graphics menus
 */
void F77_ENTRY(Kuwhag)
/* { */
  ku_whag();
}


void ku_whag()
{
  kc_window.kuwhag_called = 1;
  kjmpaddr.higz_init_C = higz_init;

  ku_what( input_styleG );
}


/*
 * initialize HIGZ if not yet done
 */
static
int higz_init( int wk_type )
{
  INTEGER iwk = wk_type;

  if( Pawc->IXHIGZ == 0 ) {
    INTEGER nwhigz = 100;
    INTEGER lun_error = 6;
    REAL xrange = 1;
    REAL yrange = 1;

    Iginit( &nwhigz );
    if( iwk == 0 )
      Igwkty( &iwk );
    Igsse( &lun_error, &iwk );
    Igrng( &xrange, &yrange );
  }
  return iwk;
}


static
void input_styleG()
{
  char *path = "";
  REAL sgsize = kc_window.sgsize;
  INTEGER wkid = 1;
  INTEGER flag = 1;
  int old_style = kc_flags.style;
  int i;

  higz_init( 0 );               /* in case STYLE G executed before KUWHAG */
  Igsrap( &sgsize );
  Iclrwk( &wkid, &flag );

  redraw_menu_level = 0;
  for( i = 0; i < MAX_MENU_LEVEL; i++ )
    last_menu_choice[i] = 0;

  while( kc_flags.style == old_style
        && !kc_flags.do_exit && !kc_flags.do_quit ) {
    char *line;

    if( old_style & KmSTYLE_xP ) {
      /* style GP */
      if( kc_window.panel_rows > 0 ) {
        line = disp_all_lines();
      }
      else {
        printf( " *** No panel defined\n" );
        line = strdup( "/KUIP/SET_SHOW/STYLE C" );
      }
      ku_last( line );
      ku_exel( line );
      free( line );
    }
    else {
      /* style G */
      line = disp_all_menus( path );

      if( strchr( line, ' ' ) == NULL ) {
        path = line;
      }
      else {
        /* command lines contain a blank */
        ku_last( line );
        ku_exel( line );
        free( line );
      }
    }
  }

  sgsize = 1;
  Igsrap( &sgsize );
  Iclrwk( &wkid, &flag );
}


/*
 * display all panel lines
 * return the command string
 */
static
char *disp_all_lines()
{
  float x1 = kc_window.sgbord;
  float x2 = 1 - kc_window.sgbord;
  float y2 = 1 - kc_window.sgyspa;
  float dy = kc_window.sgylen;
  float y;
  int n;
  char *item = NULL;

  for( y = y2, n = 0; n < kc_window.panel_rows; n++ ) {
    disp_one_line( n, x1, x2, y - dy, y, "D" );
    y -= dy;
  }

  do {
    static char opt[] = " CU";
    opt[0] = ' ';               /* request locator each time around */
    for( y = y2, n = 0; n < kc_window.panel_rows && item == NULL; n++ ) {
      item = disp_one_line( n, x1, x2, y - dy, y, opt );
      y -= dy;
      opt[0] = 'N';             /* look if locator inside other menu */
    }
  } while( item == NULL );

  return request_input( item, x2 );
}


/*
 * request additional input if button label contains special characters
 */
static
char *request_input( char *item,
                    double x2 )
{
  float x1 = kc_window.sgbord;
  float y1 = kc_window.sgsize + kc_window.sgyspa;
  float dy = kc_window.sgylen;
  char *dollar = NULL;
  char *line;
  char *p;
  int n;

  /* strip comments before looking for special characters */
  if( (p = strchr( item, '|' )) != NULL ) {
    *p = '\0';
  }

  for( n = strlen( item ); n > 0 && item[n-1] == ' '; n-- )
    item[n-1] = '\0';           /* strip trailing blanks */

  if( item[n-1] == '-' || (dollar = strchr( item, '$' )) != NULL ) {
    /* request additional input */
    char chval[80];
    int lchval = (sizeof chval);
    int choice;

    memset( chval, ' ', lchval );
    choice = disp_menu( NULL, x1, x2, y1, y1 + dy, 0, NULL, 0,
                       1, item, n, NULL, 0, chval, lchval, "IE", 0 );

    if( choice == -1000 ) {
      line = strdup( "/KUIP/SET_SHOW/STYLE C" );
    }
    else {
      char *reply = fstrdup( chval, lchval );

      if( dollar != NULL ) {
        /* replace "ABC$DEF" by "ABCinputDEF" */
        *dollar = '\0';
        line = str3dup( item, reply, dollar + 1 );
      }
      else if( n > 1 && item[n-2] == '-' ) {
        /* replace "ABC--" by "ABCinput" */
        item[n-2] = '\0';
        line = str2dup( item, reply );
      }
      else {
        /* replace "ABC-" by "ABC input" */
        item[n-1] = ' ';
        line = str2dup( item, reply );
      }
      free( reply );
    }
    free( item );
  }
  else
    line = item;

  /* append a blank to make it an executable command for the main loop */
  if( strchr( line, ' ' ) == NULL )
    line = mstrcat( line, " " );

  return line;
}


static
char *disp_one_line( int row,
                    double x1,
                    double x2,
                    double y1,
                    double y2,
                    const char *mopt )
{
  int nbutton = kc_window.panel_cols[row];
  char *button;
  size_t lbutton = 0;
  char *line = NULL;

  if( nbutton > 0 ) {
    char *opt = str2dup( "HT", mopt );
    int choice;

    button = fstrvec( kc_window.panel_keys[row], nbutton, &lbutton );
    choice = disp_menu( NULL, x1, x2, y1, y2, nbutton, button, lbutton,
                       0, NULL, 0, NULL, 0, NULL, 0, opt, 0 );
    free( button );
    free( opt );

    if( choice == -1000 ) {
      line = strdup( "/KUIP/SET_SHOW/STYLE C" );
    }
    else {
      int n = -choice;
      if( n > 0 && n <= nbutton ) {
        line = str0dup( kc_window.panel_keys[row][n-1] );
        if( kc_window.panel_keyval != NULL )
          free( kc_window.panel_keyval );
        kc_window.panel_keyval =
          (line != NULL) ? str3dup( "'", line, "'" ) : NULL;
        if( kc_window.panel_keynum == NULL )
          kc_window.panel_keynum = malloc( 16 );
        sprintf( kc_window.panel_keynum, "%d.%s", row + 1, strfromi( n, 2 ) );
      }
    }
  }

  return line;
}


/*
 * display all graphics menus down to the level of the submenu
 * return the path of the selected menu or command
 */
static
char *disp_all_menus( const char *path )
{
  KmCommand *cmd = NULL;
  KmMenu *menu_list[MAX_MENU_LEVEL];
  char *subpath = strdup( path );
  char *item = NULL;
  float x1 = kc_window.sgbord;
  float x2 = 1 - kc_window.sgbord;
  float y2 = 1 - kc_window.sgyspa;
  float dy = kc_window.sgylen;
  float y;
  int n;
  float ymhi0 = y2;
  float ymhi;
  int lev = 0;
  char *p;
  int i;

  /* build the list of menu paths, e.g. "/KUIP/ALIAS", "/KUIP", "" */
  while( (p = strrchr( subpath, '/' )) != NULL ) {
    menu_list[lev++] = find_submenu( subpath );
    *p = '\0';
  }
  free( subpath );
  menu_list[lev++] = find_submenu( "" );

  if( menu_list[0] == NULL ) {
    /* path can be a command */
    cmd = search_command( path, NULL );
    for( i = 1; i < lev; i++ )
      menu_list[i-1] = menu_list[i];
    lev--;
  }

  for( i = 0; i < lev / 2; i++ ) {
    /* invert menu_list order */
    KmMenu *m = menu_list[i];
    menu_list[i] = menu_list[lev-i-1];
    menu_list[lev-i-1] = m;
  }

  /* display panel first if defined */
  if( kc_window.panel_rows > 0 ) {
    for( y = y2, i = 0; i < kc_window.panel_rows; i++ ) {
      disp_one_line( i, x1, x2, y - dy, y, "D" );
      y -= dy;
    }
    ymhi0 = y - kc_window.sgyspa;
  }

  /* display all menus top down*/
  for( ymhi = ymhi0, i = 0; i < lev; i++ ) {
    char *opt = (i < redraw_menu_level) ? "" : "D";
    disp_one_menu( menu_list[i], &ymhi, opt, NULL );
  }
  if( cmd != NULL )
    disp_cmd_menu( cmd, ymhi0, "D" );

  do {
    static char opt[] = "NC ";
    opt[0] = ' ';               /* request locator each time around */

    if( kc_window.panel_rows > 0 ) {
      opt[2] = 'U';             /* update user keys */
      for( y = y2, n = 0; n < kc_window.panel_rows && item == NULL; n++ ) {
        item = disp_one_line( n, x1, x2, y - dy, y, opt );
        y -= dy;
        opt[0] = 'N';             /* look if locator inside other menu */
      }
      opt[2] = ' ';
      if( item != NULL ) {
        /* erase command menu before additional input request */
        if( cmd != NULL )
          disp_cmd_menu( cmd, ymhi0, "E" );
        return request_input( item, kc_window.sgsize - kc_window.sgbord );
      }
    }

    for( ymhi = ymhi0, i = 0; i < lev && item == NULL; i++ ) {
      item = disp_one_menu( menu_list[i], &ymhi, opt, &last_menu_choice[i] );
      opt[0] = 'N';             /* look if locator inside other menu */
    }
    if( item != NULL ) {
      /* number of topmenus we don't have to redraw the next time around */
      redraw_menu_level = i;
      /* erase all lower submenus */
      for( ; i < lev; i++ )
        disp_one_menu( menu_list[i], &ymhi, "E", &last_menu_choice[i] );
      if( cmd != NULL )
        disp_cmd_menu( cmd, ymhi0, "E" );
    }
    else if( cmd != NULL )
      item = disp_cmd_menu( cmd, ymhi0, opt );
  } while( item == NULL );

  return item;
}


/*
 * display graphics menu
 * return the path of the selected menu
 */
static
char *disp_one_menu( KmMenu *submenu,
                    float *yhi,
                    const char *mopt,
                    int *return_choice )
{
  float x1 = kc_window.sgsize + kc_window.sgbord;
  float x2 = 1 - kc_window.sgbord;
  float y1;
  float y2 = *yhi;
  int   nitem;
  char  *item;
  size_t litem = 0;
  char **items = (char**)malloc( sizeof(char*) );
  int choice;
  int last_choice = (return_choice != NULL) ? *return_choice : 0;
  char *opt = str2dup( "HT", mopt );
  int ncmds;
  int n = 0;
  KmMenu *menu;
  KmCommand *cmd;

  for( cmd = submenu->cmds; cmd != NULL; cmd = cmd->next ) {
    char *mark = (cmd->hidden || (cmd->action_F == NULL
                                  && cmd->action_C == NULL)) ? "-" : "";
    items = (char**)realloc( (char*)items, (n+1) * sizeof(char*) );
    items[n++] = str2dup( mark, cmd->name );
  }
  ncmds = n;

  for( menu = submenu->down; menu != NULL; menu = menu->next ) {
    items = (char**)realloc( (char*)items, (n+1) * sizeof(char*) );
    items[n++] = str2dup( "|", menu->name );
  }
  nitem = n;

  item = strlower( fstrvec( items, n, &litem ) );

  for( n = 0; n < nitem; n++ )
    free( items[n] );
  free( (char*)items );

  y1 = y2 - nitem * kc_window.sgylen;
  *yhi = y1 - kc_window.sgyspa;

  choice = disp_menu( NULL, x1, x2, y1, y2, 0, NULL, 0,
                      nitem, item, litem, NULL, 0, NULL, 0, opt, last_choice );

  free( item );
  free( opt );

  if( choice == -1000 ) {
    return strdup( "/KUIP/SET_SHOW/STYLE C" );
  }
  else if( choice > 0 && choice <= nitem ) {
    if( return_choice != NULL )
      *return_choice = choice;
    if( --choice < ncmds ) {
      for( cmd = submenu->cmds, n = 0; n < choice; n++ )
        cmd = cmd->next;
      return cmd->path;
    }
    else {
      for( menu = submenu->down, n = ncmds; n < choice; n++ )
        menu = menu->next;
      return menu->path;
    }
  }
  else
    return NULL;
}


/*
 * display graphics panel for cmd
 * return the path of the selected menu
 */
static
char *disp_cmd_menu( KmCommand *cmd,
                    double y2,
                    const char *mopt )
{
  char *title = cmd->path;
  float x1 = kc_window.sgbord;
  float x2 = kc_window.sgsize - kc_window.sgbord;
  float y1;
  float y0 = kc_window.sgsize + kc_window.sgyspa;
  int nbutton = (cmd->hidden || (cmd->action_F == NULL
                                 && cmd->action_C == NULL)) ? 1 : 2;
  static char *button = NULL;
  static size_t lbutton;
  int     nitem = 0;
  char    *item;
  size_t  litem;
  char   **items = (char**)malloc( cmd->total * sizeof(char*) );
  char    *pdef;
  size_t  lpdef;
  char   **pdefs = (char**)malloc( cmd->total * sizeof(char*) );
  int     lpval = 20 * sizeof(INTEGER);
  char    *pval = (char*)memset( malloc( cmd->total * lpval ), ' ',
                                cmd->total * lpval );
  int choice;
  char *opt = str2dup( "HP", mopt );
  char *line = NULL;
  int n;

  if( button == NULL ) {
    static char *labels[] = { "Help", "Execute" };
    button = fstrvec( labels, (sizeof labels) / sizeof(char*), &lbutton );
  }

  for( n = 0; n < cmd->total; n++ ) {
    KmParameter *par = cmd->par[n];

    if( (par->flags & KmFLAG_HIDDEN) == 0 ) {
      char *mark = (n < cmd->mandatory) ? "|" : "";

      items[nitem] = str2dup( mark, par->prompt );

      if( par->last != NULL )
        pdefs[nitem] = strdup( par->last );
      else if( par->dfault != NULL )
        pdefs[nitem] = strdup( par->dfault );
      else
        pdefs[nitem] = strdup( "" );

      if( strcmp( pdefs[nitem], " " ) == 0 )
        pdefs[nitem][0] = '\0';

      if( par->range_count > 0 ) {
        char *p = strdup( pdefs[nitem] );
        int i;
        for( i = 0; i < par->range_count; i++ ) {
          if( strcasecmp( par->range_value[i], p ) != 0 ) {
            if( pdefs[nitem][0] == '\0' )
              pdefs[nitem] = mstrcat( pdefs[nitem], " " );
            pdefs[nitem] = mstr2cat( pdefs[nitem], ",", par->range_value[i] );
            if( par->range_value[i][0] == '\0' )
              pdefs[nitem] = mstrcat( pdefs[nitem], " " );
          }
        }
        free( p );
      }
      nitem++;
    }
  }

  item = fstrvec( items, nitem, &litem );
  pdef = fstrvec( pdefs, nitem, &lpdef );

  for( n = 0; n < nitem; n++ ) {
    free( items[n] );
    free( pdefs[n] );
  }

  y1 = y2 - (nitem + 2) * kc_window.sgylen;
  if( y1 < y0 )
    y1 = y0;

  choice = disp_menu( title, x1, x2, y1, y2, nbutton, button, lbutton,
                     nitem, item, litem, pdef, lpdef, pval, lpval, opt, 0 );

  free( item );
  free( pdef );
  free( opt );

  if( choice == -1000 ) {
    line = strdup( "/KUIP/SET_SHOW/STYLE C" );
  }
  else if( choice == -1 ) {
    line = str2dup( "/KUIP/HELP ", cmd->path );
  }
  else if( choice == -2 ) {
    int kitem = 0;
    line = str2dup( cmd->path, " " );

    for( n = 0; n < cmd->total; n++ ) {
      KmParameter *par = cmd->par[n];

      if( (par->flags & KmFLAG_HIDDEN) == 0 ) {
        char *arg = fstr0dup( pval + kitem * lpval, lpval );

        if( arg == NULL ) {
          if( n < cmd->mandatory )
            line = mstrcat( line, " ' '" );
          else
            line = mstrcat( line, " !" );
        }
        else {
          if( strchr( arg, '\'' ) == NULL && strchr( arg, ' ' ) != NULL )
            line = mstr3cat( line, " '", arg, "'" );
          else
            line = mstr2cat( line, " ", arg );
          free( arg );
        }
        kitem++;
      }
      else {
        line = mstrcat( line, " !" );
      }
    }
  }
  free( pval );

  return line;
}


static
int disp_menu( const char *title,
              double x1,
              double x2,
              double y1,
              double y2,
              int nbutton,
              const char *button,
              int lbutton,
              int nitem,
              const char *item,
              int litem,
              const char *dfault,
              int ldfault,
              const char *value,
              int lvalue,
              const char *option,
              int last_choice )
{
  static char *blank = NULL;
  static size_t lblank = 0;
  int ltitle;
  INTEGER MN = 0;
  REAL X1 = x1;
  REAL X2 = x2;
  REAL Y1 = y1;
  REAL Y2 = y2;
  INTEGER NBU = nbutton;
  INTEGER N = nitem;
  INTEGER ICHOIC = last_choice;
  char *opt = strdup( option );

  if( blank == NULL )
    blank = fstrvec( NULL, 0, &lblank );

  if( title == NULL )
    title = " ";
  ltitle = strlen( title );

  if( button == NULL ) {
    button = blank;
    lbutton = lblank;
  }

  if( dfault == NULL ) {
    dfault = blank;
    ldfault = lblank;
  }

  if( value == NULL ) {
    value = blank;
    lvalue = lblank;
  }

  if( kc_flags.style & KmSTYLE_xS ) /* software characters */
    opt = mstrcat( opt, "S" );

  if( kc_flags.style & KmSTYLE_xW ) /* shadow width */
    opt = mstrcat( opt, "W" );

  if( kc_flags.style & KmSTYLE_G ) {
    F77_CALL_xCx5CxC3xC(Igmenu,&MN,title,ltitle,
                      &X1,&X2,&Y1,&Y2,&NBU,button,lbutton,
                      &N,item,litem,dfault,ldfault,value,lvalue,
                      &ICHOIC,opt,strlen(opt));
  }

  free( opt );

  return ICHOIC;
}


