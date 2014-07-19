/*
 * $Id: kuwhat.c,v 1.9 2005/04/18 15:23:10 mclareni Exp $
 *
 * $Log: kuwhat.c,v $
 * Revision 1.9  2005/04/18 15:23:10  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.8  1999/03/10 17:11:52  mclareni
 * Correct lower to upper case file name conversion
 *
 * Revision 1.7  1998/08/25 12:47:40  mclareni
 * QMGLIBC changes for Linux RH51
 *
 * Revision 1.6  1997/11/28 17:27:00  mclareni
 * Numerous mods and some new routines to get Control-C working reasonably on NT
 *
 * Revision 1.5  1997/03/24 12:51:05  gunter
 * Merge changes done for WinNT:
 *  - adding F77_ENTRY to Kuwhat, Kisigm, Kualfa and Kuqcas.
 *  - Adding signal stuff for float exceptions, adding fpceck, and calling
 *    this from what_loop
 *
 * Revision 1.4  1997/02/20 13:21:32  cremel
 * Reset global flags do_exit and do_quit to "false" before returning
 * from the "what_loop" in case kuwhat is called several times in the main
 * program (cf. mail from Mark Lakata who is using KUIP in his event display
 * program)
 *
 * Revision 1.3  1996/10/14 09:01:36  cremel
 * Change place of initialization for do_exit and do_quit (from kuwhat to
 * kuinit) in order to allow "exit" in pawlogon.kumac.
 *
 * Revision 1.2.2.1  1997/01/21 11:32:54  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.2  1996/04/04 09:15:20  cernlib
 * In ku_pros added a strdup before returning the default
 *
 * Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/04 23/05/95  11.21.15  by  Gunter Folger*/
/*-- Author : N.Cremel*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/ksig.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/khash.h"
#include "kuip/kalias.h"
#include "kuip/klink.h"
#include "kuip/kmpanel.h"
#include "kuip/mkterm.h"

static char  *mline = NULL;

static struct {
  int         count;            /* number of lines in buffer */
  char      **lines;            /* buffer of recorded lines */
  char       *kumac;            /* file name of LAST.KUMAC */
  int         frequency;        /* recording frequency */
  int         flush;            /* count of next flush */
  time_t      start;            /* starting time */
} cmd_hist;


static void  panel_setopt (int row, int col, char *option);
static void  panel_fill( int, int, const char* );
static void  panel_filp( int, int, const char*, const char* );
static void  panel_free( int );
static void  panel_push( int, int );
static int   out_last_kumac( int );
static void  set_prompt( const char* );
static char* subst_event( const char* );
static int   what_loop( void(*)() );

/*
 * C callable interface to command "panel ... "
 * (called by action routine kxpanel)
 *
 * 1) km_panel_close ("panel 0 c ...")
 * 2) km_panel_display ("panel 0 d ...")
 * 3) km_panel_reset ("panel 0 r ...")
 * 4) km_panel_key ("panel x.y command [alias_label] [pixmap]"
 *    for key definitions )
 * 5) km_panel_key_opt ("panel x.y command [alias_label] [pixmap] [opt]"
 */

/* 1) Close panel (km_close_kpanel) */
void km_panel_close (title)
  char *title;               /* if NULL close last created (current) panel */
{
  if ( kc_window.palette_flag == 0 &&  kjmpaddr.close_kpanel_C != NULL )
       (*kjmpaddr.close_kpanel_C)( title );
}

/* 2) Display panel (km_display_kpanel) */
void km_panel_display (title, geometry)
  char *title, *geometry;    /* if NULL use default values */
{
  /* Protect empty panels */
  if( kc_window.panel_rows == 0 ||
      kc_window.panel_cols == NULL ||
      kc_window.panel_keys == NULL ) {
      printf ("* Error: panel is emty\n");
      return; }

  if (title != NULL ) kc_window.panel_title = strdup (title);
  if (kc_window.palette_flag < 0) return;
  if (!kc_window.palette_flag) {
       if( kjmpaddr.disp_kpanel_C != NULL )
           (*kjmpaddr.disp_kpanel_C)( title, geometry );
  } else {
       if( kjmpaddr.disp_kmpanel_C != NULL )
           (*kjmpaddr.disp_kmpanel_C)( NULL, title, geometry );
  }
}

/* 3) km_panel_reset ("panel 0 r ...") */
void km_panel_reset( const char* name ) /* can be NULL */
{
  int i;

  for( i = 0; i < kc_window.panel_rows; i++ ) {
        panel_free( i + 1 );
  }

  if( kc_window.panel_rows != 0 ) {
      kc_window.panel_rows = 0;
      free( (char*)kc_window.panel_cols );
      kc_window.panel_cols = NULL;
      free( (char*)kc_window.panel_keys );
      kc_window.panel_keys = NULL;
      free( (char*)kc_window.panel_icons );
      kc_window.panel_icons = NULL;
      free( (char*)kc_window.panel_opts );
      kc_window.panel_opts = NULL;
  }

  if( kc_window.panel_title != NULL ) {
      free( kc_window.panel_title);
      kc_window.panel_title = NULL;
  }
  if( kc_window.panel_name != NULL ) {
      free( kc_window.panel_name);
      kc_window.panel_name = NULL;
  }
  if( name != NULL )
    kc_window.panel_name = strdup (name);
}

/* 4) km_panel_key ("panel ..." for key definitions ) */
void km_panel_key (row, col, command, alias_label, pixmap)
  int row, col;
  char *command, *alias_label, *pixmap;
{
  km_panel_key_opt (row, col, command, alias_label, pixmap, NULL);
}

/* 5) km_panel_key_opt ("panel ..." for key definitions ) */
void km_panel_key_opt (row, col, command, alias_label, pixmap, opt)
  int row, col;
  char *command, *alias_label, *pixmap, *opt;
{
  if (alias_label == NULL && pixmap == NULL) {
      /* panel x.y command */
      panel_fill( row, col, command );

       if (opt != NULL) { /* options defined */
           panel_setopt ( row, col, opt);
       }
  } else {
      /* panel x.y command [alias_label] [pixmap] [opt] */
      char *key;
      char *al_command = NULL;

      key = command;

      if( alias_label != NULL && strcmp( alias_label,  "." ) != 0 ) {
          /* alias for button label is defined */

          key = alias_label;

          if ( command != NULL && strcmp( command, "." ) != 0 ) {
               char *cm = strdup(command);
               char *al = strdup(alias_label);

               cm = quote_string( cm, 1 );
               /*  trim leading and trailing blanks fo valid alias name */
               if (kc_window.panel_name != NULL)
                    al = mstr2cat (al, "_", kc_window.panel_name);
               al = quote_string( al, 1 );
               al = strblk0 (al);
               al_command = malloc (strlen(al) + strlen(cm) + 30);
               sprintf (al_command, "/KUIP/ALIAS/CREATE %s %s 'C'", al, cm);
          }
       }

       if (pixmap != NULL) { /* button pixmap is defined */
           /* panel x.y command [alias_label] pixmap */
           panel_filp( row, col, key, pixmap);
       } else { /* button pixmap is not defined */
           /* panel x.y command [alias_label] */
           panel_fill( row, col, key);
       }

       if (opt != NULL) { /* options defined */
           panel_setopt ( row, col, opt);
       }

       if (al_command != NULL) {
           ku_exel( al_command );
           free ( al_command );
       }
  } /* if (alias_label == NULL && pixmap == NULL) */
}


/*
 * Display panel managed by kxterm
 */
static
void panel_Dkxterm( title, geometry )
  char *title, *geometry;
{
  char message[512];
  int i, j;

  if( !kc_flags.use_kxterm ) return;

  sprintf (message, "setpanelrows(%d, -1)", kc_window.panel_rows);
  send_single_kxterm_cmd(message);
  for( i = 0; i < kc_window.panel_rows; i++ ) {
       sprintf (message, "setpanelrows(%d, %d)", i, kc_window.panel_cols[i]);
       send_single_kxterm_cmd(message);
  }

  for( i = 0; i < kc_window.panel_rows; i++ ) {
       for( j = 0; j < kc_window.panel_cols[i]; j++ ) {
            if (strlen(kc_window.panel_keys[i][j]) == 0)
                sprintf (message, "setpanelkeys(%d,%d, )", i, j);
            else
                sprintf (message, "setpanelkeys(%d,%d,%s)",
                                  i, j, kc_window.panel_keys[i][j]);
            send_single_kxterm_cmd(message);
       }
  }

  sprintf (message, "display_panel(%s,%s)", title, geometry);
  send_single_kxterm_cmd(message);
}


/*
 * action routine for /KUIP/SET_SHOW/PANEL
 */
int kxpanel()
{
  double number = ku_getr();
  double absval = (number < 0) ? -number : number;
  int row = absval;
  int col = (absval - row) * 100 + 0.5;


  if( row == 0 ) {
    /* panel 0 ... */

    col = ku_npar();
    if (col == 1 || col == 2) {
      /* reset panel */
      if (col == 2) {
          char *cval1 = ku_gets();

          if (cval1[0] == 'C' || cval1[0] == 'c') {
              /* Close last created panel (km_close_kpanel) */
              km_panel_close ( NULL );
          }

          if (cval1[0] == 'D' || cval1[0] == 'd') {
              km_panel_display ( NULL, NULL );
              return 0;
          }

          if (cval1[0] != 'R' && cval1[0] != 'r')
              return 1;
      }
      km_panel_reset (NULL);
    } else { /* col > 2 */
      char *cval1 = ku_gets();

      if (cval1[0] == 'D' || cval1[0] == 'd') {
        /* Display panel (km_display_kpanel) UNLESS mode "palette" selected */
        char *title = ku_gets();
        char *geometry = ku_gets();

        km_panel_display (title, geometry);
      } else if (cval1[0] == 'C' || cval1[0] == 'c') {
        /* Close panel (km_close_kpanel) UNLESS mode "palette" selected */
        char *title = ku_gets();

        km_panel_close ( title );
      } else if (cval1[0] == 'T' || cval1[0] == 't') {
        /* Display panel managed by kxterm UNLESS mode "palette" selected */
        char *title = ku_gets();
        char *geometry = ku_gets();

        kc_window.panel_title = strdup (title);
        if (kc_window.palette_flag < 0)
            return 0;
        if (!kc_window.palette_flag)
            panel_Dkxterm (title, geometry);

      } else if (cval1[0] == 'R' || cval1[0] == 'r') {
            char *name = ku_gets();
            km_panel_reset (name);
      }
    }
  }

  else if( number < 0 ) {
    /* initialize a row with empty columns */
    panel_free( row );
    panel_push( row, col );
  }

  else if( col == 0 ) {
    /* initialize a row with variable number of columns */
    int i;
    col = ku_npar() - 1;
    panel_free( row );
    panel_push( row, col );
    for( i = 0; i < col; i++ ) {
      panel_fill( row, i + 1, ku_gets() );
    }
  }

  else {
    int np = ku_npar() - 1;

    if (np == 1) {                   /* -> panel x.y command */
                                     /*                (1)   */
       /* define key only (old panel style ) */
       km_panel_key ( row, col, ku_gets(), NULL, NULL);

    } else { /* np = 2, 3 or 4:
              * -> panel x.y command [label] [pixmap] [option]
              *                (1)     (2)      (3)     (4)
              */

       /* define key with alias and/or pixmap */
       char *cmd = ku_gets();
       char *alias = ku_gets();

       if( strcmp( alias,  "." ) == 0 ) alias = NULL;

       if (np == 2) {
           km_panel_key ( row, col, cmd, alias, NULL);
       } else if (np >= 3) {
/* N.B. Get all args before calling km_panel_key (in case of ALIAS/CREATE */
           char *pixm = ku_gets();

           if( strcmp( pixm,  "." ) == 0 ) pixm = NULL;
           if (np == 4) {
              /* Options defined */
              char *opt = ku_gets();

              km_panel_key_opt ( row, col, cmd, alias, pixm, opt);
           } else {
              km_panel_key ( row, col, cmd, alias, pixm);
           }
       }
    }  /* ... end np = 2, 3 or 4 */
  }

  return 0;
}

/*
 * action routine for /KUIP/SET_SHOW/NEWPANEL
 */
int kxnewpan()
{
  int nl = ku_geti();
  int nc = ku_geti();
  char *title = ku_gets();
  int width = ku_geti();
  int height = ku_geti();
  int xpos = ku_geti();
  int ypos = ku_geti();
  char geometry[64];
  int i;

  /* Reset panel */
  for( i = 0; i < kc_window.panel_rows; i++ ) {
       panel_free( i + 1 );
  }
  if( kc_window.panel_rows != 0 ) {
      kc_window.panel_rows = 0;
      free( (char*)kc_window.panel_cols );
      kc_window.panel_cols = NULL;
      free( (char*)kc_window.panel_keys );
      kc_window.panel_keys = NULL;
      free( (char*)kc_window.panel_icons );
      kc_window.panel_icons = NULL;
      free( (char*)kc_window.panel_opts );
      kc_window.panel_opts = NULL;
  }

  /* Fill with blanks */
  panel_fill( nl, nc, " ");


  /* Display Panel (km_display_kpanel) */
  if (width == 0 || height == 0) {
      printf( " *** Invalid value for width (%d) or height (%d) \n",
              width, height );
      return 1;
  }
  sprintf (geometry, "%dx%d+%d+%d", width, height, xpos, ypos);
  if( kjmpaddr.disp_kpanel_C != NULL )
      (*kjmpaddr.disp_kpanel_C)( title, geometry );

  return 0;
}

/*
 * action routine for /KUIP/SET_SHOW/KXTNEWPANEL
 */
int kxtnewpan()
{
  int nl = ku_geti();
  int nc = ku_geti();
  char *title = ku_gets();
  int width = ku_geti();
  int height = ku_geti();
  int xpos = ku_geti();
  int ypos = ku_geti();
  char message[512];
  int i;

  /* Reset panel */
  for( i = 0; i < kc_window.panel_rows; i++ ) {
       panel_free( i + 1 );
  }
  if( kc_window.panel_rows != 0 ) {
      kc_window.panel_rows = 0;
      free( (char*)kc_window.panel_cols );
      kc_window.panel_cols = NULL;
      free( (char*)kc_window.panel_keys );
      kc_window.panel_keys = NULL;
      free( (char*)kc_window.panel_icons );
      kc_window.panel_icons = NULL;
      free( (char*)kc_window.panel_opts );
      kc_window.panel_opts = NULL;
  }

  /* Fill with blanks */
  panel_fill( nl, nc, " ");


  /* Display Panel (km_display_kpanel) */
  if (width == 0 || height == 0) {
      printf( " *** Invalid value for width (%d) or height (%d) \n",
              width, height );
      return 1;
  }
  sprintf (message, "setnewpanel(%s,%d,%d,%d,%d,%d,%d)",
           title, nl, nc, width, height, xpos, ypos);
  send_single_kxterm_cmd(message);

  return 0;
}

/*
 * undefine a row
 */
static
void panel_free( int row )
{
  if( row <= kc_window.panel_rows ) {
    int i;
    for( i = 0; i < kc_window.panel_cols[row-1]; i++ ) {
      if( kc_window.panel_keys[row-1][i] != NULL ) {
        free( kc_window.panel_keys[row-1][i] );
        kc_window.panel_keys[row-1][i] = NULL;
      }
      if( kc_window.panel_icons[row-1][i] != NULL ) {
        free( kc_window.panel_icons[row-1][i] );
        kc_window.panel_icons[row-1][i] = NULL;
      }
      if( kc_window.panel_opts[row-1][i] != NULL ) {
        free( kc_window.panel_opts[row-1][i] );
        kc_window.panel_opts[row-1][i] = NULL;
      }
    }
    if( kc_window.panel_keys[row-1] != NULL ) {
      free( (char*)kc_window.panel_keys[row-1] );
      kc_window.panel_keys[row-1] = NULL;
    }
    if( kc_window.panel_icons[row-1] != NULL ) {
      free( (char*)kc_window.panel_icons[row-1] );
      kc_window.panel_icons[row-1] = NULL;
    }
    if( kc_window.panel_opts[row-1] != NULL ) {
      free( (char*)kc_window.panel_opts[row-1] );
      kc_window.panel_opts[row-1] = NULL;
    }
    kc_window.panel_cols[row-1] = 0;
  }
}


/*
 * increase panel to at least "row" rows and "col" columns in row "row"
 */
static
void panel_push( int row,
                int col )
{
  int i;

  if( row > kc_window.panel_rows ) {
    if( kc_window.panel_rows == 0 ) {
      kc_window.panel_cols = (int*)malloc( row * sizeof(int) );
      kc_window.panel_keys = (char***)malloc( row * sizeof(char**) );
      kc_window.panel_icons = (char***)malloc( row * sizeof(char**) );
      kc_window.panel_opts = (char***)malloc( row * sizeof(char**) );
    }
    else {
      kc_window.panel_cols = (int*)realloc( (char*)kc_window.panel_cols,
                                           row * sizeof(int) );
      kc_window.panel_keys = (char***)realloc( (char*)kc_window.panel_keys,
                                              row * sizeof(char**) );
      kc_window.panel_icons = (char***)realloc( (char*)kc_window.panel_icons,
                                              row * sizeof(char**) );
      kc_window.panel_opts = (char***)realloc( (char*)kc_window.panel_opts,
                                              row * sizeof(char**) );
    }
    for( i = kc_window.panel_rows; i < row; i++ ) {
      kc_window.panel_cols[i] = 1;
      kc_window.panel_keys[i] = (char**)malloc( sizeof(char*) );
      kc_window.panel_keys[i][0] = NULL;
      kc_window.panel_icons[i] = (char**)malloc( sizeof(char*) );
      kc_window.panel_icons[i][0] = NULL;
      kc_window.panel_opts[i] = (char**)malloc( sizeof(char*) );
      kc_window.panel_opts[i][0] = NULL;
    }
    kc_window.panel_rows = row;
  }

  if( col > kc_window.panel_cols[row-1] ) {
    if( kc_window.panel_keys[row-1] == NULL ) {
      kc_window.panel_keys[row-1] =
        (char**)malloc( col * sizeof(char**) );
    }
    else {
      kc_window.panel_keys[row-1] =
        (char**)realloc( (char*)kc_window.panel_keys[row-1],
                        col * sizeof(char**) );
    }
    if( kc_window.panel_icons[row-1] == NULL ) {
      kc_window.panel_icons[row-1] =
        (char**)malloc( col * sizeof(char**) );
    }
    else {
      kc_window.panel_icons[row-1] =
        (char**)realloc( (char*)kc_window.panel_icons[row-1],
                        col * sizeof(char**) );
    }
    if( kc_window.panel_opts[row-1] == NULL ) {
      kc_window.panel_opts[row-1] =
        (char**)malloc( col * sizeof(char**) );
    }
    else {
      kc_window.panel_opts[row-1] =
        (char**)realloc( (char*)kc_window.panel_opts[row-1],
                        col * sizeof(char**) );
    }
    for( i = kc_window.panel_cols[row-1]; i < col; i++ ) {
      kc_window.panel_keys[row-1][i] = NULL;
      kc_window.panel_icons[row-1][i] = NULL;
      kc_window.panel_opts[row-1][i] = NULL;
    }
    kc_window.panel_cols[row-1] = col;
  }
}

/*
 * Set panel buttons options:
 *     T  (Toggle) -> button is not released after command execution
 */
static void panel_setopt (int row, int col, char *option)
{
  if( kc_window.panel_opts[row-1][col-1] != NULL ) {
    free( kc_window.panel_opts[row-1][col-1] );
    kc_window.panel_opts[row-1][col-1] = NULL;
  }
  if (option == NULL || option[0] == '\0')
      kc_window.panel_opts[row-1][col-1] = strdup( " " );
  else
      kc_window.panel_opts[row-1][col-1] = strdup( option );
}

/*
 * assign key to row and column
 */
static
void panel_fill( int row,
                int col,
                const char *key )
{
  panel_push( row, col );
  if( kc_window.panel_keys[row-1][col-1] != NULL ) {
    free( kc_window.panel_keys[row-1][col-1] );
    kc_window.panel_keys[row-1][col-1] = NULL;
  }
  if (key == NULL || key[0] == '\0')
      kc_window.panel_keys[row-1][col-1] = strdup( " " );
  else
      kc_window.panel_keys[row-1][col-1] = strdup( key );
}

/*
 * assign key and pixmap to row and column
 */
static
void panel_filp( int row,
                int col,
                const char *key,
                const char *pixm )
{
  panel_push( row, col );
  if( kc_window.panel_keys[row-1][col-1] != NULL ) {
    free( kc_window.panel_keys[row-1][col-1] );
    kc_window.panel_keys[row-1][col-1] = NULL;
  }
  if( kc_window.panel_icons[row-1][col-1] != NULL ) {
    free( kc_window.panel_icons[row-1][col-1] );
    kc_window.panel_icons[row-1][col-1] = NULL;
  }
  if (key == NULL || key[0] == '\0')
      kc_window.panel_keys[row-1][col-1] = strdup( " " );
  else
      kc_window.panel_keys[row-1][col-1] = strdup( key );
  kc_window.panel_icons[row-1][col-1] = strdup( pixm );
}


/*
 * command loop
 */
void F77_ENTRY(Kuwhat)
/* { */
  ku_what( NULL );
}


void ku_what( styleG )
     void (*styleG)();
{
  ku_trap( 1, -1 );             /* install signal handler */
  if( styleG == NULL )
    ku_exel( "/KUIP/SET_SHOW/VISIBILITY '/KUIP/SET_SHOW/PANEL' 'OFF'" );

  while( 1 ) {
    if( what_loop( styleG ) == 1 )
      break;
    else {
      /* this branch is for VMS exceptions only */
      reset_break();
    }
  }
  kc_break.jump_set = 0;
  ku_trap( 0, -1 );             /* remove signal handler */
  ku_shut();                    /* clean up the mess */
}


#ifdef WIN32
void fpcheck( void )
{
/*  Check the type of the floating-point exception under Win32 */
/*  VF. 31.01.97  */
   char fpstr[30];
   switch( kc_break.fperr )
   {
   case _FPE_INVALID:
       strcpy( fpstr, "Invalid number" );
       break;
   case _FPE_OVERFLOW:
       strcpy( fpstr, "Overflow" );

       break;
   case _FPE_UNDERFLOW:
       strcpy( fpstr, "Underflow" );
       break;
   case _FPE_ZERODIVIDE:
       strcpy( fpstr, "Divide by zero" );
       break;
   default:
       strcpy( fpstr, "Other floating point error" );
       break;
   }
   printf( "Floating point exception. Error %d: %s\n", kc_break.fperr, fpstr );
}
#endif

static
int what_loop( void (*styleG)() )
{
  /* Do this in Kuinit instead (otherwise it is not possible to have
   * "exit" or "quit" in pawlogon.kumac
   */
  /*
  kc_flags.do_exit = 0;
  kc_flags.do_quit = 0;
  */

#ifdef vms

  /*
   * The signal handler calls LIB$SIG_TO_RET which lets what_loop return
   * the error number ( 1 == SS$_NORMAL )
   */
  VAXC$ESTABLISH( signal_handler );

#else
  { int sigret;  /* It is a local variable to hold sigsetjmp result.          VF.31.01.97          */
                 /* sigret = -1 means the folating-point exception under 
                                Windows has been happened */
                 /* sigret = -2 means the CTRL-C exception under Windows has been happened  VF. 19.11.97 */
  /* catch exceptions saving signal mask */
   sigret = sigsetjmp( kc_break.stack, 1 );
   if( sigret != 0 ) { /* saving signal mask */
    /* longjmp happened */
#ifdef WIN32
/* special case for floating-point exception under Windows */
      if (sigret == -1) 
                      fpcheck();
      else if (sigret == -2)
          signal_handler(SIGINT);
#endif
    reset_break();
  }
  }

#endif

  kc_break.jump_set = 1;

  while( !kc_flags.do_exit && !kc_flags.do_quit ) {
    char *cmd;

    kc_break.soft_intr = 0;
    ku_intr( 1 );

    if( kc_flags.in_application ) {
      cmd = input_line( format_prompt( kc_flags.curr_prompt ), 0 );
    }
    else if( (kc_flags.style & KmSTYLE_A) != 0 ) {
      menu_style();
      continue;
    }
    else if( (kc_flags.style & (KmSTYLE_G | KmSTYLE_M)) != 0 ) {
      (*styleG)();
      continue;
    }
    else {
      cmd = input_line( format_prompt( kc_flags.curr_prompt ), '_' );
      if( cmd != NULL ) {
        char *p = cmd;
        while( *p == ' ' )
          p++;
        if( *p == '!' ) {
          /* command history logic */
          cmd = subst_event( p );
          if( cmd == NULL )
            continue;
        }
      }
      if( cmd != NULL && kjmpaddr.user_input_F != NULL ) {
        /* allow user routine set by KUSER to change command line */

        fstrset( Kcparc.PARLST, (sizeof Kcparc.PARLST), cmd );
        IQUEST(1) = 0;
        (*kjmpaddr.user_input_F)();

        if( IQUEST(1) != 0 ) {
          cmd = "";
        }
        else {
          int len = fstrlen( Kcparc.PARLST, (sizeof Kcparc.PARLST) );
          cmd = Kcparc.PARLST;
          cmd[len] = '\0';
        }
      }
    }

    if( cmd != NULL ) {
      /* copy cmd in case input_line() is called again */
      cmd = strdup( cmd );
      ku_last( cmd );
      exec_cmd_string( cmd, 1, NULL );
      free( cmd );
    }
    else {
      ku_exel( "/KUIP/EXIT" );  /* EOF on input */
      break;
    }
    kc_break.intr_count = 0;    /* reset the ^C counter */
  }

  /* Reset global flags do_exit and do_quit to "false" in case kuwhat
   * is called again afterwards (cf. mail from Mark Lakata who is using
   * KUIP in his event display program) 
   */
  kc_flags.do_exit = 0;
  kc_flags.do_quit = 0;

  return 1;
}


/*
 * set user routine called for locator operations in Motif mode
 */
void F77_ENTRY_E(Kumloc,fun)
/* { */
  kjmpaddr.user_locate_F = fun;
}


/*
 * set user routine called for every intercepted signal
 */
void F77_ENTRY_E(Kubrek,fun)
/* { */
  kjmpaddr.user_break_F = fun;
}


/*
 * set user routine called for EXIT
 */
void F77_ENTRY_E(Kuexit,fun)
/* { */
  kjmpaddr.user_exit_F = fun;
}


/*
 * set user routine called for QUIT
 */
void F77_ENTRY_E(Kuquit,fun)
/* { */
  kjmpaddr.user_quit_F = fun;
}


/*
 * set user routine to call SIGMA
 */
void F77_ENTRY_E(Kusigm,fun)
/* { */
  kjmpaddr.user_sigma_F = fun;
}

void F77_ENTRY_I(Kisigm,iflag )
/* { */
  if( *iflag == 0 ) {
    /* return flag whether address is defined */
    *iflag = (kjmpaddr.user_sigma_F != NULL);
  }
  else if( kjmpaddr.user_sigma_F != NULL ) {
    /* call SIGMA */
    (*kjmpaddr.user_sigma_F)();
  }
}


/*
 * set user routine to flush graphics output
 */
void F77_ENTRY_E(Kugrfl,fun)
/* { */
  kjmpaddr.user_grfl_F = fun;
}


/*
 * set user routine to switch terminal to alpha mode
 */
void F77_ENTRY_E(Kuterm,fun)
/* { */
  kjmpaddr.user_term_F = fun;
}


/*
 * set user routine for command input
 */
void F77_ENTRY_E(Kuser,fun)
/* { */
  kjmpaddr.user_input_F = fun;
}


/*
 * switch terminal to alpha mode
 */
void F77_ENTRY(Kualfa)
/* { */
  ku_alfa();
}

void ku_alfa()
{
  if( kjmpaddr.user_term_F != NULL )
    (*kjmpaddr.user_term_F)();
}


/*
 * action routine for /KUIP/EXIT and /KUIP/QUIT
 */
int kxexit()
{
  char *cmd_name = ku_path();

  out_last_kumac( cmd_hist.count );

  if( strcmp( cmd_name, "/KUIP/EXIT" ) == 0 ) {

    if( kjmpaddr.disp_exit_C != NULL )
      kc_flags.do_exit = (*kjmpaddr.disp_exit_C)(); /* ask for confirmation */
    else
      kc_flags.do_exit = 1;

    if( kc_flags.do_exit ) {
      if( kjmpaddr.user_exit_F != NULL )
        (*kjmpaddr.user_exit_F)();
      if( kc_flags.use_kxterm )
        send_single_kxterm_cmd("exit");
    }
  }

  if( strcmp( cmd_name, "/KUIP/QUIT" ) == 0 ) {

    if( kjmpaddr.disp_quit_C != NULL )
      kc_flags.do_quit = (*kjmpaddr.disp_quit_C)(); /* ask for confirmation */
    else
      kc_flags.do_quit = 1;

    if( kc_flags.do_quit ) {
      if( kjmpaddr.user_quit_F != NULL )
        (*kjmpaddr.user_quit_F)();
      if( kc_flags.use_kxterm )
        send_single_kxterm_cmd("quit");
    }
  }

  if( kc_flags.in_motif && (kc_flags.do_exit || kc_flags.do_quit) ) {
    /* break the Motif event loop */
    siglongjmp( kc_break.stack, 1 );
  }

  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/PROMPT
 */
int kxprompt()
{
  char *prompt = ku_gete();

  if( prompt[0] != '\0' )
    set_prompt( prompt );
  return 0;
}


void F77_ENTRY_C(Kiprmt,cprmt)
/* { */
  char *prompt = fstr0dup( cprmt, len_cprmt );

  if( prompt != NULL ) {
    set_prompt( prompt );
    free( prompt );
  }
}


static
void set_prompt( const char *prompt )
{
  if( prompt != NULL ) {
    if( kc_value.set_prompt != NULL )
      free( kc_value.set_prompt );
    kc_value.set_prompt = strdup( prompt );

    if( kc_flags.curr_prompt != NULL )
      free( kc_flags.curr_prompt );
    kc_flags.curr_prompt = strdup( prompt );


#ifdef DBMALLOC
    malloc_mark( kc_value.set_prompt );
    malloc_mark( kc_flags.curr_prompt );
#endif

    if( kc_flags.use_kxterm ) {
      char *cmd = str3dup( "prompt(",
                          format_prompt( kc_flags.curr_prompt ), ")" );
      send_single_kxterm_cmd( cmd );
      free( cmd );
    }
  }
}


/*
 * return prompt string; substitute [] by event number
 */
char *format_prompt( const char *prompt )
{
  static char *curr_prompt = NULL;
  char *p;

  if( curr_prompt != NULL )
    free( curr_prompt );

  if( prompt == NULL)
    prompt = "Kuip >";

  curr_prompt = strdup( prompt );
  if( (p = strstr( curr_prompt, "[]" )) != NULL ) {
    int lhead = p - curr_prompt + 1;
    p[1] = '\0';                /* chop off "]..." */
    curr_prompt = mstricat( curr_prompt, cmd_hist.count + 1 );
    curr_prompt = mstrcat( curr_prompt, prompt + lhead );
  }

  return curr_prompt;
}


static
char *subst_event( const char *cmd_string )
{
  int n;

  if( cmd_string[1] == '\0' ) {
    /* show command history */
    int n0 = cmd_hist.count - cmd_hist.frequency;
    if( n0 < 0 )
      n0 = 0;
    for( n = n0; n < cmd_hist.count; n++ ) {
      printf( "!%d  %s\n", n + 1, cmd_hist.lines[n] );
    }
    return NULL;
  }

  if( cmd_string[1] == '!' ) {
    /* reexecute last command */
    n = cmd_hist.count;
  }

  else if( (n = atoi( &cmd_string[1] )) != 0 ) {
    /* reexecute numbered command */
    if( n < 0 )         /* relative from last command */
      n += cmd_hist.count + 1;
  }

  else {
    int len = strlen( cmd_string ) - 1;
    for( n = cmd_hist.count; n > 0; n-- ) {
      if( strncasecmp( cmd_hist.lines[n-1], &cmd_string[1], len ) == 0 )
        break;
    }
  }

  if( n <= 0 || n > cmd_hist.count ) {
    printf( " *** Command %s... not in history buffer\n", cmd_string );
    return NULL;
  }
  return cmd_hist.lines[n-1];
}


/*
 * action routine for /KUIP/SET_SHOW/STYLE
 */
int kxstyle()
{
  char *style = ku_getc();
  KmStyleFlag new_style;
  int i;

  ku_alfa();

  if( strcmp( style, "?" ) == 0 ) {
    printf( " Current input style is '%s'.\n", style_name( kc_flags.style ) );
    return 0;
  }

#ifndef REMOVE_OLD_STUFF
  if( 1 ) {
    static char filecase_on[] = "-FILECASE ON";
    static char filecase_off[] = "-FILECASE OFF";
    static char filecase_mixed[] = "/KUIP/SET_SHOW/FILECASE 'KEEP'";
    static char filecase_lower[] = "/KUIP/SET_SHOW/FILECASE 'CONVERT'";

    if( strcmp( style, filecase_on ) == 0 ) {
      printf( " %s '%s' is a deprecated feature.", ku_path(), filecase_on );
      printf( " Please use %s instead.\n", filecase_mixed );
      return ku_exel( filecase_mixed );
    }
    if( strcmp( style, filecase_off ) == 0 ) {
      printf( " %s '%s' is a deprecated feature.", ku_path(), filecase_off );
      printf( " Please use %s instead.\n", filecase_lower );
      return ku_exel( filecase_lower );
    }
  }
#endif

  /* set the major style flag */
  switch( style[0] ) {

  case 'A':
    new_style = KmSTYLE_A;
    break;

  case 'C':
    new_style = KmSTYLE_C;
    break;

  case 'G':
    new_style = KmSTYLE_G;
    break;

  case 'M':
    new_style = KmSTYLE_M;
    break;

  case 'X':
    new_style = KmSTYLE_XM;
    break;

  default:
    printf( " *** Invalid style option '%s'\n", style );
    return -1;
  }

  /* set the minor style flag */
  for( i = 1; style[i] != '\0'; i++ ) {

    switch( style[i] ) {

    case 'L':
      new_style |= KmSTYLE_xL;
      break;

    case 'P':
      new_style |= KmSTYLE_xP;
      break;

    case 'S':
      new_style |= KmSTYLE_xS;
      break;

    case 'W':
      new_style |= KmSTYLE_xW;
      break;
    }
  }

  /* consistency checks */
  if( (new_style & KmSTYLE_XM) == 0 && kc_flags.in_motif ) {
    printf( " *** Cannot change style in X/Motif\n - KUWHAT not called\n" );
    return -1;
  }
  if( (new_style & KmSTYLE_XM) != 0 && !kc_flags.in_motif ) {
    printf( " *** Cannot change style to X/Motif - KUWHAM not called\n" );
    return -1;
  }

  kc_flags.style = new_style;
  kc_value.set_style = style_name( new_style );

  /* start/stop Model Human Interface */
  if( (new_style & KmSTYLE_M) != 0 && (kc_flags.style & KmSTYLE_M) == 0 ) {
    Mhi_open();
  }
  if( (new_style & KmSTYLE_M) == 0 && (kc_flags.style & KmSTYLE_M) != 0 ) {
    Mhi_close();
  }

  if( (new_style & (KmSTYLE_G | KmSTYLE_M)) != 0 ) {
    int npar = ku_npar();
    int wk_type = 0;

    if( npar >= 2 || kc_window.sgylen == 0 )
      kc_window.sgylen = ku_getr();

    if( npar >= 3 || kc_window.sgsize == 0 )
      kc_window.sgsize = ku_getr();

    if( npar >= 4 || kc_window.sgyspa == 0 )
      kc_window.sgyspa = ku_getr();

    if( npar >= 5 || kc_window.sgbord == 0 )
      kc_window.sgbord = ku_getr();

    if( npar >= 6 )
      wk_type = ku_geti();

    if( (new_style & KmSTYLE_G) != 0 && kjmpaddr.higz_init_C != NULL )
      (*kjmpaddr.higz_init_C)( wk_type );
  }

  return 0;
}


/*
 * translate style flag into style name
 */
const char *style_name( KmStyleFlag flag )
{
  static char style[8];

  switch( flag & KmSTYLE_major ) {

  case KmSTYLE_A:
    if( flag & KmSTYLE_xL )
      strcpy( style, "AL" );
    else
      strcpy( style, "AN" );
    break;

  case KmSTYLE_C:
    strcpy( style, "C" );
    break;

  case KmSTYLE_G:
    strcpy( style, "G" );
    break;

  case KmSTYLE_M:
    strcpy( style, "M" );
    break;

  case KmSTYLE_XM:
    strcpy( style, "XM" );
    break;
  }

  if( flag & (KmSTYLE_G | KmSTYLE_M) ) {

    if( flag & KmSTYLE_xP )
      strcat( style, "P" );

    if( flag & KmSTYLE_xS )
      strcat( style, "S" );

    if( flag & KmSTYLE_xW )
      strcat( style, "W" );
  }

  return style;
}


/*
 * action routine for /KUIP/SET_SHOW/DOLLAR
 */
int kxdollar()
{
  char *which = ku_getc();

  ku_alfa();

  if( strcmp( which, "ON" ) == 0 ) {
    kc_flags.try_getenv = 1;
  }
  else if( strcmp( which, "OFF" ) == 0 ) {
    kc_flags.try_getenv = 0;
  }
  else if( kc_flags.try_getenv ) {
    printf( " $var is substituted if defined as environment variable.\n" );
  }
  else {
    printf( " $var is not tried as being an environment variable.\n" );
  }

  kc_value.set_dollar = kc_flags.try_getenv ? "ON" : "OFF";

  return 0;
}


/*
 * action routine for /KUIP/SET_SHOW/FILECASE
 */
int kxfilecase()
{
  static int old_fcase = 0;
  char *fcase = ku_getc();

  ku_alfa();

  if( strcmp( fcase, "KEEP" ) == 0 ) {
    old_fcase = kc_flags.keep_fcase;
    kc_flags.keep_fcase = 1;
  }
  else if( strcmp( fcase, "CONVERT" ) == 0 ) {
    old_fcase = kc_flags.keep_fcase;
    kc_flags.keep_fcase = 0;
  }
  else if( strcmp( fcase, "RESTORE" ) == 0 ) {
    kc_flags.keep_fcase = old_fcase;
  }
  else if( kc_flags.keep_fcase ) {
    printf( " Filenames are NOT case converted.\n" );
  }
  else {
    printf( " Filenames ARE case converted.\n" );
  }

  kc_value.set_filecase = kc_flags.keep_fcase ? "KEEP" : "CONVERT";

  return 0;
}


/*
 * return file case conversion
 */
LOGICAL F77_ENTRY(Kuqcas)
/* { */
  return ku_true( kc_flags.keep_fcase );
}
/*   the same for C   */
int ku_qcas()
{
  return ku_true( kc_flags.keep_fcase );
}


/*
 * case folding for file names
 */
void F77_ENTRY_C(Kufcas,chfile)
/* { */
  char *file = fstrdup( chfile, len_chfile );
  fstrset( chfile, len_chfile, ku_fcase( file ) );
  free( file );
}


char *ku_fcase( char *file )
{
#if defined(UNIX) && !defined(MSDOS)
  if( !kc_flags.keep_fcase )
    strlower( file );
#else
  if( !kc_flags.keep_fcase )
    strupper( file );
#endif
  return file;
}


/*
 * action routine for /KUIP/SET_SHOW/RECALL_STYLE
 */
int kxrecallstyle()
{
  static char *styles[] = { "KSH", "KSHO", "DCL", "DCLO" };
  static int gl_mask = 0;
  char *style = ku_getc();

  ku_alfa();

  if( strcmp( style, "NONE" ) == 0 ) {
    kc_value.set_recall_style = "NONE";
    kc_window.use_getline = 0;
  }
  else if( strcmp( style, styles[0] ) == 0 ) {
    kc_value.set_recall_style = styles[0];
    kc_window.use_getline = 1;
    gl_mask = 0;
  }
  else if( strcmp( style, styles[1] ) == 0 ) {
    kc_value.set_recall_style = styles[1];
    kc_window.use_getline = 1;
    gl_mask = 1;
  }
  else if( strcmp( style, styles[2] ) == 0 ) {
    kc_value.set_recall_style = styles[2];
    kc_window.use_getline = 1;
    gl_mask = 2;
  }
  else if( strcmp( style, styles[3] ) == 0 ) {
    kc_value.set_recall_style = styles[3];
    kc_window.use_getline = 1;
    gl_mask = 3;
  }
  else {
    if( kc_window.use_getline )
      style = styles[gl_mask];
    else
      style = "NONE";
    printf( " Current recall style is %s\n", style );
    return 0;
  }

#ifndef IBM370
  if( kc_window.use_getline )
    kc_window.use_getline = (kc_window.is_a_tty && !kc_window.is_a_pad);
  if( kc_window.use_getline ) {
    gl_config( "style", (gl_mask >> 1) & 1 );
    gl_config( "overwrite", gl_mask & 1 );
  }
#endif
  return 0;
}


/*
 * cleanup after break
 */
void reset_break()
{
#ifndef IBM370
  gl_config( "erase", 0 );
  gl_config( "noecho", 0 );
#endif

  ku_alfa();

  if( kc_flags.in_application ) {
    kc_flags.in_application = 0;
    if( kc_flags.curr_prompt != NULL )
      free( kc_flags.curr_prompt );
    kc_flags.curr_prompt = strdup( kc_value.set_prompt );
    kc_flags.appl_called = 0;
    printf( " *** Application %s aborted\n", kc_flags.appl_cmd->name );
    fclose( kc_flags.appl_stream );
    ku_close( kc_flags.appl_luno );
  }

  if( kc_flags.in_macro ) {
    kc_flags.in_macro = 0;
    printf( " *** Macro execution stopped\n" );
  }
  kc_flags.action_nesting = 0;
  kc_alias.var_table = kc_alias.global_value;

  if( kjmpaddr.disp_busy_C != NULL )
    (*kjmpaddr.disp_busy_C)(0);

  if( kjmpaddr.user_break_F != NULL )
    (*kjmpaddr.user_break_F)();
}


# ifdef SIGNAL_POSIX

static char* idle_alarm_prompt = NULL;
static sigjmp_buf idle_alarm_stack;

static
void idle_alarm_handler( int sig )
{
  printf( "%s\n", kc_value.idle_cmd );
  ku_exel( kc_value.idle_cmd );
  printf( "%s", idle_alarm_prompt );
  fflush( stdout );
  alarm( kc_value.idle_time );
  siglongjmp( idle_alarm_stack, 1 );
}
#endif


/*
 * read line from terminal
 * strip trailing blanks
 * concatenate if line ends with cont_char (unless '\0')
 * return NULL on EOF
 */
char *input_line( const char *prompt_string,
                 char cont_char )
{
  static char line[1024];
  char *prompt;
  int lprompt = strlen( prompt_string );
  int len = 0;

  if( kjmpaddr.disp_prompt_C != NULL ) {
    /* prompt in Motif widget */
    char *answer;

    int enabled = ku_intr( 0 );
    if( kc_window.use_no_echo )
      answer = (*kjmpaddr.disp_passwd_C)( prompt_string );
    else
      answer = (*kjmpaddr.disp_prompt_C)( prompt_string, NULL );
    if( enabled )
      ku_intr( enabled );

    if (answer) {
       strcpy(line, answer);
       free(answer);
       return line;
    } else {
       return NULL;
    }
  }

  ku_alfa();

  prompt = malloc( lprompt + 3 );
  strcpy( prompt, prompt_string );
  if( prompt[lprompt-1] != ' ' ) {
    prompt[lprompt++] = ' ';
    prompt[lprompt] = '\0';
  }

  while( 1 ) {
    int lline = (sizeof line) - len;
    int eof = 0;

#ifdef IBMMVS
    INTEGER nchar;
    F77_CALL_CCx(Kdialo,prompt,lprompt,&line[len],lline-1,&nchar);
    eof = (nchar < 0);
    if( !eof )
      line[len+nchar] = '\0';
#endif

#ifdef IBMVM
    if( kc_window.use_no_echo ) {
#pragma linkage(LNRDPAS,OS)
      extern char *LNRDPAS(char*);
      char *value = LNRDPAS( prompt );
      eof = (value == NULL);
      if( !eof )
        strcpy( &line[len], value );
    }
    else {
      fputs( prompt, stdout );
      eof = (fgets( &line[len], lline, stdin ) == NULL);
    }
#endif

#ifndef IBM370
    if( kc_window.use_getline ) {
      char *buf;

      if( kc_window.use_no_echo )
        gl_config( "noecho", 1 );
#if defined(CERNLIB_QMGLIBC)
      if( (buf = getline2( prompt )) == NULL )
#else
      if( (buf = getline( prompt )) == NULL )
#endif
        eof = 1;
      else
        strcpy( &line[len], buf );
      gl_config( "noecho", 0 );
    }
    else {
# ifdef SIGNAL_POSIX
      struct sigaction act;
      struct sigaction old_act;
      int old_alarm = 0;
      sigaction( SIGALRM, NULL, &old_act );
# endif

      printf( "%s", prompt );
      fflush( stdout );

# ifdef SIGNAL_POSIX
      if( kc_value.idle_time > 0 ) {

        if( idle_alarm_prompt != NULL )
          free( idle_alarm_prompt );
        idle_alarm_prompt = strdup( prompt );

        sigemptyset( &act.sa_mask );
        act.sa_flags = 0;
        act.sa_handler = idle_alarm_handler;
        sigaction( SIGALRM, &act, NULL );
        old_alarm = alarm( kc_value.idle_time );
      }
      sigsetjmp( idle_alarm_stack, 1 );
# endif

# ifdef BROKEN_F77_IO
      eof = (ku_read( 5, &line[len], lline ) < 0 );
# else
      eof = (fgets( &line[len], lline, stdin ) == NULL);
# endif

# ifdef SIGNAL_POSIX
      sigaction( SIGALRM, &old_act, NULL );
      alarm( old_alarm );
# endif

      if( !eof && ! kc_window.is_a_tty ) {
        if( kc_window.use_no_echo )
          printf( "\n" );
        else
          printf( "%s", line );
      }
    }
#endif

    if( eof ) {
      static int second_eof = 0;
      if( second_eof ) {
        printf( "\n *** EoF ***\n" );
        exit( 1 );
      }
      second_eof = 1;
      free( prompt );
      return NULL;
    }

    for( len = strlen( line ); len > 0; len-- ) {
      if( line[len-1] != '\n' && line[len-1] != ' ' )
        break;
    }

    if( len > 0 && line[len-1] == cont_char ) {
      len--;
      if( prompt[lprompt-2] != cont_char ) {
        prompt[lprompt-1] = cont_char;
        prompt[lprompt++] = ' ';
        prompt[lprompt] = '\0';
      }
    }
    else
      break;
  }
  line[len] = '\0';

#ifndef IBM370
  if( kc_window.use_getline && cont_char != '\0' )
    gl_histadd( line );
#endif

  free( prompt );
  return line;
}


/*
 * prompt for string
 */
char *ku_pros( const char *prompt,
              const char *dfault )
{
  char *value;
  if( dfault == NULL ) {
    value = input_line( prompt, 0 );
  }
  else {
    char *prmt = str4dup( prompt, " (<CR>=", dfault, ")" );
    value = input_line( prmt, 0 );
    free( prmt );
  }
  if( value == NULL ) {
    printf( "\n *** EOF ***\n" );
    exit( 1 );
  }
  if( dfault != NULL && *value == '\0' )
    value = strdup(dfault);
  return value;
}

void F77_ENTRY_CCI(Kupros,chprmt,chline,length)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  char *dfault = fstr0dup( chline, len_chline );
  *length = fstrset( chline, len_chline, ku_pros( prompt, dfault ) );
  free( prompt );
  if( dfault != NULL )
    free( dfault );
}


/*
 * prompt for uppercase string
 */
char *ku_proc( const char *prompt,
              const char *dfault )
{
  char *value = ku_pros( prompt, dfault );
  if( value != NULL )
    strupper( value );
  return value;
}

void F77_ENTRY_CCI(Kuproc,chprmt,chline,length)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  char *dfault = fstr0dup( chline, len_chline );
  *length = fstrset( chline, len_chline, ku_proc( prompt, dfault ) );
  free( prompt );
  if( dfault != NULL )
    free( dfault );
}


/*
 * prompt for filename
 */
char *ku_prof( const char *prompt,
              const char *dfault )
{
  char *value = ku_pros( prompt, dfault );
  if( value != NULL )
    ku_fcase( value );
  return value;
}

void F77_ENTRY_CCI(Kuprof,chprmt,chline,length)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  char *dfault = fstr0dup( chline, len_chline );
  *length = fstrset( chline, len_chline, ku_prof( prompt, dfault ) );
  free( prompt );
  if( dfault != NULL )
    free( dfault );
}


/*
 * prompt for password
 */
char *ku_prop( const char *prompt )
{
  char *value;
  kc_window.use_no_echo = 1;
  value = ku_pros( prompt, NULL );
  kc_window.use_no_echo = 0;
  return value;
}

void F77_ENTRY_CCI(Kuprop,chprmt,chline,length)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  *length = fstrset( chline, len_chline, ku_prop( prompt ) );
  free( prompt );
}


/*
 * prompt for integer
 */
int ku_proi( const char *prompt,
            int ifault )
{
  char dfault[32];
  sprintf( dfault, "%d", ifault );
  while( 1 ) {
    const char *line = ku_pros( prompt, dfault );
    if( line == NULL || line == dfault ) {
      return ifault;
    }
    else {
      char *tail;
      int value = fstrtoi( line, &tail );
      if( *tail == '\0' )
        return value;

      printf( " *** Invalid integer number ***\n" );
    }
  }
}

void F77_ENTRY_CI(Kuproi,chprmt,value)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  int dfault = *value;
  *value = ku_proi( prompt, dfault );
  free( prompt );
}


/*
 * prompt for real
 */
double ku_pror( const char *prompt,
               double rfault )
{
  char dfault[32];
  strcpy( dfault, strfromd( rfault, 0 ) );
  while( 1 ) {
    const char *line = ku_pros( prompt, dfault );
    if( line == NULL || line == dfault ) {
      return rfault;
    }
    else {
      char *tail;
      double value = fstrtod( line, &tail );
      if( *tail == '\0' )
        return value;

      printf( " *** Invalid real number ***\n" );
    }
  }
}

void F77_ENTRY_CR(Kupror,chprmt,value)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  double dfault = *value;
  *value = ku_pror( prompt, dfault );
  free( prompt );
}

/*
 * print a message
 */
void F77_ENTRY_CI(Kumess,chmess,value)
/* { */
  char *mess = fstrdup( chmess, len_chmess );
  int ipr = *value;

  switch( ipr ) {

  case 0:
          mline = strdup( mess );
          break;

  case 1:
          mline = mstr2cat( mline, "\n", mess );
          break;

  default:  /* display message */
          if (mline == NULL) {
            mline = strdup( mess );
          } else {
            mline = mstr2cat( mline, "\n", mess );
          }
          if (kjmpaddr.disp_select_C != NULL ) {
              static char *label[] = {"Ok"};
              int enabled = ku_intr( 0 );
              (*kjmpaddr.disp_select_C)
                        (3, mline, (sizeof label)/sizeof(char*), label, 1);
              if( enabled )
                ku_intr( enabled );
          } else {
            printf  ("%s\n", mline);
          }
          free( mline );
          mline = NULL;
          break;
  } /* switch( ipr ) */

  free( mess );
}


int ku_more( const char *question,
            const char *line )
{
  char *prompt = str2dup( question, " " );
  int n;

  if( line != NULL )
    prompt = mstr3cat( prompt, "\"", line, "\" " );

  if( kjmpaddr.disp_select_C != NULL ) {
    static char *labels[] = { "Yes", "No", "Quit", "Go" };
    int enabled = ku_intr( 0 );
    prompt = mstrcat( prompt, "?" );
    n = (*kjmpaddr.disp_select_C)( 4, prompt, (sizeof labels)/sizeof(char*),
                                  labels, 1 );
    if( enabled )
      ku_intr( enabled );
  }
  else {
    const char *answer;
    prompt = mstrcat( prompt, "(Yes/No/Quit/Go) ? " );
#ifndef IBM370
    if( kc_window.use_getline )
      gl_config( "erase", 1 );
#endif
    answer = ku_proc( prompt, NULL );
#ifndef IBM370
    if( kc_window.use_getline )
      gl_config( "erase", 0 );
#endif
    if( answer == NULL )
      answer = "Go";

    switch( answer[0] ) {
    default:
      n = 1;
      break;
    case 'N':
      n = 2;
      break;
    case 'Q':
      n = 3;
      break;
    case 'G':
      n = 4;
      break;
    }
  }
  free( prompt );

  return n;
}


/*
 * action routine for /KUIP/SET_SHOW/RECORDING
 */
int kxrecording()
{
  cmd_hist.frequency = ku_geti();
  sprintf( kc_value.set_recording, "%d", cmd_hist.frequency );
  cmd_hist.flush = cmd_hist.count + cmd_hist.frequency;
  return 0;
}


/*
 * action routine for /KUIP/LAST
 */
int kxlast()
{
  int nlines = ku_geti();

  if( cmd_hist.count > 0 )      /* do not record the LAST command itself */
    free( cmd_hist.lines[--cmd_hist.count] );

  if( nlines == 0 ) {
    char *file = ku_getf();
    FILE *kumac;

    if( cmd_hist.kumac != NULL ) {
      int i;
      out_last_kumac( cmd_hist.count );
      for( i = 0; i < cmd_hist.count; i++ )
        free( cmd_hist.lines[i] );
      free( (char*)cmd_hist.lines );
    }
    cmd_hist.lines = NULL;
    cmd_hist.count = 0;
    cmd_hist.flush = cmd_hist.frequency;
    cmd_hist.start = time( NULL );

    if( file[0] != '\0' ) {
      /*
       * If new file name is not an absolute path then prefix it by the
       * working directory at startup time.
       */
      char *path = strdup( "" );
#ifdef vms
      if( strchr( file, ':' ) == NULL
         && strchr( file, ']' ) == NULL )
        path = mstrcat( path, kc_flags.init_wdir );
#endif
#ifdef UNIX
      if( file[0] != '/' )
        path = mstrcat( path, kc_flags.init_wdir );
#endif
      path = mstrcat( path, file );
      if( cmd_hist.kumac != NULL )
        free( cmd_hist.kumac );
      cmd_hist.kumac = ku_home( path, "kumac" );
      free( path );
#ifdef DBMALLOC
      malloc_mark( cmd_hist.kumac );
#endif
    }
    else if( cmd_hist.kumac == NULL ) {
      char *path = str2dup( kc_flags.init_wdir, "last.kumac" );
      cmd_hist.kumac = ku_home( path, NULL );
      free( path );
#ifdef DBMALLOC
      malloc_mark( cmd_hist.kumac );
#endif
    }

#ifdef vms
    /* create a new version which we can overwrite each time */

    if( strchr( cmd_hist.kumac, ';' ) == NULL ) {
      kumac = fopen( cmd_hist.kumac, "w" );
      fputs( "\n", kumac );
      fclose( kumac );
      cmd_hist.kumac = mstrcat( cmd_hist.kumac, ";0" );
    }
#else
    /* make a backup copy of the existing file */

    if( (kumac = fopen( cmd_hist.kumac, "r" )) != NULL ) {
      char *old_last_kumac = ku_fcase( str2dup( cmd_hist.kumac, "old" ) );

      fclose( kumac );
      remove( old_last_kumac );
      rename( cmd_hist.kumac, old_last_kumac );
      free( old_last_kumac );
    }
#endif
  }

  else if( nlines > 0 ) {
    out_last_kumac( nlines );
  }

  else if( nlines > -99 ) {
    int n0 = cmd_hist.count + nlines;
    int n;
    if( n0 < 0 )
      n0 = 0;
    for( n = n0; n < cmd_hist.count; n++ ) {
      printf( " %s\n", cmd_hist.lines[n] );
    }
  }

  else {
    if( out_last_kumac( cmd_hist.count ) == 0 )
      ku_edit( cmd_hist.kumac, 1 );
  }

  return 0;
}


/*
 * write to history buffer to LAST.KUMAC
 */
static
int out_last_kumac( int nlines )
{
  if( cmd_hist.lines != NULL && cmd_hist.frequency > 0 ) {
    FILE *stream;

    if( cmd_hist.kumac == NULL ) {
      /* QUIT or EXIT before any LAST command */
      char *path = str2dup( kc_flags.init_wdir, "last.kumac" );
      cmd_hist.kumac = ku_home( path, NULL );
      free( path );
#ifdef vms
      cmd_hist.kumac = mstrcat( cmd_hist.kumac, ";0" );
#endif
    }
#ifdef vms
    else
      remove( cmd_hist.kumac ); /* avoid creating a new cycle each time */
#endif

    if( (stream = fopen( cmd_hist.kumac, "w" )) == NULL ) {
      ku_alfa();
      printf( " *** Cannot open %s\n", cmd_hist.kumac );
      return 1;
    }
    else {
      time_t now = time( NULL );
      int i;

      fprintf( stream, "*** Begin of history file: %s",
              asctime( localtime( &cmd_hist.start ) ) );

      for( i = cmd_hist.count - nlines; i < cmd_hist.count; i++ ) {
        fputs( cmd_hist.lines[i], stream );
        fputc( '\n', stream );
      }
      fprintf( stream, "*** End   of history file: %s",
              asctime( localtime( &now ) ) );

      fclose( stream );
    }
  }
  return 0;
}


/*
 * append line to the history buffer and flush LAST.KUMAC if necessary
 */
void ku_last( const char *line )
{
  if( line[0] == '\0' )
    return;

  if( cmd_hist.lines == NULL )
    cmd_hist.lines = (char**)malloc( (cmd_hist.count+1) * sizeof(char*) );
  else
    cmd_hist.lines = (char**)realloc( (char*)cmd_hist.lines,
                                     (cmd_hist.count+1) * sizeof(char*) );

  cmd_hist.lines[cmd_hist.count++] = strdup( line );

  if( cmd_hist.frequency > 0 && cmd_hist.count >= cmd_hist.flush ) {
    out_last_kumac( cmd_hist.count );
    cmd_hist.flush += cmd_hist.frequency;
  }
}


