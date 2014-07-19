/*
 * $Id: getline.c,v 1.3 1998/08/25 12:47:32 mclareni Exp $
 *
 * $Log: getline.c,v $
 * Revision 1.3  1998/08/25 12:47:32  mclareni
 * QMGLIBC changes for Linux RH51
 *
 * Revision 1.2  1997/03/17 16:54:56  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:32:44  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:32:55  mclareni
 * Kuip
 *
 */
#if !defined(CERNLIB_IBMALL)
/*CMZ :  2.06/03 12/01/95  17.04.22  by  Gunter Folger*/
/*-- Author :*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"


#if 0
static char     rcsid[] =
"$Id: getline.c,v 1.3 1998/08/25 12:47:32 mclareni Exp $";
static char    *copyright = "Copyright (C) 1991, 1992, Chris Thewalt";
#endif

/*
 * Copyright (C) 1991, 1992 by Chris Thewalt (thewalt@ce.berkeley.edu)
 *
 * Permission to use, copy, modify, and distribute this software
 * for any purpose and without fee is hereby granted, provided
 * that the above copyright notices appear in all copies and that both the
 * copyright notice and this permission notice appear in supporting
 * documentation.  This software is provided "as is" without express or
 * implied warranty.
 */

static int      gl_tab();  /* forward reference needed for gl_tab_hook */

/********************* exported interface ********************************/

#if defined(CERNLIB_QMGLIBC)
extern char *getline2( const char* );    /* read a line of input */
#else
extern char *getline( const char* );    /* read a line of input */
#endif
extern void  gl_setwidth( int );        /* specify width of screen */
extern void  gl_histadd( const char* ); /* adds entries to hist */

extern void  gl_char_init();            /* get ready for no echo input */
extern void  gl_char_cleanup();         /* undo gl_char_init */
extern int   gl_reset();                /* cleanup interrupted getline */
extern void  gl_reinit();               /* reinitialize interrupted getline */

int             (*gl_in_hook)() = 0;
int             (*gl_out_hook)() = 0;
int             (*gl_tab_hook)() = gl_tab;

/******************** imported interface *********************************/

/*
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>

extern int      isatty();
extern void    *malloc();
extern void     free();
extern int      raise();
*/

/******************** internal interface *********************************/

#define BUF_SIZE 1024

static int      gl_init_done = -1;      /* terminal mode flag  */
static int      gl_char_init_done = 0;  /* has gl_char_init been called */
static int      gl_termw = 80;          /* actual terminal width */
static int      gl_scroll = 27;         /* width of EOL scrolling region */
static int      gl_width = 0;           /* net size available for input */
static int      gl_extent = 0;          /* how far to redraw, 0 means all */
static int      gl_overwrite = 0;       /* overwrite mode */
static int      gl_def_overwrite = 0;   /* default overwrite mode */
static int      gl_edit_style = 0;      /* 0=ksh 1=DCL */
static int      gl_no_echo = 0;         /* do not echo input characters */
static int      gl_passwd = 0;          /* do not echo input characters */
static int      gl_erase_line = 0;      /* erase line before returning */
static int      gl_pos, gl_cnt = 0;     /* position and size of input */
static char     gl_buf[BUF_SIZE];       /* input buffer */
static char     gl_killbuf[BUF_SIZE]=""; /* killed text */
static char     gl_outbuf[BUF_SIZE]=""; /* output buffer for gl_putc() */
static char    *gl_outptr = gl_outbuf;  /* ptr to next free pos. in outbuf */
static const char *gl_prompt;           /* to save the prompt string */
static int      gl_intrc = 0;           /* keyboard SIGINT char */
static int      gl_quitc = 0;           /* keyboard SIGQUIT char */
static int      gl_suspc = 0;           /* keyboard SIGTSTP char */
static int      gl_dsuspc = 0;          /* delayed SIGTSTP char */
static int      gl_search_mode = 0;     /* search mode flag */


static void     gl_init();              /* prepare to edit a line */
static void     gl_cleanup();           /* to undo gl_init */

static void     gl_addchar();           /* install specified char */
static void     gl_del();               /* del, either left (-1) or cur (0) */
static void     gl_delword();           /* delete word */
static void     gl_error();             /* write error msg and die */
static void     gl_fixup( const char*, int, int);
                                        /* fixup state variables and screen */
static int      gl_getc();              /* read one char from terminal */
static void     gl_kill();              /* delete to EOL */
static void     gl_newline();           /* handle \n or \r */
static void     gl_putc();              /* write one char to terminal */
static void     gl_puts();              /* write a line to terminal */
static void     gl_flush();             /* write out gl_putc() buffer */
static void     gl_redraw();            /* issue \n and redraw all */
static void     gl_transpose();         /* transpose two chars */
static void     gl_yank();              /* yank killed text */

static void     hist_init();    /* initializes hist pointers */
static char    *hist_next();    /* return ptr to next item */
static char    *hist_prev();    /* return ptr to prev item */
static char    *hist_save();    /* makes copy of a string, without NL */

static void     search_addchar();       /* increment search string */
static void     search_term();          /* reset with current contents */
static void     search_back();          /* look back for current string */
static void     search_forw();          /* look forw for current string */

/************************ nonportable part *********************************/

/*
extern int      write();
extern void     exit();
*/

#ifdef TERMIO_POSIX
#include <termios.h>
static struct termios  new_termios, old_termios;
#endif

#ifdef TERMIO_BSD
#include <sys/ioctl.h>
#include <sgtty.h>
static struct sgttyb   new_tty, old_tty;
static struct tchars   tch;
static struct ltchars  ltch;
static int  typeahead_count;
static char typeahead_buffer[1024];
#endif

#ifdef TERMIO_SYSV
#include <sys/ioctl.h>
#include <termio.h>
static struct termio   new_termio, old_termio;
#endif

#ifdef vms
static int   setbuff[2];             /* buffer to set terminal attributes */
static short chan = -1;              /* channel to terminal */
static int   enable_ast;             /* reenable ^C AST */
static struct dsc$descriptor_s descrip;     /* VMS descriptor */
#endif

void
  gl_config( const char *which, int value )
{
  if( strcmp( which, "overwrite" ) == 0 )
    gl_def_overwrite = value;
  else if( strcmp( which, "style" ) == 0 )
    gl_edit_style = value;
  else if( strcmp( which, "noecho" ) == 0 )
    gl_no_echo = value;
  else if( strcmp( which, "erase" ) == 0 )
    gl_erase_line = value;
  else
    printf( "gl_config: %s ?\n", which );
}


void
  gl_char_init()          /* turn off input echo */
{
   if (isatty(0) == 0 || isatty(1) == 0)
      return;

#ifdef vms
    gl_intrc = 'C' - '@';       /* ^C */
    gl_quitc = 'Y' - '@' + 128; /* ^Y quit in DCL and yank in KSH */
#endif

#if defined(MSDOS) || defined(WIN32) || defined(CERNLIB_WINNT)
    gl_intrc = 'C' - '@';       /* ^C */
    gl_quitc = 'Y' - '@';        /* ^Y quit in DCL and yank in KSH */
#endif

#ifdef TERMIO_POSIX
    tcgetattr(0, &old_termios);
    gl_intrc = old_termios.c_cc[VINTR];
    gl_quitc = old_termios.c_cc[VQUIT];
# ifdef VSUSP
    gl_suspc = old_termios.c_cc[VSUSP];
# endif
# ifdef VDSUSP
    gl_dsuspc = old_termios.c_cc[VDSUSP];
# endif
    new_termios = old_termios;
    new_termios.c_iflag &= ~(BRKINT|ISTRIP|IXON|IXOFF|INLCR|IGNCR|ICRNL);
    new_termios.c_iflag |= (IGNBRK|IGNPAR);
    new_termios.c_lflag &= ~(ICANON|ISIG|IEXTEN|ECHO);
    new_termios.c_cc[VMIN] = 1;
    new_termios.c_cc[VTIME] = 0;
#endif

#ifdef TERMIO_BSD
    ioctl(0, TIOCGETC, &tch);
    ioctl(0, TIOCGLTC, &ltch);
    gl_intrc = tch.t_intrc;
    gl_quitc = tch.t_quitc;
    gl_suspc = ltch.t_suspc;
    gl_dsuspc = ltch.t_dsuspc;
    ioctl(0, TIOCGETP, &old_tty);
    new_tty = old_tty;
    new_tty.sg_flags |= RAW;
    new_tty.sg_flags &= ~ECHO;
#endif

#ifdef TERMIO_SYSV
    ioctl(0, TCGETA, &old_termio);
    gl_intrc = old_termio.c_cc[VINTR];
    gl_quitc = old_termio.c_cc[VQUIT];
    new_termio = old_termio;
    new_termio.c_iflag &= ~(BRKINT|ISTRIP|IXON|IXOFF);
    new_termio.c_iflag |= (IGNBRK|IGNPAR);
    new_termio.c_lflag &= ~(ICANON|ISIG|ECHO);
    new_termio.c_cc[VMIN] = 1;
    new_termio.c_cc[VTIME] = 0;
#endif

    gl_char_init_done = 1;
}

static void
  gl_char_reinit()      /* turn off input echo */
{
#ifdef TERMIO_POSIX
    tcsetattr(0, TCSANOW, &new_termios);
#endif

#ifdef TERMIO_BSD
    /*
     * switching the BSD driver to RAW mode discards the input queue
     * we save the type-ahead in a buffer which gl_getc() uses
     */
    typeahead_count = 0;

    while( 1 ) {
      fd_set readfds;
      struct timeval timeout;

      FD_ZERO( &readfds );
      FD_SET( 0, &readfds );
      timeout.tv_sec = 0;
      timeout.tv_usec = 0;

      if( select( 1, SELECT_CAST(&readfds), NULL, NULL, &timeout ) > 0 ) {
        read( 0, &typeahead_buffer[typeahead_count++], 1 );
      }
      else
        break;
    }

    ioctl(0, TIOCSETP, &new_tty);
#endif

#ifdef TERMIO_SYSV
    ioctl(0, TCSETA, &new_termio);
#endif

#ifdef vms
    /* cancel pending ^C AST */
    enable_ast = control_C_ast( 0 );
    var_descriptor( descrip, "TT:" );
    (void)sys$assign(&descrip,&chan,0,0);
    (void)sys$qiow(0,chan,IO$_SENSEMODE,0,0,0,setbuff,8,0,0,0,0);
    setbuff[1] |= TT$M_NOECHO;
    (void)sys$qiow(0,chan,IO$_SETMODE,0,0,0,setbuff,8,0,0,0,0);

    /*
     * Install exit() as ^Y AST which is called when the input terminal
     * hangs up, e.g. by closing the window before leaving the application.
     */
    sys$qiow( 0, chan, IO$_SETMODE | IO$M_CTRLYAST, 0, 0, 0,
             exit, 0, PSL$C_USER, 0, 0, 0 );
#endif /* vms */
}

void
  gl_char_cleanup()     /* undo effects of gl_char_init */
{
   if (isatty(0) == 0 || isatty(1) == 0)
      return;

#ifdef TERMIO_POSIX
    tcsetattr(0, TCSANOW, &old_termios);
#endif

#ifdef TERMIO_BSD
    ioctl(0, TIOCSETP, &old_tty);
#endif

#ifdef TERMIO_SYSV
    ioctl(0, TCSETA, &old_termio);
#endif

#ifdef vms
    setbuff[1] &= ~TT$M_NOECHO;
    (void)sys$qiow(0,chan,IO$_SETMODE,0,0,0,setbuff,8,0,0,0,0);
    sys$dassgn(chan);
    chan = -1;
    /* reenable ^C AST */
    control_C_ast( enable_ast );
#endif
}

static void
gl_flush()
/* flush gl_putc() buffer */
{
    if( gl_outptr != gl_outbuf ) {
      write( 1, gl_outbuf, gl_outptr - gl_outbuf );
      gl_outptr = gl_outbuf;
    }
}


/* execute the idle command */
static
int gl_idle()
{
  int old_timeout = kc_value.idle_time;
  kc_value.idle_time = 0;           /* in case idle command causes a prompt */

  gl_fixup( gl_prompt, -1, 0 );
  gl_puts( kc_value.idle_cmd );
  gl_putc( '\n' );
  gl_cleanup();
  ku_exel( kc_value.idle_cmd );
  gl_init();
  gl_fixup( gl_prompt, -2, gl_pos );

  kc_value.idle_time = old_timeout;
  return 0;
}

#if defined(MSDOS) && !defined(WIN32)
// +DECK, PAUSE, T=XCC, IF=WINNT. (from KERNDOS.CAR )
#  include <conio.h>
   int pause_()
   {
      int first_char;
        first_char = _getch();
        if (first_char == 0 || first_char == 0xE0) first_char = -_getch();
        return first_char;
   }
#elif defined(CERNLIB_WINNT)
/*_____________________________________________________________________________*/
int pause_(){
 static HANDLE hConsoleInput = NULL;
 static iCharCount = 0;
 static int chLastChar = 0;
 
 DWORD cRead;
 
 INPUT_RECORD pirBuffer;
 KEY_EVENT_RECORD *KeyEvent= (KEY_EVENT_RECORD *)&(pirBuffer.Event);
 
 if (!hConsoleInput) hConsoleInput = GetStdHandle(STD_INPUT_HANDLE);
 
 if (iCharCount) iCharCount--;      /* Whether several symbols had been read */
 else {
   chLastChar = 0;
   while (chLastChar == 0) {
     if (!ReadConsoleInput(hConsoleInput,       /* handle of a console input buffer    */ 
                           &pirBuffer,          /* address of the buffer for read data */
                           1,                   /* number of records to read           */ 
                           &cRead               /* address of number of records read   */
        )) return 0;
 
     if (pirBuffer.EventType == KEY_EVENT  && KeyEvent->bKeyDown == TRUE){
         iCharCount = KeyEvent->wRepeatCount - 1;
         chLastChar = ((int) (KeyEvent->uChar).AsciiChar & 0xffff);
         if (chLastChar)
              OemToCharBuff((char const *)&chLastChar,(char *)&chLastChar,1);
         else
              chLastChar = - (KeyEvent->wVirtualScanCode);
     }
   }
 }
 return chLastChar;
 
}
#endif

static int
gl_getc()
/* get a character without echoing it to screen */
{
#if defined(MSDOS) || defined(CERNLIB_WINNT) || defined(WIN32)
# define k_ctrl_C   3
# define k_ctrl_Z  26
# define k_ctrl_Q  17
# define k_ctrl_K  11
# define k_rt_arr -77
# define k_lt_arr -75
# define k_up_arr -72
# define k_dn_arr -80
# define k_PGUP   -73
# define k_PGDW   -81
# define k_HOME   -71
# define k_END    -79
# define k_INS    -82
# define k_DEL    -83
# define k_ENTER   13
# define k_CR      13
# define k_BS       8
# define k_ESC     27
# define k_alt_H  -35
# define k_beep     7
# ifndef CERNLIB_WINNT
    int get_cursor__(int *,int *);
    int display_off__(int *);
    int display_on__();
    int locate_(int *,int *);
    int ixc, iyc;
# endif

#endif


#ifdef vms
    short iosb[4];
#endif

    int             c;
    char            ch;

    gl_flush();

#ifdef UNIX

# ifdef TERMIO_BSD
    if( typeahead_count > 0 ) {
      int i;
      c = typeahead_buffer[0];
      typeahead_count--;
      for( i = 0; i < typeahead_count; i++ )
        typeahead_buffer[i] = typeahead_buffer[i+1];
      return c;
    }
# endif

# ifndef MSDOS
    do {

#  ifdef HAVE_SELECT
      if( kc_value.idle_time > 0 ) {
        fd_set readfds;
        struct timeval timeout;

        FD_ZERO( &readfds );
        FD_SET( 0, &readfds );
        timeout.tv_sec = kc_value.idle_time;
        timeout.tv_usec = 0;

        if( select( 1, SELECT_CAST(&readfds), NULL, NULL, &timeout ) == 0 )
          return gl_idle();
      }
#  endif

       errno = 0;
       c = (read(0, &ch, 1) > 0)? ch : -1;
    } while (errno == EINTR);

# else /* MSDOS */

    c = pause_();
    if (c < 0) {
        switch (c) {
          case k_up_arr: c =  16;   /* up -> ^P */
            break;
          case k_dn_arr: c =  14;   /* down -> ^N */
            break;
          case k_lt_arr: c =   2;   /* left -> ^B */
            break;
          case k_rt_arr: c =   6;   /* right -> ^F */
            break;
          case k_INS:    c =  15;   /* right -> ^F */
            break;
          case k_DEL:    c =   4;   /* Delete character under cursor */
            break;
          case k_END:    c =   5;   /* Moves cursor to end of line */
            break;
          case k_HOME:   c =   1;   /* Moves cursor to beginning of line */
            break;
#  ifndef CERNLIB_WINNT
          case k_PGUP: c = 0; display_on__();
            break;
          case k_PGDW: c =0; get_cursor__(&ixc, &iyc);
                             display_off__(&iyc);
                             locate_(&ixc,&iyc);
            break;
#  endif
          default: c = 0;    /* make it garbage */
        }
    }
    else {
      switch(c) {
          case k_ESC:    c =  'U' - '@'; /* Clear full line  -> ^U */
            break;
          case k_ctrl_C:   raise(SIGINT);          /* Ctrl-C handle */
            break;
          default:
            break;
        }
    }
# endif
#endif

#ifdef vms
    c = '\0';
    if( kc_value.idle_time > 0 ) {
      sys$qiow( 0, chan, IO$_TTYREADALL | IO$M_TIMED, iosb, 0, 0,
               &c, 1, kc_value.idle_time + 1, 0, 0, 0 );
      if( iosb[0] == SS$_TIMEOUT )
        return gl_idle();
    }
    else {
      sys$qiow( 0, chan, IO$_TTYREADALL, iosb, 0, 0,
               &c, 1, 0, 0, 0, 0 );
    }
    if( iosb[0] == SS$_NORMAL )
      c &= 0177;                /* get a char */
    else
      c = -1;                   /* EOF */
#endif

    return c;
}

static void
  gl_putc(c)
int c;
{
    char   ch = c;

#if defined(WIN32) || defined(CERNLIB_WINNT)
    CharToOemBuff((char const *)&c,&ch,1);
#endif

    if( gl_passwd && isgraph( c ) )
      ch = ' ';

    /* buffer characters because VMS C-I/O is sooo slow */
    *gl_outptr++ = ch;

#ifdef TERMIO_MAP_NL
    if (ch == '\n') {           /* BSD in RAW mode, map NL to NL,CR */
        *gl_outptr++ = '\r';
    }
#endif

    *gl_outptr = '\0';
}

/******************** fairly portable part *********************************/

static void
  gl_puts(buf)
char *buf;
{
/*
    int len = strlen(buf);

    write(1, buf, len);
*/
    while( *buf != '\0' )
      gl_putc( *buf++ );
}

static void
  gl_error(buf)
char *buf;
{
    int len = strlen(buf);

    gl_cleanup();
#if defined(WIN32) || defined(CERNLIB_WINNT)
    {
      char *OemBuf = (char *)malloc(2*len);
      CharToOemBuff(buf,OemBuf,len);
      write(2, OemBuf, len);
      free(OemBuf);
    }
#else
    write(2, buf, len);
#endif
    /* exit(1); */
}

static void
  gl_init()       /* set up variables and terminal */
{
    if (gl_init_done < 0) {             /* -1 only on startup */
        hist_init();
    }

    if (!gl_char_init_done)
       gl_char_init();

    gl_char_reinit();
    gl_init_done = 1;
    gl_overwrite = gl_def_overwrite;
    gl_outptr = gl_outbuf;      /* reset gl_putc() buffer */
}

static void
  gl_cleanup()       /* undo effects of gl_init, as necessary */
{
    gl_flush();
    if (gl_init_done > 0)
        gl_char_cleanup();
    gl_init_done = 0;
}

void
  gl_reinit()        /* reinitialize terminal */
{

    if (isatty(0) == 0 || isatty(1) == 0)
       return;

    if (gl_init_done == 0) {
       gl_char_reinit();
       gl_init_done = 1;
    }
}

int
gl_reset()                    /* undo effects of gl_init, as necessary */
{
    int set = 0;

    if (isatty(0) == 0 || isatty(1) == 0)
       return set;

    if (gl_init_done > 0) {
        gl_char_cleanup();
        gl_init_done = 0;
        set = 1;
    }

    return set;
}

void
  gl_setwidth(w)
int w;
{
    if (w > 20) {
        gl_termw = w;
        gl_scroll = w / 3;
    } else {
        gl_error("\n*** Error: minimum screen width is 21\n");
    }
}

#if defined(CERNLIB_QMGLIBC)
char *getline2( const char *prompt )
#else
char *getline( const char *prompt )
#endif
{
    int             c, loc, tmp;
    int             sig;

    if( prompt != NULL )
      gl_prompt = prompt;
    else
      gl_prompt = "";

    gl_buf[0] = 0;
    gl_init();
    if (gl_in_hook)
        gl_in_hook(gl_buf);
    gl_fixup(gl_prompt, -2, BUF_SIZE);
    while ((c = gl_getc()) != -1) {  /* -1 for EOF */
        gl_extent = 0;          /* reset to full extent */
#if !defined(WIN32) && !defined(CERNLIB_WINNT)
        if (isprint(c)) {
#else
        if (c >= ' ') {
#endif
            if (gl_search_mode)
               search_addchar(c);
            else
               gl_addchar(c);
        } else {
            int dcl_xlate = (gl_edit_style == 1);

            if (gl_search_mode) {
                if (c == '\033' || c == '\016' || c == '\020') {
                    search_term();
                    c = 0;              /* ignore the character */
                } else if (c == '\010' || c == '\177') {
                    search_addchar(-1); /* unwind search string */
                    c = 0;
                } else if (c != '\022' && c != '\023') {
                    search_term();      /* terminate and handle char */
                }
            }

            if( c == '\033' ) {
              c = gl_getc();
              if (c == '[' || c == 'O')
                c = gl_getc();

              switch( c ) {
              case 'A':       /* up */
                c = 'P' - '@';
                dcl_xlate = 0;
                break;
              case 'B':       /* down */
                c = 'N' - '@';
                dcl_xlate = 0;
                break;
              case 'C':       /* right */
                c = 'F' - '@';
                dcl_xlate = 0;
                break;
              case 'D':       /* left */
                c = 'B' - '@';
                dcl_xlate = 0;
                break;
              default:        /* who knows */
                gl_putc('\007');
                c = 0;
                break;

              case '2':
                c = gl_getc();
                if( gl_getc() == '~' ) { /* DEC function keys */
                  switch( c ) {
                  case '4':       /* F12 == ^H */
                    c = 'H' - '@';
                    break;
                  case '5':       /* F13 == ^J */
                    c = 'J' - '@';
                    break;
                  case '6':       /* F14 == ^A */
                    c = 'A' - '@';
                    break;
                  default:
                    gl_putc('\007');
                    c = 0;
                    break;
                  }
                }
              }
            }

            if( c != 0 && dcl_xlate ) {
              /* translate DCL control characters */

              switch( c ) {
              case '\001':      /* ^A toggles overwrite mode */
                c = 'O' - '@';
                break;
              case '\002':      /* ^B recall previous line */
                c = 'P' - '@';
                break;
              case '\004':      /* ^D moves cursor left */
                c = 'B' - '@';
                break;
              case '\010':      /* ^H moves cursor to beginning of line */
                c = 'A' - '@';
                break;
              case '\012':      /* ^J deletes previous word */
                gl_delword();
                c = 0;
                break;
              case '\022':      /* ^R refreshes line */
                c = 'L' - '@';
                break;
              case '\030':      /* ^X flush type-ahead then ^U */
              case '\025':      /* ^U deletes from beginning of line */
                strcpy( &gl_buf[0], &gl_buf[gl_pos] );
                gl_fixup( gl_prompt, 0, 0 );
                c = 0;
                break;
              case '\031':      /* ^Y aborts */
                c = gl_quitc;
                break;
              case '\032':      /* ^Z is EOF */
                c = 'D' - '@';
                break;
                /* the following codes are the same in both modes */
              case '\003':      /* ^C interrupts */
              case '\005':      /* ^E moves cursor to end of line */
              case '\006':      /* ^F moves cursor right */
              case '\011':      /* ^I moves to next tab position */
              case '\015':      /* ^M terminates input */
              case '\016':      /* ^N recalls next command */
              case '\177':      /* DEL deletes character */
                break;
              default:          /* the rest rings a bell */
                c = 'G' - '@';
              }
            }

            switch (c) {
              case '\n': case '\r':                     /* newline */
                gl_newline();
                gl_cleanup();
                return gl_buf;
                /*NOTREACHED*/
                break;
              case '\001': gl_fixup(gl_prompt, -1, 0);          /* ^A */
                break;
              case '\002': gl_fixup(gl_prompt, -1, gl_pos-1);   /* ^B */
                break;
              case '\004':                                      /* ^D */
                if (gl_cnt == 0) {
                  /* ignore ^D for completely empty line
                    gl_buf[0] = 0;
                    gl_cleanup();
                    gl_putc('\n');
                    return gl_buf; */
                } else {
                    gl_del(0);
                }
                break;
              case '\005': gl_fixup(gl_prompt, -1, gl_cnt);     /* ^E */
                break;
              case '\006': gl_fixup(gl_prompt, -1, gl_pos+1);   /* ^F */
                break;
              case '\010': case '\177': gl_del(-1);     /* ^H and DEL */
                break;
              case '\t':                                        /* TAB */
                if (gl_tab_hook) {
                    tmp = gl_pos;
                    loc = gl_tab_hook(gl_buf, strlen(gl_prompt), &tmp);
                    if (loc >= 0 || tmp != gl_pos)
                        gl_fixup(gl_prompt, loc, tmp);
                }
                break;
              case '\013': gl_kill();                           /* ^K */
                break;
              case '\014': gl_redraw();                         /* ^L */
                break;
              case '\016':                                      /* ^N */
                strcpy(gl_buf, hist_next());
                if (gl_in_hook)
                    gl_in_hook(gl_buf);
                gl_fixup(gl_prompt, 0, BUF_SIZE);
                break;
              case '\017': gl_overwrite = !gl_overwrite;        /* ^O */
                break;
              case '\020':                                      /* ^P */
                strcpy(gl_buf, hist_prev());
                if (gl_in_hook)
                    gl_in_hook(gl_buf);
                gl_fixup(gl_prompt, 0, BUF_SIZE);
                break;
              case '\022': search_back(1);                      /* ^R */
                break;
              case '\023': search_forw(1);                      /* ^S */
                break;
              case '\024': gl_transpose();                      /* ^T */
                break;
              case '\025': gl_fixup(gl_prompt,-1,0); gl_kill(); /* ^U */
                break;
              case '\031': gl_yank();                           /* ^Y */
                break;
              default:          /* check for a terminal signal */
#if !defined(MSDOS) || defined(WIN32) || defined(CERNLIB_WINNT)
                if (c > 0) {    /* ignore 0 (reset above) */
                    sig = 0;
# ifndef vms
#  ifdef SIGINT
                    if (c == gl_intrc)
                        sig = SIGINT;
#  endif
#  ifdef SIGQUIT
                    if (c == gl_quitc)
                        sig = SIGQUIT;
#  endif
#  ifdef SIGTSTP
                    if (c == gl_suspc || c == gl_dsuspc)
                        sig = SIGTSTP;
#  endif
# else  /* vms */
                    if (c == gl_intrc)
                        sig = SS$_DEBUG;
                    if (c == gl_quitc)
                        sig = SS$_CONTROLY;
# endif /* vms */

                    if (sig != 0) {
                        gl_cleanup();
# ifndef vms
#  ifdef SIGTSTP
                        /*
                         * Send signal to process group to propagate ^Z to the
                         * interactive shell in case the program was started
                         * from a shell script.
                         */
#   ifdef GETPGRP_BSD
                        kill( -getpgrp( 0 ), sig );
#   else
                        kill( -getpgrp(), sig );
#   endif
#  else
#ifdef WIN32
                        if (sig == SIGINT) GenerateConsoleCtrlEvent(CTRL_C_EVENT,0);
                        else 
#endif
                        raise(sig);
#  endif
# else
                        lib$signal( sig );
# endif
                        gl_init();
                        gl_redraw();
                        c = 0;
                    }
                }
#endif
                if (c != 0)   /*  warn user for strange character typed */
                    gl_putc('\007');
                break;
            }
        }
    }
    gl_cleanup();
    return NULL;                /* EOF reached */
}

static void
  gl_addchar(c)
int c;
/* adds the character c to the input buffer at current location */
{
    int  i;

    if (gl_cnt >= BUF_SIZE - 1)
        gl_error("\n*** Error: getline(): input buffer overflow\n");
    if (gl_overwrite == 0 || gl_pos == gl_cnt) {
        for (i=gl_cnt; i >= gl_pos; i--)
            gl_buf[i+1] = gl_buf[i];
        gl_buf[gl_pos] = c;
        gl_fixup(gl_prompt, gl_pos, gl_pos+1);
    } else {
        gl_buf[gl_pos] = c;
        gl_extent = 1;
        gl_fixup(gl_prompt, gl_pos, gl_pos+1);
    }
}

static void
  gl_yank()
/* adds the kill buffer to the input buffer at current location */
{
    int  i, len;

    len = strlen(gl_killbuf);
    if (len > 0) {
        if (gl_overwrite == 0) {
            if (gl_cnt + len >= BUF_SIZE - 1)
                gl_error("\n*** Error: getline(): input buffer overflow\n");
            for (i=gl_cnt; i >= gl_pos; i--)
                gl_buf[i+len] = gl_buf[i];
            for (i=0; i < len; i++)
                gl_buf[gl_pos+i] = gl_killbuf[i];
            gl_fixup(gl_prompt, gl_pos, gl_pos+len);
        } else {
            if (gl_pos + len > gl_cnt) {
                if (gl_pos + len >= BUF_SIZE - 1)
                    gl_error("\n*** Error: getline(): input buffer overflow\n");
                gl_buf[gl_pos + len] = 0;
            }
            for (i=0; i < len; i++)
                gl_buf[gl_pos+i] = gl_killbuf[i];
            gl_extent = len;
            gl_fixup(gl_prompt, gl_pos, gl_pos+len);
        }
    } /* else
        gl_putc('\007'); */
}

static void
  gl_transpose()
/* switch character under cursor and to left of cursor */
{
    int    c;

    if (gl_pos > 0 && gl_cnt > gl_pos) {
        c = gl_buf[gl_pos-1];
        gl_buf[gl_pos-1] = gl_buf[gl_pos];
        gl_buf[gl_pos] = c;
        gl_extent = 2;
        gl_fixup(gl_prompt, gl_pos-1, gl_pos);
    } /* else
        gl_putc('\007'); */
}

static void
  gl_newline()
/*
 * Cleans up entire line before returning to caller. A \n is appended.
 * If line longer than screen, we redraw starting at beginning
 */
{
    int change = gl_cnt;
    int len = gl_cnt;
    int loc = gl_width - 5;     /* shifts line back to start position */

    if (gl_cnt >= BUF_SIZE - 1)
        gl_error("\n*** Error: getline(): input buffer overflow\n");
    if (gl_out_hook) {
        change = gl_out_hook(gl_buf);
        len = strlen(gl_buf);
    }
    if( gl_erase_line ) {
      char gl_buf0 = gl_buf[0];
      gl_buf[0] = '\0';
      gl_fixup("", 0, 0);
      gl_buf[0] = gl_buf0;
    }
    else {
      if (loc > len)
        loc = len;
      gl_fixup(gl_prompt, change, loc); /* must do this before appending \n */
      gl_putc('\n');
    }
    gl_buf[len] = '\n';
    gl_buf[len+1] = '\0';
}

static void
  gl_del(loc)
int loc;
/*
 * Delete a character.  The loc variable can be:
 *    -1 : delete character to left of cursor
 *     0 : delete character under cursor
 */
{
    int i;

    if ((loc == -1 && gl_pos > 0) || (loc == 0 && gl_pos < gl_cnt)) {
        for (i=gl_pos+loc; i < gl_cnt; i++)
            gl_buf[i] = gl_buf[i+1];
        gl_fixup(gl_prompt, gl_pos+loc, gl_pos+loc);
    } /* else
        gl_putc('\007'); */
}

static void
  gl_delword()
/*
 * Delete previous word (^J/LF on VMS)
 */
{
    static char punct[] = " `!$&(-=+]}\\|;:'\",<.>?";
    int i = gl_pos;

    while( i > 0 && strchr( punct, gl_buf[i-1] ) != NULL )
      i--;
    while( i > 0 && strchr( punct, gl_buf[i-1] ) == NULL )
      i--;

    strcpy( gl_buf + i, gl_buf + gl_pos );
    gl_fixup(gl_prompt, i, i );
}

static void
  gl_kill()
/* delete from current position to the end of line */
{
    if (gl_pos < gl_cnt) {
        strcpy(gl_killbuf, gl_buf + gl_pos);
        gl_buf[gl_pos] = '\0';
        gl_fixup(gl_prompt, gl_pos, gl_pos);
    } /* else
        gl_putc('\007'); */
}

static void
  gl_redraw()
/* emit a newline, reset and redraw prompt and current input line */
{
    if (gl_init_done > 0) {
        gl_putc('\n');
        gl_fixup(gl_prompt, -2, gl_pos);
    }
}

static void
  gl_fixup( const char *prompt,
           int change,
           int cursor )
/*
 * This function is used both for redrawing when input changes or for
 * moving within the input line.  The parameters are:
 *   prompt:  compared to last_prompt[] for changes;
 *   change : the index of the start of changes in the input buffer,
 *            with -1 indicating no changes, -2 indicating we're on
 *            a new line, redraw everything.
 *   cursor : the desired location of the cursor after the call.
 *            A value of BUF_SIZE can be used  to indicate the cursor should
 *            move just past the end of the input line.
 */
{
    static int   gl_shift;      /* index of first on screen character */
    static int   off_right;     /* true if more text right of screen */
    static int   off_left;      /* true if more text left of screen */
    static char *last_prompt = NULL;
    int          left = 0, right = -1;          /* bounds for redraw */
    int          pad;           /* how much to erase at end of line */
    int          backup;        /* how far to backup before fixing */
    int          new_shift;     /* value of shift based on cursor */
    int          extra;         /* adjusts when shift (scroll) happens */
    int          i;
    int          new_right = -1; /* alternate right bound, using gl_extent */
    int          l1, l2;

    if( last_prompt == NULL )
      last_prompt = strdup( "" );

    if (change == -2) {   /* reset */
        gl_pos = gl_cnt = gl_shift = off_right = off_left = 0;
        gl_passwd = 0;
        gl_puts(prompt);
        gl_passwd = gl_no_echo;
        free( last_prompt );
        last_prompt = strdup( prompt );
        change = 0;
        gl_width = gl_termw - strlen(prompt);
    } else if (strcmp(prompt, last_prompt) != 0) {
        l1 = strlen(last_prompt);
        l2 = strlen(prompt);
        gl_cnt = gl_cnt + l1 - l2;
        free( last_prompt );
        last_prompt = strdup( prompt );
        backup = gl_pos - gl_shift + l1;
        for (i=0; i < backup; i++)
            gl_putc('\b');
        gl_passwd = 0;
        gl_puts(prompt);
        gl_passwd = gl_no_echo;
        gl_pos = gl_shift;
        gl_width = gl_termw - l2;
        change = 0;
    }
    pad = (off_right)? gl_width - 1 : gl_cnt - gl_shift;   /* old length */
    backup = gl_pos - gl_shift;
    if (change >= 0) {
        gl_cnt = strlen(gl_buf);
        if (change > gl_cnt)
            change = gl_cnt;
    }
    if (cursor > gl_cnt) {
        /* BUF_SIZE means end of line */
        /* if (cursor != BUF_SIZE)
            gl_putc('\007'); */
        cursor = gl_cnt;
    }
    if (cursor < 0) {
        /* gl_putc('\007'); */
        cursor = 0;
    }
    if (off_right || (off_left && cursor < gl_shift + gl_width - gl_scroll / 2))
        extra = 2;                      /* shift the scrolling boundary */
    else
        extra = 0;
    new_shift = cursor + extra + gl_scroll - gl_width;
    if (new_shift > 0) {
        new_shift /= gl_scroll;
        new_shift *= gl_scroll;
    } else
        new_shift = 0;
    if (new_shift != gl_shift) {        /* scroll occurs */
        gl_shift = new_shift;
        off_left = (gl_shift)? 1 : 0;
        off_right = (gl_cnt > gl_shift + gl_width - 1)? 1 : 0;
        left = gl_shift;
        new_right = right = (off_right)? gl_shift + gl_width - 2 : gl_cnt;
    } else if (change >= 0) {           /* no scroll, but text changed */
        if (change < gl_shift + off_left) {
            left = gl_shift;
        } else {
            left = change;
            backup = gl_pos - change;
        }
        off_right = (gl_cnt > gl_shift + gl_width - 1)? 1 : 0;
        right = (off_right)? gl_shift + gl_width - 2 : gl_cnt;
        new_right = (gl_extent && (right > left + gl_extent))?
                     left + gl_extent : right;
    }
    pad -= (off_right)? gl_width - 1 : gl_cnt - gl_shift;
    pad = (pad < 0)? 0 : pad;
    if (left <= right) {                /* clean up screen */
        for (i=0; i < backup; i++)
            gl_putc('\b');
        if (left == gl_shift && off_left) {
            gl_putc('$');
            left++;
        }
        for (i=left; i < new_right; i++)
            gl_putc(gl_buf[i]);
        gl_pos = new_right;
        if (off_right && new_right == right) {
            gl_putc('$');
            gl_pos++;
        } else {
            for (i=0; i < pad; i++)     /* erase remains of prev line */
                gl_putc(' ');
            gl_pos += pad;
        }
    }
    i = gl_pos - cursor;                /* move to final cursor location */
    if (i > 0) {
        while (i--)
           gl_putc('\b');
    } else {
        for (i=gl_pos; i < cursor; i++)
            gl_putc(gl_buf[i]);
    }
    gl_pos = cursor;
}

static int
  gl_tab(buf, offset, loc)
char  *buf;
int    offset;
int   *loc;
/* default tab handler, acts like tabstops every 8 cols */
{
    int i, count, len;

    len = strlen(buf);
    count = 8 - (offset + *loc) % 8;
    for (i=len; i >= *loc; i--)
        buf[i+count] = buf[i];
    for (i=0; i < count; i++)
        buf[*loc+i] = ' ';
    i = *loc;
    *loc = i + count;
    return i;
}

/******************* History stuff **************************************/

#ifndef HIST_SIZE
#define HIST_SIZE 100
#endif

static int      hist_pos = 0, hist_last = 0;
static char    *hist_buf[HIST_SIZE];

static void
  hist_init()
{
    int i;

    hist_buf[0] = "";
    for (i=1; i < HIST_SIZE; i++)
        hist_buf[i] = (char *)0;
}

void
  gl_histadd( const char *buf )
{
    static char *prev = 0;
    const char *p = buf;
    int len;

    while (*p == ' ' || *p == '\t' || *p == '\n')
        p++;
    if (*p) {
        len = strlen(buf);
        if (strchr(p, '\n'))    /* previously line already has NL stripped */
            len--;
        if (prev == 0 || strlen(prev) != len ||
                            strncmp(prev, buf, len) != 0) {
            hist_buf[hist_last] = hist_save(buf);
            prev = hist_buf[hist_last];
            hist_last = (hist_last + 1) % HIST_SIZE;
            if (hist_buf[hist_last] && *hist_buf[hist_last]) {
                free(hist_buf[hist_last]);
            }
            hist_buf[hist_last] = "";
        }
    }
    hist_pos = hist_last;
}

static char *
  hist_prev()
/* loads previous hist entry into input buffer, sticks on first */
{
    char *p = 0;
    int   next = (hist_pos - 1 + HIST_SIZE) % HIST_SIZE;

    if (hist_buf[hist_pos] != 0 && next != hist_last) {
        hist_pos = next;
        p = hist_buf[hist_pos];
    }
    if (p == 0) {
        p = "";
        /* gl_putc('\007'); */
    }
    return p;
}

static char *
  hist_next()
/* loads next hist entry into input buffer, clears on last */
{
    char *p = 0;

    if (hist_pos != hist_last) {
        hist_pos = (hist_pos+1) % HIST_SIZE;
        p = hist_buf[hist_pos];
    }
    if (p == 0) {
        p = "";
        /* gl_putc('\007'); */
    }
    return p;
}

static char *
  hist_save(p)
char *p;
/* makes a copy of the string */
{
    char *s;
    int   len = strlen(p);
    char *nl = strchr(p, '\n');

    if (nl) {
        if ((s = malloc(len)) != 0) {
            strncpy(s, p, len-1);
            s[len-1] = 0;
        }
    } else {
        if ((s = malloc(len+1)) != 0) {
            strcpy(s, p);
        }
    }
    if (s == 0)
        gl_error("\n*** Error: hist_save() failed on malloc\n");
    return s;
}

/******************* Search stuff **************************************/

static char  search_prompt[101];  /* prompt includes search string */
static char  search_string[100];
static int   search_pos = 0;      /* current location in search_string */
static int   search_forw_flg = 0; /* search direction flag */
static int   search_last = 0;     /* last match found */

static void
  search_update(c)
int c;
{
    if (c == 0) {
        search_pos = 0;
        search_string[0] = 0;
        search_prompt[0] = '?';
        search_prompt[1] = ' ';
        search_prompt[2] = 0;
    } else if (c > 0) {
        search_string[search_pos] = c;
        search_string[search_pos+1] = 0;
        search_prompt[search_pos] = c;
        search_prompt[search_pos+1] = '?';
        search_prompt[search_pos+2] = ' ';
        search_prompt[search_pos+3] = 0;
        search_pos++;
    } else {
        if (search_pos > 0) {
            search_pos--;
            search_string[search_pos] = 0;
            search_prompt[search_pos] = '?';
            search_prompt[search_pos+1] = ' ';
            search_prompt[search_pos+2] = 0;
        } else {
            gl_putc('\007');
            hist_pos = hist_last;
        }
    }
}

static void
  search_addchar(c)
int  c;
{
    char *loc;

    search_update(c);
    if (c < 0) {
        if (search_pos > 0) {
            hist_pos = search_last;
        } else {
            gl_buf[0] = 0;
            hist_pos = hist_last;
        }
        strcpy(gl_buf, hist_buf[hist_pos]);
    }
    if ((loc = strstr(gl_buf, search_string)) != 0) {
        gl_fixup(search_prompt, 0, loc - gl_buf);
    } else if (search_pos > 0) {
        if (search_forw_flg) {
            search_forw(0);
        } else {
            search_back(0);
        }
    } else {
        gl_fixup(search_prompt, 0, 0);
    }
}

static void
  search_term()
{
    gl_search_mode = 0;
    if (gl_buf[0] == 0)         /* not found, reset hist list */
        hist_pos = hist_last;
    if (gl_in_hook)
        gl_in_hook(gl_buf);
    gl_fixup(gl_prompt, 0, gl_pos);
}

static void
  search_back(new_search)
int new_search;
{
    int    found = 0;
    char  *p, *loc;

    search_forw_flg = 0;
    if (gl_search_mode == 0) {
        search_last = hist_pos = hist_last;
        search_update(0);
        gl_search_mode = 1;
        gl_buf[0] = 0;
        gl_fixup(search_prompt, 0, 0);
    } else if (search_pos > 0) {
        while (!found) {
            p = hist_prev();
            if (*p == 0) {              /* not found, done looking */
               gl_buf[0] = 0;
               gl_fixup(search_prompt, 0, 0);
               found = 1;
            } else if ((loc = strstr(p, search_string)) != 0) {
               strcpy(gl_buf, p);
               gl_fixup(search_prompt, 0, loc - p);
               if (new_search)
                   search_last = hist_pos;
               found = 1;
            }
        }
    } else {
        gl_putc('\007');
    }
}

static void
  search_forw(new_search)
int new_search;
{
    int    found = 0;
    char  *p, *loc;

    search_forw_flg = 1;
    if (gl_search_mode == 0) {
        search_last = hist_pos = hist_last;
        search_update(0);
        gl_search_mode = 1;
        gl_buf[0] = 0;
        gl_fixup(search_prompt, 0, 0);
    } else if (search_pos > 0) {
        while (!found) {
            p = hist_next();
            if (*p == 0) {              /* not found, done looking */
               gl_buf[0] = 0;
               gl_fixup(search_prompt, 0, 0);
               found = 1;
            } else if ((loc = strstr(p, search_string)) != 0) {
               strcpy(gl_buf, p);
               gl_fixup(search_prompt, 0, loc - p);
               if (new_search)
                   search_last = hist_pos;
               found = 1;
            }
        }
    } else {
        gl_putc('\007');
    }
}
#endif
