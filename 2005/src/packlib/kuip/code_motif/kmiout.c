/*
 * $Id: kmiout.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmiout.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 06/01/95  17.10.55  by  N.Cremel*/
/*-- Author :    N.Cremel   03/04/92*/
/***********************************************************************
 *                                                                     *
 *   Input / Output re-direction                                       *
 *                                                                     *
 *   (from file init.c)                                                *
 *                                                                     *
 ***********************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/klink.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/ksig.h"

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#ifdef VMS
#include <processes.h>
#include <unixio.h>
#endif /* VMS */

#include "mkdims.h"
#include "mkutfm.h"
#include "mkutdm.h"
#include "kuip/mkutfu.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxsc.h"
#include "mkuxs1.h"
#undef CONTEXT_MACRO_ACCESS
#include "kuip/mkterm.h"
#if defined(CERNLIB_VMS)
#include "kuip/mkqio.h"
#endif


#if 0
/* Icon file for help */
#define help_width 50
#define help_height 50
static char help_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x0f, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xfe, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7f, 0xfe, 0x00, 0x00,
   0x00, 0x00, 0x80, 0x07, 0xe0, 0x01, 0x00, 0x00, 0x00, 0xc0, 0x03, 0xc0,
   0x03, 0x00, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0xe0,
   0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x07, 0x00, 0x00,
   0x00, 0xe0, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x03,
   0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1e, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0xfc, 0xc3, 0x00, 0x00, 0x08,
   0x00, 0x00, 0x06, 0xc4, 0xc1, 0x00, 0x08, 0x00, 0x00, 0x02, 0x22, 0x81,
   0x00, 0x08, 0x00, 0x00, 0x02, 0x22, 0x01, 0x01, 0x08, 0x00, 0x00, 0x03,
   0x22, 0x01, 0x01, 0x08, 0x00, 0x00, 0x01, 0x21, 0x02, 0x02, 0x08, 0x00,
   0x00, 0x81, 0x21, 0x02, 0x02, 0x08, 0x00, 0x00, 0x61, 0x20, 0x02, 0x72,
   0x08, 0x00, 0x00, 0x1d, 0x00, 0x06, 0x54, 0x08, 0x00, 0x00, 0x01, 0x10,
   0x04, 0x94, 0x08, 0x00, 0x00, 0x01, 0x10, 0x06, 0x94, 0x04, 0x00, 0x00,
   0x01, 0xfc, 0x0b, 0x1c, 0x07, 0x00, 0x00, 0x01, 0x10, 0x18, 0x18, 0x03,
   0x00, 0x00, 0x01, 0x08, 0x30, 0x18, 0x00, 0x00, 0x00, 0x01, 0x08, 0x60,
   0x18, 0x00, 0x00, 0x80, 0x01, 0x08, 0x40, 0x10, 0x00, 0x00, 0x80, 0x01,
   0x0c, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};
#endif


#define MAX_CMD_LENGTH  512
#define INC_LENGTH      128


/* extern data */
extern Widget km_scrollOK;
extern Widget km_scrollOK1;
extern Widget km_scrollCL1;

extern XmFontList    help_font;
extern XFontStruct  *help_font_struct;

extern void  (*user_mail_C)( /* Widget */ );

/***********************************************************************
 *                                                                     *
 *   Forward declarations of extern functions.                         *
 *                                                                     *
 ***********************************************************************/
extern C_PROTO_1(Widget create_kuipScroll, Widget);
extern C_PROTO_1(Widget create_kuipScroll1, Widget);
extern C_PROTO_2(void kxterm_show_browser, char **, int);
extern C_PROTO_2(void kxterm_show_panel, char **, int);
extern C_PROTO_2(void kxterm_set_panel, char **, int);
extern C_PROTO_2(void kxterm_set_echo_mode, char **, int);
extern C_PROTO_2(void kxterm_iconify, char **, int);
extern C_PROTO_2(void kxterm_raise, char **, int);
extern C_PROTO_2(void kxterm_help, char **, int);
extern C_PROTO_2(void kxterm_geometry, char **, int);
extern C_PROTO_2(void kxterm_about, char **, int);
extern C_PROTO_2(void kxterm_mail, char **, int);


/***********************************************************************
 *                                                                     *
 *   Forward declarations of global functions.                         *
 *                                                                     *
 ***********************************************************************/
C_PROTO_1(void km_WarnCB,
                         String message);
C_PROTO_1(Widget km_kuipScroll,
                         int help_flag);
C_PROTO_1(Widget km_kuipScroll1,
                         int help_flag);

/* local functions */
static C_PROTO_3(void destroy_scroll,
                         Widget w,
                         Widget dialog,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_3(void clear_scroll,
                         Widget w,
                         Widget dialog,
                         XmAnyCallbackStruct *call_data);
static C_PROTO_2(char *max_string,
                         char *str, Dimension *dh);


/* local data */
static Widget last_dialog = NULL;


/***********************************************************************
 *                                                                     *
 *   Kxterm menu and action definitions.                               *
 *                                                                     *
 ***********************************************************************/
static char *view_menu[] = {
   "M:View: Sep :                      : separator",
   "M:View: Command Panel  : #@panel   : button: C: <Key>F1: F1",
   "M:View: New Command Panel : #@newpanel : button: N: <Key>F2: F2",
   "M:View: Sep :                      : separator",
   "M:View: Browser        : #@browser : button: B: <Key>F3: F3",
   NULL
};

static char *option_menu[] = {
   "M:Option: Sep :                           : separator",
   "M:Option: Echo Command : #@echo           : toggle1 : E",
   "M:Option: Timing       : timing           : toggle  : T",
   "M:Option: Sep          :                  : separator",
   "M:Option: Iconify      : #@iconify(UNMAP) : button  : I",
   "M:Option: Raise Window : #@raise          : button  : R",
   NULL
};

static char *help_menu[] = {
   "M:Help: Sep :                                            : separator",
   "M:Help: On Application      : #@help(HELP_EXE)           : button",
   "M:Help: On Application Resources  : #@help(HELP_EXE_RESOURCES) : button",
   "M:Help: On Kuip Resources   : #@help(HELP_RESOURCES)     : button",
   "M:Help: On Browser          : #@help(HELP_BROWSER)       : button",
   "M:Help: On Panel            : #@help(HELP_PANEL)         : button",
   "M:Help: On System Functions : #@help(FUNCTIONS)          : button",
   NULL
};

/*
 * Action commands that can be send by kxterm to the application
 * (the commands "geometry" and "about" are predifined in kxterm).
 */
static KxtermActionsRec actions[] = {
   {"panel",      kxterm_show_panel},      /* for Command Panel menu button */
   {"newpanel",   kxterm_set_panel},       /* for New Command Panel */
   {"browser",    kxterm_show_browser},    /* for Browser menu button */
   {"echo",       kxterm_set_echo_mode},   /* for Echo Command menu toggle */
   {"iconify",    kxterm_iconify},         /* for Iconify menu button */
   {"raise",      kxterm_raise},           /* for Raise menu button */
   {"help",       kxterm_help},            /* for Help menu buttons */
   {"geometry",   kxterm_geometry},        /* sent when moving kxterm around */
   {"about",      kxterm_about},           /* for Application about button */
   {"mail",       kxterm_mail},            /* for Application mail dialog */
   {NULL,         NULL},
};


/***********************************************************************
 *                                                                     *
 *   Handle warning messages.                                          *
 *                                                                     *
 ***********************************************************************/
void km_WarnCB(message)
       String message;
{
   /* warning coming from popup menu, ignore */
   if (strstr(message,"non-existant passive grab"))
      return;

   /* warning issued when tearing off an already teared off menu, ignore */
   if (strstr(message,"XtRemoveGrab asked to remove a widget not on the list"))
      return;

   fprintf(stderr, "Warning: %s\n", message);
}

/***********************************************************************
 *                                                                     *
 *   Exec program "cmd" and set up pipes between it and us.            *
 *                                                                     *
 ***********************************************************************/
static void talkto( cmd, argv )
     char *cmd;
     char **argv;
{
   int   to_child[2];   /* pipe descriptors from parent->child */
   int   to_parent[2];  /* pipe descriptors from child->parent */
   int   pid;

#ifdef VMS
   int   sts, fd, fn;
   char  mbxname[256];
   char  mbx_to_child[256], mbx_to_parent[256];
   FILE  *fp;

   sts = pipe(to_child);
   if (sts != 0) {
      fprintf(stderr,"pipe(to_child) error %d\n",sts);
      kc_flags.use_kxterm = 0;
      return;
   }
   getname(to_child[1],mbx_to_child,1);

   sts = pipe(to_parent);
   if (sts != 0) {
      fprintf(stderr,"pipe(to_parent) error %d\n",sts);
      kc_flags.use_kxterm = 0;
      close(to_child[0]);
      close(to_child[1]);
      return;
   }
   getname(to_parent[0],mbx_to_parent,1);

   if ((pid = vfork()) == 0) {     /* in the child      */

      fd = dup2(to_child[0],0);    /* redirect stdin    */
      fd = dup2(to_parent[1],1);   /* redirect stdout   */
      execv(cmd, argv);            /* exec the new cmd  */

   } else if (pid > 0) {           /* in the parent     */

      /* redirect stdin  */
      fd = dup2(to_parent[0],0);
      fp = freopen(mbx_to_parent,"r",stdin);
      if (fp == NULL) fprintf(stderr,"freopen stdin error\n");

      /* redirect stdout */
      fd = dup2(to_child[1],1);
      fp = freopen(mbx_to_child,"w",stdout);
      if (fp == NULL) fprintf(stderr,"freopen stdout error\n");
      /*
       * use line buffering because mailbox uses record I/O and
       * each printf/putchar would give a separate line
       */
      setvbuf(stdout, NULL, _IOLBF, 8192);

      /* redirect stderr */
      fd = dup2(to_child[1],2);
      fp = freopen(mbx_to_child,"w",stderr);
      if (fp == NULL) fprintf(stderr,"freopen stderr error\n");
      setbuf(stderr, NULL);

      /* define logical names for FORTRAN I/O */

      getname(fileno(stdin),mbxname,1);
      VMScrelnm("SYS$INPUT",mbxname);
      VMScrelnm("SYS$COMMAND",mbxname);
      VMScrelnm("FOR$ACCEPT",mbxname);

      getname(fileno(stdout),mbxname,1);
      VMScrelnm("SYS$OUTPUT",mbxname);
      VMScrelnm("FOR$PRINT",mbxname);
      VMScrelnm("FOR$TYPE",mbxname);

      close(to_child[0]);          /* close pipes */
      close(to_child[1]);
      close(to_parent[0]);
      close(to_parent[1]);

      /*
       * wait for handshake from child;  otherwise part of the welcome message
       * may be lost if kxterm start-up is too slow
       */
      getchar();

   }

#else /* VMS */

   if( pipe(to_child) != 0 ) {
     fprintf(stderr,"Cannot open pipe to child.\n");
     kc_flags.use_kxterm = 0;
     return;
   }

   if( pipe(to_parent) != 0 ) {
     fprintf(stderr,"Cannot open pipe to parent.\n");
     kc_flags.use_kxterm = 0;
     close(to_child[0]);
     close(to_child[1]);
     return;
   }

   if ((pid = vfork()) == 0) {     /* in the child    */

      close(0);                    /* redirect stdin  */
      dup(to_child[0]);
      close(to_child[0]);
      close(1);                    /* redirect stdout */
      dup(to_parent[1]);
      close(to_parent[1]);
      close(to_child[1]);          /* close pipes used by parent */
      close(to_parent[0]);

      execv(cmd, argv);            /* exec the new cmd */
      fprintf(stderr,"Could execute %s.\n", cmd);
      _exit( 0 );

   } else if (pid > 0) {           /* in the parent   */

      close(0);                    /* redirect stdin  */
      dup(to_parent[0]);
      close(to_parent[0]);
      close(1);                    /* redirect stdout */
      dup(to_child[1]);
      close(2);                    /* redirect stderr */
      dup(to_child[1]);
      close(to_child[1]);
      setbuf(stdout, NULL);        /* no buffered output */
      setbuf(stderr, NULL);
      close(to_child[0]);          /* close pipes used by child */
      close(to_parent[1]);

      /*
       * wait for handshake from child;  otherwise part of the welcome message
       * may be lost if kxterm start-up is too slow
       */
      getchar();

   }

#endif /* VMS */

   else {                          /* error!       */
      fprintf(stderr,"Couldn't fork process %s.\n", cmd);
      fprintf(stderr,"Your simple xterm will now handle I/O.\n");
      kc_flags.use_kxterm = 0;
      close(to_child[0]);
      close(to_child[1]);
      close(to_parent[0]);
      close(to_parent[1]);
   }
}

/***********************************************************************
 *                                                                     *
 *   Returns next command from the command buffer. Commands are        *
 *   delimited by a \n. When no command is found the routine returns   *
 *   NULL. The returned command is removed from the command buffer.    *
 *   A line ending with an _ is concatenated with the next line.       *
 *   Space for the command will be allocated and must be freed by the  *
 *   caller. When the last line in the command buffer ends with an _   *
 *   it stays in the buffer and the routine returns NULL.              *
 *                                                                     *
 ***********************************************************************/
static char *get_command( cmdbuf )
     char *cmdbuf;
{
   char    *s, *s2, *cmd, *tbuf;
   int      max_length = MAX_CMD_LENGTH;

   cmd = XtCalloc(max_length, 1);

   /*
    * Search for a \n.
    */
   tbuf = cmdbuf;
   s = strchr(tbuf, '\n');
   while (s) {
      /*
       * Copy command to cmd
       */
      while ((s-tbuf) + strlen(cmd) + 1 > max_length) {
         max_length += INC_LENGTH;
         cmd = XtRealloc(cmd, max_length);
      }

      strncat(cmd, tbuf, s-tbuf);

      /*
       * Handle concatenation
       */
      s2 = km_strend(cmd, '_');
      if (s2) {
         *s2 = '\0';      /* remove _ */
         s2 = strchr(s+1, '\n');
         if (s2) {
            tbuf = s+1;
            s = s2;
         } else
            s = NULL;
      } else {
         strcpy(cmdbuf, s+1);
         return cmd;
      }
   }
   XtFree(cmd);
   return NULL;
}

/***********************************************************************
 *                                                                     *
 *   Execute a command coming from the kxterm process.                 *
 *                                                                     *
 ***********************************************************************/
static void execute_command( w, fid, id )
     Widget w;
     int *fid;
     XtInputId *id;
{
   char     buf[BUFSIZ];
   char    *cmd;
   int      status = 0;
   int      nbytes;
   static int    max_length = MAX_CMD_LENGTH;
   static char  *cmdbuf;

   if (!cmdbuf)
      cmdbuf = XtCalloc(max_length, 1);

   /*
    * Get all pending input from pipe.
    */
#ifdef VMS
   while ((nbytes = MbxRead(buf, BUFSIZ-1, (Widget)0)) > 0) {
#else  /* VMS */
   while ((nbytes = read(*fid, buf, BUFSIZ-1)) > 0) {
#endif /* VMS */
      /*
       * Null terminate the string.
       */
      buf[nbytes] = '\0';

      while (strlen(cmdbuf) + nbytes + 1 > max_length) {
         max_length += INC_LENGTH;
         cmdbuf = XtRealloc(cmdbuf, max_length);
      }

      strcat(cmdbuf, buf);

      /* break loop if last character was a \n */
      if (buf[nbytes-1] == '\n')
         break;
   }

   /*
    * Cancel interrupts which are pending because ^C was typed in Kxterm
    * before entering the command.
    */
   kc_break.intr_pending = 0;

   while (status != KUMAC_UNWIND && (cmd = get_command(cmdbuf)) != NULL) {

      if (!km_strempty(cmd)) {

         if (strlen(cmd) > MAX_CMD_LENGTH)
            printf("*** Command too long ***\n");
         else {
            /* execute command */
            if (!strncmp(cmd, ESCAPE, strlen(ESCAPE)))
               handle_kxterm_action(cmd);  /* handle special commands */
            else {

               int old_echo = kc_flags.echo_command;
               char *s = format_prompt( kc_flags.curr_prompt );
               km_all_cursor(KM_WAIT_CURSOR);
               kc_flags.echo_command = 0;
               if (s[strlen(s)-1] == ' ')
                  printf( "%s%s\n", s, cmd );
               else
                  printf( "%s %s\n", s, cmd );
               status = ku_exec(cmd);
               ku_last(cmd);
               kc_flags.echo_command = old_echo;
               km_reset_allbrowser(True); /* overkill, change later */
               km_all_cursor(KM_MAIN_CURSOR);
               kc_break.intr_count = 0;
            }
         }
      }
      XtFree(cmd);
   }

   /*
    * Send STX (start-transmission) character to tell kxterm we are
    * ready for a next command.
    */
   if (kc_flags.use_kxterm) {
      char *prompt = str3dup( "prompt(",
                             format_prompt( kc_flags.curr_prompt ), ")" );
      send_single_kxterm_cmd( prompt );
      free( prompt );
      printf("\2");
      fflush( stdout );
   }
}

/***********************************************************************
 *                                                                     *
 *   Start the kxterm KUIP terminal emulator.                          *
 *                                                                     *
 *   The following happens in this routine:                            *
 *   - Listen for input coming from stdin.                             *
 *   - Fork kxterm and redirect stdin, stdout and stderr, via pipes,   *
 *     from and to kxterm.                                             *
 *   - Send application menu's to kxterm.                              *
 *                                                                     *
 ***********************************************************************/
int start_kxterm( title, geometry, font, text_font, prompt )
     char *title;
     char *geometry;
     char *font;
     char *text_font;
     char *prompt;
{
   char  *argv[15];
   char   pid[10];
   char  *kxterm;

   int status = 0;

   /*
    * Add callback execute_command() -- invoked when input is available
    * from stdin
    */
#ifdef VMS
   /*
    * Create a lock which we can use on the kxterm side to see if the
    * application process is still alive.
    * Kxterm is not allowed to exit before the application process because
    * otherwise it can get stuck waiting for the mailbox message to be read.
    */
   int alive_lock[2];
   char lock_name[32];
   struct dsc$descriptor_s lock_dsc;

   sprintf( lock_name, "ALIVE%d", getpid() );
   var_descriptor( lock_dsc, lock_name );
   sys$enqw( 0, LCK$K_EXMODE, alive_lock, 0,
            &lock_dsc, 0, NULL, 0, NULL, PSL$C_USER, 0 );

   XtAppAddInput(UxAppContext,
                 MBX_EVENT_FLAG,         /* On OpenVMS, wait on event flag  */
                 &MbxMessage.ioStatus,   /* I/O Status Block                */
                 (XtInputCallbackProc)execute_command,
                 NULL);
#else  /* VMS */
   XtAppAddInput(UxAppContext, fileno(stdin),
                 (XtPointer)XtInputReadMask,
                 (XtInputCallbackProc)execute_command,
                 NULL);
#endif /* VMS */

   /*
    * If kxterm can be found in PATH, start it.
    */
   if( (kxterm = getenv( "KXTERM" )) == NULL )
     kxterm = ku_qexe( "kxterm" );

   if ((kc_flags.use_kxterm = (kxterm != NULL))) {

      /*
       * Exec the program "kxterm" and set up pipes.
       *
       * argv[1] is the class name used for setting kxterm resources.
       */
      sprintf(pid, "%d", (int)getpid());
      argv[0]  = strdup(kxterm);
      argv[1]  = str2dup("Kx", prompt);
      argv[2]  = strdup("-name");
      argv[3]  = strdup(title);
      argv[4]  = strdup("-geometry");
      argv[5]  = strdup(geometry);
      argv[6]  = strdup("-pid");
      argv[7]  = strdup(pid);
      argv[8]  = strdup("-prompt");
      argv[9]  = str2dup(prompt, "> ");
      argv[10]  = strdup("-font");
      argv[11] = strdup(font);
      argv[12] = strdup("-tfont");
      argv[13] = strdup(text_font);
      argv[14] = NULL;
      talkto(argv[0], argv);
   } else {
      fprintf(stderr,"\
Couldn't find the KUIP terminal emulator kxterm.\n\
Check your environment variables KXTERM and PATH.\n\
Your present terminal window will now handle I/O.\n");
      status = -1;
   }

   if (kc_flags.use_kxterm) {

      /* fix the help menu to include the real application name */
      char s[128], s1[128];

      /* On VMS, set up the mailbox after talkto sets up the pipes */
#ifdef VMS
      MbxSetup();
#endif /* VMS */

      sprintf(s, "M:Help: On %s : #@help(HELP_EXE) : button", prompt);
      help_menu[1] = s;
      sprintf(s1,
      "M:Help: On %s Resources : #@help(HELP_EXE_RESOURCES) : button", prompt);
      help_menu[2] = s1;

      /*
       * Create View, Option and Help menus in kxterm
       */
      send_kxterm_cmd(view_menu);
      send_kxterm_cmd(option_menu);
      send_kxterm_cmd(help_menu);

      /*
       *  In case the application has specified a mail action routine
       *  add the "Mail Developers" item to the Help menu in kxterm.
       */
      if (user_mail_C) {
         sprintf(s, "M:Help: Sep :   : separator");
         send_single_kxterm_cmd(s);
         sprintf(s, "M:Help: Mail %s Developers : #@mail : button", prompt);
         send_single_kxterm_cmd(s);
      }

      /*
       * Register kxterm actions
       */
      kxterm_add_actions(actions);
   }

   return status;
}

/***********************************************************************
 *                                                                     *
 *   Set up HELP or normal text in a scrolled window.                  *
 *   (Widget is created by Architect : create_kuipScroll)              *
 *                                                                     *
 ***********************************************************************/
Widget km_kuipScroll(help_flag)
   int help_flag;  /* 0 : only scrolled text (not necessarily for a Help),
                      1 : Help scrolled window (command or Help item in CDF) */
{
   Widget   dialog;

#if 0
   /* all Help appears centered in the "Main Browser" */
   dialog = create_kuipScroll(km_main_browser);
#else
   /* all Help appears centered in the "Executive Window" (as all the popup
      dialogs) */
   dialog = create_kuipScroll(km_toplevel);
#endif
   UxKuipScrollContext = (_UxCkuipScroll *)UxGetContext(dialog);

   XtVaSetValues(XtParent(dialog), XmNdeleteResponse, XmDESTROY, NULL);

   if (help_flag) {
#if 0
      Pixmap        pixmap;
      Display      *display = XtDisplay(dialog);

      /* set icon for help popup */
      pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                     help_bits, help_width, help_height);
      XtVaSetValues(XtParent(dialog), XmNiconPixmap, pixmap, NULL);
#endif

      /* set help_font (proportional) if defined */
      if (help_font != NULL )
          XtVaSetValues(scrollText, XmNfontList, help_font, NULL);
   }

   /* pass widget ID to application */
   if (klnkaddr.user_TopWid_C != NULL)
      (*klnkaddr.user_TopWid_C)("kuipScroll",dialog);

   XmTextSetAddMode(scrollText, True);

   /*
    *   install callbacks
    */
   XtAddCallback(km_scrollOK, XmNactivateCallback,
                 (XtCallbackProc)destroy_scroll, dialog);
   XtAddCallback(dialog, XmNdestroyCallback,
                 (XtCallbackProc)destroy_scroll, dialog);
   /* This is to avoid the window to disappear under kxterm */
   XtAddEventHandler (dialog, FocusChangeMask, False,
                      (XtEventHandler)km_focus_window, NULL);


   return (dialog);
}

/***********************************************************************
 *                                                                     *
 *   Set up HELP or normal text in a scrolled window.                  *
 *   (Widget is created by Architect : create_kuipScroll1)             *
 *   N.B. Same as km_kuipScroll but with a "Clear" button.             *
 *                                                                     *
 ***********************************************************************/
Widget km_kuipScroll1(help_flag)
   int help_flag;  /* 0 : only scrolled text (not necessarily for a Help),
                      1 : Help scrolled window (command or Help item in CDF) */
{
   Widget   dialog;

   /* all Help appears centered in the "Executive Window" (as all the popup
      dialogs) */
   dialog = create_kuipScroll1(km_toplevel);
   UxKuipScroll1Context = (_UxCkuipScroll1 *)UxGetContext(dialog);

   XtVaSetValues(XtParent(dialog), XmNdeleteResponse, XmDESTROY, NULL);

   if (help_flag) {
      /* set help_font (proportional) if defined */
      if (help_font != NULL )
          XtVaSetValues(scrollText1, XmNfontList, help_font, NULL);
   }

   /* pass widget ID to application */
   if (klnkaddr.user_TopWid_C != NULL)
      (*klnkaddr.user_TopWid_C)("kuipScroll1",dialog);

   XmTextSetAddMode(scrollText1, True);

   /*
    *   install callbacks
    */
   XtAddCallback(km_scrollOK1, XmNactivateCallback,
                 (XtCallbackProc)destroy_scroll, dialog);
   XtAddCallback(dialog, XmNdestroyCallback,
                 (XtCallbackProc)destroy_scroll, dialog);
   XtAddCallback(km_scrollCL1, XmNactivateCallback,
                 (XtCallbackProc)clear_scroll, dialog);

   return (dialog);
}

C_DECL_5 (void km_display_sctext,
    char *, title,
    char *, text,
    Position, xr,
    Position, yr,
    int, flag)
{
    km_create_sctext (title, text, xr, yr, flag);
}

/***********************************************************************
 *                                                                     *
 *   Display some text in a scrollable window                          *
 *   (use same widget as the HELP dialog built with Architect).        *
 *                                                                     *
 *   For flag = 1 text is a HELP (use special font, pixmap and title)  *
 *                                                                     *
 ***********************************************************************/
C_DECL_5 (Widget km_create_sctext,
    char *, title,
    char *, text,
    Position, xr,
    Position, yr,
    int, flag)
{
   Widget        dialog;
   char          str[256];
   XmString      xmstr;
   char         *mtext;
   Dimension     dw = 0;
   Position      xp, yp;

   dialog = km_kuipScroll (flag);
   UxKuipScrollContext = (_UxCkuipScroll *)UxGetContext(dialog);

   /* Set window title, position, width and height */
   if (flag == 1) {
       strcpy(str, "Help on: ");
       strcat(str, title);
   } else {
       strcpy(str, title);
   }
   xmstr = XmStringCreateLtoR(str, XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(dialog, XmNdialogTitle, xmstr, NULL);
   XmStringFree(xmstr);

   /* calculate dialog width according to maximum text line length */
   if (help_font_struct != NULL && text != NULL) {
       Dimension dh;

       mtext = max_string (text, &dh);
       if (mtext != NULL) {
           dw = (Dimension) XTextWidth(help_font_struct, mtext, strlen(mtext));
       }
       dw += 50; dh += 150;
       XtVaSetValues(dialog, XmNwidth, dw, NULL);
       if (dh > 0 && dh < 350) XtVaSetValues(dialog, XmNheight, dh, NULL);
   }

   /*
    * Position dialog with on offset of 25 by 25 from the last dialog.
    */
   if (last_dialog) {
      XtVaGetValues(last_dialog, XmNx, &xp, XmNy, &yp, NULL);
      /* don't increase xp and yp, the window manager adds the border widths */
      XtVaSetValues(dialog, XmNdefaultPosition, False,
                            XmNx, xp, XmNy, yp, NULL);
   }

   last_dialog = dialog;

   /* insert text */
   if (text != NULL) km_insert_string(scrollText, text, False);

   UxPopupInterface(dialog, XtGrabNone);
   return (dialog);
}

/***********************************************************************
 *                                                                     *
 *   Find and return the maximum string in buffer str.                 *
 *                                                                     *
 ***********************************************************************/
static char *max_string( str, dh)
     char *str;
     Dimension *dh;
{
     int i;

     char *max_cval = NULL;
     char *buf = strdup (str);
     int mlen = 0;
     int p = 0;
     int nbl = 0;

     *dh = 0;

     if (str == NULL) return (NULL);

     for (i = 0; str[i] != '\0'; i++) {
          if (str[i] == '\n') {
              char *cval;
              int len;

              nbl ++;
              buf[i] = '\0';
              cval = strdup(buf+p);
              len = strlen(cval);
              if (help_font_struct != NULL) {
                  int dr, far, fdr;
                  XCharStruct overall;

                  if (len != 0) {
                      Dimension dh1;

                      XTextExtents(help_font_struct, cval, len,
                                   &dr, &far, &fdr, &overall);
                      dh1 = (Dimension) (overall.ascent + overall.descent);
                      if (dh1 > *dh) *dh = dh1;
                  }
              }

              if( len > mlen ) {
                  mlen = len;
                  max_cval = strdup(cval);
              }
              p = i+1;
              free (cval);
          }
     }
     *dh = *dh * nbl;
     free (buf);

     return (max_cval);
}

/***********************************************************************
 *                                                                     *
 *   Destroy the scroll window dialog (e.g. for HELP)                  *
 *                                                                     *
 ***********************************************************************/
static void destroy_scroll(w, dialog, call_data)
   Widget w;
   Widget dialog;
   XmAnyCallbackStruct *call_data;
{
    UxDestroyInterface(dialog);
    km_reset_appShell (dialog);

    if (dialog == km_scrOutput)  km_scrOutput  = NULL;
    if (dialog == km_HigzOutput) km_HigzOutput = NULL;
    if (dialog == last_dialog)   last_dialog   = NULL;
}

/***********************************************************************
 *                                                                     *
 *   Clear the scroll window dialog                                    *
 *                                                                     *
 ***********************************************************************/
static void clear_scroll(w, dialog, call_data)
   Widget w;
   Widget dialog;
   XmAnyCallbackStruct *call_data;
{
   XmTextPosition   pos;

   /* get top character and delete from beginning to top character */
   XtVaGetValues(scrollText1, XmNtopCharacter, &pos, NULL);

   XmTextReplace(scrollText1, (XmTextPosition)0, pos-1, NULL);
}


/***********************************************************************
 *                                                                     *
 *   Display some Text from FORTRAN in a scrolled text window          *
 *                                                                     *
 *                                                                     *
 ***********************************************************************/
void F77_ENTRY_C(Kumout,buftext)
/* {*/
  char *text = fstr0dup( buftext, len_buftext );
  char title[MAX_string];

  if (km_scrOutput == NULL ) {
      sprintf (title, "%s Output", class_name);
      km_scrOutput = km_create_sctext (title, text, 0, 0, 0);
  } else {
      UxKuipScrollContext = (_UxCkuipScroll *)UxGetContext(km_scrOutput);
      km_insert_string(scrollText, text, True);
  }
}

