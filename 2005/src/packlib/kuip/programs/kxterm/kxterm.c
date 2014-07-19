/*
 * $Id: kxterm.c,v 1.2 2005/04/18 15:23:10 mclareni Exp $
 *
 * $Log: kxterm.c,v $
 * Revision 1.2  2005/04/18 15:23:10  mclareni
 * Mods submitted by Kevin B. McCarty to fix insecure Cernlib uses of /tmp.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 10/01/95  08.59.54  by  N.Cremel*/
/*-- Author :    Fons Rademakers   8/01/93*/
/***********************************************************************
 *                                                                     *
 *   KXTERM -- Kuip Terminal Emulator.                                 *
 *                                                                     *
 *   This terminal emulator combines the best features from the (now   *
 *   defunct) Apollo DM pads (like: input and transcript pads,         *
 *   automatic file backup of transcript, search in transcript pad,    *
 *   etc.) and the Korn shell emacs-style command line editing and     *
 *   command line recall mechanism.                                    *
 *                                                                     *
 ***********************************************************************/

#include "kuip/kuip.h"

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/MessageB.h>
#include <X11/IntrinsicP.h>
#if defined(__hpux) && XtSpecificationRelease == 4
#include <X11/Protocols.h>
#else
#include <Xm/Protocols.h>
#endif
#include <X11/cursorfont.h>
#include <Xm/AtomMgr.h>

#include "menu.h"
#include "utils.h"
#include "actions.h"
#include "uxxt.h"
#if defined(CERNLIB_VMS)
#include "kuip/mkqio.h"
#endif

#define strrstr   my_strrstr
#if defined(__hpux)
#undef strrstr
#endif


#define MAX_CMD_LENGTH    512
#define INC_LENGTH        128
#define MAX_FILE_LENGTH   256
#define MAX_ITEMS          30

#define CURSOR              1
#define LINE                2

#define ESCAPE            "#@" /* escape sequence for special kxterm commands */
#define STX               '\2' /* Start Transmission: application is ready */


#ifdef _NO_PROTO
#  define ZEROargs
#else
#  define ZEROargs void
#endif

/* kxterm icon */
#define kxterm_width 50
#define kxterm_height 50
static char kxterm_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x1c, 0x00,
   0x00, 0x60, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x80, 0x01, 0x00, 0xc0,
   0x55, 0x55, 0x55, 0x55, 0x06, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x04,
   0x00, 0x40, 0x55, 0x55, 0x55, 0x55, 0x05, 0x00, 0x40, 0xaa, 0xaa, 0xaa,
   0xaa, 0x06, 0x00, 0x40, 0xfd, 0xff, 0xff, 0xff, 0x05, 0x00, 0x40, 0xfe,
   0xff, 0xff, 0x7f, 0x06, 0x00, 0x40, 0x0d, 0x00, 0xe0, 0x7f, 0x05, 0x00,
   0x40, 0xee, 0xff, 0xef, 0x7f, 0x06, 0x00, 0x40, 0xed, 0xff, 0xeb, 0x7f,
   0x05, 0x00, 0x40, 0xee, 0xff, 0xef, 0x7f, 0x06, 0x00, 0x40, 0xed, 0xff,
   0xef, 0x7f, 0x05, 0x00, 0x40, 0x0e, 0x00, 0xe0, 0x7f, 0x06, 0x00, 0x40,
   0xed, 0xff, 0xef, 0x7f, 0x05, 0x00, 0x40, 0x2e, 0xfe, 0xeb, 0x7f, 0x06,
   0x00, 0x40, 0xed, 0xff, 0x0f, 0x40, 0x05, 0x00, 0x40, 0xee, 0xff, 0xef,
   0x5f, 0x06, 0x00, 0x40, 0xed, 0xff, 0xef, 0x5f, 0x05, 0x00, 0x40, 0x0e,
   0x00, 0xe0, 0x5f, 0x06, 0x00, 0x40, 0xfd, 0x7f, 0xff, 0x5f, 0x05, 0x00,
   0x40, 0xfe, 0x7f, 0xff, 0x5f, 0x06, 0x00, 0x40, 0xfd, 0x7f, 0xf1, 0x5f,
   0x05, 0x00, 0x40, 0xfe, 0x7f, 0xff, 0x5f, 0x06, 0x00, 0x40, 0xfd, 0x7f,
   0x00, 0x40, 0x05, 0x00, 0x40, 0xfe, 0xff, 0xff, 0x7f, 0x06, 0x00, 0x40,
   0xfd, 0xff, 0xff, 0x7f, 0x05, 0x00, 0x40, 0xfe, 0xff, 0xff, 0x7f, 0x06,
   0x00, 0x40, 0x05, 0x00, 0x00, 0x00, 0x05, 0x00, 0x40, 0xaa, 0xaa, 0xaa,
   0xaa, 0x06, 0x00, 0x40, 0x55, 0x55, 0x55, 0x55, 0x05, 0x00, 0x80, 0xff,
   0xff, 0xff, 0xff, 0x03, 0x00, 0x00, 0xc0, 0xff, 0xff, 0x0f, 0x00, 0x00,
   0x00, 0xe0, 0x1f, 0xc0, 0x0f, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
   0x01, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0xc0, 0x50, 0x55,
   0x15, 0x2a, 0x02, 0x00, 0x60, 0xa8, 0xaa, 0x2a, 0x54, 0x04, 0x00, 0x30,
   0x54, 0x55, 0x55, 0xa8, 0x08, 0x00, 0x18, 0xaa, 0xaa, 0xaa, 0x50, 0x11,
   0x00, 0x0c, 0x15, 0x00, 0x40, 0xa1, 0x22, 0x00, 0x04, 0x00, 0x00, 0x00,
   0x00, 0x40, 0x00, 0xac, 0xaa, 0xaa, 0xaa, 0xaa, 0x6a, 0x00, 0x54, 0x55,
   0x55, 0x55, 0x55, 0x55, 0x00, 0xfc, 0xff, 0xff, 0xff, 0xff, 0x7f, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00};

/* Start houglass bitmaps (for wait_cursor) */
#define hourglass_width 32
#define hourglass_height 32
#define hourglass_x_hot 15
#define hourglass_y_hot 15
static char hourglass_bits[] = {
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0x40, 0x00, 0x00, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0x60, 0x00, 0x00, 0x03, 0x60, 0x00, 0x00, 0x03,
   0x60, 0x00, 0x00, 0x03, 0x60, 0xc0, 0x01, 0x03, 0xc0, 0xa8, 0x8e, 0x01,
   0x80, 0x55, 0xdd, 0x00, 0x00, 0xab, 0x6f, 0x00, 0x00, 0x56, 0x37, 0x00,
   0x00, 0xec, 0x1b, 0x00, 0x00, 0xd8, 0x0d, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0xa0, 0x02, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0xb0, 0x06, 0x00,
   0x00, 0x98, 0x0c, 0x00, 0x00, 0x8c, 0x18, 0x00, 0x00, 0x06, 0x30, 0x00,
   0x00, 0x83, 0x60, 0x00, 0x80, 0xc1, 0xc3, 0x00, 0xc0, 0xb0, 0x86, 0x01,
   0x60, 0x5c, 0x1d, 0x03, 0x60, 0x2a, 0x39, 0x03, 0x60, 0xdd, 0x7f, 0x03,
   0x60, 0x00, 0x00, 0x03, 0xe0, 0xff, 0xff, 0x03, 0x40, 0x00, 0x00, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03};

#define hourglass_mask_width 32
#define hourglass_mask_height 32
static char hourglass_mask_bits []= {
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xc0, 0xff, 0xff, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xc0, 0xff, 0xff, 0x01,
   0x80, 0xff, 0xff, 0x00, 0x00, 0xff, 0x7f, 0x00, 0x00, 0xfe, 0x3f, 0x00,
   0x00, 0xfc, 0x1f, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xf8, 0x0f, 0x00, 0x00, 0xfc, 0x1f, 0x00, 0x00, 0xfe, 0x3f, 0x00,
   0x00, 0xff, 0x7f, 0x00, 0x80, 0xff, 0xff, 0x00, 0xc0, 0xff, 0xff, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xc0, 0xff, 0xff, 0x01,
   0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03};
/* ... end houglass bitmaps */


/* extern functions */
extern Widget create_kuipIo(ZEROargs); /* routine created by Architect */
extern Widget create_saveAsTrDialog(ZEROargs);
extern Widget create_inputTrDialog(ZEROargs);
extern Widget create_searchDialog(ZEROargs);

extern void warn_user(          /* from dialog.c */
#ifndef _NO_PROTO
                      Widget parent, char *warning
#endif
                      );
extern void inform_user(
#ifndef _NO_PROTO
                        Widget parent, char *warning
#endif
                        );

extern void help_cb(
#ifndef _NO_PROTO
                    Widget w, int *type, XmAnyCallbackStruct *call_data
#endif
                   );

extern void set_kxtpanel_rows (
#ifndef _NO_PROTO
                    int nrow, int ncol, int last
#endif
                   );

extern void set_kxtpanel_keys (
#ifndef _NO_PROTO
                    int row, int col, char *key
#endif
                   );

extern void display_kxtpanel (
#ifndef _NO_PROTO
                    char *title, char *geometry
#endif
                   );

/* extern widgets */
extern Widget panedWindow;             /* kuipIo interface */
extern Widget cwdLabel;
extern Widget lcdLabel;
extern Widget outputText;
extern Widget outputHoldButton;
extern Widget inputText;
extern Widget inputHoldButton;
extern Widget menuBar;
extern Widget aboutButton;
extern Widget aboutAppButton;
extern Widget saveTransButton;
extern Widget saveTransAsButton;
extern Widget printButton;
extern Widget killButton;
extern Widget exitButton;
extern Widget cutButton;
extern Widget copyButton;
extern Widget pasteButton;
extern Widget searchButton;
extern Widget showInputButton;
extern Widget clearTrButton;
extern Widget helpKxtermButton;
extern Widget helpResourceButton;
extern Widget helpEditButton;
extern Widget closeInputTrButton;     /* Input Transcript Dialog */
extern Widget inputTrText;
extern Widget caseToggle;             /* Search Dialog */
extern Widget previousSearchButton;
extern Widget nextSearchButton;
extern Widget allSearchButton;
extern Widget closeSearchButton;
extern Widget searchText;

/* global functions */
void send_cmd(
#ifndef _NO_PROTO
                     char *
#endif
                    );

/* local functions */
static void exit_action(
#ifndef _NO_PROTO
                        char **, int
#endif
                       );
static void quit_action(
#ifndef _NO_PROTO
                        char **, int
#endif
                       );
static void cwd_action(
#ifndef _NO_PROTO
                       char **, int
#endif
                      );
static void lcd_action(
#ifndef _NO_PROTO
                       char **, int
#endif
                      );
static void prompt_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void raise_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void cursor_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void newpanel_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void panelrows_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void panelkeys_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void display_panel_action(
#ifndef _NO_PROTO
                          char **, int
#endif
                         );

static void SendSignal(
#ifndef _NO_PROTO
                       Widget, XEvent *, String *, Cardinal *
#endif
                       );
static void GetPreviousCommand(
#ifndef _NO_PROTO
                               Widget, XEvent *, String *, Cardinal *
#endif
                               );
static void ResubmitCommand(
#ifndef _NO_PROTO
                            Widget, XEvent *, String *, Cardinal *
#endif
                            );
static void ToggleHold(
#ifndef _NO_PROTO
                       Widget, XEvent *, String *, Cardinal *
#endif
                       );

static void application_cmd_cb(
#ifndef _NO_PROTO
                               Widget, char *, XmAnyCallbackStruct *
#endif
                               );
static void application_toggle_cb(
#ifndef _NO_PROTO
                                  Widget, char*, XmToggleButtonCallbackStruct*
#endif
                                  );

/* global data */
XtAppContext     UxAppContext;
Display         *UxDisplay;
int              UxScreen;
Colormap         UxColormap;
Widget           UxTopLevel;
Widget           kuipIo;               /* kxterm toplevel widget */

XtInputId        input_id;

Boolean          hold_input;
Boolean          hold_output;

Boolean          do_flush           = False;
Boolean          changing_text      = False;
Boolean          stop_sending       = False;
Boolean          flow_control       = False;
int              dont_move          = 0;

Boolean          application_alive  = True;
Boolean          user_said_exit     = False;
int              parent_pid         = 0;
char            *application_prompt = NULL;
char            *application_name   = NULL;
char            *prog_name;             /* name of this invocation of kxterm */

char            *input_tr_text      = NULL;
char            *transcript_text    = NULL;
Boolean          lower_text         = False;

char             transcript_file[MAX_FILE_LENGTH]; /* transcript pad file */
FILE            *tfp;                    /* file pointer to transcript_file */
Boolean          tr_tmp_file = False;    /* is transcript file a tmp file ? */

char             last_command[MAX_CMD_LENGTH];
Boolean          kxterm_iconic = False;

Cursor main_cursor, wait_cursor;

int              Error;         /* wanted by libPW.a routines */


/*
 * Action commands that can be send by an application to kxterm.
 */
static KxtermActionsRec kxterm_actions[] = {
   {"exit",      exit_action},      /* exit of application and kxterm */
   {"quit",      quit_action},      /* quit of application */
   {"cwd",       cwd_action},       /* change the current working directory */
   {"lcd",       lcd_action},       /* change the system working directory */
   {"prompt",    prompt_action},    /* change the application prompt */
   {"raise",     raise_action},     /* pop up  kxterm window */
   {"cursor",    cursor_action},    /* change kxterm cursor shape */
   {"setnewpanel",    newpanel_action},      /* display new KXTERM panel */
   {"setpanelrows",   panelrows_action},     /* set KXTERM panel nrow & ncol */
   {"setpanelkeys",   panelkeys_action},     /* et KXTERM panel keys */
   {"display_panel",  display_panel_action}, /* display KXTERM panel */
   {NULL,        NULL},
};

static String fallbacks[] = {
   "*selectColor:                green",
   "*kuipIo_shell.geometry:      +0+0",
   "*outputForm.height:          205",
/* "*keyboardFocusPolicy:        pointer",*/
   "*doubleClickInterval:        400",
   NULL
};

static XrmOptionDescRec options[] = {
   {"-name",     "*kuipIo_shell.title",      XrmoptionSepArg, NULL},
   {"-name",     "*kuipIo_shell.iconName",   XrmoptionSepArg, NULL},
   {"-title",    "*kuipIo_shell.title",      XrmoptionSepArg, NULL},
   {"-n",        "*kuipIo_shell.iconName",   XrmoptionSepArg, NULL},
   {"-geometry", "*kuipIo_shell.geometry",   XrmoptionSepArg, NULL},
   {"-iconic",   "*kuipIo_shell.iconic",     XrmoptionNoArg,  "True"},
   {"-tfont",    "*XmText*fontList",         XrmoptionSepArg, NULL},
   {"-font",     "*fontList",                XrmoptionSepArg, NULL},
};

static XtActionsRec actions[] = {
   {"SendSignal",           SendSignal},
   {"GetPreviousCommand",   GetPreviousCommand},
   {"ResubmitCommand",      ResubmitCommand},
   {"ToggleHold",           ToggleHold},
};

/* emacs translations for the inputText widget */
static char *translation_table =
  "Ctrl <Key>b:            backward-character()\n\
   Alt <Key>b:             backward-word()\n\
   Meta <Key>b:            backward-word()\n\
   Shift Alt <Key>b:       backward-word(extend)\n\
   Shift Meta <Key>b:      backward-word(extend)\n\
   Alt <Key>[:             backward-paragraph()\n\
   Meta <Key>[:            backward-paragraph()\n\
   Shift Alt <Key>[:       backward-paragraph(extend)\n\
   Shift Meta <Key>[:      backward-paragraph(extend)\n\
   Alt <Key><:             beginning-of-file()\n\
   Meta <Key><:            beginning-of-file()\n\
   Ctrl <Key>a:            beginning-of-line()\n\
   Shift Ctrl <Key>a:      beginning-of-line(extend)\n\
   Ctrl <Key>osfInsert:    copy-clipboard()\n\
   Shift <Key>osfDelete:   cut-clipboard()\n\
   Shift <Key>osfInsert:   paste-clipboard()\n\
   Alt <Key>>:             end-of-file()\n\
   Meta <Key>>:            end-of-file()\n\
   Ctrl <Key>e:            end-of-line()\n\
   Shift Ctrl <Key>e:      end-of-line(extend)\n\
   Ctrl <Key>f:            forward-character()\n\
   Alt <Key>]:             forward-paragraph()\n\
   Meta <Key>]:            forward-paragraph()\n\
   Shift Alt <Key>]:       forward-paragraph(extend)\n\
   Shift Meta <Key>]:      forward-paragraph(extend)\n\
   Ctrl Alt <Key>f:        forward-word()\n\
   Ctrl Meta <Key>f:       forward-word()\n\
   Ctrl <Key>d:            kill-next-character()\n\
   Alt <Key>BackSpace:     kill-previous-word()\n\
   Meta <Key>BackSpace:    kill-previous-word()\n\
   Ctrl <Key>w:            key-select() kill-selection()\n\
   Ctrl <Key>y:            unkill()\n\
   Ctrl <Key>k:            kill-to-end-of-line()\n\
   Ctrl <Key>u:            beginning-of-line() kill-to-end-of-line()\n\
   Alt <Key>Delete:        kill-to-start-of-line()\n\
   Meta <Key>Delete:       kill-to-start-of-line()\n\
   Ctrl <Key>o:            newline-and-backup()\n\
   Ctrl <Key>j:            newline-and-indent()\n\
   Ctrl <Key>n:            next-line() GetPreviousCommand(NEXT)\n\
   Ctrl <Key>osfLeft:      page-left()\n\
   Ctrl <Key>osfRight:     page-right()\n\
   Ctrl <Key>p:            previous-line() GetPreviousCommand()\n\
   Ctrl <Key>g:            process-cancel()\n\
   Ctrl <Key>l:            redraw-display()\n\
   Ctrl <Key>osfDown:      next-page()\n\
   Ctrl <Key>osfUp:        previous-page()\n\
   <Key>osfDown:           next-line() GetPreviousCommand(NEXT)\n\
   <Key>osfUp:             previous-line() GetPreviousCommand()\n\
   Ctrl <Key>space:        set-anchor()\n\
   Ctrl <Key>c:            SendSignal(SIGINT)\n\
   Ctrl <Key>h:            ToggleHold()\n\
   Shift <Key>F8:          ResubmitCommand()\n\
   <Key>F8:                ResubmitCommand(EXEC)";

/***********************************************************************
 *                                                                     *
 *   Free transcript text.                                             *
 *                                                                     *
 ***********************************************************************/
void free_transcript_text()
{
   XtFree(transcript_text);
   transcript_text = NULL;
   lower_text = False;
}

/***********************************************************************
 *                                                                     *
 *   Get a line from the text. Pos_type can be either CURSOR or LINE.  *
 *   When it is CURSOR pos is a cursor position in the text. When it   *
 *   is LINE and pos > 0 then it is a line pos from the beginning of   *
 *   the text and when pos < 0 then it is from the end of the text.    *
 *   The line returned should be freed by the calling routine to       *
 *   prevent memory loss and does not contain the \n character.        *
 *                                                                     *
 ***********************************************************************/
static char *get_line( text, pos, pos_type )
     char *text;
     int pos;
     int pos_type;
{
    char  *s, *line = NULL;
    int    pos1, pos2;

    if (!text || !*text)
       return NULL;

    switch (pos_type) {
       case CURSOR:
          /*
           * From pos search forward to the first '\n', then backwards
           * to the previous '\n'.
           */
          pos2 = 0;
          if ((s = strchr(&text[pos], '\n'))) {
             pos2 = (int)(s - text);
             text[pos2] = '\0';
          }

          if ((s = strrchr(text, '\n'))) {
             pos1 = (int)(s - text);
             line = XtMalloc(strlen(&text[pos1+1]) + 1);
             strcpy(line, &text[pos1+1]);
          } else {
             line = XtMalloc(strlen(text) + 1);
             strcpy(line, text);
          }

          if (pos2)
             text[pos2] = '\n';
          break;
       case LINE:
          if (!pos) return NULL;
          pos1 = 0;
          if (pos > 0) {
             for (s = text; (s = strchr(s, '\n')) != NULL; s++)
                if (++pos1 == pos) {
                   line = get_line(text, (int)(s - text), CURSOR);
                   return line;
                }
             return NULL;
          } else {
             pos2 = 0;
             while ((s = strrchr(text, '\n'))) {
                if (pos2) text[pos2] = '\n';
                if (--pos1 == pos) {
                   line = get_line(text, (int)(s - text), CURSOR);
                   return line;
                }
                pos2 = (int)(s - text);
                text[pos2] = '\0';
             }
             if (pos2) text[pos2] = '\n';
             return NULL;
          }
          break;
    }
    return line;
}

/***********************************************************************
 *                                                                     *
 *   Insert a string at the end of a XmText widget.                    *
 *                                                                     *
 ***********************************************************************/
void insert_string( text_widget, buf, show )
     Widget text_widget;
     char *buf;
     int show;
{
   XmTextPosition pos = XmTextGetLastPosition(text_widget);
   XmTextInsert(text_widget, pos, buf);

   if (show) {
      pos = XmTextGetLastPosition(text_widget);
      XmTextSetInsertionPosition(text_widget, pos);
      XmTextShowPosition(text_widget, pos);
   }

   /* write into transcript file */
   if (tfp && text_widget == outputText) {
      fprintf(tfp, "%s", buf);

      /*
       * transcript_text is not valid anymore,
       * free the space and reset pointer
       */
      free_transcript_text();
   }
}

/***********************************************************************
 *                                                                     *
 *   Checks if a line begins with a prompt. If it does it returns      *
 *   the length of the prompt otherwise 0.                             *
 *                                                                     *
 ***********************************************************************/
int begins_with_prompt(char *line)
{
   int len = 0;

   if (application_prompt) {
      char *s;
      int pl = strlen(application_prompt);

      /* simple case: direct match */
      if (!strncmp(line, application_prompt, pl))
         return pl;

      /* variable prompt of type: paw [xxx] */
      if ((s = strchr(application_prompt, '['))) {

         int l = s - application_prompt + 1;
         int extra = pl-(strchr(application_prompt,']')-application_prompt+1);

         if (!strncmp(line, application_prompt, l)) {
            len = strchr(line, ']') - line + 1 + extra;
	    if (len < 0) len = 0;
         }
      }
   }

   return len;
}

/***********************************************************************
 *                                                                     *
 *   Show prompt in inputText widget. Kxterm is ready to send next     *
 *   command.                                                          *
 *                                                                     *
 ***********************************************************************/
void show_prompt()
{
   if (application_prompt) {
      char *input = XmTextGetString(inputText);
      XmTextPosition pos = XmTextGetInsertionPosition(inputText);

      /* don't insert more than one prompt */
      if (begins_with_prompt(input)) {
         XtFree(input);
         return;
      }

      XmTextInsert(inputText, (XmTextPosition) 0, application_prompt);

      pos += strlen(application_prompt);
      XmTextSetInsertionPosition(inputText, pos);

      XtFree(input);
   }
}

/***********************************************************************
 *                                                                     *
 *   Close transcript file and exit kxterm.                            *
 *                                                                     *
 ***********************************************************************/
void exit_kxterm()
{
   /* close transcript file and if it is a tmp file remove it */
   /* (the result of tmpfile() is removed automatically upon being closed) */
   if (tfp) {
      fclose(tfp);
   }

   exit(0);
}

/***********************************************************************
 *                                                                     *
 *   Handle menu building escape commands coming from the application. *
 *                                                                     *
 ***********************************************************************/
void menu_escape( cmd )
     char *cmd;
{
   char      *s, *menu, *menu_name;
   Widget     pulldown;
   MenuItem  *menu_item;

   /* get name of menu, e.g. *FileMenu */
   if ((s = strchr(cmd, ':'))) {
      menu      = XtCalloc(s-cmd+1, 1);
      menu_name = XtCalloc(s-cmd+5+1, 1);
      strncpy(menu, cmd, s-cmd);
      sprintf(menu_name, "*%sMenu", menu);

      menu_item = (MenuItem *)XtMalloc(MAX_ITEMS*sizeof(MenuItem));
      if (add_menu_items(s+1, application_cmd_cb, application_toggle_cb,
                         menu_item, MAX_ITEMS)) {

         /* does menu already exist? */
         pulldown = XtNameToWidget(menuBar, menu_name);
         if (pulldown) {
            /* add to existing pulldown menu */
            BuildMenu(pulldown, XmMENU_PULLDOWN, menu, menu[0], menu_item,
                      True);
         } else {
            /* create new pulldown menu */
            BuildMenu(menuBar, XmMENU_PULLDOWN, menu, menu[0], menu_item,
                      False);
         }
      }
      XtFree((char *)menu_item);
      XtFree(menu);
      XtFree(menu_name);
   }
}

/***********************************************************************
 *                                                                     *
 *   Handle exit action coming from the application.                   *
 *                                                                     *
 ***********************************************************************/
static void exit_action( argv, argc )
     char **argv;
     int  argc;
{
   user_said_exit = True;
}

/***********************************************************************
 *                                                                     *
 *   Handle quit action coming from the application.                   *
 *                                                                     *
 ***********************************************************************/
static void quit_action( argv, argc )
     char **argv;
     int  argc;
{
   user_said_exit = True;
}

/***********************************************************************
 *                                                                     *
 *   Handle cwd action coming from the application.                    *
 *   This action displays the current working directory in the         *
 *   cwdLabel widget.                                                  *
 *                                                                     *
 ***********************************************************************/
static void cwd_action( argv, argc )
     char **argv;
     int  argc;
{
   XmString xms;

   if (argc)
      xms = XmStringCreate(argv[0], XmSTRING_DEFAULT_CHARSET);
   else
      xms = XmStringCreate("", XmSTRING_DEFAULT_CHARSET);

   XtVaSetValues(cwdLabel, XmNlabelString, xms, NULL);
   XmStringFree(xms);
}

/***********************************************************************
 *                                                                     *
 *   Handle lcd action coming from the application.                    *
 *   This action displays the current system working directory in the  *
 *   lcdLabel widget. The system working directory is changed by the   *
 *   kuip command lcd.                                                 *
 *                                                                     *
 ***********************************************************************/
static void lcd_action( argv, argc )
     char **argv;
     int  argc;
{
   XmString xms;

   if (argc)
      xms = XmStringCreate(argv[0], XmSTRING_DEFAULT_CHARSET);
   else
      xms = XmStringCreate("", XmSTRING_DEFAULT_CHARSET);

   XtVaSetValues(lcdLabel, XmNlabelString, xms, NULL);
   XmStringFree(xms);
}

/***********************************************************************
 *                                                                     *
 *   Handle prompt action coming from the application.                 *
 *   This action changes the application_prompt.                       *
 *                                                                     *
 ***********************************************************************/
static void prompt_action( argv, argc )
     char **argv;
     int  argc;
{
   char *newp;

   if (argc) {
      if (strempty(argv[0]))
         newp = NULL;
      else if (argv[0][strlen(argv[0])-1] != ' ') {
         /* prompt should end with ' ' */
         newp = malloc(strlen(argv[0]) + 2);
         sprintf(newp, "%s ", argv[0]);
      } else
         newp = strdup(argv[0]);
   } else
      newp = NULL;

   if (application_prompt) {
      int   lnew = strlen(newp);
      char *input = XmTextGetString(inputText);
      XmTextPosition pos = XmTextGetInsertionPosition(inputText);

      XFlush(UxDisplay);

      changing_text = True;

      /* replace old prompt by new prompt */
      if (!strncmp(input, application_prompt, strlen(application_prompt))) {
         XmTextReplace(inputText, (XmTextPosition) 0,
                       (XmTextPosition) strlen(application_prompt), newp);
      } else
         XmTextInsert(inputText, (XmTextPosition) 0, newp);

     /* changing_text = False; */

      if (newp)
         pos += lnew - strlen(application_prompt);
      else
         pos -= strlen(application_prompt);

      if (pos < lnew) pos = lnew;

      XmTextSetInsertionPosition(inputText, pos);

      XFlush(UxDisplay);

      changing_text = False;

      XtFree(input);
      free(application_prompt);

      if (newp)
         application_prompt = strdup(newp);
      else
         application_prompt = NULL;

   } else {

      if (newp)
         application_prompt = strdup(newp);
      else
         application_prompt = NULL;

      show_prompt();
   }

   if (newp) free(newp);
}

/***********************************************************************
 *                                                                     *
 *   Handle raise action coming from the application.                  *
 *   This action popups the executive window.                          *
 *                                                                     *
 ***********************************************************************/
static void raise_action( argv, argc )
     char **argv;
     int  argc;
{
   XtPopup( XtParent(kuipIo), XtGrabNone );

   XMapRaised(XtDisplay(kuipIo), XtWindow(XtParent(kuipIo)));
   XFlush(XtDisplay(kuipIo));
}

/***********************************************************************
 *                                                                     *
 *   Handle raise action coming from the application.                  *
 *   This action popups the executive window.                          *
 *                                                                     *
 ***********************************************************************/
static void cursor_action( argv, argc )
     char **argv;
     int  argc;
{
   XSetWindowAttributes attrs;

   if (!argc || strempty(argv[0])) return;

   if (!strcmp(argv[0], "wait")) {
      attrs.cursor = wait_cursor;
      XChangeWindowAttributes(UxDisplay, XtWindow(kuipIo), CWCursor, &attrs);
    } else if (!strcmp(argv[0], "main")) {
      attrs.cursor = main_cursor;
      XChangeWindowAttributes(UxDisplay, XtWindow(kuipIo), CWCursor, &attrs);
  }
}

static void newpanel_action( argv, argc )
     char **argv;
     int  argc;
{
   char *title;
   char geometry[128];
   int nc, nl, i, j;

   if (argc) {
       title = strdup(argv[0]);
       nl = atoi (argv[1]);
       nc = atoi (argv[2]);
       set_kxtpanel_rows (nl, 0, -1);
       for (i = 0; i < nl; i++) {
            set_kxtpanel_rows (i, nc, 0);
       }
       for (i = 0; i < nl; i++) {
           for (j = 0; j < nc; j++) {
               set_kxtpanel_keys (i, j, " ");
           }
       }
       sprintf (geometry,"%sx%s+%s+%s", argv[3], argv[4], argv[5], argv[6]);
       display_kxtpanel (title, geometry);
       free(title);
   }
}

static void panelrows_action( argv, argc )
     char **argv;
     int  argc;
{
   int nl, nc;

   if (argc) {
       nl = atoi (argv[0]);
       nc = atoi (argv[1]);
       if (nc == -1) {
         set_kxtpanel_rows (nl, 0, -1);
       } else {
         set_kxtpanel_rows (nl, nc, 0);
       }
   }
}

static void panelkeys_action( argv, argc )
     char **argv;
     int  argc;
{
   int nr, nc;

   if (argc) {
       nr = atoi (argv[0]);
       nc = atoi (argv[1]);
       if (strempty(argv[2]))
           set_kxtpanel_keys (nr, nc, " ");
       else
           set_kxtpanel_keys (nr, nc, argv[2]);
   }
}

static void display_panel_action( argv, argc )
     char **argv;
     int  argc;
{
   char *title, *geometry;

   if (argc) {
       title = strdup(argv[0]);
       geometry = strdup(argv[1]);
       display_kxtpanel (title, geometry);
       free (title);
       free (geometry);
   }
}

/***********************************************************************
 *                                                                     *
 *   Handle special (escaped) commands coming from the application.    *
 *   An escaped command starts with the ESCAPE sequence and ends with  *
 *   a newline (\n).                                                   *
 *   There are two type of command:                                    *
 *    1) Action commands with format: <ESCAPE><cmd>\n                  *
 *       The <cmd> part is a kxterm action, e.g. "exit" or             *
 *       "cwd(//lun1)". For all defined actions see the action table   *
 *       defined in deck actions.                                      *
 *    2) Menu building commands with format:                           *
 *       <ESCAPE>M:<Menu name>:<item name>:[<cmd>]:                    *
 *                 [<button | toggle | separator>]:[<nmemonic>]:       *
 *                 [<short cut>]:[<printed short cut>]\n               *
 *       The fields: <cmd>, <button...>, <nmemonic>, <short cut>,      *
 *                   <printed short cut> are optional and may be       *
 *                   replaced by a blank. If <cmd> is omitted          *
 *                   <item name> will be used as <cmd>. If <button...> *
 *                   is omitted button will be used. For the other     *
 *                   items there is no default.                        *
 *                                                                     *
 ***********************************************************************/
void handle_escapes( cmd )
     char *cmd;
{
   char      *s;

   s = cmd;
   s += strlen(ESCAPE);   /* skip ESCAPE */

   if (!strncasecmp(s, "M:", 2)) {
      s += 2;
      menu_escape(s);
   } else
      handle_kxterm_action(cmd);
}

/***********************************************************************
 *                                                                     *
 *   Send command to application.                                      *
 *                                                                     *
 ***********************************************************************/
void send_cmd( str )
     char *str;
{
   int      l;
   char    *line, *cmd;

   /* cannot send anything when application is dead (pipes are broken) */
   if (!application_alive)
      return;

   line = str;

   /* strip off prompt */
   while ((l = begins_with_prompt(line)))
      line += l;

   cmd = strip(line);

   printf("%s\n", cmd);

   if (flow_control)
      stop_sending = True;

   /* write to input pad tmp file */
   if (!strempty(cmd)) {
      char *str1 = XtMalloc(strlen(cmd)+2);
      strcpy(last_command, cmd);
      sprintf(str1, "%s\n", cmd);
      if (inputTrText) {
         insert_string(inputTrText, str1, True);
         XtFree(input_tr_text);
         input_tr_text = NULL;
      }
      XtFree(str1);
   }

   XtFree(cmd);
}

/***********************************************************************
 *                                                                     *
 *   Send kxterm control (escaped) command to application.             *
 *                                                                     *
 ***********************************************************************/
static void send_kxterm_cmd( str )
     char *str;
{
   /*
    * cannot send any commands when application is dead (pipes are broken)
    * or when flow_control is not yet enabled
    */
   if (!application_alive || !flow_control)
      return;

   if (!strncmp(str, ESCAPE, strlen(ESCAPE))) {
      if (str[strlen(str)-1] == '\n')
         printf("%s", str);
      else
         printf("%s\n", str);
   }
}

/***********************************************************************
 *                                                                     *
 *   Toggle a pushbutton widget (switching shadow colors).             *
 *                                                                     *
 ***********************************************************************/
void TogglePushButton( button, set )
     Widget button;
     int set;
{
   Pixel  ts, bs;

   if (set) {
      XtVaGetValues(button, XmNbottomShadowColor, &bs, XmNtopShadowColor, &ts,
                    NULL);
      XtVaSetValues(button, XmNbottomShadowColor, ts, XmNtopShadowColor, bs,
                    NULL);
   } else {
      XtVaGetValues(button, XmNbottomShadowColor, &ts, XmNtopShadowColor, &bs,
                    NULL);
      XtVaSetValues(button, XmNbottomShadowColor, bs, XmNtopShadowColor, ts,
                    NULL);
   }
}

/***********************************************************************
 *                                                                     *
 *   Flush input. Check if there is still an \n terminated string in   *
 *   the inputText widget and send it to the application.              *
 *                                                                     *
 ***********************************************************************/
static void flush_input( new_insert )
     XmTextPosition *new_insert;
{
   char           *input, *line, *s;
   int             len, len2, delete_to;
   XmTextPosition  pos;

   if (hold_input || stop_sending) return;

   changing_text = True;

   input = XmTextGetString(inputText);
   s = strchr(input, '\n');
   if (s) {
      /*
       * Only flush up to the line containing the cursor.
       */
      if (new_insert)
         pos = *new_insert;
      else
         pos = XmTextGetInsertionPosition(inputText);
      len = s - input;
      if (pos > len) {
         line = XtCalloc(len+1, 1);
         strncpy(line, input, len);

         send_cmd(line);

         delete_to = len + 1;

         XtFree(line);

         /* delete text from beginning to current pos */
         XmTextReplace(inputText, (XmTextPosition)0,
                                  (XmTextPosition)delete_to, NULL);

         if (new_insert) {
            *new_insert -= delete_to;
            XmTextSetInsertionPosition(inputText, *new_insert);

            /*
             * Now some horrible piece of code to find out how many times
             * we should ignore the MotionVerifyCallback that will be
             * called a different number of times depending on where the
             * the cursor moved to. In the ideal case the above code
             * would have been sufficient. Whenever the text widget will
             * be fixed we could remove the following mess.
             */
#if XmREVISION > 1
            dont_move = 1;
#else
            s = strchr(input+delete_to, '\n');
            if (s) {
               len2 = s - (input+delete_to);
               if (len == len2)
                  dont_move = 0;
               else if (*(input+delete_to+*new_insert) == '\n')
                  dont_move = 1;
               else
                  dont_move = 2;
            } else
               dont_move = 0;
#endif
         }
      }
   }

   XtFree(input);
   changing_text = False;
}

/***********************************************************************
 *                                                                     *
 *   Write the transcript file.                                        *
 *                                                                     *
 ***********************************************************************/
static void write_transcript( file )
     char *file;
{
   /*
    * copy the temp file to user specified file, close temp file and open
    * user specified file again
    */
   if (tfp) {
      fflush(tfp);
      if (!strcmp(transcript_file, file))
         return;
      else {
         char line[MAX_FILE_LENGTH + 50];
	 FILE *stream;

         if (!(stream = fopen(file, "w"))) {
            sprintf(line, "Cannot open transcript file %s",
                    file);
            warn_user(kuipIo, line);
         }
         else {
            /* copy current transcript to new file */
	    rewind( tfp );
            while( fgets( line, (sizeof line), tfp ) != NULL )
               fputs( line, stream );
	    fclose( stream );
            fclose( tfp );
	    tfp = fopen(file, "a+");
            strcpy(transcript_file, file);
	    
            if (tr_tmp_file) {
               tr_tmp_file = False;
            }
            fflush(tfp);
         }
      }
   }
}

/***********************************************************************
 *                                                                     *
 *   Execute an application defined menu button.                       *
 *                                                                     *
 ***********************************************************************/
static void application_cmd_cb( w, cmd, cbs )
     Widget w;
     char *cmd;
     XmAnyCallbackStruct *cbs;
{
   char  *s;

   if (!application_alive)
      return;

   s = XtMalloc(strlen(cmd) + 20);
   sprintf(s, "%s\n", cmd);

   /*
    * Send special (hidden) kuip commands, like #@browser, directly without
    * going via the inputPad. This will prevent these commands from being
    * recorded in the input transcript.
    */
   if (!strncmp(s, ESCAPE, strlen(ESCAPE))) {
      send_kxterm_cmd(s);

      /* dirty, KUIP specific code */
      sprintf(s, "%siconify(UNMAP)", ESCAPE);
      if (!strcasecmp(cmd, s)) {
         IconifyShell(kuipIo);
         kxterm_iconic = True;
      }
   } else
      insert_string(inputText, s, True);

   XtFree(s);
}

/***********************************************************************
 *                                                                     *
 *   Execute an application defined toggle menu button. These commands *
 *   must recognize the 'on' and 'off' options.                        *
 *                                                                     *
 ***********************************************************************/
static void application_toggle_cb( w, cmd, cbs )
     Widget w;
     char *cmd;
     XmToggleButtonCallbackStruct *cbs;
{
   char  *s;

   if (!application_alive)
      return;

   s = XtMalloc(strlen(cmd) + 10);

   /*
    * Send special (hidden) kuip commands, like #@browser, directly without
    * going via the inputPad. This will prevent these commands from being
    * recorded in the input transcript.
    */
   if (!strncmp(cmd, ESCAPE, strlen(ESCAPE))) {

      if (cbs->set)
         sprintf(s, "%s(ON)\n", cmd);
      else
         sprintf(s, "%s(OFF)\n", cmd);
      send_kxterm_cmd(s);

   } else {

      if (cbs->set)
         sprintf(s, "%s on\n", cmd);
      else
         sprintf(s, "%s off\n", cmd);
      insert_string(inputText, s, True);

   }

   XtFree(s);
}

/***********************************************************************
 *                                                                     *
 *   Get the arguments for the SAVE AS command and execute the         *
 *   command.                                                          *
 *                                                                     *
 ***********************************************************************/
static void get_saveAsArg( w, cd, call_data )
     Widget w;
     XtPointer cd;
     XmFileSelectionBoxCallbackStruct *call_data;
{
   char    *file;
#ifdef vms
   char    *p;
#endif

   XmStringGetLtoR(call_data->value, XmSTRING_DEFAULT_CHARSET, &file);
   if (strempty(file)) {
      warn_user(w, "You must give a file name.");
      XtFree(file);
      return;
   }
#ifdef vms
   if ((p = strchr(file, ';')) != NULL)
      *p = '\0';
   file = XtRealloc(file, strlen(file) + 3);
   strcat(file, ";0");
   if (strchr(file, ']') == NULL) {
#else
   if (*file != '/') {
#endif
      /*
       * if it is not a directory, determine the full pathname
       * of the selection by concatenating it to the "dir" part
       */
      char *dir, *newfile;
      XmStringGetLtoR(call_data->dir, XmSTRING_DEFAULT_CHARSET, &dir);
      newfile = XtMalloc(strlen(dir) + strlen(file) + 1);
      sprintf(newfile, "%s%s", dir, file);
      XtFree(dir);
      XtFree(file);
      file = newfile;
   }

   popdown_dialog(w);

   write_transcript(file);

   XtFree(file);
}

/***********************************************************************
 *                                                                     *
 *   Show about... dialog.                                             *
 *                                                                     *
 ***********************************************************************/
static void about_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   static Widget dialog = NULL;

   if (w == aboutButton) {

      if (!dialog) {
         Display  *display;
         Pixmap    pixmap;
         Pixel     fg, bg;

         dialog = XmCreateMessageDialog(kuipIo, "aboutDialog", NULL, 0);

         display = XtDisplay(dialog);
         XtVaGetValues(dialog, XmNforeground, &fg, XmNbackground, &bg, NULL);
         pixmap = XCreatePixmapFromBitmapData(display,
                                     DefaultRootWindow(display),
                                     kxterm_bits, kxterm_width, kxterm_height,
                             fg, bg, DefaultDepthOfScreen(XtScreen(dialog)));

         XtVaSetValues(dialog, RES_CONVERT(XmNmessageString,
                    "Kxterm\n\nThe Kuip Terminal Emulator\n\nVersion 1.02\n"),
                     XmNmessageAlignment, XmALIGNMENT_CENTER,
                     RES_CONVERT(XmNdialogTitle, "About Kxterm"),
                     XmNsymbolPixmap, pixmap,
                     NULL);

         /* unmanage the Cancel and Help buttons */
         XtUnmanageChild(XmMessageBoxGetChild(dialog,
                                              XmDIALOG_CANCEL_BUTTON));
         XtUnmanageChild(XmMessageBoxGetChild(dialog,
                                              XmDIALOG_HELP_BUTTON));
      }

      /* popup dialog */
      XtManageChild(dialog);

      /*
       * If the dialog was already open, XtManageChild does nothing. In
       * this case, at least make sure the window is raised to the top
       * of the window tree (or as high as it can get).
       */
      XRaiseWindow(XtDisplay(dialog), XtWindow(XtParent(dialog)));

   } else {
      char *cmd = XtMalloc(strlen(ESCAPE)+7);
      sprintf(cmd, "%sabout\n", ESCAPE);
      send_kxterm_cmd(cmd);
      XtFree(cmd);
   }
}

/***********************************************************************
 *                                                                     *
 *   Save the transcript pad in a file.                                *
 *                                                                     *
 ***********************************************************************/
static void save_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   XmAnyCallbackStruct   cbs1;

   if (!tr_tmp_file)
      write_transcript(transcript_file);
   else {
      cbs1.reason = 0;
      cbs1.event  = (XEvent *) NULL;
      XtCallCallbacks(saveTransAsButton, XmNactivateCallback, &cbs1);
   }
}

/***********************************************************************
 *                                                                     *
 *   Popup fileselectionbox.                                           *
 *                                                                     *
 ***********************************************************************/
static void show_save_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   static Widget dialog;
   XmString  xmmask;

   if (!dialog) {
      dialog = create_saveAsTrDialog();

      /* unmanage the Help button */
      XtUnmanageChild(XmFileSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
      /*
       *   install callbacks
       */
      InstallMwmCloseCallback(dialog, (XtCallbackProc)cancel_cb,
                              (XtPointer)dialog);

      XtAddCallback(dialog, XmNokCallback,
                    (XtCallbackProc)get_saveAsArg, NULL);
      XtAddCallback(dialog, XmNcancelCallback,
                    (XtCallbackProc)cancel_cb, dialog);

      /* store w in userData */
      XtVaSetValues(dialog, XmNuserData, w, NULL);
   }

   XtVaGetValues(dialog, XmNdirMask, &xmmask, NULL);
   XmFileSelectionDoSearch(dialog, xmmask);

   /* make menu item insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(dialog);
}

/***********************************************************************
 *                                                                     *
 *   Exit callback routine.                                            *
 *                                                                     *
 ***********************************************************************/
static void exit_cb( widget, client_data, call_data )
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{

   if (widget == killButton) {

      if (parent_pid) {
         kill(parent_pid, SIGQUIT);
         parent_pid = 0;
      }
      application_alive = False;
      return;
      /***
   } else if (widget == quitButton) {

      send_cmd("/kuip/quit");
      ***/
   } else {

      user_said_exit = True;
      if (!application_alive)
         exit_kxterm();
      send_cmd("/kuip/exit");
   }
}

/***********************************************************************
 *                                                                     *
 *   Return widget that hold current selection.                        *
 *                                                                     *
 ***********************************************************************/
static Widget get_selection_owner(w)
   Widget w;
{
   Display *dpy = XtDisplay(w);
   Widget   text_w;
   Window   win;
   static Atom primary_atom;
   static Atom clipboard_atom;

   if (!primary_atom)
      primary_atom = XmInternAtom(dpy, "PRIMARY", False);

   if (!clipboard_atom)
      clipboard_atom = XmInternAtom(dpy, "CLIPBOARD", False);

   win = XGetSelectionOwner(dpy, primary_atom);
   if (win == None)
      win = XGetSelectionOwner(dpy, clipboard_atom);

   text_w = XtWindowToWidget(dpy, win);

   return(text_w);
}

/***********************************************************************
 *                                                                     *
 *   Cut and paste callback routine.                                   *
 *                                                                     *
 ***********************************************************************/
static void cut_paste_cb(widget, item, cbs)
     Widget    widget;
     XtPointer item;
     XtPointer cbs;
{
   Widget  text_w;
   int     i = (int) item;
   Boolean result = True;

   text_w = get_selection_owner(widget);
   if (!text_w) {
      XBell(UxDisplay, 0);
      return;
   }

   switch (i) {
      case 0:
         if (text_w == inputText)
            result = XmTextCut(text_w, CurrentTime);
         else {
            result = XmTextCopy(text_w, CurrentTime);
            XBell(UxDisplay, 0);
         }
         break;
      case 1:
         result = XmTextCopy(text_w, CurrentTime);
         break;
      case 2:
         result = XmTextPaste(inputText);
         break;
   }

   if (!result)
      XBell(UxDisplay, 0);
}

/***********************************************************************
 *                                                                     *
 *   Search a string in the transcript pad.                            *
 *                                                                     *
 ***********************************************************************/
static void search_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   XmTextPosition   pos, last_pos;
   char            *text, *pattern, *p, last_char;
   Boolean          case_convert    = False;
   int              pat_len, nfound = 0;

   /* unhighlight any previously found pattern */
   pos = XmTextGetLastPosition(outputText);
   XmTextSetHighlight(outputText, 0, pos, XmHIGHLIGHT_NORMAL);

   if (!XmToggleButtonGetState(caseToggle))
      case_convert = True;
   else {
      if (lower_text)
         free_transcript_text();
   }

   if (transcript_text)
      text = transcript_text;
   else {
      if (!(text = XmTextGetString(outputText)) || !*text) {
         warn_user(w, "No text to search.");
         return;
      }
      transcript_text = text;
   }

   if (!(pattern = XmTextGetString(searchText)) || !*pattern) {
      warn_user(w, "Specify a search pattern.");
      return;
   }
   pat_len = strlen(pattern);

   if (case_convert) {
      lower(pattern);
      if (!lower_text)
         lower(text);
      lower_text = True;
   }

   if (w == nextSearchButton) {
      /* start searching at current position */
      pos = XmTextGetCursorPosition(outputText);
      if ((p = strstr(&text[pos], pattern))) {
         nfound++;
         /* get position where pattern was found */
         pos = (XmTextPosition)(p - text);
         XmTextSetInsertionPosition(outputText, pos+pat_len);
         XmTextSetHighlight(outputText, pos, pos+pat_len,
                            XmHIGHLIGHT_SELECTED);
      }
   }
   if (w == previousSearchButton) {
      last_pos  = XmTextGetCursorPosition(outputText);
      last_char = text[last_pos];
      text[last_pos] = '\0';
      if ((p = strrstr(&text[0], pattern))) {
         nfound++;
         /* get position where pattern was found */
         pos = (XmTextPosition)(p - text);
         XmTextSetInsertionPosition(outputText, pos);
         XmTextSetHighlight(outputText, pos, pos+pat_len,
                            XmHIGHLIGHT_SELECTED);
      }
      text[last_pos] = last_char;
   }
   if (w == allSearchButton) {
      /* start at beginning of text */
      for (p = &text[0]; (p = strstr(p, pattern)) != NULL; p++) {
         nfound++;
         /* get position where pattern was found */
         pos = (XmTextPosition)(p - text);
         XmTextSetInsertionPosition(outputText, pos);
         XmTextSetHighlight(outputText, pos, pos+pat_len,
                            XmHIGHLIGHT_SELECTED);
      }
   }
   if (!nfound)
      XBell(UxDisplay, 0);

   /*
    * Don't free text because it will be used for next searches
    */
   /* XtFree(text); */
   XtFree(pattern);
}

/***********************************************************************
 *                                                                     *
 *   Popup search dialog.                                              *
 *                                                                     *
 ***********************************************************************/
static void show_search_cb( wgt, cd, cbs )
     Widget wgt;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   static Widget       dialog;
   static Dimension    h_org, w_org;
   static Position     x_org, y_org;
   Dimension           h, w, hd, wd;
   Position            x, y;

   if (!dialog) {
      dialog = create_searchDialog();

      /*
       *   install callbacks
       */
      InstallMwmCloseCallback(dialog, (XtCallbackProc)cancel_cb,
                              (XtPointer)dialog);

      XtAddCallback(closeSearchButton, XmNactivateCallback,
                    (XtCallbackProc)unmanage_cb, dialog);

      XtAddCallback(previousSearchButton, XmNactivateCallback,
                    (XtCallbackProc)search_cb, NULL);
      XtAddCallback(nextSearchButton, XmNactivateCallback,
                    (XtCallbackProc)search_cb, NULL);
      XtAddCallback(allSearchButton, XmNactivateCallback,
                    (XtCallbackProc)search_cb, NULL);

      /* store w in userData */
      XtVaSetValues(dialog, XmNuserData, wgt, NULL);
   }

   /*
    * get kuipIo position and place the search dialog in the lower right corner
    */
   XtVaGetValues(XtParent(kuipIo), XmNx, &x, XmNy, &y,
                                   XmNheight, &h,
                                   XmNwidth,  &w,
                                   NULL);
   if (x != x_org || y != y_org || h != h_org || w != w_org) {
      x_org = x;
      y_org = y;
      h_org = h;
      w_org = w;
      XtVaGetValues(dialog, XmNheight, &hd, XmNwidth, &wd, NULL);
      x = x_org + w_org - wd;
      if (x < 0) x = 0;
      y = y_org + h_org - hd;
      if (y < 0) y = 0;
      XtVaSetValues(dialog, XmNx, x, XmNy, y, NULL);
   }

   /* popup dialog */
   XtManageChild(dialog);

   /*
    * If the dialog was already open, XtManageChild does nothing. In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XRaiseWindow(XtDisplay(dialog), XtWindow(XtParent(dialog)));

   /* when explicit set the keyboard focus to inputText */
   XmProcessTraversal(searchText, XmTRAVERSE_CURRENT);
}

/***********************************************************************
 *                                                                     *
 *   Show in a text widget all commands issued via the input pad.      *
 *                                                                     *
 ***********************************************************************/
static void show_input_cb( wgt, cd, cbs )
     Widget wgt;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   static Widget       dialog;
   static Dimension    h_org, w_org;
   static Position     x_org, y_org;
   Dimension           h, w, hd, wd;
   Position            x, y;

   if (!dialog) {
      dialog = create_inputTrDialog();

      /*
       *   install callbacks
       */
      InstallMwmCloseCallback(dialog, (XtCallbackProc)cancel_cb,
                              (XtPointer)dialog);

      XtAddCallback(closeInputTrButton, XmNactivateCallback,
                    (XtCallbackProc)unmanage_cb, dialog);

      /* store w in userData */
      XtVaSetValues(dialog, XmNuserData, wgt, NULL);
   }

   /* when called via init_kuipio do not popup dialog */
   if (!cbs->event) return;

   /*
    * get kuipIo position and place the search dialog in the lower right corner
    */
   XtVaGetValues(XtParent(kuipIo), XmNx, &x, XmNy, &y,
                                   XmNheight, &h,
                                   XmNwidth,  &w,
                                   NULL);
   if (x != x_org || y != y_org || h != h_org || w != w_org) {
      x_org = x;
      y_org = y;
      h_org = h;
      w_org = w;
      XtVaGetValues(dialog, XmNheight, &hd, XmNwidth, &wd, NULL);
      x = x_org + w_org - wd/2;
      if (x < 0) x = 0;
      y = y_org + 35;
      XtVaSetValues(dialog, XmNx, x, XmNy, y, NULL);
   }

   /* popup dialog */
   XtManageChild(dialog);

   /*
    * If the dialog was already open, XtManageChild does nothing. In
    * this case, at least make sure the window is raised to the top
    * of the window tree (or as high as it can get).
    */
   XRaiseWindow(XtDisplay(dialog), XtWindow(XtParent(dialog)));
}

/***********************************************************************
 *                                                                     *
 *   Clear line of top of the transcript pad.                          *
 *                                                                     *
 ***********************************************************************/
static void clear_top_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   XmTextPosition   pos;

   /* get top character and delete from beginning to top character */
   XtVaGetValues(outputText, XmNtopCharacter, &pos, NULL);

   XmTextReplace(outputText, (XmTextPosition)0, pos-1, NULL);
}

/***********************************************************************
 *                                                                     *
 *   Check output coming from application for special kxterm commands. *
 *                                                                     *
 ***********************************************************************/
static void get_escaped_cmd( w, buf )
     Widget w;
     char *buf;
{
   static char *cache = NULL;

   /* cache is used in case an escaped command spans buf boundaries */

   if( cache == NULL ) {
     cache = XtMalloc( strlen( buf ) + 1 );
     strcpy( cache, buf );
   }
   else {
     int n = strlen( cache );
     cache = XtRealloc( cache, n + strlen( buf ) + 1 );
     strcpy( cache + n, buf );
   }

   while( cache[0] != '\0' ) {
     char *s;

     if( cache[0] == '#' && cache[1] == '@' ) {
       /* buffer starts with a command */

       if( (s = strchr( cache, '\n' ) ) != NULL ) {
         /* command is complete */
         *s = '\0';             /* chop off at \n */
         handle_escapes( cache );
         strcpy( cache, s + 1 );
       }
       else {
         /* wait for rest coming in */
         break;
       }
     }

     else if( (s = strstr( cache, "\n#@" )) != NULL ) {
       /* command preceded by normal text */
       s[1] = '\0';
       insert_string( w, cache, !hold_output );
       cache[0] = '#';
       strcpy( cache + 1, s + 2 );
     }

     else if( cache[strlen(cache)-1] == '#' ) {
       if( (s = strrchr( cache, '\n' )) != NULL ) {
         /* output complete lines */
         char c = s[1];
         s[1] = '\0';
         insert_string( w, cache, !hold_output );
         cache[0] = c;
         strcpy( cache + 1, s + 2 );
       }
       /* wait because @ may follow with next buffer */
       break;
     }

     else {
       /* it is just plain text */
       insert_string( w, cache, !hold_output );
       cache[0] = '\0';
     }
   }
}

/***********************************************************************
 *                                                                     *
 *   Connection to application is broken.                              *
 *                                                                     *
 ***********************************************************************/
static void pipe_broken()
{
  application_alive = False;
  if( user_said_exit ) {
    exit_kxterm();
  }
  else {
    XSetWindowAttributes attrs;

    insert_string(outputText, "\n\n\
*** Application has died or is running in a loop. ***\n\n\
Kxterm has been disconnected from application.\n\
Select Exit from the File menu to exit Kxterm.\n\n", True);

    attrs.cursor = main_cursor;
    XChangeWindowAttributes(UxDisplay, XtWindow(kuipIo), CWCursor, &attrs);

    XtRemoveInput(input_id);
  }
}


/***********************************************************************
 *                                                                     *
 *   Write output coming from the application in the outputText        *
 *   widget.                                                           *
 *                                                                     *
 ***********************************************************************/
static void get_from_application( w, fid, id )
     Widget w;
     int *fid;
     XtInputId *id;
{
   char          buf[BUFSIZ];
   char         *s;
   int           nbytes;
   static int    max_buf = BUFSIZ;
   static char  *bigbuf;

   if (!bigbuf)
      bigbuf = XtCalloc(max_buf, 1);

   /*
    * Get all pending input and write to the outputText widget.
    */
#ifdef VMS
   while ((nbytes = MbxRead(buf, BUFSIZ-1, w)) > 0) {
#else  /* VMS */
   while ((nbytes = read(*fid, buf, BUFSIZ-1)) > 0) {
#endif /* VMS */
      /*
       * Null terminate the string.
       */
      buf[nbytes] = '\0';

      while (strlen(bigbuf) + nbytes + 1 > max_buf) {
         max_buf += INC_LENGTH;
         bigbuf = XtRealloc(bigbuf, max_buf);
      }

      strcat(bigbuf, buf);

      /* break loop if last character is either a \n ot STX */
      if (buf[nbytes-1] == '\n' || buf[nbytes-1] == STX)
         break;
   }

#ifndef vms
   if (nbytes == 0) {
      pipe_broken();
      return;
   }
#endif

   /*
    * If STX character (i.e. application ready), send next command
    * in input buffer and remove STX character from buffer.
    */
   if ((s = strchr(bigbuf, STX))) {
      flow_control = True;
      stop_sending = False;
      show_prompt();
      flush_input(0);
      strcpy(s, s+1);
   }

   /*
    * Check output coming from application for special kxterm commands.
    */
   get_escaped_cmd(w, bigbuf);
   bigbuf[0] = '\0';
}

/***********************************************************************
 *                                                                     *
 *   Toggle the Hold button.                                           *
 *                                                                     *
 ***********************************************************************/
static void toggle_hold_button( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   if (w == inputHoldButton) {
      if (!hold_input)
         hold_input = True;
      else {
         hold_input = False;
         flush_input(0);
      }
      TogglePushButton(w, hold_input);
   } else {
      if (!hold_output)
         hold_output = True;
      else
         hold_output = False;
      TogglePushButton(w, hold_output);
   }
}

/***********************************************************************
 *                                                                     *
 *   Input callback routine. This routine tells the input_changed_cb   *
 *   to flush the input when a \n has been typed or pasted in.         *
 *                                                                     *
 ***********************************************************************/
static void input_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmTextVerifyCallbackStruct *cbs;
{
   static Boolean handling_newline = False;

   dont_move = 0;
   do_flush  = False;

   /*
    * Prevent deletion of the prompt.
    */
   if (application_prompt && !changing_text) {
      char *input = XmTextGetString(inputText);
      XmTextPosition pos = strlen(application_prompt);
      if (!strncmp(input, application_prompt, (size_t)pos) &&
          cbs->startPos < pos) {
         XtFree(input);
         if (cbs->endPos > pos) {
            char *s = XtCalloc(cbs->text->length+1, 1);
            strncat(s, cbs->text->ptr, cbs->text->length);
            XmTextReplace(inputText, pos, cbs->endPos, s);
            XmTextSetInsertionPosition(inputText, cbs->endPos);
            XtFree(s);
         }
         cbs->doit = False;
         return;
      }
      XtFree(input);
   }

   if (!cbs->text->length)
      return;

   /*
    * When input pad not in hold mode force \n character at end of line,
    * like korn shell.
    */
   if (!handling_newline && !hold_input &&
       cbs->text->length == 1 && cbs->text->ptr[0] == '\n') {
      char *s, *s1, *input = XmTextGetString(inputText);
      XmTextPosition pos;
      if ( (s = strchr(input,'\n')) != NULL )
         pos = s-input;
      else
         pos = XmTextGetLastPosition(inputText);

      handling_newline = True;    /* flag to prevent recursion */
      s1 = XtCalloc(cbs->text->length+1, 1);
      strncat(s1, cbs->text->ptr, cbs->text->length);
      XmTextInsert(inputText, pos, s1);
      XmTextSetInsertionPosition(inputText, pos+1);
      XtFree(s1);
      handling_newline = False;

      XtFree(input);
      cbs->doit = False;
      return;
   }

   if (strchr(cbs->text->ptr, '\n'))
      do_flush = True;
}

/***********************************************************************
 *                                                                     *
 *   Input cursor move callback routine. When the cursor is moved down *
 *   a line the line will be submitted.                                *
 *                                                                     *
 ***********************************************************************/
static void input_move_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmTextVerifyCallbackStruct *cbs;
{
   XmTextPosition  pos;

   /* prevent recursive calling */
   if (changing_text) return;

   if (dont_move > 0) {
      cbs->doit = False;
      dont_move--;
      return;
   }

   /*
    * Prevent the cursor from moving into the prompt.
    */
   if (application_prompt) {
      char *input = XmTextGetString(inputText);
      pos = strlen(application_prompt);
      if (!strncmp(input, application_prompt, (size_t)pos) &&
          cbs->newInsert < pos) {
         XtFree(input);
         XmTextSetInsertionPosition(inputText, pos);
         cbs->doit = False;
         return;
      }
      XtFree(input);
   }

   /*
    * If the cursor is moved down, flush the buffer.
    *
    * newInsert is not correct if the cursor is moved down to a shorter line
    * using the next-line() or process-down() actions. In that case the
    * new cursor position is the old position + the line length. To correct
    * this the widget calls two more times the cursor position routines
    * (that in turn call this callback). Check what 1.2 is doing with this.
    */
   if (cbs->newInsert > cbs->currInsert) {
      pos = cbs->newInsert;

      flush_input(&cbs->newInsert);

      /*
       * If pos has been changed in flush_input cancel motion that
       * triggered this callback
       */
      if (pos != cbs->newInsert)
            cbs->doit = False;
   }
}

/***********************************************************************
 *                                                                     *
 *   Input changed callback routine. If a piece of text is inserted    *
 *   all lines terminated by \n will be submitted (unless the Hold     *
 *   button is active). The global flag changing_text prevents         *
 *   recursive calling of this routine when deleting text (in input_cb *
 *   and flush_input.                                                  *
 *                                                                     *
 ***********************************************************************/
static void input_changed_cb( w, cd, cbs )
     Widget w;
     XtPointer cd;
     XmAnyCallbackStruct *cbs;
{
   if (changing_text) return;

   if (do_flush) {
      flush_input(0);
      do_flush = False;
   }
}

/***********************************************************************
 *                                                                     *
 *   Action routine that will send a signal to the parent process.     *
 *                                                                     *
 ***********************************************************************/
static void SendSignal( w, event, argv, args )
     Widget w;
     XEvent *event;
     String *argv;
     Cardinal *args;
{
   if (*args == 1 && !strcmp(argv[0], "SIGINT")) {
      if (parent_pid) {
#ifdef vms
         int kxterm_lock[2];
         int sigint_lock[2];
         char lock_name[32];
         struct dsc$descriptor_s lock;

         /*
          * We cannot use kill() to send a SIGINT from kxterm to the main
          * process.  Instead we requests the lock which calls the blocking AST
          * in the main process.
          */

         sprintf( lock_name, "KXTERM%d", parent_pid );
         var_descriptor( lock, lock_name );
         sys$enqw( 0, LCK$K_EXMODE, kxterm_lock, 0,
                  &lock, 0, NULL, 0, NULL, PSL$C_USER, 0 );

         sprintf( lock_name, "SIGINT%d", parent_pid );
         var_descriptor( lock, lock_name );
         sys$enqw( 0, LCK$K_EXMODE, sigint_lock, 0,
                  &lock, 0, NULL, 0, NULL, PSL$C_USER, 0 );

         sys$deq( sigint_lock[1], NULL, PSL$C_USER, 0 );
         sys$deq( kxterm_lock[1], NULL, PSL$C_USER, 0 );
#else
         kill(parent_pid, SIGINT);
#endif
       }
   } else
      fprintf(stderr, "%s: Wrong signal specified.\n", prog_name);
}

/***********************************************************************
 *                                                                     *
 *   Action routine that will get the previous command into            *
 *   the input pad.                                                    *
 *                                                                     *
 ***********************************************************************/
static void GetPreviousCommand( w, event, argv, args )
     Widget w;
     XEvent *event;
     String *argv;
     Cardinal *args;
{
   XmTextPosition   pos;
   char            *s, *text, *line;
   static int       line_cnt = 0;
   static int       line_max = -999999;

   if (w == inputText) {

      /*
       * Return if input is on hold or if input pad contains more than one line.
       */
      if (hold_input) return;

      s = XmTextGetString(inputText);
      if (strchr(s, '\n')) {
         XtFree(s);
         return;
      }

      pos = strlen(s);
      XtFree(s);

      /* get input transcript text */
      if (input_tr_text)
         text = input_tr_text;
      else {
         if (!(text = XmTextGetString(inputTrText)) || !*text)
            return;
         input_tr_text = text;
         line_cnt = 0;
         line_max = -999999;
      }

      if (*args == 1 && !strcmp(argv[0], "NEXT"))
         line_cnt++;
      else
         line_cnt--;

      if (line_cnt > 0) {
         line_cnt = 0;
         XBell(UxDisplay, 0);
         return;
      }

      if (line_cnt < line_max) {
         line_cnt = line_max;
         XBell(UxDisplay, 0);
         return;
      }

      line = get_line(text, line_cnt, LINE);

      if (application_prompt)
         XmTextReplace(inputText, (XmTextPosition)strlen(application_prompt),
                       pos, line);
      else
         XmTextReplace(inputText, (XmTextPosition) 0, pos, line);

      pos = XmTextGetLastPosition(inputText);
      XmTextSetInsertionPosition(inputText, pos);

      if (!line && line_cnt < 0) {
         XBell(UxDisplay, 0);
         line_max = line_cnt;
      }

      XtFree(line);
   }
}

/***********************************************************************
 *                                                                     *
 *   Action routine that will resubmit the last executed command.      *
 *                                                                     *
 ***********************************************************************/
static void ResubmitCommand( w, event, argv, args )
     Widget w;
     XEvent *event;
     String *argv;
     Cardinal *args;
{
   XmTextPosition   pos;
   char            *text, *cmd, *line = NULL;

   if (w == inputText) {

      cmd = last_command;

   } else {

      int l;

      if (lower_text)
         free_transcript_text();

      if (transcript_text)
         text = transcript_text;
      else {
         if (!(text = XmTextGetString(outputText)) || !*text)
            return;
         transcript_text = text;
      }
      pos = XmTextGetInsertionPosition(outputText);
      line = get_line(text, (int)pos, CURSOR);
      if ((l = begins_with_prompt(line)))
         cmd = line + l;
      else
         cmd = line;

   }

   if (!strempty(cmd)) {
      if (*args == 1 && !strcmp(argv[0], "EXEC")) {
         char *s = XtMalloc(strlen(cmd) + 2);
         sprintf(s, "%s\n", cmd);
         insert_string(inputText, s, True);
         XtFree(s);
      } else
         insert_string(inputText, cmd, True);
   }

   XtFree(line);
}

/***********************************************************************
 *                                                                     *
 *   Toggle the Hold button.                                           *
 *                                                                     *
 ***********************************************************************/
static void ToggleHold( w, event, argv, args )
     Widget w;
     XEvent *event;
     String *argv;
     Cardinal *args;
{
   XmAnyCallbackStruct   cbs;

   cbs.reason = 0;
   cbs.event  = (XEvent *) NULL;

   if (w == inputText)
      XtCallCallbacks(inputHoldButton, XmNactivateCallback, &cbs);
   else
      XtCallCallbacks(outputHoldButton, XmNactivateCallback, &cbs);
}

/***********************************************************************
 *                                                                     *
 *   Watch the state of the window (iconic or normal).                 *
 *   If the state becomes normal send the command #@iconify(MAP) to    *
 *   the application. KUIP will interpret this and de-iconify the      *
 *   toplevel shells of the application.                               *
 *                                                                     *
 ***********************************************************************/
static void state_watcher( w, client_data, event )
     Widget w;
     XtPointer client_data;
     XConfigureEvent *event;
{
   char             cmd[80];
   Position         x, y;
   Dimension        width, height;
   static int       init = 0;

   switch (event->type) {
      case MapNotify:           /* de-iconify */
         /*
          * De-iconfy all toplevel shells when kxterm was iconified using
          * the Iconify menu item.
          */
         if (kxterm_iconic) {
            sprintf(cmd, "%siconify(MAP)\n", ESCAPE);
            send_kxterm_cmd(cmd);
            kxterm_iconic = False;
         }
         break;
      case UnmapNotify:         /* iconify */
         break;
      case ConfigureNotify:
         XtVaGetValues(XtParent(kuipIo), XmNx, &x, XmNy, &y,
                       XmNwidth, &width, XmNheight, &height, NULL);
         /*
          * The first time this routine is called with the real size of the
          * toplevel shell, the other times the size of the toplevel shell
          * has been reduced by the WM decorations.
          */
         if (init) {
            sprintf(cmd, "%sgeometry(%d,%d,%d,%d)\n", ESCAPE, x, y,
                    width, height);
            send_kxterm_cmd(cmd);
         }

         if (!init)
            init++;
         break;
      default:
         break;
   }
}

/***********************************************************************
 *                                                                     *
 *   Initialize kuipIo interface.                                      *
 *                                                                     *
 ***********************************************************************/
static void init_kuipio()
{
   Pixmap                 pixmap;
   Display               *display;
   XColor    color_defs[2];
   Pixmap    hourglass_pixmap, hourglass_mask_pixmap;
   XmAnyCallbackStruct    cbs;

   /* create interface */
   kuipIo = create_kuipIo();

   /* set Kxterm icon */
   display = XtDisplay(kuipIo);
   pixmap = XCreateBitmapFromData(display, DefaultRootWindow(display),
                                  kxterm_bits, kxterm_width, kxterm_height);
   XtVaSetValues(XtParent(kuipIo), XmNiconPixmap, pixmap, NULL);

   /* set Kxterm cursors */
   main_cursor = XCreateFontCursor(UxDisplay, XC_left_ptr);
   /* build wait cursor with hourglass pixmap */
   color_defs[0].pixel = BlackPixel(UxDisplay, UxScreen);
   color_defs[1].pixel = WhitePixel(UxDisplay, UxScreen);
   XQueryColors(UxDisplay, UxColormap, color_defs, 2);
   hourglass_pixmap = XCreatePixmapFromBitmapData(UxDisplay,
                         DefaultRootWindow(UxDisplay), hourglass_bits,
                         hourglass_width, hourglass_height,
                         WhitePixel(UxDisplay, UxScreen),
                         BlackPixel(UxDisplay, UxScreen), 1);
   hourglass_mask_pixmap = XCreatePixmapFromBitmapData(UxDisplay,
                         DefaultRootWindow(UxDisplay), hourglass_mask_bits,
                         hourglass_mask_width, hourglass_mask_height,
                         WhitePixel(UxDisplay, UxScreen),
                         BlackPixel(UxDisplay, UxScreen), 1);
   wait_cursor = XCreatePixmapCursor(UxDisplay, hourglass_pixmap,
                                     hourglass_mask_pixmap,
                                     &color_defs[0], &color_defs[1],
                                     hourglass_x_hot, hourglass_y_hot);

   /* add callback to window manager close button */
   InstallMwmCloseCallback(kuipIo, (XtCallbackProc)exit_cb,
                           (XtPointer)NULL);

   /* install new action functions */
   XtAppAddActions(UxAppContext, actions, XtNumber(actions));

   /* override translation table for inputText and outputText widget */
   XtOverrideTranslations(inputText,
                          XtParseTranslationTable(translation_table));
   XtOverrideTranslations(outputText,
                          XtParseTranslationTable(translation_table));

   /* add File menu callbacks */
   XtAddCallback(aboutButton, XmNactivateCallback,
                 (XtCallbackProc)about_cb, NULL);
   XtAddCallback(aboutAppButton, XmNactivateCallback,
                 (XtCallbackProc)about_cb, NULL);
   XtAddCallback(saveTransButton, XmNactivateCallback,
                 (XtCallbackProc)save_cb, NULL);
   XtAddCallback(saveTransAsButton, XmNactivateCallback,
                 (XtCallbackProc)show_save_cb, NULL);
   XtSetSensitive(printButton, False);
   XtAddCallback(killButton, XmNactivateCallback, (XtCallbackProc)exit_cb,
                 NULL);
   XtAddCallback(exitButton, XmNactivateCallback, (XtCallbackProc)exit_cb,
                 NULL);

   /* add Edit menu callbacks */
   XtAddCallback(cutButton, XmNactivateCallback,
                 (XtCallbackProc)cut_paste_cb, (int *)0);
   XtAddCallback(copyButton, XmNactivateCallback,
                 (XtCallbackProc)cut_paste_cb, (int *)1);
   XtAddCallback(pasteButton, XmNactivateCallback,
                 (XtCallbackProc)cut_paste_cb, (int *)2);
   XtAddCallback(searchButton, XmNactivateCallback,
                 (XtCallbackProc)show_search_cb, NULL);

   /* add View menu callbacks */
   XtAddCallback(showInputButton, XmNactivateCallback,
                 (XtCallbackProc)show_input_cb, NULL);

   /* create input transcript dialog, but do not popup dialog */
   cbs.reason = 0;
   cbs.event = (XEvent *) NULL;
   XtCallCallbacks(showInputButton,XmNactivateCallback, &cbs);

   /* add Option menu callbacks */
   XtAddCallback(clearTrButton, XmNactivateCallback,
                 (XtCallbackProc)clear_top_cb, NULL);

   /* add Help menu callbacks */
   XtAddCallback(helpKxtermButton, XmNactivateCallback,
                 (XtCallbackProc)help_cb, (int *)0);
   XtAddCallback(helpResourceButton, XmNactivateCallback,
                 (XtCallbackProc)help_cb, (int *)1);
   XtAddCallback(helpEditButton, XmNactivateCallback,
                 (XtCallbackProc)help_cb, (int *)2);

   /* hold push button callbacks */
   XtAddCallback(inputHoldButton, XmNactivateCallback,
                 (XtCallbackProc)toggle_hold_button, NULL);
   XtAddCallback(outputHoldButton, XmNactivateCallback,
                 (XtCallbackProc)toggle_hold_button, NULL);

   /* input, output text callbacks */
   XtAddCallback(inputText, XmNmodifyVerifyCallback, (XtCallbackProc)input_cb,
                 NULL);
   XtAddCallback(inputText, XmNmotionVerifyCallback,
                 (XtCallbackProc)input_move_cb, NULL);
   XtAddCallback(inputText, XmNvalueChangedCallback,
                 (XtCallbackProc)input_changed_cb, NULL);

   /*
    * Add application name to some menu buttons
    */
   if (application_name) {
      char *s = XtMalloc(strlen(application_name) + 10);
      sprintf(s, "About %s...", application_name);
      XtVaSetValues(aboutAppButton, RES_CONVERT(XmNlabelString, s), NULL);

      sprintf(s, "Kill %s", application_name);
      XtVaSetValues(killButton, RES_CONVERT(XmNlabelString, s), NULL);
      XtFree(s);
   } else
      XtUnmanageChild(aboutAppButton);

   /*
    * Add tab groups for using with explicit keyboard focus policy.
    */
   TurnOffSashTraversal(panedWindow);
   XtVaSetValues(inputHoldButton,  XmNtraversalOn, False,
                                   XmNnavigationType, XmNONE, NULL);
   XtVaSetValues(outputHoldButton, XmNtraversalOn, False,
                                   XmNnavigationType, XmNONE, NULL);
   XtVaSetValues(inputText,        XmNtraversalOn, True,
                                   XmNnavigationType, XmTAB_GROUP, NULL);
   XtVaSetValues(outputText,       XmNtraversalOn, True,
                                   XmNnavigationType, XmTAB_GROUP, NULL);

   /* popup interface */
   UxPopupInterface(kuipIo, XtGrabNone);

   /*
    * when explicit set the keyboard focus to inputText (after the
    * interface has been poped up)
    */
   XmProcessTraversal(inputText, XmTRAVERSE_CURRENT);
}

/***********************************************************************
 *                                                                     *
 *   Get non X11 command line options.                                 *
 *                                                                     *
 ***********************************************************************/
static void get_options( argc, argv )
     int argc;
     char **argv;
{
   int     c;
   char   *arg, *opt, *s;

   prog_name = strdup(argv[0]);

   /* scan options */
   while (--argc) {
      arg = *++argv;
      c = *arg;
      if (c == '-') {
         opt = ++arg;
         if (!strcmp(opt, "pid")) {
            arg = *++argv; --argc;
            parent_pid = atoi(arg);
         }
         else if (!strcmp(opt, "prompt")) {
            arg = *++argv; --argc;
            application_prompt = strdup(arg);
            if ((s = strchr(application_prompt, '>'))) {
               application_name = calloc((s-application_prompt)+1, 1);
               strncpy(application_name, application_prompt,
                       s-application_prompt);
            }
         }
         else {
            fprintf(stderr,"\
Usage: %s [class] [-name name] [-title title] [-n iconname]\n\
     [-geometry geom] [-font fn] [-tfont fn]\n\
     [-iconic] [-pid pid] [-prompt prompt]\n",
                    prog_name);
            exit(1);
         }
      }
   }
}


#ifdef vms
/***********************************************************************
 *                                                                     *
 *   Check if application is dead or alive.                            *
 *                                                                     *
 ***********************************************************************/
static void application_died( int ppid )
{
  if( ppid != 0 ) {
    static int alive_lock[2];
    char lock_name[32];
    struct dsc$descriptor_s lock;

    application_alive = True;
    sprintf( lock_name, "ALIVE%d", ppid );
    var_descriptor( lock, lock_name );
    sys$enq( 0, LCK$K_EXMODE, alive_lock, 0,
             &lock, 0, application_died, 0, NULL, PSL$C_USER, 0 );
  }
  else {
    /* the lock has been granted because the application has died */
    pipe_broken();
  }
}
#endif


/***********************************************************************
 *                                                                     *
 *   Main program.                                                     *
 *                                                                     *
 ***********************************************************************/
int main( argc, argv )
     unsigned int argc;
     char **argv;
{

#if XtSpecificationRelease <= 4 /* X11R4 */
   Cardinal xtargc = argc;
#else                           /* X11R5 */
   int xtargc = argc;
#endif
   char *class = "KXterm";

   if (argv[1] && argv[1][0] != '-')
      class = argv[1];

   /* init toolkit */
   UxTopLevel = XtAppInitialize(&UxAppContext, class,
                                options, XtNumber(options),
                                &xtargc, argv, fallbacks, NULL, 0);
   get_options(xtargc, argv);

   UxDisplay = XtDisplay(UxTopLevel);
   UxScreen = XDefaultScreen(UxDisplay);
   UxColormap = DefaultColormap(UxDisplay, UxScreen);

   XtVaSetValues(UxTopLevel,
                 XmNmappedWhenManaged, False,
                 XmNx, DisplayWidth(UxDisplay, UxScreen)/2,
                 XmNy, DisplayWidth(UxDisplay, UxScreen)/2,
                 XmNwidth, 1,
                 XmNheight, 1,
                 NULL);

   /* create and manage interface */
   init_kuipio();

   /* open tmpfile to save transcript pad */
   if (!(tfp = tmpfile())) {
      insert_string(outputText,
                    "*** Cannot open tmp file to save transcript pad ***\n",
                    False);
      XtSetSensitive(saveTransButton, False);
      XtSetSensitive(saveTransAsButton, False);
   }
   else {
      tr_tmp_file = True;
      /* since we don't know the filename created by tmpfile(), make it
       * the null string: */
      strcpy(transcript_file, "");
   }

   /* unbuffered I/O */
   setbuf(stdin,  NULL);
   setbuf(stdout, NULL);

   /*
    * Register kxterm actions
    */
   kxterm_add_actions(kxterm_actions);

   /*
    * Install an event handler that looks for StructureNotify events
    * to see if kxterm is being iconified, de-iconified, resized or moved.
    */
   XtAddEventHandler(XtParent(kuipIo), StructureNotifyMask, False,
                     (XtEventHandler)state_watcher, (XtPointer)NULL);

   /*
    * Add callback get_from_application() -- invoked when input is available
    * from stdin.
    */
#ifdef VMS
   application_died( parent_pid );

   input_id = XtAppAddInput(UxAppContext,
                 MBX_EVENT_FLAG,         /* On OpenVMS, wait on event flag  */
                 &MbxMessage.ioStatus,   /* I/O Status Block                */
                 (XtInputCallbackProc)get_from_application,
                 outputText);
   MbxSetup();
#else  /* VMS */
   input_id = XtAppAddInput(UxAppContext, fileno(stdin),
                            (XtPointer)XtInputReadMask,
                            (XtInputCallbackProc)get_from_application,
                            outputText);
#endif /* VMS */

   /* send parent a message that we are ready to receive from its stdout */
   printf( "\n" );

   /* kxterm is ready for input; show prompt */
   show_prompt();

   /* start loop */
   XtRealizeWidget(UxTopLevel);
   XtAppMainLoop(UxAppContext);

   return 0;
}


