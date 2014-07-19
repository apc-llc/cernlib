/*
 * $Id: kflag.h,v 1.1.1.1 1996/03/08 15:33:00 mclareni Exp $
 *
 * $Log: kflag.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */

typedef enum {
  KmMACRO_COMMAND = 0,          /* don't look for macros */
  KmMACRO_AUTO,                 /* look for macros before commands */
  KmMACRO_AUTOREVERSE           /* look for macros after commands */
} KmMacOrder;

typedef enum {
  KmSTYLE_A  = 0x0001,          /* Alpha menus */
  KmSTYLE_C  = 0x0002,          /* Command line */
  KmSTYLE_G  = 0x0004,          /* Graphics menus */
  KmSTYLE_M  = 0x0008,          /* Model Human Interface */
  KmSTYLE_XM = 0x0020,          /* Motif/X11 */
  KmSTYLE_major = 0x00FF,       /* A..X are mutually exclusive */
  KmSTYLE_xL = 0x0100,          /* Alpha Letter menus */
  KmSTYLE_xP = 0x0200,          /* Panel style GP or MP */
  KmSTYLE_xS = 0x0400,          /* Graphics with Software fonts */
  KmSTYLE_xW = 0x0800           /* Graphics with shadowed Width */
} KmStyleFlag;

typedef enum {
  KmTIMING_OFF,                 /* no timing */
  KmTIMING_ON,                  /* time typed commands */
  KmTIMING_ALL                  /* time individual commands inside macro */
} KmTiming;


EXTERN struct {
  int         do_exit;          /* set by KXEXIT */
  int         do_quit;          /* set by KXQUIT */
  char       *curr_prompt;      /* current prompt string */
  char       *last_cmd;         /* last command for $LAST */
  KmTiming    timing;           /* timing on/off/all */
  time_t      real_time;        /* real time at last timing off */
  clock_t     user_time;        /* CPU time at last timing off */
  int         in_macro;         /* command executed in macro */
  int         in_application;   /* application mode is active */
  int         appl_called;      /* application is executing */
  char       *appl_exit;        /* string which leaves application mode */
  KmCommand  *appl_cmd;         /* command which handles application */
  char        appl_file[256];   /* temporary file to pass application text */
  FILE       *appl_stream;      /* C stream used for writing appl_file */
  int         appl_luno;        /* Fortran logical unit opened for appl_file */
  char        help_file[256];   /* temporary file to view help text */
  char        uhlp_file[256];   /* temporary file to get user help text */
  int         uhlp_luno;        /* Fortran logical unit opened for user help */
  int         in_motif;         /* Motif mode is active */
  int         action_nesting;   /* levels of nested ku_exec() calls */
  int         echo_command;     /* echo commands in Motif mode */
  char       *echo_prompt;      /* prompt string for echo commands */
  KmStyleFlag style;            /* input mode */
  int         try_getenv;       /* flag if $name should look-up environment */
  int         keep_fcase;       /* flag if no case conversion for filenames */
  int         use_kxterm;       /* flag if kxterm should be used */
  int         use_server;       /* flag if edit server should be used */
  int         editor_exit;      /* flag set if edit server sent SIGUSR1 */
  char       *editor_cbuf;      /* buffer of for edit servers commands */
  char       *macro_path;       /* MACRO/DEFAULT search path */
  KmMacOrder  macro_search;     /* MACRO/DEFAULT search order */
  int         temp_vectors;     /* number of ?SIGMA vectors */
  char        init_wdir[256];   /* initial working directory */
  int         vread_luno;       /* Fortran logical unit for VECTOR/READ */
  int         vwrite_luno;      /* Fortran logical unit for VECTOR/WRITE */
  int         trace_on;         /* flag set by TRACE ON */
  int         trace_wait;       /* flag set by TRACE ON WAIT */
  int         trace_wait_save;  /* flag set by TRACE ON WAIT */
  int         trace_full;       /* flag set by TRACE ON FULL */
  int         trace_debug;      /* flag set by TRACE ON DEBUG */
  int         trace_test;       /* flag set by TRACE ON TEST */
  int         quit_macro;       /* flag set by WAIT command */
} kc_flags;


EXTERN struct {
  char       *set_break;        /* SET_SHOW/BREAK */
  char        set_columns[8];   /* SET_SHOW/COLUMNS */
  char       *set_command;      /* SET_SHOW/COMMAND */
  char       *set_dollar;       /* SET_SHOW/DOLLAR */
  char       *set_filecase;     /* SET_SHOW/FILECASE */
  char       *set_host_editor;  /* SET_SHOW/HOST_EDITOR */
  char       *set_host_psviewer; /* SET_SHOW/HOST_PSVIEWER */
  char       *set_host_pager;   /* SET_SHOW/HOST_PAGER */
  char       *set_host_shell;   /* SET_SHOW/HOST_SHELL */
  char       *set_prompt;       /* SET_SHOW/PROMPT */
  char       *set_recall_style; /* SET_SHOW/RECALL_STYLE */
  char        set_recording[8]; /* SET_SHOW/RECORDING */
  char       *set_root;         /* SET_SHOW/ROOT */
  const char *set_style;        /* SET_SHOW/STYLE */
  char       *set_timing;       /* SET_SHOW/TIMING */
  char       *help_edit;        /* HELP edit mode */
  char       *defaults_path;    /* MACRO/DEFAULTS search path */
  char       *defaults_order;   /* MACRO/DEFAULTS search order */
  int         idle_time;        /* IDLE timer in seconds */
  char       *idle_cmd;         /* IDLE command */
  char      **set_print_cmds;   /* SET_SHOW/HOST_PRINTER commands */
  char      **set_print_exts;   /* ... depending on file extension */
} kc_value;


typedef struct {
  int         top;
  int         left;
  int         width;
  int         height;
} KmWindowDsc;

EXTERN struct {
  KmWindowDsc edit_pad;         /* coordinates for edit window */
  KmWindowDsc help_pad;         /* coordinates for readonly window */
  int         voffset;          /* vertical offset */
  int         hoffset;          /* horizontal offset */
  int         shift_max;        /* maximum number of shifted pads */
  int         shift_now;        /* current shift count */
  int         shift_dir;        /* shift direction +/-1 */
  int         is_a_pad;         /* flag if running in an Apollo DM pad */
  int         is_a_tty;         /* flag if stdin and stdout at terminal */
  int         use_getline;      /* flag for using getline() or normal read */
  int         use_no_echo;      /* flag for using no echo in password prompt */
  int         term_width;       /* terminal width in columns */
  int         kuwhag_called;    /* allow style G */
  float       sgylen;
  float       sgsize;
  float       sgyspa;
  float       sgbord;
  int         panel_rows;       /* number of rows in style GP panel */
  int        *panel_cols;       /* number of columns in each row */
  char     ***panel_keys;       /* key labels */
  char       *panel_keynum;     /* value of $KEYNUM */
  char       *panel_keyval;     /* value of $KEYVAL */
  char     ***panel_icons;      /* key icons */
  char     ***panel_opts;       /* key options */
  char       *panel_title;      /* panel title (given when displaying panel) */
  char       *panel_name;       /* panel name (given when starting panel) */
  int         palette_flag;     /* flag if palette is requested */
} kc_window;


extern const char* style_name( KmStyleFlag );


