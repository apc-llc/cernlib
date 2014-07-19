typedef struct _KmCommand {     /*                                         */ __
  struct _KmCommand *next;      /* link to next command                    */ __
  char         *path;           /* command path                            */ __
  char         *name;           /* command name                            */ __
  int           hidden;         /* flag if command is invisible            */ __
  int           level;          /* depth of submenus                       */ __
  int           total;          /* total number of parameters              */ __
  int           mandatory;      /* number of mandatory parameters          */ __
  KmParameter **par;            /* list of total parameter descriptions    */ __
  int           list_par;       /* index+1 of parameter taking a list      */ __
  int           xcount;         /* count number of action calls            */ __
  SUBROUTINE   *action_F;       /* action routine                          */ __
  IntFunc      *action_C;       /* action routine                          */ __
  SUBROUTINE   *user_help_F;    /* user help routine                       */ __
  pCharFunc    *user_help_C;    /* user help routine                       */ __
  int          nguidance;       /* number of lines in guidance text        */ __
  char        **guidance;       /* help text                               */ __
  int          nkeyword;        /* number of lines for keywords            */ __
  char        **keyword;        /* list of keywords                        */ __
  int          nhlink;          /* number of lines for links               */ __
  char        **hlink;          /* list of links                           */ __
  int           argc;           /* number of arguments entered             */ __
  char        **argv;           /* argc argument values                    */ __
  char         *argline;        /* argument line as entered                */ __
  int          *argoffs;        /* argc offsets into argline for KUGETE    */ __
} KmCommand;                    /*                                         */ __
                                /*                                         */ __
