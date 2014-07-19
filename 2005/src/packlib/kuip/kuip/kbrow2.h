typedef struct _BrAction {      /*                                         */ __
  struct _BrAction *next;       /* link to next action binding             */ __
  BrActFlag   flags;            /*                                         */ __
  char       *text;             /* text line in menu                       */ __
  char       *user_text;        /* user text overriding CDF text (malloced)*/ __
  char       *accel;            /* accelerator                             */ __
  char       *exec;             /* action commands                         */ __
  SUBROUTINE *call_F;           /* action routine                          */ __
  IntFunc    *call_C;           /* action function                         */ __
  BrActTag    tag;              /* for which window the action is defined  */ __
  struct _BrClass *class;       /* pointer to BrClass in case of open menu */ __
} BrAction;                     /*                                         */ __
                                /*                                         */ __
typedef struct _BrClass {       /*                                         */ __
  struct _BrClass *next;        /* link to next browsable class            */ __
  char       *name;             /* unique identifier name                  */ __
  char       *title;            /* title for popup menu (maybe NULL)       */ __
  SUBROUTINE *scan_km_F;        /* user function scanning the directory    */ __
  pCharFunc  *scan_km_C;        /* user function scanning the directory    */ __
  SUBROUTINE *scan_br_F;        /* user function scanning for browsables   */ __
  pCharFunc  *scan_br_C;        /* user function scanning for browsables   */ __
  BrAction   *root;             /* list of actions in root window          */ __
  BrAction   *open;             /* list of actions in open menu            */ __
} BrClass;                      /*                                         */ __
                                /*                                         */ __
