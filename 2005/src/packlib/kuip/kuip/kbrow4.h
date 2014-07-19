typedef enum {                  /*                                         */ __
  KmButtSensitive       = 0x00, /* sensitive button                        */ __
  KmButtNonSensitive    = 0x01, /* non-sensitive button ('NS')             */ __
  KmButtToggleSensitive = 0x02, /* toggle-sensitive button ('TS')          */ __
  KmButtSensitivityMask = 0x03, /* mask for sensitivity type               */ __
  KmButtSeparator       = 0x04, /* put separator in menu ('/')             */ __
  KmButtBrowser         = 0x08  /* button is in main browser ('BR')        */ __
} KmButtFlag;                   /*                                         */ __
                                /*                                         */ __
typedef struct _KmButton {      /*                                         */ __
  struct _KmButton *next;       /* button label or menu item               */ __
  char       *menu;             /* menu name or NULL for buttons           */ __
  char       *label;            /* button label or menu item               */ __
  SUBROUTINE *call_F;           /* callback routine (FORTRAN)              */ __
  IntFunc    *call_C;           /* callback routine (C)                    */ __
  char       *action;           /* name of callback routine                */ __
  char       *mnemo;            /* button mnemonic                         */ __
  char       *accel;            /* button accelerator                      */ __
  char       *accel_text;       /* button accelerator text                 */ __
  KmButtFlag  flags;            /* sensitivity type etc.                   */ __
  KmWidget    widget;           /* Motif widget ID                         */ __
} KmButton;                     /*                                         */ __
                                /*                                         */ __
