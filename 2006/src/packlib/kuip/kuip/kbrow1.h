typedef unsigned long KmPixmap; /* Pixmap from <X11/X.h>                   */ __
typedef void *KmWidget;         /* Widget from <X11/Intrinsic.h>           */ __
typedef void *KmCalldata;       /* XmAnyCallbackStruct from <Motif/Xm.h>   */ __
                                /*                                         */ __
typedef enum {                  /*                                         */ __
  BRACT_OPEN = 0,               /*                                         */ __
  BRACT_ROOT = 1,               /*                                         */ __
  BRACT_CONT = 2,               /*                                         */ __
  BRACT_GRAF = 3                /*                                         */ __
} BrActTag;                     /*                                         */ __
                                /*                                         */ __
typedef enum {                  /*                                         */ __
  BrActUpdate    = 0x01,        /* browser window has to be updated ('!')  */ __
  BrActSeparator = 0x02,        /* put separator in menu ('/')             */ __
  BrActToggle    = 0x04,        /* register as toggle button               */ __
  BrActToggleOn  = 0x08,        /* toggle state is on                      */ __
  BrActSensitive = 0x10         /* button is sensitive                     */ __
} BrActFlag;                    /*                                         */ __
                                /*                                         */ __
