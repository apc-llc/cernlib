typedef enum {                  /*                                         */ __
  KmFLAG_FORGET = 0x01,         /* last value is not kept for Motif panels */ __
  KmFLAG_MINUS  = 0x02,         /* -VALUE is not an abbrev for CHOPT=VALUE */ __
  KmFLAG_QUOTE  = 0x04,         /* do not remove quotes                    */ __
  KmFLAG_VARARG = 0x08,         /* append additional args to this param.   */ __
  KmFLAG_CONST  = 0x10,         /* do not allow to assign a value          */ __
  KmFLAG_HIDDEN = 0x20,         /* do not show in menus                    */ __
  KmFLAG_SEPARATE = 0x40        /* treat arguments as separate tokens      */ __
} KmParFlag;                    /*                                         */ __
                                /*                                         */ __
typedef enum {                  /*                                         */ __
  KmTYPE_CHAR   = 'C',          /* character string                        */ __
  KmTYPE_FILE   = 'F',          /* file name                               */ __
  KmTYPE_INT    = 'I',          /* integer                                 */ __
  KmTYPE_OPTION = 'O',          /* option                                  */ __
  KmTYPE_REAL   = 'R'           /* real                                    */ __
} KmParType;                    /*                                         */ __
                                /*                                         */ __
