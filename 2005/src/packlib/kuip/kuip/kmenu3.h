typedef KmParInt KmParReal;     /* real and int have the same fields       */ __
                                /*                                         */ __
typedef struct {                /*                                         */ __
  char         *name;           /* parameter name                          */ __
  int           abbrev;         /* minimum length that name is recognized  */ __
  char         *prompt;         /* prompt string                           */ __
  char         *dfault;         /* default value                           */ __
  char         *last;           /* last value for Motif panel (malloced)   */ __
  int           width;          /* width of input field                    */ __
  int           range_count;    /* number of items in range_value          */ __
  char        **range_value;    /* list of allowed values                  */ __
  int           select_count;   /* number of items in select_count         */ __
  char        **select_value;   /* list of values for selection box        */ __
  KmParFlag     flags;          /* special flags                           */ __
  KmParType     type;           /* parameter type                          */ __
  void        *ptype;           /* structure pointer selected by type      */ __
} KmParameter;                  /*                                         */ __
                                /*                                         */ __
