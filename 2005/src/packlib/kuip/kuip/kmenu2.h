typedef struct {                /* file name                               */ __
  char         *filter_default; /* filter wildcard                         */ __
  char         *filter_current; /* current filter                          */ __
} KmParFile;                    /*                                         */ __
                                /*                                         */ __
typedef struct {                /*                                         */ __
  char         *range_lower;    /* lower value of range                    */ __
  char         *range_upper;    /* upper value of range                    */ __
  char         *slider_lower;   /* lower limit for slider                  */ __
  char         *slider_upper;   /* upper limit for slider                  */ __
  int           decimals;       /* number of decimals used for slider      */ __
} KmParInt;                     /*                                         */ __
                                /*                                         */ __
typedef struct {                /* option                                  */ __
  char        **text;           /* explanations (parallel to range_value)  */ __
  int          *mutex;          /* mutex group to which text belongs       */ __
  int          *radio;          /* radio group to which text belongs       */ __
} KmParOption;                  /*                                         */ __
                                /*                                         */ __
