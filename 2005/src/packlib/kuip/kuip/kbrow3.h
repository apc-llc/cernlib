typedef struct _KmIcon {        /*                                         */ __
  struct _KmIcon *next;         /* link to next icon                       */ __
  char       *name;             /* unique identifier name                  */ __
  int         width;            /* width of the pixmap                     */ __
  int         height;           /* height of the pixmap                    */ __
  char       *bitmap;           /* bitmap data                             */ __
  KmPixmap    pix;              /* filled in Motif part                    */ __
  KmPixmap    hi_pix;           /* high lighted pixmap                     */ __
} KmIcon;                       /*                                         */ __
                                /*                                         */ __
typedef struct _KmClass {       /*                                         */ __
  struct _KmClass *next;        /* link to next object class               */ __
  int         is_dir;           /* flag if class has is a directory        */ __
  char       *name;             /* unique identifier name                  */ __
  char       *title;            /* title for popup menu (maybe NULL)       */ __
  char       *big_icon;         /* name of the big icon                    */ __
  KmIcon     *bicon;            /* pointer to the big icon structure       */ __
  char       *sm_icon;          /* name of the small icon                  */ __
  KmIcon     *sicon;            /* pointer to the small icon structure     */ __
  SUBROUTINE *user_icon_F;      /* user function to return icon bitmap     */ __
  IntFunc    *user_icon_C;      /* user function to return icon bitmap     */ __
  BrAction   *cont;             /* list of actions in content window       */ __
  BrAction   *graf;             /* list of actions in graphics window      */ __
  int         obj_count;        /* number of objects in content window     */ __
} KmClass;                      /*                                         */ __
                                /*                                         */ __
