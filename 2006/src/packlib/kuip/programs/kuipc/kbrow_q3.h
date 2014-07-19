/*
 * $Id: kbrow_q3.h,v 1.2 1996/04/16 13:42:57 gunter Exp $
 *
 * $Log: kbrow_q3.h,v $
 * Revision 1.2  1996/04/16 13:42:57  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:38  gunter
 * Mods to compile kuipc.
 *  The header files can be deleted and remade from the .m4 files. Use gnu's m4,
 *  ie.:
 *  gmake headers M4=/usr/local/bin/m4
 *  And then
 *  gmake kuipc
 *
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */


"typedef struct _KmIcon {        /*                                         */ \
  struct _KmIcon *next;         /* link to next icon                       */ \
  char       *name;             /* unique identifier name                  */ \
  int         width;            /* width of the pixmap                     */ \
  int         height;           /* height of the pixmap                    */ \
  char       *bitmap;           /* bitmap data                             */ \
  KmPixmap    pix;              /* filled in Motif part                    */ \
  KmPixmap    hi_pix;           /* high lighted pixmap                     */ \
} KmIcon;                       /*                                         */ \
                                /*                                         */ \
typedef struct _KmClass {       /*                                         */ \
  struct _KmClass *next;        /* link to next object class               */ \
  int         is_dir;           /* flag if class has is a directory        */ \
  char       *name;             /* unique identifier name                  */ \
  char       *title;            /* title for popup menu (maybe NULL)       */ \
  char       *big_icon;         /* name of the big icon                    */ \
  KmIcon     *bicon;            /* pointer to the big icon structure       */ \
  char       *sm_icon;          /* name of the small icon                  */ \
  KmIcon     *sicon;            /* pointer to the small icon structure     */ \
  SUBROUTINE *user_icon_F;      /* user function to return icon bitmap     */ \
  IntFunc    *user_icon_C;      /* user function to return icon bitmap     */ \
  BrAction   *cont;             /* list of actions in content window       */ \
  BrAction   *graf;             /* list of actions in graphics window      */ \
  int         obj_count;        /* number of objects in content window     */ \
} KmClass;                      /*                                         */ \
                                /*                                         */ \
"
