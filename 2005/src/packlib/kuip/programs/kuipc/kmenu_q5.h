/*
 * $Id: kmenu_q5.h,v 1.2 1996/04/16 13:43:03 gunter Exp $
 *
 * $Log: kmenu_q5.h,v $
 * Revision 1.2  1996/04/16 13:43:03  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:48  gunter
 * Mods to compile kuipc.
 *  The header files can be deleted and remade from the .m4 files. Use gnu's m4,
 *  ie.:
 *  gmake headers M4=/usr/local/bin/m4
 *  And then
 *  gmake kuipc
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */


"typedef struct _KmMenu {        /*                                         */ \
  struct _KmMenu *next;         /* link to next menu                       */ \
  struct _KmMenu *down;         /* link to submenu                         */ \
  char         *path;           /* path of parent menu                     */ \
  char         *name;           /* menu name                               */ \
  int           level;          /* depth of submenus                       */ \
  KmCommand    *cmds;           /* link to first command                   */ \
  int          nguidance;       /* number of lines in guidance text        */ \
  char        **guidance;       /* help text                               */ \
  int          nkeyword;        /* number of lines for keywords            */ \
  char        **keyword;        /* list of keywords                        */ \
  int          nhlink;          /* number of lines for links               */ \
  char        **hlink;          /* list of links                           */ \
} KmMenu;                       /*                                         */ \
                                /*                                         */ \
extern void klnkbrcl();         /*                                         */ \
extern void klnkicon();         /*                                         */ \
extern void klnkkmcl();         /*                                         */ \
extern void klnkmenu();         /*                                         */ \
                                /*                                         */ \
"
