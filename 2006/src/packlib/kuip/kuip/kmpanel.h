/*
 * $Id: kmpanel.h,v 1.1.1.1 1996/03/08 15:33:01 mclareni Exp $
 *
 * $Log: kmpanel.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */

/* kmpanel.h: C callable interface to command "panel ..." */
/*
   1) km_panel_close ("panel 0 c ...")
   2) km_panel_display ("panel 0 d ...")
   3) km_panel_reset ("panel 0 r ...")
   4) km_panel_key ("panel x.y command [alias_label] [pixmap]"
   5) km_panel_key_opt ("panel x.y command [alias_label] [pixmap] opt"
*/

#ifdef IBMVM
#define km_panel_close km_pacl
#define km_panel_display km_padi
#define km_panel_reset km_pare
#define km_panel_key km_pake
#define km_panel_key_opt km_pako
#endif

/* km_panel_close : C callable interface to command "panel 0 c" (close) */
extern void          km_panel_close(
                                  char *title );

/* km_panel_display : C callable interface to command "panel 0 d" (display) */
extern void          km_panel_display(
                                  char *title,
                                  char *geometry);

/* km_panel_reset : C callable interface to command "panel 0 r" (reset panel) */
extern void          km_panel_reset(
                                  const char *name);

/* km_panel_key : C callable interface to command "panel" for key definition */
extern void          km_panel_key(
                                  int row,
                                  int col,
                                  char *command,
                                  char *alias_label,
                                  char *pixmap);
extern void          km_panel_key_opt(
                                  int row,
                                  int col,
                                  char *command,
                                  char *alias_label,
                                  char *pixmap,
                                  char *opt);
