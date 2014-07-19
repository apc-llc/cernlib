/*
 * $Id: klink.h,v 1.2 1996/04/16 13:26:44 gunter Exp $
 *
 * $Log: klink.h,v $
 * Revision 1.2  1996/04/16 13:26:44  gunter
 * Mods to compile kuipc. The includes needed only for kuipc where moved
 *  to kuipc, together with m4 files to create/update these header files.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */
/* klink.h: demand linking of special routines */

#define __ /* Null */

#include "klink1.h"

#undef __


EXTERN struct {
  SUBROUTINE *user_exit_F;      /* set by KUEXIT */
  SUBROUTINE *user_quit_F;      /* set by KUQUIT */
  SUBROUTINE *user_break_F;     /* set by KUBREK */
  SUBROUTINE *user_edit_F;      /* set by KUEUSR */
  SUBROUTINE *user_comis_F;     /* set by KUCOMV */
  SUBROUTINE *user_sigma_F;     /* set by KUSIGM */
  SUBROUTINE *user_grfl_F;      /* set by KUGRFL */
  SUBROUTINE *user_term_F;      /* set by KUTERM */
  SUBROUTINE *user_input_F;     /* set by KUSER */
  SUBROUTINE *user_locate_F;    /* set by KUMLOC */
  /* indirect calls to avoid linking Motif                                   */
  IntFunc    *disp_panel_C;     /* display command panel (km_display_cmdpan) */
  IntFunc    *disp_kpanel_C;    /* display KUIP panel (km_display_kpanel)    */
  IntFunc    *close_kpanel_C;   /* close KUIP panel (km_close_kpanel)        */
  IntFunc    *disp_kmpanel_C;   /* add panel inside palette (km_panel_add)   */
  IntFunc    *disp_text_C;      /* display text widget (km_display_sctext)   */
  IntFunc    *disp_choice_C;    /* display a choice of commands (?)          */
  IntFunc    *disp_clean_C;     /* clean before action (km_destroy_all_popup)*/
  IntFunc    *disp_flush_C;     /* flush event queue (FlushEvents)           */
  IntFunc    *disp_busy_C;      /* show busy cursor (km_all_cursor)          */
  IntFunc    *disp_exit_C;      /* ask confirmation for exit                 */
  IntFunc    *disp_quit_C;      /* ask confirmation for exit                 */
  IntFunc    *disp_select_C;    /* select from a number of buttons           */
  IntFunc    *disp_cmd_list_C;  /* display list of commands (km_cmd_list)    */
  IntFunc    *disp_list_C;      /* display list of items (km_display_list)   */
  IntFunc    *callb_kmenu;      /* kuipList callback in KMENU (km_kmenu_OK)  */
  CharFunc   *disp_prompt_C;    /* prompt for input                          */
  CharFunc   *disp_passwd_C;    /* prompt for password input                 */
  /* indirect calls to avoid linking HIGZ without style G                    */
  IntFunc    *higz_init_C;      /* initialize menu mode */
} kjmpaddr;


#undef StartQuotedInclude
#undef EndQuotedInclude
