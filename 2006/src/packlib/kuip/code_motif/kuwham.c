/*
 * $Id: kuwham.c,v 1.1.1.1 1996/03/08 15:33:07 mclareni Exp $
 *
 * $Log: kuwham.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:07  mclareni
 * Kuip
 *
 */
/*CMZ :  2.07/03 09/05/95  12.02.21  by  N.Cremel*/
/*-- Author :    Alfred Nathaniel   25/08/92*/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kcom.h"
#include "kuip/ksig.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/klink.h"

#include "kuip/mkutfu.h"

extern C_PROTO_1(void km_create_windows, char*);
extern C_PROTO_0(int  km_enter_loop);
extern C_PROTO_0(void km_realize_windows);
extern C_PROTO_3(void update_browser, void *, int, char);
extern int km_destroy_all_popup();
extern int km_display_cmdpan();
extern int km_display_kpanel();
extern int km_close_kpanel();
extern int km_panel_add();
extern int km_exit_confirm();
extern int km_quit_confirm();
extern int km_FlushEvents();
extern int km_all_cursor();
extern int km_display_sctext();
extern int km_cmd_list();
extern int km_display_list();
extern int km_kmenu_OK();

int Error;                      /* want by libPW.a routines */

static int init = 0;


static
void init_motif( name )
     char *name;
{
  kc_flags.do_exit = 0;
  kc_flags.do_quit = 0;
  kc_flags.in_motif = 1;
  kc_flags.echo_command = 0;
  kc_break.intr_count = 0;
  kc_flags.echo_prompt = str0dup( name );
  kc_flags.style = KmSTYLE_XM;
  ku_exel( "/KUIP/HELP -NOEDIT" );   /* display helptexts in Motif windows */
  ku_exel( "/KUIP/SET_SHOW/HOST_EDITOR 'xterm -e view &'" );
  ku_exel( "/KUIP/SET_SHOW/HOST_PAGER more");

  /* set Motif specific functions */
  kjmpaddr.disp_panel_C = km_display_cmdpan;
  kjmpaddr.disp_kpanel_C = km_display_kpanel;
  kjmpaddr.close_kpanel_C = km_close_kpanel;
  kjmpaddr.disp_kmpanel_C = km_panel_add;
  kjmpaddr.disp_text_C = km_display_sctext;
  kjmpaddr.disp_choice_C = km_display_sctext; /* !!! */
  kjmpaddr.disp_clean_C = km_destroy_all_popup;
/*  kjmpaddr.disp_flush_C = km_FlushEvents; */
  kjmpaddr.disp_busy_C = km_all_cursor;
  kjmpaddr.disp_exit_C = km_exit_confirm;
  kjmpaddr.disp_quit_C = km_quit_confirm;
  kjmpaddr.disp_prompt_C = km_prompt_answer;
  kjmpaddr.disp_passwd_C = km_passwd_prompt;
  kjmpaddr.disp_select_C = km_select;
  kjmpaddr.disp_cmd_list_C = km_cmd_list;
  kjmpaddr.disp_list_C = km_display_list;
  kjmpaddr.callb_kmenu = km_kmenu_OK;

  km_create_windows( name );       /* create the application windows */
  if( name != NULL )
    free( name );
  km_realize_windows();

  init = 1;
}


void F77_ENTRY_C(Kuwham,chname)
/* { */
  if( !init ) {
    char *name = fstr0dup( chname, len_chname );
    init_motif( name );
  }

  /* turn echo mode on only after initialization is finished */
  kc_flags.echo_command = 1;

  if (kc_flags.use_kxterm) {
      printf("\2");
      fflush( stdout );
   }

  /* update the main browser, files may have been opened via pawlogon */
  update_browser(NULL, 0, 0);

  /* catch exceptions saving signal mask */
  if( sigsetjmp( kc_break.stack, 1 ) != 0 ) { /* saving signal mask */
    /* longjmp happened */
    reset_break();
  }

  kc_break.jump_set = 1;
  ku_trap( 1, -1 );

  while( !kc_flags.do_exit && !kc_flags.do_quit ) {
    if( km_enter_loop() != 1 )  /* only returns for VMS exceptions */
      reset_break();
  }

  kc_break.jump_set = 0;
  ku_trap( 0, -1 );
  ku_shut();
}


void F77_ENTRY_C(Kuinim,chname)
/* { */
  char *name = fstr0dup( chname, len_chname );
  init_motif( name );
}


