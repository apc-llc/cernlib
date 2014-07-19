/*
 * $Id: kmenu.h,v 1.2 1996/04/16 13:26:46 gunter Exp $
 *
 * $Log: kmenu.h,v $
 * Revision 1.2  1996/04/16 13:26:46  gunter
 * Mods to compile kuipc. The includes needed only for kuipc where moved
 *  to kuipc, together with m4 files to create/update these header files.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */
/* kmenu.h: data structures for menu and command definitions */

#define __ /* Null */

#include "kmenu1.h"

#include "kmenu2.h"

#include "kmenu3.h"

#include "kmenu4.h"

#include "kmenu5.h"

#include "kmenu6.h"

#undef __

/*
 * temporary fix until we can reserve an extra word in KmCommand structure
 * to count keyboard and macro commands separately
 */
#define XCOUNT_SHIFT   10
#define XCOUNT_OFFSET (1 << XCOUNT_SHIFT)
#define XCOUNT_MASK   (XCOUNT_OFFSET - 1)

extern void  check_version( int, int );
extern void  check_edit_server(void);
extern int   exec_cmd_string( const char*, int, int(*)() );
extern int   exec_decoded_cmd( KmCommand* );
extern KmMenu* find_submenu( const char* );
extern char* fmt_cmd_help( KmCommand*, int );
extern void  menu_style(void);
extern void  print_cmd_list( KmCommand**, const char* );
extern void  reset_arg_list( KmCommand* );
extern KmMenu** root_menu_list( const char* );
extern KmCommand* search_command( const char*, KmCommand*** );


