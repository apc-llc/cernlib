/*
 * $Id: kmterm.c,v 1.1.1.1 1996/03/08 15:32:58 mclareni Exp $
 *
 * $Log: kmterm.c,v $
 * Revision 1.1.1.1  1996/03/08 15:32:58  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/15 09/08/94  09.53.56  by  Alfred Nathaniel*/
/*-- Author :    Fons Rademakers   16/01/93*/
/***********************************************************************
 *                                                                     *
 *   Non X and Motif dependend routines related with kxterm.           *
 *                                                                     *
 *   (from file init.c)                                                *
 *                                                                     *
 ***********************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"
#include "kuip/kflag.h"
#include "kuip/mkterm.h"


static int                 num_kxterm_actions;
static KxtermActionList    kxterm_actions;


/***********************************************************************
 *                                                                     *
 *   Send a single kxterm command.                                     *
 *                                                                     *
 ***********************************************************************/
void send_single_kxterm_cmd(cmd)
     char *cmd;
{
   if (!kc_flags.use_kxterm) return;

   if (*(cmd+strlen(cmd)-1) != '\n')
      printf("%s%s\n", ESCAPE, cmd);
   else
      printf("%s%s", ESCAPE, cmd);
}

/***********************************************************************
 *                                                                     *
 *   Send a number of kxterm command.                                  *
 *                                                                     *
 ***********************************************************************/
void send_kxterm_cmd(cmd)
     char **cmd;
{
   int  i;

   for (i = 0; cmd[i]; i++)
      send_single_kxterm_cmd(cmd[i]);
}

/***********************************************************************
 *                                                                     *
 *   Return kxterm action proc belonging to string.                    *
 *                                                                     *
 ***********************************************************************/
static KxtermActionProc kxterm_get_proc(string)
     char *string;
{
    char *s, *s1;
    int   i, l, n = num_kxterm_actions;

    s = string + strlen(ESCAPE);
    if ((s1 = strchr(s,'(')))
       l = s1 - s;
    else
       l = strlen(s);

    for (i = 0; i < n; i++)
       if (!strncmp(kxterm_actions[i].string, s, l))
          return kxterm_actions[i].proc;

    return (KxtermActionProc) NULL;

}

/***********************************************************************
 *                                                                     *
 *   Parse action arguments.                                           *
 *                                                                     *
 ***********************************************************************/
void k_parse_args( const char *cmd,
                  char **argv,
                  int *argc )
{
   char *s;

   *argc   = 0;
   argv[0] = (char *)NULL;

   /* remove any trailing newline character */
   if ((s = strrchr(cmd, '\n')))
      *s = '\0';

   /* position at beginning of argument list */
   s = strchr(cmd, '(');
   if (!s) return;

   s = strtok(s, "(,)");
   while (s) {
      argv[(*argc)++] = strdup(s);
      s = strtok(NULL, ",)");
   }
}

/***********************************************************************
 *                                                                     *
 *   Register kxterm actions.                                          *
 *                                                                     *
 ***********************************************************************/
void kxterm_add_actions(actions)
     KxtermActionList actions;
{
   int  i, n = num_kxterm_actions;
   int  nact = 0;

   for (i = 0; actions[i].string; i++)
      nact++;

   if (!kxterm_actions)
      kxterm_actions = (KxtermActionList) malloc(
                                       (nact+n)*sizeof(KxtermActionsRec));
   else
      kxterm_actions = (KxtermActionList) realloc(kxterm_actions,
                                       (nact+n)*sizeof(KxtermActionsRec));

   for (i = n; i < n+nact; i++) {
      kxterm_actions[i].string = strdup(actions[i-n].string);
      kxterm_actions[i].proc   = actions[i-n].proc;
   }

   num_kxterm_actions += nact;
}

/***********************************************************************
 *                                                                     *
 *   Handle kxterm actions.                                            *
 *                                                                     *
 ***********************************************************************/
void handle_kxterm_action( cmd )
     char *cmd;
{
    KxtermActionProc  proc;
    char             *argv[15];
    int               argc, i;

    if ((proc = kxterm_get_proc(cmd))) {
       k_parse_args(cmd, argv, &argc);

       (proc)(argv, argc);

       for (i = 0; i < argc; i++)
          free(argv[i]);
    }
}


/*
 * prompt for a string
 * in Motif mode: change prompt string in Kxterm and read from stdin
 */
void F77_ENTRY_CCI(Kuinps,chprmt,chline,length)
/* { */
  char *prompt = fstrdup( chprmt, len_chprmt );
  *length = fstrset( chline, len_chline, ku_inps( prompt ) );
  free( prompt );
}

char *ku_inps( const char *prompt )
{
#ifndef vms
  if( kc_flags.in_motif && kc_flags.use_kxterm ) {
    static char line[1024];
    char *cmd = str3dup( "prompt(", prompt, " )\2" );
    int i;

    send_single_kxterm_cmd( cmd );
    free( cmd );

    for( i = 0; read( 0, &line[i], 1 ) == 1; i++ ) {
      if( line[i] == '\n' ) {
        break;
      }
    }
    line[i] = '\0';
    printf( "%s %s\n", prompt, line );
    return line;
  }
#endif

  return ku_pros( prompt, NULL );
}


/*
 * if keyboard input is available return 1 otherwise 0
 */
INTEGER Kuqkey()
{
  return ku_qkey();
}

int ku_qkey()
{
#ifdef HAVE_SELECT
  while( 1 ) {
    fd_set readfds;
    struct timeval timeout;
    char ch;

    FD_ZERO( &readfds );
    FD_SET( 0, &readfds );
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;

    if( select( 1, SELECT_CAST(&readfds), NULL, NULL, &timeout ) == 0 )
      return 0;

    read( 0, &ch, 1 );
    if( ch == '\n' )
      return 1;
  }
#else

  return 0;

#endif
}
