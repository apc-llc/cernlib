/*
 * $Id: actions.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: actions.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.03/09 01/09/93  15.14.30  by  Alfred Nathaniel*/
/*-- Author :    Fons Rademakers   24/02/93*/
/***********************************************************************
 *                                                                     *
 *   Kxterm action handling routines.                                  *
 *                                                                     *
 *   Except for the sequences and the routines send_... the same as    *
 *   deck /CKUIP/KMTERM.                                               *
 *                                                                     *
 ***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Xm/Xm.h>

#define ESCAPE            "#@"

#include "utils.h"
#include "actions.h"


static int                 num_kxterm_actions;
static KxtermActionList    kxterm_actions;


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
static void parse_args( cmd, argv, argc )
     char *cmd;
     char **argv;
     int *argc;
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
       parse_args(cmd, argv, &argc);

       (proc)(argv, argc);

       for (i = 0; i < argc; i++)
          free(argv[i]);
    }
}
