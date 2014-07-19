/*
 * $Id: actions.h,v 1.2 1996/04/11 10:04:20 cernlib Exp $
 *
 * $Log: actions.h,v $
 * Revision 1.2  1996/04/11 10:04:20  cernlib
 * Remobe fortran comment lines
 *
 * Revision 1.1.1.1  1996/03/08 15:33:05  mclareni
 * Kuip
 *
 */

typedef void (*KxtermActionProc)(
#ifndef _NO_PROTO
    char**              /* params */,
    int                 /* num_params */
#endif
);

typedef struct _KxtermActionsRec{
    char               *string;
    KxtermActionProc    proc;
} KxtermActionsRec;

typedef KxtermActionsRec  *KxtermActionList;

extern void kxterm_add_actions(
#ifndef _NO_PROTO
                               KxtermActionList
#endif
                              );
extern void handle_kxterm_action(
#ifndef _NO_PROTO
                                 char *
#endif
                                );

