/*
 * $Id: utils.h,v 1.3 1999/09/28 16:35:52 mclareni Exp $
 *
 * $Log: utils.h,v $
 * Revision 1.3  1999/09/28 16:35:52  mclareni
 * Remove strdup redefinition
 *
 * Revision 1.2  1996/04/11 10:04:24  cernlib
 * Remobe fortran comment lines
 *
 * Revision 1.1.1.1  1996/03/08 15:33:05  mclareni
 * Kuip
 *
 */
/***********************************************************************
 *                                                                     *
 *   Utils.h, contains the utility routine prototypes.                 *
 *                                                                     *
 ***********************************************************************/
#ifndef _utils_h
#define _utils_h

#ifdef _NO_PROTO
#  define const
#endif

extern Boolean strempty(
#ifndef _NO_PROTO
                        char *
#endif
                        );
extern char *my_strrstr(
#ifndef _NO_PROTO
                        const char *s1, const char *s2
#endif
                        );
extern char *strip(
#ifndef _NO_PROTO
                   char *
#endif
                   );
extern char *strend(
#ifndef _NO_PROTO
                    char *str, int c
#endif
                    );
extern void lower(
#ifndef _NO_PROTO
                  char *str
#endif
                  );
extern void popdown_dialog(
#ifndef _NO_PROTO
                           Widget dialog
#endif
                           );
extern void cancel_cb(
#ifndef _NO_PROTO
                      Widget, Widget, XmAnyCallbackStruct *
#endif
                      );
extern void unmanage_cb(
#ifndef _NO_PROTO
                        Widget, Widget, XmAnyCallbackStruct *
#endif
                        );
extern void InstallMwmCloseCallback(
#ifndef _NO_PROTO
                                    Widget, XtCallbackProc, XtPointer
#endif
                                    );
extern void IconifyShell(
#ifndef _NO_PROTO
                         Widget
#endif
                         );
extern void TurnOffSashTraversal(
#ifndef _NO_PROTO
                            Widget
#endif
                            ); 

#if !defined(__convexc__)
extern int   strcasecmp(
#ifndef _NO_PROTO
                        const char *s1, const char *s2
#endif
                        );
extern int   strncasecmp(
#ifndef _NO_PROTO
                         const char *s1, const char *s2, size_t n
#endif
                         );
#endif /* __convexc__ */

#endif /* _utils_h */
