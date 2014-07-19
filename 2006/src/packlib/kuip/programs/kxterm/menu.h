/*
 * $Id: menu.h,v 1.2 1996/04/11 10:04:23 cernlib Exp $
 *
 * $Log: menu.h,v $
 * Revision 1.2  1996/04/11 10:04:23  cernlib
 * Remobe fortran comment lines
 *
 * Revision 1.1.1.1  1996/03/08 15:33:03  mclareni
 * Kuip
 *
 */
/***********************************************************************
 *                                                                     *
 *   Menu.h, contains the prototypes for the menu building routines,   *
 *   and the defintion of the MenuItem data type.                      *
 *                                                                     *
 ***********************************************************************/
#ifndef _menu_h
#define _menu_h

typedef struct _menu_item {
   char               *label;
   WidgetClass        *class;
   Boolean             set;     /* default setting in case of toggle button */
   char                mnemonic;
   char               *accelerator;
   char               *accel_text;
   void              (*callback)();
   caddr_t             callback_data;
   struct _menu_item  *subitems;
} MenuItem;

extern int add_menu_items(
#ifndef _NO_PROTO
                          char *, void (*)(), void (*)(), MenuItem *, int
#endif
                          );
extern Widget BuildMenu(
#ifndef _NO_PROTO
                        Widget, int, char *, int, MenuItem *, int
#endif
                        );

#endif /* _menu_h */
