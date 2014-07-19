/*
 * $Id: menu.c,v 1.1.1.1 1996/03/08 15:33:04 mclareni Exp $
 *
 * $Log: menu.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:04  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/03 10/01/95  09.18.41  by  N.Cremel*/
/*-- Author :    Fons Rademakers   8/01/93*/
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeBG.h>

#include "menu.h"
#include "utils.h"


/***********************************************************************
 *                                                                     *
 *   Fill MenuItem array.                                              *
 *   Function returns the number of items stored in the array, 0 when  *
 *   an error occured.                                                 *
 *                                                                     *
 ***********************************************************************/
int add_menu_items( item_string, push_cb, toggle_cb, items, max_items )
     char *item_string;
     void (*push_cb)();
     void (*toggle_cb)();
     MenuItem *items;
     int max_items;
{
   char     *str, *sav, *s1, *s2, *class;
   int      item       = 0;
   int      i, l;

   l = strlen(item_string);
   if (*(item_string+l-1) != '\n') {
      str = XtMalloc(l+2);
      sprintf(str, "%s\n", item_string);
   } else {
      str = XtMalloc(l+1);
      strcpy(str, item_string);
   }
   sav = str;

   while ((s1 = strchr(str, '\n'))) {
      s1++;
      items[item].label          = NULL;
      items[item].class          = NULL;
      items[item].set            = False;
      items[item].mnemonic       = '\0';
      items[item].accelerator    = NULL;
      items[item].accel_text     = NULL;
      items[item].callback       = NULL;
      items[item].callback_data  = NULL;
      items[item].subitems       = NULL;
      i  = 0;
      s2 = strtok(str, ":\n");
      while (s2) {
         switch (i) {
            case 0:
               items[item].label = strip(s2);
               break;
            case 1:
               items[item].callback_data = (caddr_t) strip(s2);
               break;
            case 2:
               class = strip(s2);
               lower(class);
               if (*class == 'b') {
                  items[item].class = &xmPushButtonGadgetClass;
                  items[item].callback = push_cb;
               }
               else if (*class == 't') {
                  items[item].class = &xmToggleButtonGadgetClass;
                  items[item].callback = toggle_cb;
                  if (strchr(class, '1'))
                     items[item].set = True;
               }
               else if (*class == 's')
                  items[item].class = &xmSeparatorGadgetClass;
               else if (*class == 'l')
                  items[item].class = &xmLabelGadgetClass;
               XtFree(class);
               break;
            case 3:
               class = strip(s2);
               items[item].mnemonic = *class;
               XtFree(class);
               break;
            case 4:
               items[item].accelerator = strip(s2);
               break;
            case 5:
               items[item].accel_text  = strip(s2);
               break;
         }
         if (s1 == strchr(s2, '\0')+1) break;
         i++;
         s2 = strtok((char *)NULL, ":\n");
      }
      if (items[item].callback_data && !*((int*)(items[item].callback_data))) {
         XtFree(items[item].callback_data);
         items[item].callback_data = NULL;
      }
      if (!items[item].callback_data) {
         items[item].callback_data = XtCalloc(strlen(items[item].label)+1, 1);
         strcpy(items[item].callback_data, items[item].label);
      }
      if (!items[item].class) {
         items[item].class = &xmPushButtonGadgetClass;
         items[item].callback = push_cb;
      }
      str = s1;
      item++;
      if (item >= max_items) {
         item--;
         items[item].label = NULL;
         XtFree(sav);
         return ++item;
      }
      items[item].label = NULL;
      if (!*s1) break;
   }
   XtFree(sav);

   return item;
}

/***********************************************************************
 *                                                                     *
 * Build popup, option and pulldown menus, depending on the menu_type. *
 * It may be XmMENU_PULLDOWN, XmMENU_OPTION or XmMENU_POPUP. Pulldowns *
 * return the CascadeButton that pops up the menu. Popups return the   *
 * menu. Option menus are created, but the RowColumn that acts as the  *
 * option "area" is returned unmanaged. (The user must manage it.)     *
 * Pulldown menus are built from cascade buttons, so this function     *
 * also builds pullright menus.  The function also adds the right      *
 * callback for PushButton or ToggleButton menu items.                 *
 *                                                                     *
 ***********************************************************************/
Widget BuildMenu( parent, menu_type, menu_title, menu_mnemonic,
                 items, add_to_menu )
     Widget parent;
     int menu_type;
     char *menu_title;
     int menu_mnemonic;
     MenuItem *items;
     int add_to_menu;
{
    Widget    menu, cascade, widget;
    char     *name;
    int       i;
    XmString  str;

    if (add_to_menu) {
       menu = parent;
    } else {
       name = XtCalloc(strlen(menu_title) + 6, 1);
       if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION) {
          sprintf(name, "%sMenu", menu_title);
          menu = XmCreatePulldownMenu(parent, name, NULL, 0);
       } else if (menu_type == XmMENU_POPUP) {
          sprintf(name, "%sPopup", menu_title);
          menu = XmCreatePopupMenu(parent, name, NULL, 0);
       } else {
          XtWarning("Invalid menu type passed to BuildMenu()");
          XtFree(name);
          return NULL;
       }
       XtFree(name);

       /* Pulldown menus require a cascade button to be made */
       if (menu_type == XmMENU_PULLDOWN) {
          str = XmStringCreateSimple(menu_title);
          cascade = XtVaCreateManagedWidget(menu_title,
                                            xmCascadeButtonGadgetClass, parent,
                                            XmNsubMenuId,   menu,
                                            XmNlabelString, str,
                                            XmNmnemonic,    menu_mnemonic,
                                            NULL);
          XmStringFree(str);
       } else if (menu_type == XmMENU_OPTION) {
          /* Option menus are a special case, but not hard to handle */
          Arg args[2];
          str = XmStringCreateSimple(menu_title);
          XtSetArg(args[0], XmNsubMenuId, menu);
          XtSetArg(args[1], XmNlabelString, str);
          /*
          * This really isn't a cascade, but this is the widget handle
          * we're going to return at the end of the function.
          */
          cascade = XmCreateOptionMenu(parent, menu_title, args, 2);
          XmStringFree(str);
       }
    }

    /* Now add the menu items */
    for (i = 0; items[i].label != NULL; i++) {
        /*
         * If subitems exist, create the pull-right menu by calling this
         * function recursively.  Since the function returns a cascade
         * button, the widget returned is used..
         */
        if (items[i].subitems)
            if (menu_type == XmMENU_OPTION) {
                XtWarning("You can't have submenus from option menu items.");
                continue;
            } else
                widget = BuildMenu(menu, XmMENU_PULLDOWN,
                                   items[i].label, items[i].mnemonic,
                                   items[i].subitems, False);
        else
            widget = XtVaCreateManagedWidget(items[i].label,
                *items[i].class, menu,
                NULL);

        /*
         * If toggle button set default state, set can only be true when
         * class = xmToggleButtonWidgetClass
         */
        if (items[i].set)
           XtVaSetValues(widget, XmNset, True, NULL);

        /*
         * Whether the item is a real item or a cascade button with a
         * menu, it can still have a mnemonic.
         */
        if (items[i].mnemonic)
            XtVaSetValues(widget, XmNmnemonic, items[i].mnemonic, NULL);

        /*
         * Any item can have an accelerator, except cascade menus. But,
         * we don't worry about that; we know better in our declarations.
         */
        if (items[i].accelerator) {
            str = XmStringCreateSimple(items[i].accel_text);
            XtVaSetValues(widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
            XmStringFree(str);
        }

        if (items[i].callback)
            XtAddCallback(widget,
                (items[i].class == &xmToggleButtonWidgetClass ||
                 items[i].class == &xmToggleButtonGadgetClass) ?
                    XmNvalueChangedCallback : /* ToggleButton class */
                    XmNactivateCallback,      /* PushButton class */
                items[i].callback, items[i].callback_data);
    }

    /*
     * For popup menus, just return the menu; pulldown menus, return
     * the cascade button; option menus, return the thing returned
     * from XmCreateOptionMenu().  This isn't a menu, or a cascade button!
     */
    return menu_type == XmMENU_POPUP ? menu : cascade;
}
