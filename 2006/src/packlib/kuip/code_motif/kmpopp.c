/*
 * $Id: kmpopp.c,v 1.1.1.1 1996/03/08 15:33:08 mclareni Exp $
 *
 * $Log: kmpopp.c,v $
 * Revision 1.1.1.1  1996/03/08 15:33:08  mclareni
 * Kuip
 *
 */
/*CMZ :  2.06/00 23/09/94  14.48.16  by  N.Cremel*/
/*-- Author :*/
/*-- Author :    N.Cremel   03/04/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck KMPOPP                       */
/*                                                                            */
/*           General utilitity routines for handling popup menus :            */
/*                                                                            */
/*           - km_build_popup : Build a popup menu and return its widget      */
/*           - km_destroy_all_popup : destroy all KUIP?Motif popup menus      */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"

#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>
#include <Xm/RowColumn.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeBG.h>
#include <Xm/ToggleBG.h>

#include "mkdims.h"
#include "mkutfm.h"
#include "mkutdm.h"
#include "kuip/browh1.h"
#include "browh2.h"
#include "mkuxxt.h"


extern XmAnyCallbackStruct *current_callback_data;


extern C_PROTO_1( void file_execute,
                         char *lun);
extern C_PROTO_3( void dir_execute,
                         Widget browser,
                         KmObject *kmobj,
                         char *lun);
extern C_PROTO_3(void update_browser,
                         Widget widget,
                         int list,
                         Boolean refresh);


C_PROTO_3(void popup_activate,
                         Widget widget,
                         caddr_t tag,
                         XmAnyCallbackStruct *call_data);



static void popdown_menu(w, tag, cbs)
   Widget   w;
   int      tag;
   XmAnyCallbackStruct *cbs;
{
   km_destroy_all_popup();
}

/***********************************************************************
 *                                                                     *
 *   Build a popup menu and return its widget and the default (i.e.    *
 *   first) pushbutton in the menu. The default button is used for     *
 *   double clicking.                                                  *
 *                                                                     *
 ***********************************************************************/
Widget km_build_popup(parent, menu_title, items, menu_type, default_button)
   Widget     parent;
   char      *menu_title;
   MenuItem  *items;
   int        menu_type;   /* XmMENU_POPUP, XmMENU_PULLDOWN */
   Widget    *default_button;   /* first pushbutton in menu */
{
   Widget shell, Popup, widget, wtmp;
   int i;
   Arg args[2];

   static char *last_label = NULL;

   XtSetArg(args[0], XmNwidth, 1);
   XtSetArg(args[1], XmNheight, 1);
   shell = XmCreateMenuShell(parent, "_popup", args, 2);
   XtSetArg(args[0], XmNrowColumnType, menu_type);
   Popup = XmCreateRowColumn(shell, menu_title, args, 1);
   XtAddCallback(shell, XmNpopdownCallback,
                (XtCallbackProc)popdown_menu, NULL);
   XtVaCreateManagedWidget("popup_label",
                                  xmLabelWidgetClass, Popup,
                                  RES_CONVERT( XmNlabelString, menu_title),
                                  XmNalignment, XmALIGNMENT_CENTER,
                                  NULL );
   XtVaCreateManagedWidget("popup_sep",
                                  xmSeparatorGadgetClass, Popup,
                                  NULL );


   /* Now add the menu items */
   for (i = 0; items[i].label; i++) {
      Boolean toggle = False;

      /* Test if label is the same as previous one (toggle) */
      if (last_label != NULL && strcmp(items[i].label, last_label) == 0)
          toggle = True;
      if (last_label != NULL) free (last_label);
      last_label = strdup (items[i].label);
      if (toggle) continue;
      if (items[i].separator) {
       Widget sep = XmCreateSeparatorGadget(Popup, "sep", args, 0);
       XtManageChild(sep);
      }
      if (items[i].subitems) {
       Widget new =
          km_build_popup(Popup, items[i].label, items[i].subitems,
                 XmMENU_PULLDOWN, &wtmp);
       widget = XtVaCreateManagedWidget(items[i].label,
                    xmCascadeButtonGadgetClass, Popup,
                    XmNsubMenuId, new,
                    NULL);
      } else
       widget = XtVaCreateManagedWidget(items[i].label,
                    *items[i].class, Popup, NULL);

      if (items[i].default_button)
       *default_button = widget;

      if (items[i].mnemonic)
       XtVaSetValues(widget, XmNmnemonic, items[i].mnemonic, NULL);
      if (items[i].accelerator) {
       XmString str = XmStringCreateSimple(items[i].accel_text);
       XtVaSetValues(widget,
                XmNaccelerator, items[i].accelerator,
                XmNacceleratorText, str,
                NULL);
       XmStringFree(str);
      }
      if (items[i].callback) {
          if (*items[i].class == xmPushButtonGadgetClass) {
              XtAddCallback(widget, XmNactivateCallback,
                   items[i].callback, items[i].callback_data);
          } else if (*items[i].class == xmToggleButtonGadgetClass) {
              if (items[i].set)
                  XtVaSetValues(widget, XmNset, TRUE, NULL);
              else
                  XtVaSetValues(widget, XmNset, FALSE, NULL);
              XtAddCallback(widget, XmNvalueChangedCallback,
                   items[i].callback, items[i].callback_data);
          }
      }
   }
   return Popup;
}


/******************************************************************************/
/*  km_destroy_all_popup : destroy all popup menus (for button 3 use)         */
/******************************************************************************/
void km_destroy_all_popup ()
{
extern C_PROTO_0(void km_reset_graph_popup);
extern C_PROTO_0(void km_reset_class_popup);
extern C_PROTO_0(void km_reset_object_popup);
    XEvent event;

    km_reset_graph_popup ();
    km_reset_class_popup();
    km_reset_object_popup();

    while ( XEventsQueued( km_Display, QueuedAlready) > 0) {
            XNextEvent (km_Display, &event);
            XtDispatchEvent (&event); }
    XSync(km_Display, False);
}


/***********************************************************************
 *                                                                     *
 *   Callbacks for "Popup" menu items in the class window              *
 *                                                                     *
 ***********************************************************************/
void popup_activate (widget, tag, call_data)
     Widget widget;
     caddr_t tag;
     XmAnyCallbackStruct *call_data;
{
   poptag_str *s_tag = (poptag_str *)tag;
   XEvent  evt;
   BrAction *bract1;

   evt = *(call_data->event);
   current_callback_data = call_data;

   km_all_cursor(KM_WAIT_CURSOR);

   km_destroy_all_popup();

   /* for toggle take either bract or bract_next */
   bract1 = s_tag->bract;

   if (s_tag->bract->flags & BrActToggle) {
   /* Toggle behavior */
      if (s_tag->bract->flags & BrActToggleOn) {
          XtVaSetValues(widget, XmNset, FALSE, NULL);
          s_tag->bract->flags &= ~BrActToggleOn;
      } else {
          XtVaSetValues(widget, XmNset, TRUE, NULL);
          s_tag->bract->flags |= BrActToggleOn;
          bract1 = s_tag->bract_next;
      }
   }

   switch (s_tag->flag) {
      case 1:    /* Class window menu ... */

         if (s_tag->command_num == 1) {
            /* for 1st command (List) : execute default action*/
            current_callback_data = call_data;
            file_execute (s_tag->pathname);
         } else {
            km_get_curpos (call_data->event, &km_xcur_pos, &km_ycur_pos);
            exec_action(bract1, s_tag->pathname, NULL, s_tag->panel,
                        widget, call_data);

            /* Update the class window of all browsers */
            if (s_tag->bract->flags & BrActUpdate)
               update_browser (NULL, FILE_LIST, False);
         }

         break;

      case 2:    /* Object window menu ... */

         if (s_tag->kmobj->class->is_dir && s_tag->command_num == 1) {
            /* This is a directory ... */
            /* for 1st command (List) : execute default action for directory */
            dir_execute (s_tag->browser, s_tag->kmobj, s_tag->pathname);
         } else {
           km_get_curpos (call_data->event, &km_xcur_pos, &km_ycur_pos);
           exec_action (bract1, s_tag->pathname,
                        s_tag->kmobj->name, s_tag->panel, widget, call_data);

           /* Update this browser's class and object windows */
           if (s_tag->bract->flags & BrActUpdate)
              update_browser (NULL, 0, False);
         }

         break;

      case 3:    /* Graphics window menu ... */
         km_get_curpos (call_data->event, &km_xcur_pos, &km_ycur_pos);

         if ( s_tag->panel || (evt.xbutton.state == ControlMask) )
            exec_action (bract1, s_tag->pathname, s_tag->item, True,
                         widget, call_data);
         else
            exec_action (bract1, s_tag->pathname, s_tag->item, False,
                         widget, call_data);

         break;

   } /* end ... switch (s_tag->flag) */

   km_all_cursor(KM_MAIN_CURSOR);

}
