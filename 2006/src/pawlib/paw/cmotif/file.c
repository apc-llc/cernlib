/*
 * $Id: file.c,v 1.3 1996/11/19 16:16:10 cremel Exp $
 *
 * $Log: file.c,v $
 * Revision 1.3  1996/11/19 16:16:10  cremel
 * Add a protection in "Open Hbook File..." in case the user clicks OK without
 * entering a filename in the "Open File" entry (otherwise it seems to crash
 * on AIX).
 *
 * Revision 1.2  1996/03/12 16:58:50  cremel
 * cpp run over the CDFs
 * Add new resource "hbookfile" to specify the extension for HBOOK files.
 * This can now be specify by the user in .Xdefaults, e.g.:
 *
 * Paw++*hbookfile: dat
 *
 * (default is "hbook").
 *
 * Revision 1.1.1.1  1996/03/01 11:38:56  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.06/09 16/01/95  14.37.37  by  O.Couet*/
/*-- Author :*/
#include <stdio.h>
#include <string.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include <Xm/ToggleBG.h>
#include <Xm/MessageB.h>
#include <Xm/Text.h>
#include <Xm/SelectioB.h>

#include "hmotif/pawm.h"
#include "hmotif/openfbdialog.h"


/* extern functions */
extern Widget create_openFbDialog(Widget);
extern Widget create_closeFbDialog(Widget);


/* local functions */
static void toggle_new_file(Widget, XtPointer, XmToggleButtonCallbackStruct *);
static void reclist_cb(Widget, XtPointer, XmListCallbackStruct *);
static void get_openArg(Widget, MenuCbStruct *,
                        XmFileSelectionBoxCallbackStruct *);
static void fill_unit_list(Widget dialog);
static void get_closeArg(Widget, MenuCbStruct *,
                         XmSelectionBoxCallbackStruct *);


/***********************************************************************
 *                                                                     *
 *   Find the browser dialog.                                          *
 *                                                                     *
 ***********************************************************************/
static Widget find_browser(Widget child)
{
   String  name;
   Widget  browser;

   /*
    * starting from a child widget find back the
    * parent file browser widget to set the right context.
    */
   browser = child;
   do {
      browser = XtParent(browser);
      name = XtName(browser);
   } while (browser && strcmp(name, "kuipBrowser"));

   return browser;
}

/***********************************************************************
 *                                                                     *
 *   Find the browser sequence number.                                 *
 *                                                                     *
 ***********************************************************************/
static int which_browser(Widget brows)
{
   int  i;

   for (i = 0; i < MAX_BROWSERS; i++) {
      if (browser[i].widget == brows) return i;
   }

   return -1;
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) FILE argument popup.                                *
 *                                                                     *
 ***********************************************************************/
void show_openFbDialog(Widget w, caddr_t client_data,
                         XmAnyCallbackStruct *call_data)
{
   Widget               brows, dialog;
   MenuCbStruct        *menu_item;
   XmStringTable        xmstr_list;
   XmString            *xlsel, xmmask, xmstr;
   char                 unit[3], lrecl[5], *s, file[80];
   int                  i, cnt, lcnt, which;

   brows = find_browser(w);
   which = which_browser(brows);

   if (!browser[which].open_dialog) {
      dialog = create_openFbDialog(brows);

      /*
       * get context of openFbDialog
       */
      UxOpenFbDialogContext = (_UxCopenFbDialog *)UxGetContext(dialog);

      /*
       *   install callbacks
       */
      menu_item = make_menuStruct(w, dialog);
      InstallMwmCloseCallback(XtParent(dialog), (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_item);
      XtAddCallback(fileSelBox, XmNokCallback,
                    (XtCallbackProc)get_openArg, menu_item);
      XtAddCallback(fileSelBox, XmNcancelCallback,
                    (XtCallbackProc)cancel_cb, menu_item);
      XtAddCallback(fileSelBox, XmNhelpCallback,
                    (XtCallbackProc)help_cb, "HISTO/FILE");
      XtAddCallback(fileTogNew, XmNvalueChangedCallback,
                    (XtCallbackProc)toggle_new_file, NULL);

      XtAddCallback(recordLengthList, XmNbrowseSelectionCallback,
                    (XtCallbackProc)reclist_cb, NULL);
      XtAddCallback(recordLengthList, XmNsingleSelectionCallback,
                    (XtCallbackProc)reclist_cb, NULL);
      XtAddCallback(recordLengthList, XmNdefaultActionCallback,
                    (XtCallbackProc)reclist_cb, NULL);

      /* store MenuCbSruct in userData */
      XtVaSetValues(dialog, XmNuserData, menu_item, NULL);

      browser[which].open_dialog = dialog;

      /* fill the record length list */
      xmstr_list = (XmStringTable)XtMalloc(6*sizeof(XmString *));
      for (i = 0; i < 6; i++) {
            if (i == 0) cnt = 0;
            if (i == 1) cnt = 512;
            sprintf(lrecl,"%d", cnt);
            xmstr_list[i] = XmStringCreateSimple(lrecl);
            cnt *= 2;
      }
      XmListSetPos(recordLengthList, 1);
      XmListDeselectAllItems(recordLengthList);
      XmListDeleteAllItems(recordLengthList);
      XmListAddItems(recordLengthList, xmstr_list, 6, 0);
      XmListSelectPos(recordLengthList, 1, False);
      for (i = 0; i < 6; i++)
         XmStringFree(xmstr_list[i]);
      XtFree((char *)xmstr_list);
   } else {

      /*
       * get context of openFbDialog
       */
      UxOpenFbDialogContext =
                (_UxCopenFbDialog *)UxGetContext(browser[which].open_dialog);

      /* rescan the directory */
      XtVaGetValues(fileSelBox, XmNdirMask, &xmmask, NULL);
      XmFileSelectionDoSearch(fileSelBox, xmmask);

   }

   /* set record length */
   XtVaGetValues(recordLengthList, XmNselectedItemCount, &lcnt,
                                   XmNselectedItems,     &xlsel, NULL);
   XmTextSetString(recordLength, NULL);
   if (!lcnt)
      XmTextSetString(recordLength, "1024");
   else {
      s = extract_normal_string(xlsel[0]);
      XmTextSetString(recordLength, s);
      XtFree(s);
   }

   /* set Filter with correct hbook file extension */
   sprintf(file,"*.%s", appres.hbook_file);
   xmstr = XmStringCreateSimple(file);
   XtVaSetValues(fileSelBox, XmNdirMask, xmstr, NULL);
   XmStringFree(xmstr);

   /* fill the list with available unit numbers */
   cnt = 0;
   xmstr_list = (XmStringTable)XtMalloc(99*sizeof(XmString *));
   for (i = 0; i < 99; i++) {
      if (!pawlun.lunit[i]) {
         cnt++;
         sprintf(unit,"%d", i+1);
         xmstr_list[cnt-1] = XmStringCreateSimple(unit);
      }
   }
   XmListSetPos(fileUnits, 1);
   XmListDeselectAllItems(fileUnits);
   XmListDeleteAllItems(fileUnits);
   XmListAddItems(fileUnits, xmstr_list, cnt, 0);
   XmListSelectPos(fileUnits, 1, False);

   for (i = 0; i < cnt; i++)
      XmStringFree(xmstr_list[i]);
   XtFree((char *)xmstr_list);

   /* make readonly mode again the default */
   XmToggleButtonGadgetSetState(fileTogRo,  True,  False);
   XmToggleButtonGadgetSetState(fileTogUp,  False, False);
   XmToggleButtonGadgetSetState(fileTogNew, False, True);

   /* make menu item insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(browser[which].open_dialog);
}

/***********************************************************************
 *                                                                     *
 *   De-activate some parts of the file selection box when in "new     *
 *   file mode".                                                       *
 *                                                                     *
 ***********************************************************************/
static void toggle_new_file(Widget widget, XtPointer client_data,
                            XmToggleButtonCallbackStruct *call_data)
{
   Widget   w;
   XmString xmstr;

   /*
    * get context of openFbDialog
    */
   UxOpenFbDialogContext = (_UxCopenFbDialog *)UxGetContext(widget);

   if (call_data->set) {
      /* deactivate not-relevant widgets */
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_DIR_LIST);
      XtSetSensitive(w, False);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_DIR_LIST_LABEL);
      XtSetSensitive(w, False);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_FILTER_LABEL);
      XtSetSensitive(w, False);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_FILTER_TEXT);
      XtSetSensitive(w, False);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_LIST);
      XtSetSensitive(w, False);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_LIST_LABEL);
      XtSetSensitive(w, False);

      /* when creating new file set recordlength to 1024 */
      xmstr = XmStringCreateSimple("1024");
      XmListDeselectAllItems(recordLengthList);
      XmListSelectItem(recordLengthList, xmstr, True);
      XmStringFree(xmstr);

      /* change prompt string */
      xmstr = XmStringCreateSimple("Create File");
      XtVaSetValues(fileSelBox, XmNselectionLabelString, xmstr, NULL);
      XmStringFree(xmstr);
   } else {
      /* activate relevant widgets */
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_DIR_LIST);
      XtSetSensitive(w, True);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_DIR_LIST_LABEL);
      XtSetSensitive(w, True);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_FILTER_LABEL);
      XtSetSensitive(w, True);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_FILTER_TEXT);
      XtSetSensitive(w, True);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_LIST);
      XtSetSensitive(w, True);
      w = XmFileSelectionBoxGetChild(fileSelBox, XmDIALOG_LIST_LABEL);
      XtSetSensitive(w, True);

      /* when opening existing file set recordlength to 0 */
      xmstr = XmStringCreateSimple("0");
      XmListDeselectAllItems(recordLengthList);
      XmListSelectItem(recordLengthList, xmstr, True);
      XmStringFree(xmstr);

      /* change prompt string */
      xmstr = XmStringCreateSimple("Open File");
      XtVaSetValues(fileSelBox, XmNselectionLabelString, xmstr, NULL);
      XmStringFree(xmstr);
   }
}

/***********************************************************************
 *                                                                     *
 *   Handle actions in the recordLengthList and set the record length  *
 *   in the recordLength text widget.                                  *
 *                                                                     *
 ***********************************************************************/
static void reclist_cb(Widget w, XtPointer client_data,
                       XmListCallbackStruct *cbs)
{
   char  *lrecl;

   /*
    * get context of openFbDialog
    */
   UxOpenFbDialogContext = (_UxCopenFbDialog *)UxGetContext(w);

   switch (cbs->reason) {
      case XmCR_SINGLE_SELECT:
      case XmCR_BROWSE_SELECT:
      case XmCR_DEFAULT_ACTION:
         lrecl = extract_normal_string(cbs->item);
         XmTextSetString(recordLength, lrecl);
         XtFree(lrecl);
   }
}

/***********************************************************************
 *                                                                     *
 *   Get the arguments for the FILE command and execute the            *
 *   command.                                                          *
 *                                                                     *
 ***********************************************************************/
static void get_openArg(Widget w, MenuCbStruct *item,
                        XmFileSelectionBoxCallbackStruct *call_data)
{
   char     *file, *comstr, *unit, *lrecl;
   int       cnt, flen;
   XmString *xsel;

   /*
    * get context of openFbDialog
    */
   UxOpenFbDialogContext = (_UxCopenFbDialog *)UxGetContext(w);

   file = extract_normal_string(call_data->value);
   flen = strlen(file);
   if (strempty(file)) {
      km_warn_user(w, "You must give a file name.", "");
      XtFree(file);
      return;
   } else if (file[flen-1] == '/') {
     /* User has pressed OK in the FileBrowser without selecting a file */
      km_warn_user(w, "Invalid file name for \'Open File\' (directory).", "");
      XtFree(file);
      return;
   }
#ifdef vms
   if ( strchr( file, ':' ) == NULL && strchr( file, ']' ) == NULL ) {
#else
   if (*file != '/' && *file != '~') {
#endif
      /*
       * if it is not a directory, determine the full pathname
       * of the selection by concatenating it to the "dir" part
       */
      char *dir, *newfile;
      dir = extract_normal_string(call_data->dir);
      newfile = XtMalloc(strlen(dir) + strlen(file) + 1);
      sprintf(newfile, "%s%s", dir, file);
      XtFree(dir);
      XtFree(file);
      file = newfile;
   }

   comstr = XtCalloc(MAX_CMD_LENGTH, 1);

   lrecl = XmTextGetString(recordLength);

   XtVaGetValues(fileUnits, XmNselectedItemCount,   &cnt,
                            XmNselectedItems,       &xsel, NULL);
   unit = extract_normal_string(xsel[0]);

   if (XmToggleButtonGadgetGetState(fileTogRo))
      sprintf(comstr, "Histogram/File %s %s %s -X", unit, file, lrecl);
   else if (XmToggleButtonGadgetGetState(fileTogUp))
      sprintf(comstr, "Histogram/File %s %s %s -XU", unit, file, lrecl);
   else
      sprintf(comstr, "Histogram/File %s %s %s -N", unit, file, lrecl);

   execute_kuip_cmd(comstr);
   km_reset_allbrowser(True);

   XtFree(unit);
   XtFree(lrecl);
   XtFree(file);
   XtFree(comstr);

   popdown_argument_dialog(item);
}

/***********************************************************************
 *                                                                     *
 *   Show (manage) CLOSE argument popup.                               *
 *                                                                     *
 ***********************************************************************/
void show_closeFbDialog(Widget w, caddr_t client_data,
                             XmAnyCallbackStruct *call_data)
{
   Widget        dialog, brows;
   MenuCbStruct *menu_item;
   int           which;

   brows = find_browser(w);
   which = which_browser(brows);

   if (!browser[which].close_dialog) {
      dialog = create_closeFbDialog(brows);
      /*
       *   install callbacks
       */
      menu_item = make_menuStruct(w, dialog);
      InstallMwmCloseCallback(XtParent(dialog), (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_item);
      XtAddCallback(dialog, XmNokCallback,
                    (XtCallbackProc)get_closeArg, menu_item);
      XtAddCallback(dialog, XmNapplyCallback,
                    (XtCallbackProc)get_closeArg, menu_item);
      XtAddCallback(dialog, XmNcancelCallback,
                    (XtCallbackProc)cancel_cb, menu_item);
      XtAddCallback(dialog, XmNhelpCallback,
                    (XtCallbackProc)help_cb, "FORTRAN/CLOSE");

      /* store MenuCbSruct in userData */
      XtVaSetValues(dialog, XmNuserData, menu_item, NULL);

      browser[which].close_dialog = dialog;
   }

   fill_unit_list(browser[which].close_dialog);

   /* make menu item insensitive and popup dialog */
   XtSetSensitive(w, False);
   XtManageChild(browser[which].close_dialog);
}

/***********************************************************************
 *                                                                     *
 *   Fill the list with all open units (currently only open HBOOK 93)  *
 *   files are considered.                                             *
 *                                                                     *
 ***********************************************************************/
static void fill_unit_list(Widget dialog)
{
   Widget               listw;
   XmString             xmstr;
   XmStringTable        xmlist;
   char                 lun[6], *file, *unit;
   int                  i, ll, lf, list, cnt, maxstr;

   list   = FILE_LIST;
   maxstr = MAX_STRING;
   cnt    = 0;
   xmlist = (XmStringTable)XtMalloc(99*sizeof(XmString *));
   for (i = 0; i < 99; i++) {
      if (pawlun.lunit[i] == 3) {
         cnt++;
         sprintf(lun,"LUN%d", i+1);
         ll = strlen(lun);
         file = XtCalloc(MAX_STRING, 1);
         unit = XtCalloc(MAX_STRING+9, 1);
         Gettitle(&list, lun, ll, file, maxstr);
         for( lf = maxstr; lf > 0 && file[lf-1] == ' '; lf-- );
         file[lf] = '\0';
         sprintf(unit, "LUN%d: %s", i+1, file);
         xmlist[cnt-1] = XmStringCreateSimple(unit);
         XtFree(file);
         XtFree(unit);
      }
   }

   listw = XmSelectionBoxGetChild(dialog, XmDIALOG_LIST);
   XmListSetPos(listw, 1);
   XmListDeselectAllItems(listw);
   XmListDeleteAllItems(listw);
   XmListAddItems(listw, xmlist, cnt, 0);
   xmstr = XmStringCreateSimple("");
   XtVaSetValues(dialog, XmNtextString, xmstr, NULL);

   XmStringFree(xmstr);
   for (i = 0; i < cnt; i++)
      XmStringFree(xmlist[i]);
   XtFree((char *)xmlist);
}

/***********************************************************************
 *                                                                     *
 *   Get the arguments for the CLOSE command and execute the           *
 *   command.                                                          *
 *                                                                     *
 ***********************************************************************/
static void get_closeArg(Widget w, MenuCbStruct *item,
                         XmSelectionBoxCallbackStruct *call_data)
{
   char     *file, *comstr;
   int       unit, i;

   file = extract_normal_string(call_data->value);
   if (strempty(file)) {
      km_warn_user(w, "You must give a file name.", "");
      XtFree(file);
      return;
   }

   if (strlen(file) < 4)
      i = sscanf(file, "%d", &unit);
   else
      i = sscanf(file+3, "%d", &unit);

   if (i == 0 || unit < 1 || unit > 99) {
      km_warn_user(w, "Illegal unit specification.", "");
      XtFree(file);
      return;
   }

   comstr = XtCalloc(MAX_CMD_LENGTH, 1);

   sprintf(comstr, "Fortran/Close %d", unit);

   execute_kuip_cmd(comstr);
   km_reset_allbrowser(True);

   XtFree(file);
   XtFree(comstr);

   if (call_data->reason == XmCR_OK)
      popdown_argument_dialog(item);
   else
      fill_unit_list(item->dialog);
}

