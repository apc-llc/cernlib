/*
 * $Id: att_font.c,v 1.2 2000/10/03 14:11:22 couet Exp $
 *
 * $Log: att_font.c,v $
 * Revision 1.2  2000/10/03 14:11:22  couet
 * - Negative fonts where not properlly treated in the fonts panel
 *
 * Revision 1.1.1.1  1996/03/01 11:38:53  mclareni
 * Paw
 *
 */
#include "paw/pilot.h"
/*CMZ :  2.05/01 22/03/94  16.50.52  by  Fons Rademakers*/
/*-- Author :*/
/*****************************************************************************
 *                                                                           *
 *    Manage the font Dialog Box                                             *
 *                                                                           *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <Xm/Xm.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "hmotif/uxxt.h"
#include "hmotif/pawm.h"

#define IDTEXT 101
#define IDLABELS 102
#define IDVALUES 103
#define IDCOMMENT 104
#define IDGLOBAL 105

typedef struct {
   char *label;
   char *code;
   Widget object;
   Widget font;
   Widget pre;
} TEtat;

/* extern functions */

/*  extern Widget  create_AttributesFormDialog();*/
   extern Widget  create_fontDialog();
   extern Widget create_fontSettingsDialog();

/* extern widgets */
extern Display *UxDisplay;
extern Widget FontRowCol,PrecisionRowCol,FontRow,PrecisionMenu,FontMenu;
extern Widget attributesFormDialog;
extern Widget OkFontButton,ResetFontButton,CloseFontButton;
extern Widget TextButton,LabelsButton,CommentButton,ValuesButton,GlobalButton;
extern Widget Font0,Font1,Font2,Font3,Font4,Font5,Font5,Font6,Font7,Font8,
              Font9,Font10,Font11,Font12,Font13;
extern Widget Pre0,Pre1,Pre2;
extern Widget widget_fontSettings,ResetFontSettingsBut,ApplyFontSettingsBut,
              CloseFontSettingsBut;

/* global data */
Widget FontDialog=(Widget) NULL;
Widget widget_fontSettings=(Widget) NULL;

/* global function */


/* local function */
static void MajButton(Widget w, Widget font,Widget pre,int postab);
static void InitTabEtat();
static void VoirFontDialog(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data);
static void OkFontDialog(Widget w,caddr_t client_data,
                         XmAnyCallbackStruct *call_data);
static void CloseFontDialog (Widget w,caddr_t client_data,
                             XmAnyCallbackStruct *call_data);
static void ResetFontDialog (Widget w,caddr_t client_data,
                             XmAnyCallbackStruct *call_data);
static  void ResetGlobal_cb (Widget w,caddr_t client_data,
                             XmAnyCallbackStruct *call_data);
static void popdown_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs);

/* local data */
static Widget CObject; /* currently modified object ie label axis global etc */
static TEtat TabEtat[4];
static char *StrFontTab[]={"-*-times-medium-i-normal--18*",
   "-*-times-bold-r-normal--18*",
   "-*-times-bold-i-normal--18*","-*-helvetica-medium-r-normal--18*",
   "-*-helvetica-medium-o-normal--18*","-*-helvetica-bold-r-normal--18*",
   "-*-helvetica-bold-o-normal--18*","-*-courier-medium-r-normal--18*",
   "-*-courier-medium-o-normal--18*","-*-courier-bold-r-normal--18*",
   "-*-courier-bold-o-normal--18*","-*-symbol-medium-r-normal--18*",
   "-*-times-medium-r-normal--18*","fixed"};

/*****************************************************************************
 *                                                                           *
 *   Get Attributes for Font                                                 *
 *   WARNING: temp must be free by calling function                          *
 *                                                                           *
 *****************************************************************************/
char *get_attributes_fontSettings(char *label)
{
   char *temp;
   float r;

   temp=XtCalloc(5,1);

   strcpy(temp,"?");
   strcat(temp,label);
   Hplset(temp,&r);
   sprintf(temp,"%d",(int) r);
   return temp;
}

/*****************************************************************************
 *                                                                           *
 *   Put Attributes for font                                                 *
 *                                                                           *
 *****************************************************************************/
void put_attributes_fontSettings(char *label,char *value)
{
   int i;
   int fontNumber;
   int currentValue;

   for (i=0;(i<4) && (*label!=*TabEtat[i].code);i++);
   currentValue=atoi(value);
   if (currentValue<10) {
         TabEtat[i].font=Font0;
      } else
      {
        fontNumber= currentValue /10;
        currentValue=currentValue-fontNumber*10;
        switch(fontNumber) {
           case 1:
              TabEtat[i].font=Font1;
              break;
           case 2:
              TabEtat[i].font=Font2;
              break;
           case 3:
              TabEtat[i].font=Font3;
              break;
           case 4:
              TabEtat[i].font=Font4;
              break;
           case 5:
              TabEtat[i].font=Font5;
              break;
           case 6:
              TabEtat[i].font=Font6;
              break;
           case 7:
              TabEtat[i].font=Font7;
              break;
           case 8:
              TabEtat[i].font=Font8;
              break;
           case 9:
              TabEtat[i].font=Font9;
              break;
           case 10:
              TabEtat[i].font=Font10;
              break;
           case 11:
              TabEtat[i].font=Font11;
              break;
           case 12:
              TabEtat[i].font=Font12;
              break;
           case 13:
              TabEtat[i].font=Font13;
              break;
           default :
              fprintf(stderr,"Unknown font in put_attributes_font\n");

        } /* switch */
      } /* else */
      switch ((int) currentValue) {
         case 0:
            TabEtat[i].pre=Pre0;
            break;
         case 1 :
            TabEtat[i].pre=Pre1;
            break;
         case 2 :
            TabEtat[i].pre=Pre2;
            break;
         default :
            fprintf(stderr,"Unknown precision in put_attributes_font\n");
      } /* switch */
       MajButton(TabEtat[i].object,TabEtat[i].font,TabEtat[i].pre,i);
}
/*****************************************************************************
 *                                                                           *
 *   Popdown callback. This callback is called when the FontSettingsDialog   *
 *   widget is popped down.                                                  *
 *                                                                           *
 *****************************************************************************/
static void popdown_cb(Widget w, XtPointer cd, XmAnyCallbackStruct *cbs)
{
  /*
   *   Make sure popup's are unmanaged
   */
   if (FontDialog) {
      XtUnmanageChild(FontDialog);
      XtSetSensitive(FontRow, True);
   }
}

/*****************************************************************************
 *                                                                           *
 *        Update button with the new font and precision                      *
 *                                                                           *
 *****************************************************************************/
static void MajButton(Widget w, Widget font,Widget pre,int postab)
{
   XmFontList police;
   XFontStruct *info_police;
   char strlabel[20];
   int prenum,policenum;

   XtVaGetValues(pre,XmNuserData,&prenum,NULL);
   prenum--;
   XtVaGetValues(font,XmNuserData,&policenum,NULL);
   policenum--;
   if (( info_police=XLoadQueryFont(UxDisplay,StrFontTab[policenum]))== NULL)
      fprintf(stderr,"Unable to load font %s \n ",StrFontTab[policenum]);
   police=XmFontListCreate(info_police,XmSTRING_DEFAULT_CHARSET);
   XtVaSetValues(w,XmNfontList,police,NULL);
   /* XFreeFont(UxDisplay,info_police);*/

   sprintf(strlabel,"%s (%d)",TabEtat[postab].label,prenum);
   XtVaSetValues(w,XmNlabelString,XmStringCreateLtoR(strlabel,
                 XmSTRING_DEFAULT_CHARSET),NULL);
   XmFontListFree(police);
}

/*****************************************************************************
 *                                                                           *
 *        Initialize the array : state of the font                           *
 *                                                                           *
 *****************************************************************************/
static void InitTabEtat()
{
   int i;
   char temp[6];
   float currentValue;
   int fontNumber;

   TabEtat[0].label="Text and title";
   TabEtat[0].object=TextButton;
   TabEtat[0].font=Font0;  /* keep this value  :
                              useful if invalid value return by getatt */
   TabEtat[0].pre=Pre0; /* idem */
   TabEtat[0].code="T";

   TabEtat[1].label="Axis Labels";
   TabEtat[1].object=LabelsButton;
   TabEtat[1].font=Font0;
   TabEtat[1].pre=Pre0;
   TabEtat[1].code="L";

   TabEtat[2].label="Comment";
   TabEtat[2].object=CommentButton;
   TabEtat[2].font=Font0;
   TabEtat[2].pre=Pre0;
   TabEtat[2].code="C";

   TabEtat[3].label="Axis Values";
   TabEtat[3].object=ValuesButton;
   TabEtat[3].font=Font0;
   TabEtat[3].pre=Pre0;
   TabEtat[3].code="V";

   TabEtat[4].label="Global Title";
   TabEtat[4].object=GlobalButton;
   TabEtat[4].font=Font0;
   TabEtat[4].pre=Pre0;
   TabEtat[4].code="G";

   for (i=0;i<=4;i++) {
      sprintf(temp,"?%sFON",TabEtat[i].code);
      Hplset(temp,&currentValue);
      if (abs(currentValue)<10) {
         TabEtat[i].font=Font0;
      } else {
        fontNumber= abs(currentValue) /10;
        currentValue=abs(currentValue)-fontNumber*10;
        switch(fontNumber) {
           case 1:
              TabEtat[i].font=Font1;
              break;
           case 2:
              TabEtat[i].font=Font2;
              break;
           case 3:
              TabEtat[i].font=Font3;
              break;
           case 4:
              TabEtat[i].font=Font4;
              break;
           case 5:
              TabEtat[i].font=Font5;
              break;
           case 6:
              TabEtat[i].font=Font6;
              break;
           case 7:
              TabEtat[i].font=Font7;
              break;
           case 8:
              TabEtat[i].font=Font8;
              break;
           case 9:
              TabEtat[i].font=Font9;
              break;
           case 10:
              TabEtat[i].font=Font10;
              break;
           case 11:
              TabEtat[i].font=Font11;
              break;
           case 12:
              TabEtat[i].font=Font12;
              break;
           case 13:
              TabEtat[i].font=Font13;
              break;
           default :
              fprintf(stderr,"Unknown font in InitTabEtat\n");
        } /* switch */
      } /* else */
      switch ((int) currentValue) {
         case 0:
            TabEtat[i].pre=Pre0;
            break;
         case 1 :
            TabEtat[i].pre=Pre1;
            break;
         case 2 :
            TabEtat[i].pre=Pre2;
            break;
         default :
            fprintf(stderr,"Unknown precision in InitTabEtat\n");
      } /* switch */
       MajButton(TabEtat[i].object,TabEtat[i].font,TabEtat[i].pre,i);
   } /* for */
}

/*****************************************************************************
 *                                                                           *
 *     Popup the Font Dialog                                                 *
 *                                                                           *
 *****************************************************************************/
static void VoirFontDialog(Widget w,caddr_t client_data,
                           XmAnyCallbackStruct *call_data)
{
   int i=0;

   CObject=w;
   while (TabEtat[i].object != CObject) i++;


   XtVaSetValues(FontMenu,XmNmenuHistory,TabEtat[i].font,NULL);

   XtVaSetValues(PrecisionMenu,XmNmenuHistory,TabEtat[i].pre,NULL);

   XtVaSetValues(FontDialog,XmNdialogTitle,
           XmStringCreateLtoR(TabEtat[i].label,XmSTRING_DEFAULT_CHARSET),NULL);
   XtSetSensitive(FontRow,False);
   XtManageChild(FontDialog);
}

/*****************************************************************************
 *                                                                           *
 *         Manage OK button of font dialog                                   *
 *                                                                           *
 *****************************************************************************/
static  void OkFontDialog(Widget w,caddr_t client_data,
                          XmAnyCallbackStruct *call_data)
{
   int val1,val2;
   int i=0;
   Widget WPre,WFont;
   char *ComStr;

   XtVaGetValues(FontRowCol,XmNmenuHistory,&WFont,NULL);
   XtVaGetValues(PrecisionRowCol,XmNmenuHistory,&WPre,NULL);

   while (TabEtat[i].object != CObject) i++;
   TabEtat[i].font=WFont;
   TabEtat[i].pre=WPre;

   XtVaGetValues(WFont,XmNuserData,&val1,NULL);
   XtVaGetValues(WPre,XmNuserData,&val2,NULL);

   MajButton(CObject,TabEtat[i].font,TabEtat[i].pre,i);
   ComStr=XtCalloc(MAX_CMD_LENGTH,1);
   sprintf(ComStr,"SET %sFON  %d%d",
           TabEtat[i].code,(val1==14) ? 0 : val1 ,val2-1);

   execute_kuip_cmd(ComStr);
   if (appres.auto_refresh) {
      plot_current_histo();
   }
   XtFree(ComStr);

   XtSetSensitive(FontRow,True);
   XtUnmanageChild(FontDialog);
}

/*****************************************************************************
 *                                                                           *
 *    Manage Reset Button of fontSettings = Global Reset                     *
 *                                                                           *
 *****************************************************************************/
static  void ResetGlobal_cb (Widget w,caddr_t client_data,
                             XmAnyCallbackStruct *call_data)
{
   execute_kuip_cmd("SET CFON");
   execute_kuip_cmd("SET TFON");
   execute_kuip_cmd("SET LFON");
   execute_kuip_cmd("SET VFON");
   execute_kuip_cmd("SET GFON");
   InitTabEtat();
  /*      if (appres.auto_refresh) { */
  /* -999 because of Global reset from histo.c don't want to replot */
   if ( (appres.auto_refresh) && (call_data->reason != -999)) {
      plot_current_histo();
   }
}

/*****************************************************************************
 *                                                                           *
 *    Manage Apply Button of fontSettings                                    *
 *                                                                           *
 *****************************************************************************/
static  void ApplyFontSettings_cb (Widget w,caddr_t client_data,
                                   XmAnyCallbackStruct *call_data)
{
   plot_current_histo();
}

/*****************************************************************************
 *                                                                           *
 *    Manage Close Button of font dialog                                     *
 *                                                                           *
 *****************************************************************************/
static  void CloseFontDialog (Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data)
{
   XtUnmanageChild(FontDialog);
   XtSetSensitive(FontRow,True);
}

/*****************************************************************************
 *                                                                           *
 *    Manage Reset Button of fontDialog                                      *
 *                                                                           *
 *****************************************************************************/
static  void ResetFontDialog (Widget w,caddr_t client_data,
                              XmAnyCallbackStruct *call_data)
{
   int i=0;

   while (TabEtat[i].object != CObject) i++;
   TabEtat[i].font=Font0;
   TabEtat[i].pre=Pre0;
   XtVaSetValues(FontMenu,XmNmenuHistory,TabEtat[i].font,NULL);
   XtVaSetValues(PrecisionMenu,XmNmenuHistory,TabEtat[i].pre,NULL);
}

/***********************************************************************
 *                                                                     *
 *   Unmanage dialog. This routine should be called by the             *
 *   popdown_cb routine of the parent.                                 *
 *                                                                     *
 ***********************************************************************/
void unmanage_fontSettings(void)
{
   if (widget_fontSettings) {
      MenuCbStruct  *item;
      XtVaGetValues(widget_fontSettings, XmNuserData, &item, NULL);
      popdown_argument_dialog(item);
   }
}

/*****************************************************************************
 *                                                                           *
 *      Initialize the font dialog box and popupInterface                    *
 *                                                                           *
 *****************************************************************************/
void show_fontSettings(Widget w)
{

   MenuCbStruct *menu_button;

   if (!widget_fontSettings) {

      widget_fontSettings = create_fontSettingsDialog();

      menu_button = make_menuStruct(w, widget_fontSettings);
      InstallMwmCloseCallback(XtParent(widget_fontSettings),
                              (XtCallbackProc)cancel_cb,
                              (XtPointer)menu_button);
      XtVaSetValues(widget_fontSettings, XmNuserData, menu_button, NULL);

      FontDialog = create_fontDialog();

      InitTabEtat();
      if (appres.auto_refresh)
         XtSetSensitive(ApplyFontSettingsBut,False);
      else
         XtSetSensitive(ApplyFontSettingsBut,True);

      /****
       * Callback
       ******/

      XtAddCallback(XtParent(widget_fontSettings), XmNpopdownCallback,
                    (XtCallbackProc)popdown_cb, NULL);
      XtAddCallback(TextButton,XmNactivateCallback,
                    (XtCallbackProc) VoirFontDialog,NULL);
      XtAddCallback(LabelsButton,XmNactivateCallback,
                    (XtCallbackProc) VoirFontDialog,NULL);
      XtAddCallback(CommentButton,XmNactivateCallback,
                    (XtCallbackProc) VoirFontDialog,NULL);
      XtAddCallback(ValuesButton,XmNactivateCallback,
                    (XtCallbackProc) VoirFontDialog,NULL);
      XtAddCallback(GlobalButton,XmNactivateCallback,
                    (XtCallbackProc) VoirFontDialog,NULL);
      XtAddCallback(OkFontButton,XmNactivateCallback,
                    (XtCallbackProc) OkFontDialog,NULL);
      XtAddCallback(CloseFontButton,XmNactivateCallback,
                    (XtCallbackProc) CloseFontDialog,NULL);
      XtAddCallback(ResetFontButton,XmNactivateCallback,
                    (XtCallbackProc) ResetFontDialog,NULL);
      XtAddCallback(CloseFontSettingsBut,XmNactivateCallback,
                    (XtCallbackProc) cancel_cb,menu_button);
      XtAddCallback(ResetFontSettingsBut,XmNactivateCallback,
                    (XtCallbackProc) ResetGlobal_cb,NULL);
      XtAddCallback(ApplyFontSettingsBut,XmNactivateCallback,
                    (XtCallbackProc) ApplyFontSettings_cb,NULL);
   }

   XtManageChild(widget_fontSettings);
   XtSetSensitive(w,False);
}

